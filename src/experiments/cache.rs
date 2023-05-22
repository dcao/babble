//! Provide a mechanism to cache the results of experiments.

use super::Summary;
use serde::{Deserialize, Serialize};
use std::{
    collections::BTreeMap,
    fs,
    marker::PhantomData,
    path::{Path, PathBuf},
};
use time::{
    format_description::well_known::{iso8601, Iso8601},
    OffsetDateTime,
};

const CACHE_DIR: &str = "harness/data_gen/cache";
const ISO8601_CONFIG: iso8601::EncodedConfig =
    iso8601::Config::DEFAULT.set_use_separators(false).encode();
const DATE_FORMAT: Iso8601<ISO8601_CONFIG> = Iso8601;

/// A cache of experiment results.
#[derive(Clone, Debug)]
pub struct Cache<Op> {
    path: PathBuf,
    index: BTreeMap<String, PathBuf>,
    phantom: PhantomData<Op>,
}

// This lint gives false positives for higher-rank trait bounds.
#[allow(single_use_lifetimes)]
impl<Op> Cache<Op>
where
    Op: Serialize + for<'b> Deserialize<'b>,
{
    /// Load an experiment cache from the given directory. If the directory does
    /// not already contain a cache, create a new empty cache in that directory.
    ///
    /// # Errors
    ///
    /// Errors if the directory does not exist, can't be opened, or contains a
    /// malformed cache.
    pub fn new() -> anyhow::Result<Self> {
        let dir_name = OffsetDateTime::now_utc().format(&DATE_FORMAT)?;
        let path = Path::new(CACHE_DIR).join(dir_name);
        Self::from_dir(path)
    }

    /// Load an experiment cache from the given directory. If the directory does
    /// not already contain a cache, create a new empty cache in that directory.
    ///
    /// # Errors
    ///
    /// Errors if the directory does not exist, can't be opened, or contains a
    /// malformed cache.
    pub fn from_dir(path: PathBuf) -> anyhow::Result<Self> {
        fs::create_dir_all(&path)?;

        let mut cache = Self {
            path,
            index: BTreeMap::new(),
            phantom: PhantomData,
        };

        let index_file = cache.index_file();
        if index_file.exists() {
            let index_str = fs::read_to_string(&index_file)?;
            cache.index = ron::from_str(&index_str)?;
        } else {
            cache.flush()?;
        };

        Ok(cache)
    }

    /// Return the directory where the cache is stored.
    #[must_use]
    pub fn path(&self) -> &Path {
        self.path.as_ref()
    }

    fn index_file(&self) -> PathBuf {
        self.path.join("index.ron")
    }

    fn flush(&self) -> anyhow::Result<()> {
        let serialized_index = ron::to_string(&self.index)?;
        fs::write(self.index_file(), serialized_index)?;
        Ok(())
    }

    /// Return `true` if the cache contains results for `experiment`. If it does
    /// not, return `false`.
    #[must_use]
    pub fn contains(&self, experiment: &str) -> bool {
        self.index.contains_key(experiment)
    }

    /// Add an experiment to the cache, identified by the string `name`.
    ///
    /// # Errors
    ///
    /// Errors if there is a problem accessing the cache.
    pub fn insert<S: Into<String>>(
        &mut self,
        experiment: S,
        result: &Summary<Op>,
    ) -> anyhow::Result<()> {
        let experiment = experiment.into();
        let experiment_file = self.path.join(format!("experiment-{}.ron", &experiment));
        let serialized_result = ron::to_string(&result)?;
        fs::write(&experiment_file, serialized_result)?;
        self.index.insert(experiment, experiment_file);
        self.flush()
    }

    /// Return the results of the given `experiment`. If the results have not
    /// been cached, return `None`.
    ///
    /// # Errors
    ///
    /// Errors if the cache is malformed.
    pub fn get(&self, experiment: &str) -> anyhow::Result<Option<Summary<Op>>> {
        if let Some(file) = self.index.get(experiment) {
            let experiment_str = fs::read_to_string(file)?;
            Ok(Some(ron::from_str(&experiment_str)?))
        } else {
            Ok(None)
        }
    }

    /// Return the results of the given `experiment`. If the results have not
    /// been cached, run `default` to get the results, add them to the cache,
    /// and then return them.
    ///
    /// # Errors
    ///
    /// Errors if the cache is malformed.
    pub fn get_or_insert_with<F: FnOnce() -> Summary<Op>>(
        &mut self,
        experiment: &str,
        default: F,
    ) -> anyhow::Result<Summary<Op>> {
        if let Some(summary) = self.get(experiment)? {
            Ok(summary)
        } else {
            let summary = default();
            self.insert(experiment, &summary)?;
            Ok(summary)
        }
    }
}
