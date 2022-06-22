//! Provide a mechanism to cache the results of experiments.

use super::Summary;
use serde::{Deserialize, Serialize};
use std::{
    collections::BTreeMap,
    fs,
    marker::PhantomData,
    path::{Path, PathBuf},
};

/// A cache of experiment results.
#[derive(Clone, Debug)]
pub struct ExperimentCache<'a, Op> {
    path: &'a Path,
    index: BTreeMap<String, PathBuf>,
    phantom: PhantomData<Op>,
}

// This lint gives false positives for higher-rank trait bounds.
#[allow(single_use_lifetimes)]
impl<'a, Op> ExperimentCache<'a, Op>
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
    pub fn new<P: AsRef<Path>>(path: &'a P) -> anyhow::Result<Self> {
        let mut cache = Self {
            path: path.as_ref(),
            index: BTreeMap::new(),
            phantom: PhantomData,
        };

        let index_file = cache.index_file();
        if !index_file.exists() {
            cache.flush()?;
        } else {
            let index_str = fs::read_to_string(&index_file)?;
            cache.index = ron::from_str(&index_str)?
        };

        Ok(cache)
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
