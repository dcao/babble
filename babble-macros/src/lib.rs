//! Procedural macros for `babble`.

#![warn(
    clippy::all,
    clippy::pedantic,
    anonymous_parameters,
    elided_lifetimes_in_paths,
    missing_copy_implementations,
    missing_debug_implementations,
    single_use_lifetimes,
    trivial_casts,
    unreachable_pub,
    unused_lifetimes,
    missing_docs
)]

use proc_macro::TokenStream;
use quote::quote;
use syn::parse_macro_input;

mod parse;

use parse::RewriteRules;

/// Convenience macro for defining a [`Vec`] of `Rewrite<L, N>`s.
///
/// # Examples
/// ```ignore
/// rewrite_rules! {
///     add_commute: "(+ ?a ?b)" => "(+ ?b ?a)";
///     invert_twice: "?a" <=> "(/ 1 (/ 1 ?a))" if is_not_zero("?a");
///     mult_one: "(* ?a 1)" <=> "?a";
///     div_self: "(/ ?a ?a)" => "1" if is_not_zero("?a");
/// }
/// ```
#[proc_macro]
pub fn rewrite_rules(tokens: TokenStream) -> TokenStream {
    let input = parse_macro_input!(tokens as RewriteRules);
    let output = quote! { #input };
    TokenStream::from(output)
}
