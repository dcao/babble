#[macro_export]
macro_rules! rewrite_rule {
    (
        $name:ident : $lhs:literal => $rhs:literal
            $(where $cond:expr)?
    ) => {
        ::egg::rewrite!(::std::stringify!($name); $lhs => $rhs $(if $cond)?)
    };
    (
        $name:ident : $lhs:literal <=> $rhs:literal
    ) => {
        ::egg::rewrite!(::std::stringify!($name); $lhs => $rhs)
    };
    }

#[macro_export]
macro_rules! rewrite_rules {
    (
        $($name:ident : $lhs:tt $arrow:tt $rhs:tt $(where $cond:tt)?;)*
    ) => {
        ::std::vec![$($crate::rewrite_rule!($name : $lhs $arrow $rhs $(where $cond)?)),*]
    };
}
