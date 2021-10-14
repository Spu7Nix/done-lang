use crate::interpreter::Value;

macro_rules! check_value {
    ($val:expr, $type:ident) => {
        if let val_variant!($type get a) = $val {
            a
        } else {
            panic!("expected {}", stringify!($type))
        }
    };
}

macro_rules! val_variant {
    (number $(get $out:ident)? $(make $in:expr)?) => {
        Value::Number($($out)? $($in)?)
    };

    (string $(get $out:ident)? $(make $in:expr)?) => {
        Value::Str($($out)? $($in)?)
    }; // ...
}

macro_rules! builtins {
    {
        $( fn $($name:ident)+ ($($argtype:ident $argname:ident),*) -> $ret_type:ident => $output:expr,)*
    } => {
        pub const BUILTINS: &[(&[&str; 2], &[&str; 2], fn(Vec<Value>) -> Value)] = &[
            $(
                (
                    &[$(stringify!($name)),+],
                    &[$(stringify!($argtype)),*],
                    | args | {
                        let mut i = 0;
                        $(
                            #[allow(non_snake_case)]
                            let $argname = check_value!(args[i], $argtype);
                            i += 1;
                        )*
                        val_variant!($ret_type make $output)
                    }
                ),
            )*
        ];
    };
}

builtins! {
    fn added with (number A, number B) -> number => A + B,
    fn subtracted by (number A, number B) -> number => A - B,
    fn multiplied by (number A, number B) -> number => A * B,
    fn divided by (number A, number B) -> number => A / B,
}
