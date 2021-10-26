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
    };

    (list $(get $out:ident)? $(make $in:expr)?) => {
        Value::List($($out)? $($in)?)
    };

    (make $in:expr) => {
        $in
    };
}

macro_rules! builtin_funcs {
    {
        $( fn $($name:ident)+ ($($argtype:ident $argname:ident),*) -> $ret_type:ident => $output:expr,)*
    } => {
        pub const BUILTIN_FUNCS: &[(&[&str], &[&str], fn(Vec<Value>) -> Value)] = &[
            $(
                (
                    &[$(stringify!($name)),+],
                    &[$(stringify!($argtype)),*],
                    | args | {
                        let mut i = 0;
                        $(
                            #[allow(non_snake_case)]
                            let $argname = check_value!(&args[i], $argtype);
                            i += 1;
                        )*
                        val_variant!($ret_type make $output)
                    }
                ),
            )*
        ];
    };
}

macro_rules! builtin_patterns {
    {
        $(is $($name:ident)+ ($($argtype:ident $argname:ident),*) => $output:expr,)*
    } => {
        pub const BUILTIN_PATTERNS: &[(&[&str], &[&str], fn(Vec<Value>) -> bool)] = &[
            $(
                (
                    &[$(stringify!($name)),+],
                    &[$(stringify!($argtype)),*],
                    | args | {
                        let mut i = 0;
                        $(
                            #[allow(non_snake_case)]
                            let $argname = check_value!(&args[i], $argtype);
                            i += 1;
                        )*
                        $output
                    }
                ),
            )*
        ];
    };
}

macro_rules! builtin_props {
    {
        $($($name:ident)+ : of $argtype:ident $argname:ident $( -> $ret_type:ident)? => $output:expr,)*
    } => {
        pub const BUILTIN_PROPS: &[(&[&str], &str, fn(Value) -> Value)] = &[
            $(
                (
                    &[$(stringify!($name)),+],
                    stringify!($argtype),
                    | args | {


                        #[allow(non_snake_case)]
                        let $argname = check_value!(&args, $argtype);


                        val_variant!($($ret_type)? make $output)
                    }
                ),
            )*
        ];
    };
}

builtin_funcs! {
    fn added with (number A, number B) -> number => A + B,
    fn subtracted by (number A, number B) -> number => A - B,
    fn multiplied by (number A, number B) -> number => A * B,
    fn divided by (number A, number B) -> number => A / B,

    fn floored (number A) -> number => A.floor(),
    fn ceiled (number A) -> number => A.ceil(),
    fn rounded (number A) -> number => A.round(),

    fn uppercased (string A) -> string => A.to_uppercase(),
    fn lowercased (string A) -> string => A.to_lowercase(),

    fn joined with (list A, string B) -> string => {
        let strings = A.iter().map(|el| if let Value::Str(s) = el { s.clone() } else { panic!("expected string") }).collect::<Vec<_>>();
        strings.join(B)
    },
}

builtin_patterns! {
    is equal to (number A, number B) => (A - B).abs() < f64::EPSILON,
    is greater than (number A, number B) => A > B,
    is less than (number A, number B) => A < B,
    is empty (list A) => A.is_empty(),
}

builtin_props! {
    length : of list A -> number => A.len() as f64,
    first item : of list A => A.first().expect("no first element in empty list").clone(),
}
