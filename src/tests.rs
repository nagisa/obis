use crate::{CodeParseError, ReducedCodeParseError};

macro_rules! test_valid_code {
    ($name: ident, $a:literal - $b:literal : $c:literal . $d:literal . $e:literal * $f:literal + $leftover: literal) => {
        #[test]
        fn $name() {
            let mut input = format!("{}-{}:{}.{}.{}*{}", $a, $b, $c, $d, $e, $f).into_bytes();
            input.extend($leftover);
            match crate::Code::parse(&input) {
                Err(e) => panic!("expected successful code parse, got error {e:?}"),
                Ok((code, leftover)) => {
                    println!("parsed: {code:?} with leftover bytes: {leftover:?}");
                    assert_eq!(code.a, $a);
                    assert_eq!(code.b, $b);
                    assert_eq!(code.c, $c);
                    assert_eq!(code.d, $d);
                    assert_eq!(code.e, $e);
                    assert_eq!(code.f, $f);
                    assert_eq!(leftover, $leftover);
                }
            }
        }
    };
}

macro_rules! test_invalid_code {
    ($name: ident, $input: literal, $error: expr) => {
        #[test]
        fn $name() {
            match crate::Code::parse($input) {
                Err(e) => assert_eq!(e, $error),
                Ok((code, leftover)) => {
                    panic!("expected an error: parsed {code:?} with leftover bytes: {leftover:?}");
                }
            }
        }
    };
}

test_valid_code!(test_code_all_zero, 0-0:0 .0 .0*0 + b"");
test_valid_code!(test_code_all_max, 255-255:255 .255 .255*255 + b"");
test_valid_code!(test_code_diff_groups, 5-4:3 .2 .1*9 + b",9");
test_invalid_code!(
    test_code_truncated,
    b"1-2:3.4.5*",
    CodeParseError::GroupF(10)
);
test_invalid_code!(
    test_code_missing_start,
    b"-2:3.4.5*6",
    CodeParseError::GroupA(0)
);
test_invalid_code!(
    test_code_group_a_too_large,
    b"256-2:3.4.5*6",
    CodeParseError::GroupA(0)
);
test_invalid_code!(
    test_code_group_b_too_large,
    b"1-99999999999999999999999999999999999999999999999:3.4.5*6",
    CodeParseError::GroupB(2)
);
test_invalid_code!(
    test_code_group_c_too_large,
    b"1-2:300.4.5*6",
    CodeParseError::GroupC(4)
);
test_invalid_code!(
    test_code_group_d_too_large,
    b"1-2:3.400.5*6",
    CodeParseError::GroupD(6)
);
test_invalid_code!(
    test_code_group_e_too_large,
    b"1-2:3.4.500*6",
    CodeParseError::GroupE(8)
);
test_invalid_code!(
    test_code_group_f_too_large,
    b"1-2:3.4.5*600",
    CodeParseError::GroupF(10)
);
test_invalid_code!(
    test_code_interior_whitespace,
    b"1 -2:3.4.5*6",
    CodeParseError::ExpectedDash(1)
);
test_invalid_code!(
    test_code_interior_whitespace_2,
    b"1- 2:3.4.5*6",
    CodeParseError::GroupB(2)
);
test_invalid_code!(
    test_code_interior_whitespace_3,
    b"1-2 :3.4.5*6",
    CodeParseError::ExpectedColon(3)
);
test_invalid_code!(
    test_code_interior_whitespace_4,
    b"1-2: 3.4.5*6",
    CodeParseError::GroupC(4)
);
test_invalid_code!(
    test_code_mixed_separators_1,
    b"1.2.3.4.5.6",
    CodeParseError::ExpectedDash(1)
);
test_invalid_code!(
    test_code_mixed_separators_2,
    b"1-2.3.4.5*6",
    CodeParseError::ExpectedColon(3)
);

test_invalid_code!(
    test_code_invalid_separators_1,
    b"1!2:3.4.5*6",
    CodeParseError::ExpectedDash(1)
);

test_invalid_code!(
    test_code_invalid_separators_2,
    b"1-2:3#4.5*6",
    CodeParseError::ExpectedDot(5)
);

macro_rules! test_valid_reduced_code {
    ($name: ident, $input: literal => $(a: $a:expr,)? $(b: $b:expr,)? c: $c:literal, d: $d:literal $(,e: $e:expr)? $(,f: $f:literal)? $(,manual_reset: $mr:literal)? ,$leftover:literal) => {
        #[test]
        fn $name() {
            match crate::ReducedCode::parse($input) {
                Err(e) => panic!("expected successful code parse, got error {e:?}"),
                Ok((code, leftover)) => {
                    println!("parsed: {code:?} with leftover bytes: {leftover:?}");
                    $(assert_eq!(code.a(), Some($a));)?
                    $(assert_eq!(code.b(), Some($b));)?
                    assert_eq!(code.c(), $c);
                    assert_eq!(code.d(), $d);
                    $(assert_eq!(code.e(), Some($e));)?
                    $(assert_eq!(code.f(), Some($f));)?
                    $(assert_eq!(code.manual_reset(), $mr);)?
                    assert_eq!(leftover, $leftover);
                }
            }
        }
    };
}

macro_rules! test_invalid_reduced_code {
    ($name: ident, $input: literal, $error: expr) => {
        #[test]
        fn $name() {
            match crate::ReducedCode::parse($input) {
                Err(e) => assert_eq!(e, $error),
                Ok((code, leftover)) => {
                    panic!("expected an error: parsed {code:?} with leftover bytes: {leftover:?}");
                }
            }
        }
    };
}

test_valid_reduced_code!(test_reduced_code_full, b"1-2:3.4.5*6" => a: 1, b: 2, c: 3, d: 4, e: 5, f: 6, manual_reset: false, b"");
test_valid_reduced_code!(test_reduced_code_no_a, b"1:2.3.4*5" => b: 1, c: 2, d: 3, e: 4, f: 5, manual_reset: false, b"");
test_valid_reduced_code!(test_reduced_code_no_b, b"1-2.3.4*5" => a: 1, c: 2, d: 3, e: 4, f: 5, manual_reset: false, b"");
test_valid_reduced_code!(test_reduced_code_no_ab, b"1.2.3*4" => c: 1, d: 2, e: 3, f: 4, manual_reset: false, b"");
test_valid_reduced_code!(test_reduced_code_no_abe, b"1.2*3" => c: 1, d: 2, f: 3, manual_reset: false, b"");
test_valid_reduced_code!(test_reduced_code_no_abe_mr, b"1.2&3" => c: 1, d: 2, f: 3, manual_reset: true, b"");
test_valid_reduced_code!(test_reduced_code_no_f, b"1:2.3.4" => b: 1, c: 2, d: 3, e: 4, manual_reset: false, b"");
test_valid_reduced_code!(test_reduced_code_cd, b"1.2" => c: 1, d: 2, manual_reset: false, b"");
test_valid_reduced_code!(test_reduced_code_cd_leftovers, b"1.2(0*kWh)" => c: 1, d: 2, manual_reset: false, b"(0*kWh)");
test_valid_reduced_code!(test_reduced_code_full_leftovers, b"1-2:3.4.5&6(0*kWh)" => a: 1, b: 2, c: 3, d: 4, e: 5, f: 6, manual_reset: true, b"(0*kWh)");
test_valid_reduced_code!(test_reduced_code_no_e_leftovers, b"1-2:3.4*6*6" => a: 1, b: 2, c: 3, d: 4, f: 6, b"*6");
test_valid_reduced_code!(test_reduced_code_invalid_separators_leftover, b"1-2:3.4:6" => a: 1, b: 2, c: 3, d: 4, b":6");

test_invalid_reduced_code!(
    test_reduced_code_no_d,
    b"1-2:4..5*6",
    ReducedCodeParseError::Group(6)
);

test_invalid_reduced_code!(
    test_reduced_code_no_cd,
    b"1-2:4*6",
    ReducedCodeParseError::GroupCMissing(5)
);
