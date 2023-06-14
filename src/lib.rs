#![cfg_attr(not(test), no_std)]
#![doc = include_str!("../README.mkd")]

#[cfg(test)]
mod tests;

/// A representation of an OBIS code, as defined by the IEC 62056-61 standard.
pub struct Code {
    /// The value group A defines the media (energy type) to which the metering is related.
    ///
    /// Non-media related information is handled as abstract data.
    ///
    /// 0. abstract objects;
    /// 1. electricity;
    /// 6. heat;
    /// 7. gas;
    /// 8. water…
    pub a: u8,
    /// The value group B defines the channel number.
    ///
    /// I.e. the number of the input of a metering equipment having several inputs for the
    /// measurement of energy of the same or different types (e.g. in data concentrators,
    /// registration units). Data from different sources can thus be identified. The definitions
    /// for this value group are independent from the value group A.
    pub b: u8,
    /// The value group C defines the abstract or physical data items related to the information
    /// source concerned.
    ///
    /// For example current, voltage, power, volume, temperature. The definitions depend on the
    /// value of the value group A. Further processing, classification and storage methods are
    /// defined by value groups D, E and F.
    ///
    /// For abstract data, value groups D to F provide further classification of data identified by
    /// value groups A to C.
    pub c: u8,
    /// The value group D defines types, or the result of the processing of physical quantities
    /// identified with the value groups A and C, according to various specific algorithms.
    ///
    /// The algorithms can deliver energy and demand quantities as well as other physical
    /// quantities.
    pub d: u8,
    /// The value group E defines further processing or classification of quantities identified by
    /// value groups A to D.
    pub e: u8,
    /// The value group F defines the storage of data, identified by value groups A to E, according
    /// to different billing periods. Where this is not relevant, this value group can be used for
    /// further classification.
    pub f: u8,
}

/// A representation of a reduced OBIS code, as defined by the Annex A of the IEC 62056-61 standard.
pub struct ReducedCode {
    /// OBIS code is only required to hold C and D groups.
    ///
    /// All other groups may be absent, but are still required to uniquely represent a COSEM object
    /// based on contextual information.
    ///
    /// * LSB (0th bit) being set specifies that group A is present;
    /// * Next available LSB (1st bit) set –> group B is present;
    /// * 2nd bit -> gruop C is present;
    /// * ...
    /// * MSB (7th bit) being set specifies that the delimiter before group F is `&` instead of `*`.
    flags: u8,
    code: Code,
}

/// Could not parse a [`Code`].
#[derive(Debug, PartialEq, Eq)]
pub enum CodeParseError {
    /// Could not parse the digits for group A at offset `{0}`
    GroupA(usize),
    /// Expected a `-` separator at offset `{0}`
    ExpectedDash(usize),
    /// Could not parse the digits for group B at offset `{0}`
    GroupB(usize),
    /// Expected a `:` separator at offset `{0}`
    ExpectedColon(usize),
    /// Could not parse the digits for group C at offset `{0}`
    GroupC(usize),
    /// Expected a `.` separator at offset `{0}`
    ExpectedDot(usize),
    /// Could not parse the digits for group D at offset `{0}`
    GroupD(usize),
    /// Could not parse the digits for group E at offset `{0}`
    GroupE(usize),
    /// Expected a `*` separator at offset `{0}`
    ExpectedStar(usize),
    /// Could not parse the digits for group F at offset `{0}`
    GroupF(usize),
}

/// Could not parse a [`ReducedCode`].
#[derive(Debug, PartialEq, Eq)]
pub enum ReducedCodeParseError {
    /// Could not parse the group digits at offset `{0}`
    Group(usize),
    /// Could not parse an expected separator at offset `{0}`
    Separator(usize),
    /// Could not parse a required group C at offset `{0}`
    GroupCMissing(usize),
}

impl core::fmt::Display for Code {
    fn fmt(&self, fmt: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        let Self { a, b, c, d, e, f } = self;
        core::fmt::Write::write_fmt(fmt, format_args!("{a}-{b}:{c}.{d}.{e}*{f}"))
    }
}

impl core::fmt::Debug for Code {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        f.write_str("Code(")?;
        core::fmt::Display::fmt(self, f)?;
        f.write_str(")")
    }
}

impl core::fmt::Display for ReducedCode {
    fn fmt(&self, fmt: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        let Self {
            flags,
            code: Code { a, b, c, d, e, f },
        } = self;
        if (flags & Self::FLAG_HAS_A) != 0 {
            core::fmt::Write::write_fmt(fmt, format_args!("{a}-"))?;
        }
        if (flags & Self::FLAG_HAS_B) != 0 {
            core::fmt::Write::write_fmt(fmt, format_args!("{b}:"))?;
        }
        core::fmt::Write::write_fmt(fmt, format_args!("{c}.{d}"))?;
        if (flags & Self::FLAG_HAS_E) != 0 {
            core::fmt::Write::write_fmt(fmt, format_args!(".{e}"))?;
        }
        if (flags & Self::FLAG_HAS_F) != 0 {
            if *flags >= Self::FLAG_MANUAL_RESET {
                core::fmt::Write::write_fmt(fmt, format_args!("&{f}"))?;
            } else {
                core::fmt::Write::write_fmt(fmt, format_args!("*{f}"))?;
            }
        }
        Ok(())
    }
}

impl core::fmt::Debug for ReducedCode {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        f.write_str("ReducedCode(")?;
        core::fmt::Display::fmt(self, f)?;
        f.write_str(")")
    }
}

impl core::fmt::Display for CodeParseError {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        use CodeParseError::*;
        match self {
            GroupA(offset) => f.write_fmt(format_args!("could not parse group A at {}", offset)),
            ExpectedDash(offset) => f.write_fmt(format_args!("expected `-` at {}", offset)),
            GroupB(offset) => f.write_fmt(format_args!("could not parse group B at {}", offset)),
            ExpectedColon(offset) => f.write_fmt(format_args!("expected `:` at {}", offset)),
            GroupC(offset) => f.write_fmt(format_args!("could not parse group C at {}", offset)),
            ExpectedDot(offset) => f.write_fmt(format_args!("expected `.` at {}", offset)),
            GroupD(offset) => f.write_fmt(format_args!("could not parse group D at {}", offset)),
            GroupE(offset) => f.write_fmt(format_args!("could not parse group E at {}", offset)),
            ExpectedStar(offset) => f.write_fmt(format_args!("expected `*` at {}", offset)),
            GroupF(offset) => f.write_fmt(format_args!("could not parse group F at {}", offset)),
        }
    }
}

impl core::fmt::Display for ReducedCodeParseError {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        use ReducedCodeParseError::*;
        match self {
            Group(offset) => f.write_fmt(format_args!("could not parse a group at {}", offset)),
            Separator(offset) => {
                f.write_fmt(format_args!("could not parse a separator at {}", offset))
            }
            GroupCMissing(offset) => f.write_fmt(format_args!(
                "required group C is not present at {}",
                offset
            )),
        }
    }
}

#[inline]
fn parse_code_group<E>(error: E, input: &[u8], offset: &mut usize) -> Result<u8, E> {
    let Some(buffer) = input.get(*offset..)  else { return Err(error) };
    let (a, used) = <u8 as atoi::FromRadix10Checked>::from_radix_10_checked(buffer);
    if used == 0 {
        return Err(error);
    }
    let a = a.ok_or(error)?;
    *offset = offset.wrapping_add(used);
    Ok(a)
}

#[inline]
fn get_code_separator<E>(error: E, input: &[u8], offset: &mut usize) -> Result<u8, E> {
    let Some(buffer) = input.get(*offset..)  else { return Err(error) };
    let Some((separator, _)) = buffer.split_first() else { return Err(error) };
    Ok(*separator)
}

#[inline]
fn expect_code_separator<E>(error: E, sep: u8, input: &[u8], offset: &mut usize) -> Result<(), E> {
    match get_code_separator((), input, offset) {
        Ok(s) if s == sep => {
            *offset = offset.wrapping_add(1);
            Ok(())
        }
        _ => Err(error),
    }
}

impl Code {
    /// Parse a full OBIS code.
    ///
    /// Note, that the current implementation expects the code to be in the `A-B:C.D.E*F` format
    /// (i.e. using the same delimiters between groups as in [`ReducedCode`]s) where `A`, `B`, `C`,
    /// `D`, `E` and `F` are all decimal integers in range from 0 to 255, inclusive.
    pub fn parse(input: &[u8]) -> Result<(Code, &[u8]), CodeParseError> {
        let mut offset = 0;
        let a = parse_code_group(CodeParseError::GroupA(offset), input, &mut offset)?;
        expect_code_separator(
            CodeParseError::ExpectedDash(offset),
            b'-',
            input,
            &mut offset,
        )?;
        let b = parse_code_group(CodeParseError::GroupB(offset), input, &mut offset)?;
        expect_code_separator(
            CodeParseError::ExpectedColon(offset),
            b':',
            input,
            &mut offset,
        )?;
        let c = parse_code_group(CodeParseError::GroupC(offset), input, &mut offset)?;
        expect_code_separator(
            CodeParseError::ExpectedDot(offset),
            b'.',
            input,
            &mut offset,
        )?;
        let d = parse_code_group(CodeParseError::GroupD(offset), input, &mut offset)?;
        expect_code_separator(
            CodeParseError::ExpectedDot(offset),
            b'.',
            input,
            &mut offset,
        )?;
        let e = parse_code_group(CodeParseError::GroupE(offset), input, &mut offset)?;
        expect_code_separator(
            CodeParseError::ExpectedStar(offset),
            b'*',
            input,
            &mut offset,
        )?;
        let f = parse_code_group(CodeParseError::GroupF(offset), input, &mut offset)?;
        Ok((
            Code { a, b, c, d, e, f },
            input.get(offset..).unwrap_or(b""),
        ))
    }
}

impl ReducedCode {
    const FLAG_HAS_A: u8 = 0x1;
    const FLAG_HAS_B: u8 = 0x2;
    const FLAG_HAS_E: u8 = 0x10;
    const FLAG_HAS_F: u8 = 0x20;
    const FLAG_MANUAL_RESET: u8 = 0x80;

    /// Parse a Reduced OBIS ID code, as defined in the Annex A of IEC 62056-61.
    ///
    /// Reduced OBIS ID codes allow groups A, B, E and F to be omitted, and also permits for
    /// carrying some additional data about the source of previous reset.
    ///
    /// The expected format is `[A-][B:]C.D[.E][(*|&)F]`, where groups `A`, `B`, `C`, `D`, `E` and
    /// `F` are all decimal integers in range 0 to 255, inclusive.
    ///
    /// Display codes (replacing group digits with letters) are not currently supported.
    pub fn parse(input: &[u8]) -> Result<(ReducedCode, &[u8]), ReducedCodeParseError> {
        let mut offset = 0;
        let mut flags = 0;
        let (mut a, mut b) = (0, 0);
        let (c, d);
        let (mut e, mut f) = (0, 255);
        let (mut num, mut sep);

        num = parse_code_group(ReducedCodeParseError::Group(offset), input, &mut offset)?;
        sep = get_code_separator(ReducedCodeParseError::Separator(offset), input, &mut offset)?;
        if sep == b'-' {
            a = num;
            flags |= Self::FLAG_HAS_A;
            offset = offset.wrapping_add(1);
            num = parse_code_group(ReducedCodeParseError::Group(offset), input, &mut offset)?;
            sep = get_code_separator(ReducedCodeParseError::Separator(offset), input, &mut offset)?;
        }
        if sep == b':' {
            b = num;
            flags |= Self::FLAG_HAS_B;
            offset = offset.wrapping_add(1);
            num = parse_code_group(ReducedCodeParseError::Group(offset), input, &mut offset)?;
            sep = get_code_separator(ReducedCodeParseError::Separator(offset), input, &mut offset)?;
        }
        if sep == b'.' {
            c = num;
            offset = offset.wrapping_add(1);
            d = parse_code_group(ReducedCodeParseError::Group(offset), input, &mut offset)?;
        } else {
            return Err(ReducedCodeParseError::GroupCMissing(offset));
        }

        'done: {
            sep = match get_code_separator((), input, &mut offset) {
                Ok(s) => s,
                _ => break 'done,
            };
            if sep == b'.' {
                offset = offset.wrapping_add(1);
                num = parse_code_group(ReducedCodeParseError::Group(offset), input, &mut offset)?;
                e = num;
                flags |= Self::FLAG_HAS_E;
                sep = match get_code_separator((), input, &mut offset) {
                    Ok(s) => s,
                    _ => break 'done,
                }
            }
            if sep == b'&' {
                flags |= Self::FLAG_MANUAL_RESET;
            }
            if sep == b'*' || sep == b'&' {
                offset = offset.wrapping_add(1);
                num = parse_code_group(ReducedCodeParseError::Group(offset), input, &mut offset)?;
                flags |= Self::FLAG_HAS_F;
                f = num;
            }
        }

        Ok((
            Self {
                code: Code { a, b, c, d, e, f },
                flags,
            },
            input.get(offset..).unwrap_or(&[]),
        ))
    }

    /// See [`Code::a`].
    pub fn a(&self) -> Option<u8> {
        if (self.flags & Self::FLAG_HAS_A) == Self::FLAG_HAS_A {
            Some(self.code.a)
        } else {
            None
        }
    }

    /// See [`Code::b`].
    pub fn b(&self) -> Option<u8> {
        if (self.flags & Self::FLAG_HAS_B) == Self::FLAG_HAS_B {
            Some(self.code.b)
        } else {
            None
        }
    }

    /// See [`Code::c`].
    pub fn c(&self) -> u8 {
        self.code.c
    }

    /// See [`Code::d`].
    pub fn d(&self) -> u8 {
        self.code.d
    }

    /// See [`Code::e`].
    pub fn e(&self) -> Option<u8> {
        if (self.flags & Self::FLAG_HAS_E) == Self::FLAG_HAS_E {
            Some(self.code.e)
        } else {
            None
        }
    }

    /// See [`Code::f`].
    pub fn f(&self) -> Option<u8> {
        if (self.flags & Self::FLAG_HAS_F) == Self::FLAG_HAS_F {
            Some(self.code.f)
        } else {
            None
        }
    }

    /// Has the last reset been performed manually?
    pub fn manual_reset(&self) -> bool {
        (self.flags & Self::FLAG_MANUAL_RESET) == Self::FLAG_MANUAL_RESET
    }
}
