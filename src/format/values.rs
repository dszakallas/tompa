use num::{Signed, Unsigned};


pub trait AsUnsigned: Unsigned where Self::Repr: Signed {
    type Repr;

    fn get(r: Self::Repr) -> Self;
}

macro_rules! as_unsigned_impl {
    ($($from:tt -> $to:tt),*) => {
        $(
            impl AsUnsigned for $to {
                type Repr = $from;

                #[inline]
                fn get(r: Self::Repr) -> Self {
                    r as Self
                }
            }
        )*
    }
}

as_unsigned_impl!(i8 -> u8, i16 -> u16, i32 -> u32, i64 -> u64);

// TODO parameter should use [u8; Self::N] once feature(generic_const_exprs) is stable
pub trait FromBytes {
    const N: usize;

    fn from_be_bytes(bytes: &[u8]) -> Self;
    fn from_ne_bytes(bytes: &[u8]) -> Self;
    fn from_le_bytes(bytes: &[u8]) -> Self;
}

macro_rules! from_bytes_impl {
    ($($to:tt),*) => {
        $(
            impl FromBytes for $to {
                const N: usize = std::mem::size_of::<$to>();

                #[inline]
                fn from_be_bytes(bytes: &[u8]) -> Self {
                    $to::from_be_bytes(std::convert::TryFrom::try_from(bytes).expect("not enough bytes"))
                }
                #[inline]
                fn from_ne_bytes(bytes: &[u8]) -> Self {
                    $to::from_ne_bytes(std::convert::TryFrom::try_from(bytes).expect("not enough bytes"))
                }
                #[inline]
                fn from_le_bytes(bytes: &[u8]) -> Self {
                    $to::from_le_bytes(std::convert::TryFrom::try_from(bytes).expect("not enough bytes"))
                }
            }
        )*
    }
}

from_bytes_impl!(u8, u16, u32, u64);
from_bytes_impl!(i8, i16, i32, i64);
from_bytes_impl!(f32, f64);
