//! Unofficial Rust bindings for the Keystone Engine
//!
//! These Rust bindings are an alternative to the ones available in the
//! [official repository](https://github.com/keystone-engine/keystone/tree/master/bindings/rust)
//! and support version 0.9.2 of the Keystone Engine.
//!
//! ## Why Release New Bindings If Official Ones Exist?
//!
//! To publish a crate on crates.io, all its dependencies must also come from crates.io. There is
//! already a [crate](https://crates.io/crates/keystone) providing bindings for Keystone. However,
//! the latest version it supports, is version 0.9.0 of Keystone. This version has bugs, which,
//! fortunately, are fixed in version 0.9.2. For this reason, and because the current crate of
//! Keystone has not been updated for years, these bindings can be used instead.
//!
//! These bindings are heavily inspired from the official ones and do not alter the API too much
//! compared to the original.
//!
//! ## Example
//!
//! This example, taken from the
//! [official repo](https://github.com/keystone-engine/keystone/blob/0.9.2/bindings/rust/examples/asm.rs),
//! should work out of the box.
//!
//! Add the following dependency to `Cargo.toml`:
//!
//! ```
//! keystone_engine = { version = "0.1.0", features = ["build-from-src"] }
//! ```
//!
//! **Note:** You can use either the `build-from-src` feature to build the Keystone Engine or
//!           `use-system-lib` if you have already installed Keystone on your system.
//!
//! You should now be able to run the following code:
//!
//! ```
//! use keystone_engine::*;
//!
//! let engine =
//!     Keystone::new(Arch::X86, Mode::MODE_32).expect("Could not initialize Keystone engine");
//!
//! engine
//!     .option(OptionType::SYNTAX, OptionValue::SYNTAX_NASM)
//!     .expect("Could not set option to nasm syntax");
//!
//! let result = engine
//!     .asm("mov ah, 0x80".to_string(), 0)
//!     .expect("Could not assemble");
//!
//! println!("ASM result: {}", result);
//!
//! if let Err(err) = engine.asm("INVALID".to_string(), 0) {
//!     println!("Error: {}", err);
//! }
//! ```
//!
//! ## Credits
//!
//!  * [Keystone Assembler Engine](http://www.keystone-engine.org/) by
//!    [Nguyen Anh Quynh](mailto:aquynh@gmail.com)
//!  * [Rust bindings](https://github.com/keystone-engine/keystone/tree) by
//!    [Remco Verhoef](mailto:remco@dutchcoders.io)

pub mod ffi;

pub use ffi::{Arch, Error, Mode, OptionType, OptionValue};

use libc::*;
use std::ffi::CStr;

// -----------------------------------------------------------------------------------------------
// Errors
// -----------------------------------------------------------------------------------------------

/// Convenient Result type wrapping Keystone errors.
pub type Result<T> = std::result::Result<T, KeystoneError>;

/// Keystone errors.
#[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash, Debug)]
pub enum KeystoneError {
    /// Errors directly handled by Keystone.
    Engine(ffi::Error),
    /// Additional error types to handle bindings-specific cases.
    Misc(MiscError),
}

impl std::error::Error for KeystoneError {}

impl std::fmt::Display for KeystoneError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            KeystoneError::Engine(e) => write!(f, "[Engine error] {}", e),
            KeystoneError::Misc(e) => write!(f, "[Misc error] {}", e),
        }
    }
}

impl From<ffi::Error> for KeystoneError {
    fn from(error: ffi::Error) -> Self {
        KeystoneError::Engine(error)
    }
}

impl From<MiscError> for KeystoneError {
    fn from(error: MiscError) -> Self {
        KeystoneError::Misc(error)
    }
}

/// Miscellaneous errors.
#[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash, Debug)]
pub enum MiscError {
    /// Error returned when a call to `ks_asm` fails.
    KsAsm,
    /// Allocation fail, possibly `ks_asm` returns NULL.
    AllocFail,
}

impl std::error::Error for MiscError {}

impl std::fmt::Display for MiscError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            MiscError::KsAsm => write!(f, "an error occured while calling ks_asm"),
            MiscError::AllocFail => write!(f, "allocation failed in ks_asm"),
        }
    }
}

// -----------------------------------------------------------------------------------------------
// API
// -----------------------------------------------------------------------------------------------

/// Output object created after assembling instructions.
#[derive(Clone, Eq, PartialEq, Ord, PartialOrd, Hash, Debug)]
pub struct KeystoneOutput {
    /// Number of instructions that were successfully encoded.
    pub stat_count: size_t,
    /// Size of the array storing the encoded instructions.
    size: size_t,
    /// A pointer of allocated encoded instructions.
    ptr: *mut u8,
}

impl KeystoneOutput {
    /// Returns encoded instructions in bytes.
    pub fn as_bytes(&self) -> &[u8] {
        let bytes = unsafe { core::slice::from_raw_parts(self.ptr, self.size as _) };
        bytes
    }
}

impl std::fmt::Display for KeystoneOutput {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        for byte in self.as_bytes() {
            f.write_fmt(format_args!("{:02x}", byte))?;
        }
        Ok(())
    }
}

impl Drop for KeystoneOutput {
    fn drop(&mut self) {
        unsafe {
            ffi::ks_free(self.ptr);
        }
    }
}

/// Reprensents a Keystone instance.
#[derive(Eq, PartialEq, Ord, PartialOrd, Hash, Debug)]
pub struct Keystone {
    /// Handle to the keystone instance.
    ks: ffi::KsHandle,
}

impl Keystone {
    /// Creates a new Keystone object.
    pub fn new(arch: ffi::Arch, mode: ffi::Mode) -> Result<Self> {
        // Check if the version returned by the library matches with the bindings'.
        if Self::version() != (ffi::API_MAJOR, ffi::API_MINOR) {
            return Err(ffi::Error::VERSION)?;
        }
        // Opens the Keystone instance.
        let mut ks = None;
        let err = unsafe { ffi::ks_open(arch, mode, &mut ks) };
        if err == ffi::Error::OK {
            Ok(Keystone {
                // FIXME: remove this panick?
                ks: ks.expect("Got NULL engine from ks_open()"),
            })
        } else {
            Err(err)?
        }
    }

    // Returns the major and minor version numbers from the library.
    pub fn version() -> (u32, u32) {
        let mut major = 0;
        let mut minor = 0;
        unsafe { ffi::ks_version(&mut major, &mut minor) };
        (major, minor)
    }

    /// Sets an option of the Keystone engine after the instance has been created.
    pub fn option(&self, opt_type: ffi::OptionType, value: ffi::OptionValue) -> Result<()> {
        let err = unsafe { ffi::ks_option(self.ks, opt_type, value) };
        if err == ffi::Error::OK {
            Ok(())
        } else {
            Err(err)?
        }
    }

    /// Assembles a program from an input string containing assembly instructions.
    ///
    /// The resulting machine code depends on the input buffer, its size, a base address and the
    /// number of instructions to encode. The method returns a [`KeystoneOutput`] object that
    /// contains the encoded instructions.
    pub fn asm(&self, insns: &CStr, address: u64) -> Result<KeystoneOutput> {
        let insns_cstr = insns;
        let mut encoding: *mut c_uchar = std::ptr::null_mut();
        let mut encoding_size: size_t = 0;
        let mut stat_count: size_t = 0;
        // Encode the input instructions.
        let err = unsafe {
            ffi::ks_asm(
                self.ks,
                insns_cstr.as_ptr(),
                address,
                &mut encoding,
                &mut encoding_size,
                &mut stat_count,
            )
        };
        if err != 0 {
            let err = unsafe { ffi::ks_errno(self.ks) };
            return Err(KeystoneError::Engine(err));
        }
        if encoding.is_null() {
            return Err(KeystoneError::Misc(MiscError::AllocFail));
        }
        Ok(KeystoneOutput {
            stat_count: stat_count,
            size: encoding_size,
            ptr: encoding,
        })
    }
}

impl Drop for Keystone {
    fn drop(&mut self) {
        unsafe { ffi::ks_close(self.ks) };
    }
}

// -----------------------------------------------------------------------------------------------
// Tests
// -----------------------------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test() {
        // Create an instance of Keystone.
        let engine_res = Keystone::new(Arch::X86, Mode::MODE_32);
        assert!(engine_res.is_ok());
        let engine = engine_res.unwrap();
        // Change an option of the engine.
        engine
            .option(OptionType::SYNTAX, OptionValue::SYNTAX_NASM)
            .expect("Could not set option to nasm syntax");
        // Assemble instructions
        let output_res = engine.asm(c"mov ah, 0x80", 0);
        assert!(output_res.is_ok());
        // Make sure the output object is sane.
        let output = output_res.unwrap();
        assert_eq!(output.as_bytes(), &[0xb4, 0x80]);
        assert_eq!(output.stat_count, 1);
        // Ensure an error is returned when invalid instructions are provided.
        assert_eq!(
            engine.asm(c"INVALID", 0),
            Err(KeystoneError::Engine(ffi::Error::ASM_MNEMONICFAIL))
        );
    }
}
