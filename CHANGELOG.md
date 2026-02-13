# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [1.0.0] - 2026-02-13

This is the first stable release of **Math.Units**, providing a robust, type-safe foundation for physical quantities in F#.

### Added
- **Core Unit System:** Implemented `Quantity<'Units>` as a foundational type for dimensionally safe calculations.
- **SI Base Units:** Full support for Length, Mass, Duration, Current, Temperature, Substance Amount, and Luminous Intensity.
- **Derived Units:** Extensive coverage including Area, Volume, Speed, Acceleration, Force, Pressure, Energy, Power, and Electrical units (Volts, Ohms, etc.).
- **Functional API:** Comprehensive `Quantity` and `Temperature` modules designed for piping (`|>`) and partial application.
- **Math.Units.Testing:** A new dedicated testing utility package providing shared FsCheck generators and approximate equality comparison logic for downstream libraries.
- **Fable Support:** Fully compatible with Fable for F# to JavaScript compilation.

### Changed
- **Architectural Split:** Decoupled testing logic into a separate project (`Math.Units.Testing`) to ensure the core library has zero external test dependencies, maximizing Fable compatibility.
- **Performance Optimization:** Converted core types (`Quantity`, `Temperature`) to **Structs** to ensure zero memory allocation during arithmetic.
- **Platform Support:** Updated to target .NET 8.0, 9.0, and 10.0.
- **API Alignment:** Refactored the API to achieve strict parity with the `elm-units` source of truth.
- **Type Safety:** Refined `Temperature` modeling to strictly distinguish between absolute points and `TemperatureDelta`.

### Fixed
- **Equality Semantics:** Restored exact structural equality for structs to ensure predictable behavior in .NET collections (Sets/Dictionaries).
- **Precision Issues:** Standardized approximate comparison logic in the test suite to handle floating-point drift across all units.
