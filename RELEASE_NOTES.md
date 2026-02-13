# Release Notes

## 1.0.0 (February 13, 2026)

This is the initial stable release of **Math.Units**, a functional-first units library for F# and Fable.

### Major Changes & Improvements

*   **Performance:** `Quantity<'Units>` and `Temperature` are now `[<Struct>]` types. This ensures zero memory overhead and provides performance parity with raw `float` operations by avoiding heap allocations.
*   **API Alignment:** Achieved full functional parity with the `elm-units` source of truth. Added module-level functions to the `Quantity` and `Temperature` modules to support idiomatic F# piping and partial application.
*   **Platform Support:** Modernized the library to target **.NET 8.0 (LTS)**, **.NET 9.0**, and **.NET 10.0**.
*   **Type Safety:** Refined `Temperature` modeling to strictly follow the "not a quantity" rule, preventing physically meaningless operations while allowing for precise `TemperatureDelta` manipulations.
*   **Stability:** Restored exact structural equality for core types while providing robust approximate comparison helpers for floating-point calculations in the test suite.
*   **Documentation:** Fully updated `README.md` with a Quick Start guide and clear value proposition. Added category indexing for better navigation in generated documentation.

### Features

*   Support for all SI base units and a comprehensive set of derived units.
*   Compile-time unit checking to prevent dimensional errors.
*   Fable compatibility for F# to JavaScript compilation.
*   Thoroughly verified with over 700 property-based and unit tests.
