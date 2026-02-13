# Math.Units

Math.Units is a functional-first library for F# that makes working with physical quantities safe, intuitive, and error-free. 

## :closed_book: [Read the Full Documentation](https://evelios.github.io/Math.Units/index.html)

Ported from the excellent [elm-units](https://package.elm-lang.org/packages/ianmackenzie/elm-units/latest/), it brings rigorous type safety to your measurements. Whether you're calculating the velocity of a spacecraft or just converting Celsius to Fahrenheit, Math.Units ensures you never accidentally add meters to seconds.


## Why use Math.Units?

Is is common place in applications to treat measurements as just `float` or primative values. This makes it dangerously easy to make mistakes like:
- Passing a value in Feet to a function expecting Meters.
- Adding a Duration to a Length.
- Forgetting a conversion factor.

**Math.Units makes these mistakes impossible.** Every quantity has its own type, so the compiler catches errors before they ever reach your users.

## Quick Start

```fsharp
open Units

let distance = Length.meters 100.0
let time = Duration.seconds 9.58

// Speed is automatically calculated and typed!
let speed = distance / time 

printfn "The speed is %f m/s" (Speed.inMetersPerSecond speed)
```

## Features at a Glance

- **Compile-time Safety:** Errors are caught while you type, not at runtime.
- **Broad Coverage:** Includes Length, Angle, Temperature, Mass, Speed, Force, Energy, Power, and many more.
- **Functional & Pipe-friendly:** Designed to work beautifully with F#'s `|>` operator.
- **Fable Ready:** Compiles to clean JavaScript for web projects.
- **Battle-tested:** Verified with property-based testing (FsCheck) for mathematical precision.

## Platform Support

We target modern .NET to ensure you get the best performance:
- **.NET 8.0** (LTS)
- **.NET 9.0** (Current)
- **.NET 10.0** (Preview)

## Development

```bash
# Build the project
dotnet build

# Run tests across all supported versions
dotnet test
```

To generate the documentation
```bash
dotnet tool restore
dotnet fsdocs watch --input Docs --eval
```

To generate the documentation for a clean release build
```bash
dotnet fsdocs watch --projects Math.Units --input Docs --eval --properties Configuration=Release
dotnet build --configuration Release && dotnet fsdocs watch --projects Math.Units --input Docs eval --properties Configuration=Release --clean
```

## Credits

A huge thank you to Ian Mackenzie and the contributors of [elm-units](https://package.elm-lang.org/packages/ianmackenzie/elm-units/latest/) for the original inspiration and design.