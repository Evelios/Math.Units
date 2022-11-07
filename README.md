# Math.Units

This is an F# functional first units library ported from [elm-units](https://package.elm-lang.org/packages/ianmackenzie/elm-units/latest/). It provides an API based on core units concepts and is built around the concepts of functional first design and type safety. From this philosophy, this library provides a way of maintaining unit conversions and manipulations which allows you to focus on the hard work without being burdened with the though of having a unit discrepancy.

### :closed_book: [Documentation](https://evelios.github.io/Math.Units/index.html)

# Overview

This library provides access to a variety of units to work with.


# Development

To generate the API documentation you need to run the following commands.

```bash
dotnet tool install fsdocs-tool
dotnet fsdocs build --input Docs --eval
dotnet fsdocs watch --input Docs --eval

dotnet fsdocs watch --projects Math.Units --input Docs --eval --properties Configuration=Releas
```


# Attribution

Thanks to Ian Mackenzie and all it's [contributors](https://github.com/ianmackenzie/elm-units/graphs/contributors) for creating the [elm-units](https://package.elm-lang.org/packages/ianmackenzie/elm-units/latest/).