# Fsharp-Geometry

This is a functional first 2D spatial geometry library heavily based on the framework [elm-geometry](https://package.elm-lang.org/packages/ianmackenzie/elm-geometry/latest/). It provides an API based on core geometry concepts and is built around the concepts of functional first design and type safety. This contains many different geometric data structures like points, vectors, arcs, polygons, and coordinate frames. It also supplies many different operations to create, transform, intersect, and perform operations on them such as intersections.

### :closed_book: [Documentation](https://evelios.github.io/fsharp-geometry/index.html)

# Overview

This library provides the following data structures

* Vectors
* Points
* Directions
* Frames
* Axes
* Line Segments
* Triangles
* Circles
* Ellipses
* Arcs
* Polylines
* Polygons
* Bounding Boxes

# Development

To generate the API documentation you need to run the following commands.

```bash
dotnet tool install fsdocs-tool
dotnet fsdocs build
dotnet fsdocs watch
```


# Attribution

Thanks to Ian Mackenzie and all it's [contributors](https://github.com/ianmackenzie/elm-geometry/graphs/contributors) for creating the [elm-geometry](https://package.elm-lang.org/packages/ianmackenzie/elm-geometry/latest/).