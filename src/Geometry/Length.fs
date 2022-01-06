module Geometry.Length

// Pixels

let pixels p : Length<Pixels> = p

let inPixels (l: 'Length Length) : float = l


// Metric

let meters m : Length<Meters> = m

let inMeters (l: 'Length Length) : float = l
