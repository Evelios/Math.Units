/// <category>Module: Unit System</category>
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Math.Units.Energy

/// Construct an energy value from a number of joules.
let joules (numJoules: float) : Energy = Quantity numJoules

/// Convert an energy value to a number of joules.
let inJoules (numJoules: Energy) : float = numJoules.Value

/// Construct an energy value from a number of kilojoules.
let kilojoules (numKilojoules: float) : Energy = joules (1000. * numKilojoules)

/// Convert an energy value to a number of kilojoules.
let inKilojoules (energy: Energy) : float = inJoules energy / 1000.

/// Construct an energy value from a number of megajoules.
let megajoules (numMegajoules: float) : Energy = joules (1.0e6 * numMegajoules)

/// Convert an energy value to a number of megajoules.
let inMegajoules (energy: Energy) : float = inJoules energy / 1.0e6

/// <summary>
/// <code>
/// Construct an energy value from a number of kilowatt hours.
///    Energy.kilowattHours 1
///    --&gt; Energy.megajoules 3.6
/// </code>
/// </summary>
let kilowattHours (numKilowattHours: float) : Energy = joules (3.6e6 * numKilowattHours)

/// Convert an energy value to a number of kilowatt hours.
let inKilowattHours (energy: Energy) : float = inJoules energy / 3.6e6
