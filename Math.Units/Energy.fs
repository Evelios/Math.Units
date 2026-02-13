/// <category>Module: Unit System</category>
/// <summary>
/// An <c>Energy</c> value represents an amount of energy (or work) in joules,
/// kilowatt hours etc. It is stored as a number of joules.
/// </summary>
/// 
/// <note>
/// <para>
/// Since <c>Joules</c> is defined as <c>Product&lt;Newtons, Meters&gt;</c>,
/// you can compute energy directly as a product of force and distance:
/// </para>
/// <code>
///     Force.newtons 5 |&gt; Quantity.times (Length.meters 4)
///     --&gt; Energy.joules 20
/// </code>
/// </note>
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Math.Units.Energy

/// <category>Conversions</category>
/// <summary>Construct an energy value from a number of joules.</summary>
/// <param name="numJoules">The number of joules.</param>
let joules (numJoules: float) : Energy = Quantity numJoules

/// <category>Conversions</category>
/// <summary>Convert an energy value to a number of joules.</summary>
/// <param name="energy">The energy value to convert.</param>
let inJoules (energy: Energy) : float = energy.Value

/// <category>Conversions</category>
/// <summary>Construct an energy value from a number of kilojoules.</summary>
/// <param name="numKilojoules">The number of kilojoules.</param>
let kilojoules (numKilojoules: float) : Energy = joules (1000. * numKilojoules)

/// <category>Conversions</category>
/// <summary>Convert an energy value to a number of kilojoules.</summary>
/// <param name="energy">The energy value to convert.</param>
let inKilojoules (energy: Energy) : float = inJoules energy / 1000.

/// <category>Conversions</category>
/// <summary>Construct an energy value from a number of megajoules.</summary>
/// <param name="numMegajoules">The number of megajoules.</param>
let megajoules (numMegajoules: float) : Energy = joules (1.0e6 * numMegajoules)

/// <category>Conversions</category>
/// <summary>Convert an energy value to a number of megajoules.</summary>
/// <param name="energy">The energy value to convert.</param>
let inMegajoules (energy: Energy) : float = inJoules energy / 1.0e6

/// <category>Conversions</category>
/// <summary>
/// <code>
/// Construct an energy value from a number of kilowatt hours.
///    Energy.kilowattHours 1
///    --&gt; Energy.megajoules 3.6
/// </code>
/// </summary>
/// <param name="numKilowattHours">The number of kilowatt hours.</param>
let kilowattHours (numKilowattHours: float) : Energy = joules (3.6e6 * numKilowattHours)

/// <category>Conversions</category>
/// <summary>Convert an energy value to a number of kilowatt hours.</summary>
/// <param name="energy">The energy value to convert.</param>
let inKilowattHours (energy: Energy) : float = inJoules energy / 3.6e6
