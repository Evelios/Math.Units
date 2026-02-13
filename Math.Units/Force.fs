/// <category>Module: Unit System</category>
/// <summary>
/// A <c>Force</c> value represents a force in newtons, pounds force etc. It is
/// stored as a number of newtons.
/// </summary>
/// 
/// <note>
/// <para>
/// Since <c>Newtons</c> is defined as <c>Product&lt;Kilograms,
/// MetersPerSecondSquared&gt;</c>, you can compute force directly as a
/// product of mass and acceleration:
/// </para>
/// <code>
///     let mass =
///         Mass.kilograms 10
///     let acceleration =
///         Acceleration.metersPerSecondSquared 2
///     mass |&gt; Quantity.times acceleration
///     --&gt; Force.newtons 20
/// </code>
/// </note>
    
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Math.Units.Force

/// <category>Metric</category>
/// <summary>Construct a force value from a number of newtons.</summary>
/// <param name="numNewtons">The number of newtons.</param>
let newtons (numNewtons: float) : Force = Quantity numNewtons

/// <category>Metric</category>
/// <summary>Convert a force value to a number of newtons.</summary>
/// <param name="force">The force value to convert.</param>
let inNewtons (force: Force) : float = force.Value

/// <category>Metric</category>
/// <summary>Construct a force value from a number of kilonewtons.</summary>
/// <param name="numKilonewtons">The number of kilonewtons.</param>
let kilonewtons (numKilonewtons: float) : Force = newtons (1000. * numKilonewtons)

/// <category>Metric</category>
/// <summary>Convert a force value to a number of kilonewtons.</summary>
/// <param name="force">The force value to convert.</param>
let inKilonewtons (force: Force) : float = inNewtons force / 1000.

/// <category>Metric</category>
/// <summary>Construct a force value from a number of meganewtons.</summary>
/// <param name="numMeganewtons">The number of meganewtons.</param>
let meganewtons (numMeganewtons: float) : Force = newtons (1.0e6 * numMeganewtons)

/// <category>Metric</category>
/// <summary>Convert a force value to a number of meganewtons.</summary>
/// <param name="force">The force value to convert.</param>
let inMeganewtons (force: Force) : float = inNewtons force / 1.0e6

/// <category>Imperial</category>
/// <summary>Construct a force value from a number of pounds force.</summary>
/// <param name="numPounds">The number of pounds force.</param>
let pounds (numPounds: float) : Force =
    newtons (Constants.poundForce * numPounds)

/// <category>Imperial</category>
/// <summary>Convert a force value to a number of pounds force.</summary>
/// <param name="force">The force value to convert.</param>
let inPounds (force: Force) : float = inNewtons force / Constants.poundForce

/// <category>Imperial</category>
/// <summary>
/// Construct a force value from a number of kips (kilopounds force).
/// <code>
///    Force.kips 2
///    --&gt; Force.pounds 2000
/// </code>
/// </summary>
/// <param name="numKips">The number of kips.</param>
let kips (numKips: float) : Force = pounds (1000. * numKips)

/// <category>Imperial</category>
/// <summary>Convert a force value to a number of kips.</summary>
/// <param name="force">The force value to convert.</param>
let inKips (force: Force) : float = inPounds force / 1000.
