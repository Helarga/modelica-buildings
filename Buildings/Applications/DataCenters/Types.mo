within Buildings.Applications.DataCenters;
package Types "Package with type definitions"
  extends Modelica.Icons.TypesPackage;

  type CoolingModes = enumeration(
      FreeCooling "Free cooling mode",
      PartialMechanical "Partial mechanical cooling",
      FullMechanical "Holiday") "Full mechanical cooling" annotation (
      Documentation(info="<html>
<p>
Enumeration for the type cooling mode.
</p>
<ol>
<li>
FreeCooling
</li>
<li>
PartialMechanical
</li>
<li>
FullMechanical
</li>
</ol>
</html>", revisions="<html>
<ul>
<li>
August 29, 2017, by Michael Wetter:<br/>
First implementation.
</li>
</ul>
</html>"));

  type CompressorModes = enumeration(
      Off    "Cmpressor is off",
      Stage1 "Partial load mechanical cooling",
      Stage2 "Full load mechanical cooling") "Compressor staging."
    annotation (
      Documentation(info="<html>
<p>
Enumeration for the type Compressor mode.
</p>
<ol>
<li>
Off
</li>
<li>
Stage1 with 66.7% of the compressor nominal load
</li>
<li>
Stage2 with 100% of the compressor nominal load.
</li>
</ol>
</html>",   revisions="<html>
<ul>

</ul>
</html>"));
  annotation (Documentation(info="<html>
<p>
This package contains type definitions.
</p>
</html>"));
end Types;
