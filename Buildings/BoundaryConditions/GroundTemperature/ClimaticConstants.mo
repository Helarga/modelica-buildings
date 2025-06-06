within Buildings.BoundaryConditions.GroundTemperature;
package ClimaticConstants "Surface temperature climatic constants"
  extends Modelica.Icons.MaterialPropertiesPackage;

  record Generic "Generic climatic constants"
    extends Modelica.Icons.Record;
    parameter Modelica.Units.SI.Temperature TSurMea
      "Mean annual surface temperature";
    parameter Modelica.Units.SI.TemperatureDifference TSurAmp
      "Surface temperature amplitude";
    parameter Modelica.Units.SI.Duration sinPha(displayUnit="d")
      "Phase lag of soil surface temperature";
    annotation (
      defaultComponentPrefixes="parameter",
      defaultComponentName="datCliCon",
      Documentation(info=
                   "<html>
  <p>
  This is a generic record for climatic conditions.
  </p>
  </html>", revisions="<html>
  <ul>
  <li>
  February 11, 2022, by Michael Wetter:<br/>
  Added <code>defaultComponentPrefixes</code> and documentation.
  </li>
  </ul>
  </html>"));
  end Generic;

  record Boston =
    Buildings.BoundaryConditions.GroundTemperature.ClimaticConstants.Generic (
      TSurMea=284.23,
      TSurAmp=11.57,
      sinPha=9944640) "Boston";

  record NewYork =
    Buildings.BoundaryConditions.GroundTemperature.ClimaticConstants.Generic (
      TSurMea=286.03,
      TSurAmp=11.45,
      sinPha=9728640) "New York";

  record SanFrancisco =
    Buildings.BoundaryConditions.GroundTemperature.ClimaticConstants.Generic (
      TSurMea=287.64,
      TSurAmp=3.35,
      sinPha=10419840) "San Francisco";

  annotation (Documentation(info="<html>
<p>
Surface temperature data that is used in the calculation of undisturbed soil temperature.
See <a href=\"https://tpc.ashrae.org/FileDownload?idx=a72f024c-dc69-41fa-8d5c-d1cd21df17f1\">
Climatic Constants for Calculating Subsurface Soil Temperatures</a> for
more information and a table of values.
</p>
</html>", revisions="<html>
<ul>
<li>
April 14, 2025, by Jianjun Hu:<br/>
Updated reference link. This is for
<a href=\"https://github.com/lbl-srg/modelica-buildings/issues/4177\">Buildings, issue 4177</a>.
</li>
<li>
March 17, 2021, by Baptiste Ravache:<br/>
First implementation.
</li>
</ul>
</html>"));
end ClimaticConstants;
