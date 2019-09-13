within Buildings.Applications.DHC.EnergyTransferStations.Control;
model SolSytem
  extends Modelica.Blocks.Icons.Block;

  parameter Real delY(final unit = "W/m2") = 0.01
    "Width of the smoothHeaviside function";
  parameter Buildings.Fluid.SolarCollectors.Data.GenericSolarCollector per
    "Performance data"
    annotation (choicesAllMatching=true, Placement(transformation(extent={{80,82},
            {100,102}})));
  Buildings.Fluid.SolarCollectors.Controls.BaseClasses.GCritCalc criSol(final
      slope=per.slope, final y_intercept=per.y_intercept)
    "Calculates the critical insolation based on collector design and current weather conditions"
    annotation (Placement(transformation(extent={{-82,54},{-62,74}})));
  Modelica.Blocks.Math.Add add(final k2=-1)
    "Compares the current insolation to the critical insolation"
    annotation (Placement(transformation(extent={{-40,60},{-20,80}})));
  Modelica.Blocks.Math.RealToBoolean realToBoolean
    annotation (Placement(transformation(extent={{20,60},{40,80}})));
  Modelica.Blocks.Logical.And and1
    annotation (Placement(transformation(extent={{60,0},{80,20}})));
  Modelica.Blocks.Interfaces.RealOutput pumSol(
    min=0,
    max=1,
    unit="1") "On/off control signal for the pump" annotation (Placement(transformation(extent={{100,-66},{128,-38}}),
        iconTransformation(extent={{100,32},{116,48}})));
  Buildings.BoundaryConditions.WeatherData.Bus weaBus
    "Weather data input"
    annotation (Placement(transformation(extent={{-112,74},{-92,94}}),
        iconTransformation(extent={{-112,50},{-92,70}})));
  Buildings.Controls.OBC.CDL.Interfaces.BooleanOutput actSolSys
    "Solar thermal System is active" annotation (Placement(transformation(
          extent={{100,-2},{124,22}}),  iconTransformation(extent={{100,-48},{116,
            -32}})));
  Buildings.Controls.OBC.CDL.Interfaces.RealInput TSetSol
    "Setpoint water leaving temperature from the solar system" annotation (Placement(transformation(extent={{-120,
            -28},{-100,-8}}), iconTransformation(extent={{-116,-48},{-100,-32}})));
  Modelica.Blocks.Interfaces.RealInput TTanHeaTop(final unit="K", displayUnit="degC")
    "Temperature at top level of hot buffer tank" annotation (Placement(transformation(extent={{-120,32},{-100,52}}),
        iconTransformation(extent={{-116,32},{-100,48}})));
  Buildings.Controls.Continuous.LimPID conPum(
    controllerType=Modelica.Blocks.Types.SimpleController.PI,
    yMin=0.1,
    reset=Buildings.Types.Reset.Parameter,
    y_reset=0.1,
    k=1,
    Ti(displayUnit="s") = 300,
    reverseAction=false)
   "Controller for solar system pump"
    annotation (Placement(transformation(extent={{-40,-62},{-20,-42}})));
  Modelica.Blocks.Logical.Hysteresis hysteresis(uLow=0, uHigh=3)
    annotation (Placement(transformation(extent={{2,-62},{22,-42}})));
  Buildings.Controls.OBC.CDL.Logical.Switch swi
    annotation (Placement(transformation(extent={{42,-62},{62,-42}})));
  Modelica.Blocks.Sources.Constant const(k=0)
    annotation (Placement(transformation(extent={{0,-96},{20,-76}})));
protected
  Buildings.Utilities.Math.SmoothHeaviside smoHea(final delta=delY)
    "Creates a smooth 1/0 output"
    annotation (Placement(transformation(extent={{-10,60},{10,80}})));
equation

  connect(weaBus.TDryBul,criSol. TEnv)    annotation (Line(
      points={{-102,84},{-90,84},{-90,70},{-84,70}},
      color={255,204,51},
      thickness=0.5,
      smooth=Smooth.None), Text(
      string="%first",
      index=-1,
      extent={{-6,3},{-6,3}}));
  connect(weaBus.HDirNor,add. u1) annotation (Line(
      points={{-102,84},{-60,84},{-60,76},{-42,76}},
      color={255,204,51},
      thickness=0.5,
      smooth=Smooth.None), Text(
      string="%first",
      index=-1,
      extent={{-6,3},{-6,3}}));
  connect(criSol.TIn, TTanHeaTop) annotation (Line(
      points={{-84,58},{-94,58},{-94,42},{-110,42}},
      color={0,0,127},
      pattern=LinePattern.Dash));
  connect(const.y, swi.u3) annotation (Line(points={{21,-86},{32,-86},{32,-60},
          {40,-60}}, color={0,0,127}));
  connect(add.y, smoHea.u)
    annotation (Line(points={{-19,70},{-12,70}}, color={0,0,127}));
  connect(add.u2, criSol.G_TC)
    annotation (Line(points={{-42,64},{-60.4,64}}, color={0,0,127}));
  connect(smoHea.y, realToBoolean.u)
    annotation (Line(points={{11,70},{18,70}}, color={0,0,127}));
  connect(realToBoolean.y, and1.u1) annotation (Line(points={{41,70},{50,70},{
          50,10},{58,10}}, color={255,0,255}));
  connect(actSolSys, actSolSys)
    annotation (Line(points={{112,10},{112,10}}, color={255,0,255}));
  connect(and1.y, actSolSys)
    annotation (Line(points={{81,10},{112,10}}, color={255,0,255}));
  connect(TTanHeaTop, conPum.u_m)
    annotation (Line(points={{-110,42},{-94,42},{-94,-86},{-30,-86},{-30,-64}}, color={0,0,127}));
  connect(TSetSol, conPum.u_s)
    annotation (Line(points={{-110,-18},{-80,-18},{-80,-52},{-42,-52}}, color={0,0,127}));
  connect(and1.y, conPum.trigger) annotation (Line(points={{81,10},{86,10},{86,
          -34},{-54,-34},{-54,-72},{-38,-72},{-38,-64}}, color={255,0,255}));
  connect(conPum.y, hysteresis.u)
    annotation (Line(points={{-19,-52},{0,-52}}, color={0,0,127}));
  connect(hysteresis.y, swi.u2)
    annotation (Line(points={{23,-52},{40,-52}}, color={255,0,255}));
  connect(conPum.y, swi.u1) annotation (Line(points={{-19,-52},{-10,-52},{-10,
          -36},{34,-36},{34,-44},{40,-44}}, color={0,0,127}));
  connect(swi.y, pumSol) annotation (Line(points={{64,-52},{114,-52}}, color={0,0,127}));
  connect(hysteresis.y, and1.u2) annotation (Line(points={{23,-52},{30,-52},{30,
          4},{58,4},{58,2}}, color={255,0,255}));
  annotation (Icon(coordinateSystem(preserveAspectRatio=false, extent={{-100,-100},
            {100,100}})),       Diagram(coordinateSystem(preserveAspectRatio=false,
        extent={{-100,-100},{100,100}})),
        defaultComponentName="solTheCon",
Documentation(info="<html>
<h4> solar thermal controller theory of operation </h4>
<p>
This block controls the <a href=\"Buildings.DistrictHeatingCooling.EnergyTransferStations.EnergyTransferStation.SolarThermalModule\">
Buildings.DistrictHeatingCooling.EnergyTransferStations.EnergyTransferStation.SolarThermalModule </a> to be integrated efficiently at the substation of the district heating and cooling systems especially the fourth and fifth generations.
</p>
<p>
The controller tests both:
</p>
<ul>
<li>
The useful solar energy gain based on the solar collector technical specifications and solar radiation instensity.
</ul>
</li>
<ul>
<li>
The hot buffer tank top level water temperature  <code>TTanHeaTop</code> compared to the solar setpoint temperature <code>TSetSol</code>.
</li>
</ul>
<p>
<i>
<b>
It is important to highlight the followings:
</b>
</p>
<ul>
<li>
The Boolean output signal actSolSys have to be True to turn on the solar pump.
</li>
</ul>
<ul>
<li>
Hystresis were included wherever needed to avoid system cycling.
</li>
</ul>
</i>

</html>", revisions="<html>
<ul>
<li>
 <br/>
</li>
</ul>
</html>"));
end SolSytem;
