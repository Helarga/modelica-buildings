within Buildings.Applications.DHC.EnergyTransferStations;
model SolarThermalModule "Solar thermal system module"
 //package Medium = Buildings.Media.Water;
  replaceable package Medium = Modelica.Media.Interfaces.PartialMedium;
  parameter Fluid.SolarCollectors.Data.GenericSolarCollector  per
    "Performance data" annotation(choicesAllMatching=true,
    Placement(transformation(extent={{78,72},{98,92}})));
  parameter Integer nSeg
    "Number of segments used to discretize the collector model";
  parameter Modelica.SIunits.Angle lat "Latitude";
  parameter Modelica.SIunits.Angle azi
    "Surface azimuth (0 for south-facing; -90 degree for east-facing; +90 degree for west facing";
  parameter Modelica.SIunits.Angle til
    "Surface tilt (0 for horizontally mounted collector)";
  parameter Real rho "Ground reflectance";
  parameter Fluid.SolarCollectors.Types.NumberSelection nColType=Buildings.Fluid.SolarCollectors.Types.NumberSelection.Number
    "Selection of area specification format";
  parameter Fluid.SolarCollectors.Types.SystemConfiguration sysConfig=Buildings.Fluid.SolarCollectors.Types.SystemConfiguration.Series
    "Selection of system configuration";
  parameter Real shaCoe
    "Shading coefficient. 0.0: no shading, 1.0: full shading";
  parameter Integer nPanels
    "Desired number of panels in the simulation";
  parameter Modelica.SIunits.HeatCapacity C=385*per.mDry
    "Heat capacity of solar collector without fluid (default: cp_copper*mDry*nPanels)";
  Buildings.Fluid.SolarCollectors.ASHRAE93 solCol(
    redeclare package Medium = Medium,
    C=C,
    use_shaCoe_in=false,
    rho=rho,
    nColType=nColType,
    sysConfig=sysConfig,
    per=per,
    nPanels=nPanels,
    nSeg=nSeg,
    lat=lat,
    azi=azi,
    til=til)
   "Solar thermal module"
    annotation (Placement(transformation(extent={{20,-50},{40,-30}})));
    //shaCoe=shaCoe,
    //shaCoe=shaCoe,

  Control.SolSytem solTheCon
   "Control of the solar thermal system "
    annotation (Placement(transformation(extent={{-80,20},{-60,40}})));
  Buildings.Controls.OBC.CDL.Interfaces.RealInput TSetSol(unit="K", displayUnit="degC")
    "Setpoint water leaving temperature from the solar system" annotation (Placement(transformation(extent={{-120,
            16},{-100,36}}), iconTransformation(extent={{-116,-68},{-100,-52}})));
  Modelica.Blocks.Interfaces.RealInput TTanHeaTop(final unit="K", displayUnit="degC")
    "Temperature at top level of hot buffer tank" annotation (Placement(transformation(extent={{-120,36},{-100,56}}),
        iconTransformation(extent={{-116,52},{-100,68}})));
  Buildings.BoundaryConditions.WeatherData.Bus weaBus
   "Weather bus"
    annotation (Placement(
        transformation(extent={{-124,68},{-94,96}}), iconTransformation(extent={{-120,86},
            {-100,106}})));
  Modelica.Fluid.Interfaces.FluidPort_a port_a(
    redeclare package Medium = Medium)
  "Water inlet from heating return water header"
    annotation (Placement(
        transformation(extent={{-112,-50},{-92,-30}}), iconTransformation(
          extent={{-120,-10},{-100,10}})));
  Modelica.Fluid.Interfaces.FluidPort_b port_b(
    redeclare package Medium = Medium)
   "Water outlet to heating supply water header"
    annotation (Placement(
        transformation(extent={{90,-50},{110,-30}}), iconTransformation(extent={{100,-10},
            {120,10}})));
  Buildings.Controls.OBC.CDL.Interfaces.BooleanOutput actSolSys
   "Boolean output signal(True: Solar thermal system is active)"
    annotation (
      Placement(transformation(extent={{100,16},{120,36}}), iconTransformation(
          extent={{100,-88},{120,-68}})));
  Buildings.Controls.OBC.CDL.Interfaces.RealOutput pumSol
   "Solar System pump control signal"
    annotation (Placement(transformation(
          extent={{100,36},{120,56}}), iconTransformation(extent={{100,74},
            {120,94}})));

equation
  connect(port_a, port_a)
    annotation (Line(points={{-102,-40},{-102,-40}}, color={0,127,255}));
  connect(port_a, solCol.port_a)
    annotation (Line(points={{-102,-40},{20,-40}}, color={0,127,255}));
  connect(solTheCon.actSolSys, actSolSys)
    annotation (Line(points={{-59.2,26},{110,26}}, color={255,0,255}));
  connect(weaBus, solCol.weaBus)
    annotation (Line(
      points={{-109,82},{-86,82},{-86,-30.4},{20,-30.4}},
      color={255,204,51},
      thickness=0.5));
  connect(weaBus, solTheCon.weaBus)
    annotation (Line(
      points={{-109,82},{-86,82},{-86,36},{-80.2,36}},
      color={255,204,51},
      thickness=0.5));
  connect(solCol.port_b, port_b)
    annotation (Line(points={{40,-40},{68,-40},
          {68,-40},{100,-40}}, color={0,127,255}));
  connect(solTheCon.pumSol, pumSol)
    annotation (Line(points={{-59.2,34},{90,34},{90,46},{110,46}}, color={0,0,127}));
  connect(TTanHeaTop, solTheCon.TTanHeaTop) annotation (Line(points={{-110,46},{
          -94,46},{-94,34},{-80.8,34}}, color={0,0,127}));
  connect(TSetSol, solTheCon.TSetSol)
    annotation (Line(points={{-110,26},{-80.8,26}}, color={0,0,127}));
  annotation (Icon(coordinateSystem(preserveAspectRatio=false), graphics={
        Line(
          points={{-6,-6},{8,8}},
          color={255,255,0},
          smooth=Smooth.None,
          thickness=1,
          origin={-24,30},
          rotation=90),
        Line(
          points={{-8,-8},{6,6}},
          color={255,255,0},
          smooth=Smooth.None,
          thickness=1,
          origin={28,32},
          rotation=180),
        Line(
          points={{-10,0},{10,0}},
          color={255,255,0},
          smooth=Smooth.None,
          thickness=1,
          origin={0,40},
          rotation=90),
        Rectangle(
          extent={{-100,100},{100,-100}},
          lineColor={27,0,55},
          fillColor={170,213,255},
          fillPattern=FillPattern.Solid),
        Line(
          points={{-100,0},{-76,0},{-76,-90},{66,-90},{66,-60},{-64,-60},{-64,
              -30},{66,-30},{66,0},{-64,0},{-64,28},{66,28},{66,60},{-64,60},{
              -64,86},{78,86},{78,0},{98,0},{100,0}},
          color={0,128,255},
          thickness=1,
          smooth=Smooth.None),
        Ellipse(
          extent={{40,-28},{-40,46}},
          lineColor={255,255,85},
          fillColor={255,255,170},
          fillPattern=FillPattern.Solid),
        Line(points={{0,50},{0,78},{0,78}}, color={255,255,85}),
        Line(points={{0,-38},{0,-68}}, color={255,255,85}),
        Line(points={{-42,8},{-64,8}}, color={255,255,85}),
        Line(points={{42,10},{62,10}}, color={255,255,85}),
        Line(
          points={{36,36},{62,66}},
          color={255,255,85},
          thickness=0.5),
        Line(
          points={{-68,-52},{-42,-22}},
          color={255,255,85},
          thickness=0.5),
        Line(
          points={{-30,42},{-54,68}},
          color={255,255,85},
          thickness=0.5),
        Line(
          points={{60,-54},{36,-28}},
          color={255,255,85},
          thickness=0.5),               Text(
        extent={{-150,150},{150,110}},
        textString="%name",
        lineColor={0,0,255})}),                                  Diagram(
        coordinateSystem(preserveAspectRatio=false)),
        defaultComponentName="solTheMod",
         Documentation(info="<html>

<h4> Solar thermal module </h4>
<p>
The solar thermal system includes a sollar collector from <a href=\"Buildings.Fluid.SolarCollectors.ASHRAE93\"> Buildings.Fluid.SolarCollectors.ASHRAE93</a> and a water pump.
The model is easily extensible at heating and cooling district systems, where the main purpose is to seize the available solar energy.
The system operates when the:
</p>
<ul>
<li>
The incident solar radiation is large enough to produce useful energy gain. See more details in
<a href=\"Buildings.Fluid.SolarCollectors.Controls.BaseClasses.GCritCalc\">Buildings.Fluid.SolarCollectors.Controls.BaseClasses.GCritCalc.</a>
</li>
</ul>
<ul>
<li>
The hot buffer tank top level water temperature <code>TTanHeaTop</code> is lower than the setpoint water leaving temperature from the solar thermal module <code>TSetSol</code>.
</li>
</ul>
<h4>Notice</h4>
For more details on how to integrate the solar module into a district heating and cooling substation, see <a href=\"Buildings.DistrictHeatingCooling.EnergyTransferStations.EnergyTransferStation.Substation\">
Buildings.DistrictHeatingCooling.EnergyTransferStations.EnergyTransferStation.Substation</a>.
</html>", revisions="<html>
<ul>
<li>
 <br/>
</li>
</ul>
</html>"));
end SolarThermalModule;
