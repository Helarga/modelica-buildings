within Buildings.Applications.DHC.Loads.Examples;
model CouplingSpawnZ1
  "Example illustrating the coupling of a building model to heating water and chilled water loops"
  extends Modelica.Icons.Example;
  package Medium1 = Buildings.Media.Water
    "Source side medium";
  Buildings.Applications.DHC.Loads.Examples.BaseClasses.BuildingSpawnZ1 bui(
    nPorts_aCoo=1,
    nPorts_aHea=1,
    nPorts_bHea=1,
    nPorts_bCoo=1)
    "Building"
    annotation (Placement(transformation(extent={{40,-40},{60,-20}})));
  Buildings.Fluid.Sources.MassFlowSource_T supHeaWat(
    use_m_flow_in=true,
    redeclare package Medium = Medium1,
    use_T_in=true,
    nPorts=1) "Heating water supply" annotation (Placement(transformation(
        extent={{-10,-10},{10,10}},
        rotation=0,
        origin={-30,0})));
  Modelica.Blocks.Sources.RealExpression THeaWatSup(y=bui.terUni.T_aHeaWat_nominal)
    "Heating water supply temperature"
    annotation (Placement(transformation(extent={{-80,-10},{-60,10}})));
  Modelica.Blocks.Sources.RealExpression mHeaWat_flow(y=bui.disFloHea.mReqTot_flow)
    "Heating water flow rate"
    annotation (Placement(transformation(extent={{-80,10},{-60,30}})));
  Buildings.Fluid.Sources.MassFlowSource_T supChiWat(
    use_m_flow_in=true,
    redeclare package Medium = Medium1,
    use_T_in=true,
    nPorts=1) "Chilled water supply" annotation (Placement(transformation(
        extent={{-10,-10},{10,10}},
        rotation=0,
        origin={-30,-80})));
  Modelica.Blocks.Sources.RealExpression TChiWatSup(y=bui.terUni.T_aChiWat_nominal)
    "Chilled water supply temperature"
    annotation (Placement(transformation(extent={{-80,-90},{-60,-70}})));
  Modelica.Blocks.Sources.RealExpression mChiWat_flow(y=bui.disFloCoo.mReqTot_flow)
    "Chilled water flow rate"
    annotation (Placement(transformation(extent={{-80,-70},{-60,-50}})));
  Buildings.Fluid.Sources.Boundary_pT sinHeaWat(
    redeclare package Medium = Medium1,
    p=300000,
    nPorts=1) "Sink for heating water" annotation (Placement(transformation(
        extent={{10,-10},{-10,10}},
        rotation=0,
        origin={130,0})));
  Buildings.Fluid.Sources.Boundary_pT sinChiWat(
    redeclare package Medium = Medium1,
    p=300000,
    nPorts=1) "Sink for chilled water" annotation (Placement(transformation(
        extent={{10,-10},{-10,10}},
        rotation=0,
        origin={130,-80})));
equation
  connect(THeaWatSup.y, supHeaWat.T_in) annotation (Line(points={{-59,0},{-50,0},
          {-50,4},{-42,4}}, color={0,0,127}));
  connect(mHeaWat_flow.y, supHeaWat.m_flow_in) annotation (Line(points={{-59,20},
          {-50,20},{-50,8},{-42,8}}, color={0,0,127}));
  connect(TChiWatSup.y, supChiWat.T_in) annotation (Line(points={{-59,-80},{-50,
          -80},{-50,-76},{-42,-76}}, color={0,0,127}));
  connect(mChiWat_flow.y, supChiWat.m_flow_in) annotation (Line(points={{-59,
          -60},{-50,-60},{-50,-72},{-42,-72}}, color={0,0,127}));
  connect(supChiWat.ports[1], bui.secCooSup[1]) annotation (Line(points={{-20,-80},
          {18,-80},{18,-32.9333},{40,-32.9333}}, color={0,127,255}));
  connect(supHeaWat.ports[1], bui.secHeaSup[1]) annotation (Line(points={{-20,0},
          {0,0},{0,-36},{40,-36}}, color={0,127,255}));
  connect(sinHeaWat.ports[1], bui.secHeaRet[1]) annotation (Line(points={{120,0},
          {80,0},{80,-36},{60,-36}}, color={0,127,255}));
  connect(sinChiWat.ports[1], bui.secCooRet[1]) annotation (Line(points={{120,-80},
          {68,-80},{68,-33},{60,-33}}, color={0,127,255}));
  annotation (
  experiment(
      StopTime=604800,
      Tolerance=1e-06),
  Documentation(info="<html>
<p>
This example illustrates the use of
<a href=\"modelica://Buildings.Applications.DHC.Loads.BaseClasses.PartialBuilding\">
Buildings.Applications.DHC.Loads.BaseClasses.PartialBuilding</a>,
<a href=\"modelica://Buildings.Applications.DHC.Loads.BaseClasses.PartialTerminalUnit\">
Buildings.Applications.DHC.Loads.BaseClasses.PartialTerminalUnit</a>
and
<a href=\"modelica://Buildings.Applications.DHC.Loads.BaseClasses.FlowDistribution\">
Buildings.Applications.DHC.Loads.BaseClasses.FlowDistribution</a>
in a configuration with
</p>
<ul>
<li>
a one-zone building model based on an EnergyPlus envelope model, and
</li>
<li>
no secondary pumps.
</li>
</ul>
</html>",
revisions=
"<html>
<ul>
<li>
February 21, 2020, by Antoine Gautier:<br/>
First implementation.
</li>
</ul>
</html>"),
  Diagram(
        coordinateSystem(preserveAspectRatio=false, extent={{-100,-140},{160,80}})),
  __Dymola_Commands(file="Resources/Scripts/Dymola/Applications/DHC/Loads/Examples/CouplingSpawnZ1.mos"
        "Simulate and plot"));
end CouplingSpawnZ1;