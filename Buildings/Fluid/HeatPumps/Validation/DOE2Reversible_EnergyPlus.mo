within Buildings.Fluid.HeatPumps.Validation;
model DOE2Reversible_EnergyPlus "Validation with EnergyPlus model"

  package Medium = Buildings.Media.Water "Medium model";

  parameter Data.DOE2Reversible.EnergyPlus perEP
    "EnergyPlus heat pump performance"
      annotation (Placement(transformation(extent={{80,80},{100,100}})));
  parameter Modelica.SIunits.MassFlowRate mSou_flow_nominal=perEP.hea.mSou_flow
    "Source heat exchanger nominal mass flow rate";
  parameter Modelica.SIunits.MassFlowRate mLoa_flow_nominal=perEP.hea.mLoa_flow
    "Load heat exchanger nominal mass flow rate";
  parameter Boolean reverseCycle= true
  "= true, if reversing the heatpump to cooling mode is required"
      annotation(Evaluate=true, HideResult=true, Dialog(group="Conditional inputs"));

  DOE2Reversible heaPum(
    redeclare package Medium1 = Medium,
    redeclare package Medium2 = Medium,
    m1_flow_nominal=mSou_flow_nominal,
    m2_flow_nominal=mLoa_flow_nominal,
    per=perEP,
    reverseCycle=reverseCycle,
    energyDynamics=Modelica.Fluid.Types.Dynamics.SteadyState,
    massDynamics=Modelica.Fluid.Types.Dynamics.SteadyState)
    "Water to Water heat pump"
      annotation (Placement(transformation(extent={{30,16},{50,36}})));
  Sources.MassFlowSource_T loaPum(
    redeclare package Medium = Medium,
    use_m_flow_in=false,
    m_flow=0.346,
    T=328.15,
    nPorts=1,
    use_T_in=true) "Load water pump"
     annotation (Placement(transformation(extent={{10,-10},{-10,10}},rotation=180,
       origin={-10,100})));
  Sources.MassFlowSource_T souPum(
    redeclare package Medium = Medium,
    use_m_flow_in=false,
    m_flow=0.35,
    T=280.65,
    nPorts=1,
    use_T_in=true) "Source side water pump"
    annotation (Placement(transformation(extent={{-10,-10},{10,10}},rotation=180,
       origin={70,10})));
  Buildings.Fluid.Sources.Boundary_pT cooVol(
    redeclare package Medium = Medium, nPorts=1)
    "Volume for cooling load"
      annotation (Placement(transformation(extent={{-20,-40},{0,-20}})));
  Buildings.Fluid.Sources.Boundary_pT heaVol(
    redeclare package Medium = Medium, nPorts=1)
    "Volume for heating load"
      annotation (Placement(transformation(extent={{80,22},{60,42}})));

  Modelica.Blocks.Sources.CombiTimeTable datRea(
    tableOnFile=true,
    fileName=ModelicaServices.ExternalReferences.loadResource(
        "modelica://Buildings//Resources/Data/Fluid/HeatPumps/Validation/DOE2Reversible_EnergyPlus/modelica.csv"),
    columns=2:81,
    tableName="modelica",
    smoothness=Modelica.Blocks.Types.Smoothness.ConstantSegments)
      annotation (Placement(transformation(extent={{-108,86},{-88,106}})));

  Buildings.Controls.OBC.UnitConversions.From_degC TLoaEnt
    "Block that converts entering water temperature of the load side"
      annotation (Placement(transformation(extent={{-60,86},{-40,106}})));
  Buildings.Controls.OBC.UnitConversions.From_degC TSouEnt
    "Block that converts entering water temperature of the source side"
      annotation (Placement(transformation(extent={{-60,-20},{-40,0}})));
  Buildings.Controls.OBC.UnitConversions.From_degC TSetCoo
    "Block that converts set point for leaving heating water temperature "
      annotation (Placement(transformation(extent={{-60,56},{-40,76}})));
  Modelica.Blocks.Sources.RealExpression P_EP(y=datRea.y[18])
    "EnergyPlus results: compressor power "
      annotation (Placement(transformation(extent={{-80,-60},{-60,-40}})));
  Controls.OBC.CDL.Continuous.Sources.Constant TSouLvgMin(k=35 + 273.15)
    annotation (Placement(transformation(extent={{-20,-80},{0,-60}})));
  Controls.OBC.CDL.Continuous.Sources.Constant TSouLvgMax(k=60 + 273.15)
    annotation (Placement(transformation(extent={{-20,0},{0,20}})));
  Modelica.Blocks.Sources.RealExpression QLoa_flow_EP(y=-1*datRea.y[19])
    "EnergyPlus results: load side heat flow rate"
    annotation (Placement(transformation(extent={{-80,-80},{-60,-60}})));
  Modelica.Blocks.Math.RealToInteger realToInteger1
    annotation (Placement(transformation(extent={{-58,16},{-38,36}})));
  Modelica.Blocks.Math.Product product
    annotation (Placement(transformation(extent={{-104,6},{-84,26}})));
  Controls.OBC.CDL.Continuous.Sources.Constant cons(k=-1)
    annotation (Placement(transformation(extent={{-116,-40},{-96,-20}})));
equation
  connect(heaPum.port_a1,loaPum. ports[1])
    annotation (Line(points={{30,32},{20,32},{20,100},{-1.77636e-15,100}},
                                                                      color={0,127,255}));
  connect(souPum.ports[1], heaPum.port_a2)
    annotation (Line(points={{60,10},{50,10},{50,20}},  color={0,127,255}));
  connect(cooVol.ports[1], heaPum.port_b2)
    annotation (Line(points={{0,-30},{20,-30},{20,20},{30,20}}, color={0,127,255}));
  connect(heaPum.port_b1, heaVol.ports[1])
    annotation (Line(points={{50,32},{60,32}},               color={0,127,255}));
  connect(loaPum.T_in, TLoaEnt.y)
    annotation (Line(points={{-22,96},{-38,96}}, color={0,0,127}));
  connect(TSouEnt.y, souPum.T_in)
    annotation (Line(points={{-38,-10},{92,-10},{92,6},{82,6}},     color={0,0,127}));
  connect(TSetCoo.y, heaPum.TSet)
    annotation (Line(points={{-38,66},{16,66},{16,35},{29,35}},
                        color={0,0,127}));
  connect(TSouLvgMin.y, heaPum.TSouLvg1) annotation (Line(points={{2,-70},{24,-70},
          {24,18},{29,18}}, color={0,0,127}));
  connect(TSouLvgMax.y,heaPum.TLoaMaxLvg)
    annotation (Line(points={{2,10},{18,10},{18,22},{29,22}}, color={0,0,127}));
  connect(datRea.y[21], TLoaEnt.u)
    annotation (Line(points={{-87,96},{-62,96}}, color={0,0,127}));
  connect(datRea.y[22],TSetCoo. u)
    annotation (Line(points={{-87,96},{-70,96},{-70,66},{-62,66}}, color={0,0,127}));
  connect(datRea.y[28], TSouEnt.u)
    annotation (Line(points={{-87,96},{-70,96},{-70,-10},{-62,-10}},
                                                                   color={0,0,127}));
  connect(realToInteger1.y, heaPum.uMod)
    annotation (Line(points={{-37,26},{29,26}}, color={255,127,0}));
  connect(datRea.y[16], product.u1) annotation (Line(points={{-87,96},{-82,96},
          {-82,38},{-116,38},{-116,22},{-106,22}}, color={0,0,127}));
  connect(cons.y, product.u2) annotation (Line(points={{-94,-30},{-88,-30},{-88,
          -2},{-116,-2},{-116,10},{-106,10}}, color={0,0,127}));
  connect(product.y, realToInteger1.u) annotation (Line(points={{-83,16},{-74,
          16},{-74,26},{-60,26}}, color={0,0,127}));
  annotation (Icon(coordinateSystem(preserveAspectRatio=false, extent={{-100,-100},
            {100,100}}),
               graphics={
        Ellipse(lineColor = {75,138,73},
                fillColor={255,255,255},
                fillPattern = FillPattern.Solid,
                extent={{-100,-102},{100,98}}),
        Polygon(lineColor = {0,0,255},
                fillColor = {75,138,73},
                pattern = LinePattern.None,
                fillPattern = FillPattern.Solid,
                points={{-30,64},{70,4},{-30,-56},{-30,64}})}),
        Diagram(
        coordinateSystem(preserveAspectRatio=false, extent={{-120,-120},{120,120}})),
                 __Dymola_Commands(file= "modelica://Buildings/Resources/Scripts/Dymola/Fluid/HeatPumps/Validation/DOE2_EnergyPlus.mos"
        "Simulate and plot"),
    experiment(StopTime=3000, Tolerance=1e-06),
Documentation(info="<html>
<p>
This model implements a comparative model validation of
<a href=\"Buildings.Fluid.HeatPumps.EquationFitReversible\">
Buildings.Fluid.HeatPumps.EquationFitReversible</a>
against results obtained using EnergyPlus 9.1.
<p>
The EnergyPlus results were generated using the example file <code>GSHPSimple-GLHE-ReverseHeatPump.idf</code>
from EnergyPlus 9.1, with a nominal cooling capacity of <i>39890</i> Watts and
nominal heating capacity of <i>39040</i> Watts.
</p>
</html>", revisions="<html>
<ul>
<li>
September 17, 2019, by Michael Wetter:<br/>
Revised implementation.
</li>
<li>
May 3, 2019, by Hagar Elarga:<br/>
First implementation.
</li>
</ul>
</html>"));
end DOE2Reversible_EnergyPlus;
