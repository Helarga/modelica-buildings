within Buildings.Applications.DHC.CentralPlants.Cooling;
model Ex_Machine_dp "District cooling plant model"

  package Medium = Buildings.Media.Water "Medium model";

  parameter Integer numChi(min=1, max=2)=1 "Number of chillers, maximum is 2";

  parameter Boolean show_T = true
    "= true, if actual temperature at port is computed"
    annotation(Dialog(tab="Advanced",group="Diagnostics"));

  // chiller parameters

  parameter Modelica.SIunits.MassFlowRate mCHW_flow_nominal=5
    "Nominal chilled water mass flow rate"
    annotation (Dialog(group="Chiller"));
  parameter Modelica.SIunits.Pressure dpCHW_nominal=4000
    "Pressure difference at the chilled water side"
    annotation (Dialog(group="Chiller"));
  parameter Modelica.SIunits.Power QEva_nominal=60000
    "Nominal cooling capacity of single chiller (negative means cooling)"
    annotation (Dialog(group="Chiller"));
  parameter Modelica.SIunits.MassFlowRate mMin_flow=0.2
    "Minimum mass flow rate of single chiller"
    annotation (Dialog(group="Chiller"));
  parameter Modelica.SIunits.Pressure dpCHWPum_nominal=50000
    "Nominal pressure drop of chilled water pumps"
    annotation (Dialog(group="Pump"));

  // control settings
  parameter Modelica.SIunits.Time tWai=10 "Waiting time"
    annotation (Dialog(group="Control Settings"));
  parameter Modelica.SIunits.PressureDifference dpSetPoi(displayUnit="Pa")=50000
   "Demand side pressure difference setpoint"
    annotation (Dialog(group="Control Settings"));

  // dynamics
  parameter Modelica.Fluid.Types.Dynamics energyDynamics=Modelica.Fluid.Types.Dynamics.DynamicFreeInitial
    "Type of energy balance: dynamic (3 initialization options) or steady state"
    annotation(Evaluate=true, Dialog(tab = "Dynamics", group="Equations"));
  parameter Modelica.Fluid.Types.Dynamics massDynamics=energyDynamics
    "Type of mass balance: dynamic (3 initialization options) or steady state"
    annotation(Evaluate=true, Dialog(tab = "Dynamics", group="Equations"));


 parameter Buildings.Fluid.Movers.Data.Generic perCHWPum(
    pressure=Buildings.Fluid.Movers.BaseClasses.Characteristics.flowParameters(
      V_flow={0,mCHW_flow_nominal}/1000,
      dp={(dpSetPoi+dpCHW_nominal*2),0}))
    "Performance data for chilled water pumps";

  DataCenters.ChillerCooled.Equipment.FlowMachine_dp pumCHW(
    redeclare package Medium = Medium,
    per=fill(perCHWPum, numChi),
    riseTimePump=100,
    energyDynamics=energyDynamics,
    m_flow_nominal=mCHW_flow_nominal,
    dpValve_nominal=dpCHWPum_nominal,
    num=1)      "Chilled water pumps"
    annotation (Placement(transformation(extent={{-12,6},{-32,26}})));

  Modelica.Blocks.Sources.Trapezoid dpMea[numChi](
    amplitude=2500,
    rising=200,
    width=400,
    falling=200,
    period=1000,
    offset=2500)
    annotation (Placement(transformation(extent={{24,-38},{4,-18}})));
  Fluid.Sources.Boundary_pT           expTanCHW(redeclare package Medium =
        Medium, nPorts=1)
                "Chilled water expansion tank"
    annotation (Placement(transformation(extent={{-10,-10},{10,10}},
        rotation=180,
        origin={72,14})));
  Fluid.Sources.Boundary_pT           expTanCHW1(redeclare package Medium =
        Medium,
    p=300000,
    nPorts=1)   "Chilled water expansion tank"
    annotation (Placement(transformation(extent={{-102,-2},{-82,18}})));
  Fluid.FixedResistances.PressureDrop           heaCoi2(
    redeclare package Medium = Medium,
    m_flow_nominal=5,
    dp_nominal=50000)        "Heating coil pressure drop"
    annotation (Placement(transformation(extent={{-48,2},{-68,22}})));
  Modelica.Blocks.Sources.RealExpression dpSet[numChi](y=dpSetPoi)
    annotation (Placement(transformation(extent={{22,-8},{2,12}})));
  Fluid.FixedResistances.PressureDrop           heaCoi1(
    redeclare package Medium = Medium,
    m_flow_nominal=5,
    dp_nominal=50000)        "Heating coil pressure drop"
    annotation (Placement(transformation(extent={{46,4},{26,24}})));
  Fluid.Sensors.RelativePressure senRelPre[numChi](redeclare package Medium =
        Medium) "Pressure difference across air system"
    annotation (Placement(transformation(extent={{16,66},{-4,46}})));
protected
  final parameter Medium.ThermodynamicState sta_default = Medium.setState_pTX(
    T=Medium.T_default,
    p=Medium.p_default,
    X=Medium.X_default) "Medium state at default properties";
  final parameter Modelica.SIunits.SpecificHeatCapacity cp_default=
    Medium.specificHeatCapacityCp(sta_default)
    "Specific heat capacity of the fluid";

equation

  connect(pumCHW.port_b, heaCoi2.port_a) annotation (Line(points={{-32,16},{-40,
          16},{-40,12},{-48,12}}, color={0,127,255}));
  connect(expTanCHW1.ports[1], heaCoi2.port_b) annotation (Line(points={{-82,8},
          {-76,8},{-76,12},{-68,12}}, color={0,127,255}));
  connect(pumCHW.dpSet, dpSet.y) annotation (Line(points={{-10,13.8},{-4,13.8},{
          -4,2},{1,2}}, color={0,0,127}));
  connect(pumCHW.port_a, heaCoi1.port_b) annotation (Line(points={{-12,16},{8,
          16},{8,14},{26,14}}, color={0,127,255}));
  connect(expTanCHW.ports[1], heaCoi1.port_a)
    annotation (Line(points={{62,14},{46,14}}, color={0,127,255}));
  connect(dpMea.y, pumCHW.dpMea) annotation (Line(points={{3,-28},{-6,-28},{-6,
          8},{-10,8}}, color={0,0,127}));
  connect(senRelPre.p_rel, pumCHW.u) annotation (Line(points={{6,65},{6,74},{-6,
          74},{-6,20},{-10,20}}, color={0,0,127}));
  annotation (__Dymola_Commands,
  Diagram(coordinateSystem(preserveAspectRatio=false, extent={{-140,-80},{160,100}})),
    experiment(
      StopTime=3600,
      __Dymola_NumberOfIntervals=1440,
      __Dymola_Algorithm="Dassl"),
    __Dymola_experimentSetupOutput,
    Documentation(info="<html>
<p>The schematic drawing of the Lejeune plant is shown as folowing.</p>
<p><img src=\"Resources/Images/lejeunePlant/lejeune_schematic_drawing.jpg\" alt=\"image\"/> </p>
<p>In addition, the parameters are listed as below.</p>
<p>The parameters for the chiller plant.</p>
<p><img src=\"Resources/Images/lejeunePlant/Chiller.png\" alt=\"image\"/> </p>
<p>The parameters for the primary chilled water pump.</p>
<p><img src=\"Resources/Images/lejeunePlant/PriCHWPum.png\" alt=\"image\"/> </p>
<p>The parameters for the secondary chilled water pump.</p>
<p><img src=\"Resources/Images/lejeunePlant/SecCHWPum1.png\" alt=\"image\"/> </p>
<p><img src=\"Resources/Images/lejeunePlant/SecCHWPum2.png\" alt=\"image\"/> </p>
<p>The parameters for the condenser water pump.</p>
<p><img src=\"Resources/Images/lejeunePlant/CWPum.png\" alt=\"image\"/> </p>
</html>"),
    Icon(coordinateSystem(extent={{-100,-100},{100,100}}),    graphics={
                                Rectangle(
        extent={{-100,-100},{100,100}},
        lineColor={0,0,127},
        fillColor={255,255,255},
        fillPattern=FillPattern.Solid),
        Polygon(
          points={{-62,-14},{-62,-14}},
          lineColor={238,46,47},
          fillColor={255,255,255},
          fillPattern=FillPattern.Solid),
        Polygon(
          points={{80,-60},{-80,-60},{-80,60},{-60,60},{-60,0},{-40,0},{-40,20},
              {0,0},{0,20},{40,0},{40,20},{80,0},{80,-60}},
          lineColor={95,95,95},
          fillColor={28,108,200},
          fillPattern=FillPattern.Solid),
      Rectangle(
        extent={{46,-38},{58,-26}},
        lineColor={255,255,255},
        fillColor={255,255,255},
        fillPattern=FillPattern.Solid),
      Rectangle(
        extent={{62,-38},{74,-26}},
        lineColor={255,255,255},
        fillColor={255,255,255},
        fillPattern=FillPattern.Solid),
      Rectangle(
        extent={{62,-54},{74,-42}},
        lineColor={255,255,255},
        fillColor={255,255,255},
        fillPattern=FillPattern.Solid),
      Rectangle(
        extent={{46,-54},{58,-42}},
        lineColor={255,255,255},
        fillColor={255,255,255},
        fillPattern=FillPattern.Solid),
      Rectangle(
        extent={{22,-54},{34,-42}},
        lineColor={255,255,255},
        fillColor={255,255,255},
        fillPattern=FillPattern.Solid),
      Rectangle(
        extent={{6,-54},{18,-42}},
        lineColor={255,255,255},
        fillColor={255,255,255},
        fillPattern=FillPattern.Solid),
      Rectangle(
        extent={{6,-38},{18,-26}},
        lineColor={255,255,255},
        fillColor={255,255,255},
        fillPattern=FillPattern.Solid),
      Rectangle(
        extent={{22,-38},{34,-26}},
        lineColor={255,255,255},
        fillColor={255,255,255},
        fillPattern=FillPattern.Solid),
      Rectangle(
        extent={{-18,-54},{-6,-42}},
        lineColor={255,255,255},
        fillColor={255,255,255},
        fillPattern=FillPattern.Solid),
      Rectangle(
        extent={{-34,-54},{-22,-42}},
        lineColor={255,255,255},
        fillColor={255,255,255},
        fillPattern=FillPattern.Solid),
      Rectangle(
        extent={{-34,-38},{-22,-26}},
        lineColor={255,255,255},
        fillColor={255,255,255},
        fillPattern=FillPattern.Solid),
      Rectangle(
        extent={{-18,-38},{-6,-26}},
        lineColor={255,255,255},
        fillColor={255,255,255},
        fillPattern=FillPattern.Solid),
        Text(
          extent={{-149,-114},{151,-154}},
          lineColor={0,0,255},
          textString="%name")}));
end Ex_Machine_dp;
