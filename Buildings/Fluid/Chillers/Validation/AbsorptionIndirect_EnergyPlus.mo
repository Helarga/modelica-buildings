within Buildings.Fluid.Chillers.Validation;
model AbsorptionIndirect_EnergyPlus
  "Validation with EnergyPlus model"
 package Medium = Buildings.Media.Water "Medium model";

  AbsorptionIndirectChiller absChi(
    redeclare package Medium1 = Medium,
    redeclare package Medium2 = Medium,
    per=Data.AbsorptionIndirect.AbsorptionIndirectChiller_EnergyPlus(),
    allowFlowReversal1=true,
    allowFlowReversal2=true,
    m1_flow_nominal=mCon_flow_nominal,
    m2_flow_nominal=mEva_flow_nominal,
    show_T=true,
    dp1_nominal=200,
    dp2_nominal=200,
    tau1=30,
    tau2=30,
    homotopyInitialization=true,
    energyDynamics=Modelica.Fluid.Types.Dynamics.SteadyState,
    massDynamics=Modelica.Fluid.Types.Dynamics.SteadyState,
    T1_start=25 + 273.15,
    T2_start=10 + 273.15) "Absorption indirect chiller"
    annotation (Placement(transformation(extent={{26,-10},{46,10}})));
  parameter Data.AbsorptionIndirect.AbsorptionIndirectChiller_EnergyPlus per
    "EnergyPlus chiller performance"
    annotation (Placement(transformation(extent={{32,76},{52,96}})));

  parameter Modelica.SIunits.MassFlowRate mEva_flow_nominal=per.mEva_flow_nominal
    "Evaporator nominal mass flow rate";
  parameter Modelica.SIunits.MassFlowRate mCon_flow_nominal=per.mCon_flow_nominal
    "Condenser nominal mass flow rate";

  Sources.MassFlowSource_T conPum(
    use_m_flow_in=false,
      m_flow=mCon_flow_nominal,
      use_T_in=true,
      redeclare package Medium = Medium,
    nPorts=1)
    "Condenser water pump"
      annotation (
      Placement(transformation(
        extent={{10,-10},{-10,10}},
        rotation=180,
        origin={-30,50})));
  Sources.MassFlowSource_T evaPum(
    use_m_flow_in=true,
    m_flow=mEva_flow_nominal,
      nPorts=1,
      use_T_in=true,
      redeclare package Medium = Medium)
    "Evaporator water pump"
      annotation (
      Placement(transformation(
        extent={{-12,-12},{12,12}},
        rotation=180,
        origin={82,-8})));
  Modelica.Fluid.Sources.FixedBoundary cooVol(
    redeclare package Medium = Medium, nPorts=1)
      "Volume for cooling load"
       annotation (Placement(transformation(extent={{-100,-70},{-80,-50}})));
  Modelica.Fluid.Sources.FixedBoundary heaVol(
    redeclare package Medium = Medium, nPorts=1)
      "Volume for heating load"
       annotation (Placement(transformation(extent={{120,60},{100,80}})));
  FixedResistances.PressureDrop res1(
      redeclare package Medium = Medium,
      m_flow_nominal=mCon_flow_nominal,
      dp_nominal=6000)
      "Flow resistance"
       annotation (Placement(transformation(extent={{60,30},{80,50}})));
  FixedResistances.PressureDrop res2(
      redeclare package Medium = Medium,
       m_flow_nominal=mEva_flow_nominal,
       dp_nominal=6000)
      "Flow resistance"
       annotation (Placement(transformation(extent={{-60,-70},{-40,-50}})));
    Modelica.Blocks.Math.RealToBoolean realToBoolean(threshold=1)
         annotation (Placement(transformation(extent={{-60,10},{-40,-10}})));
  Modelica.Blocks.Sources.CombiTimeTable datRea(
    tableOnFile=true,
    fileName=ModelicaServices.ExternalReferences.loadResource(
        "modelica://Buildings/Resources/Data/Fluid/Chillers/IndirectAbsorptionChiller/modelica.csv"),
    columns=2:70,
    tableName="modelica",
    smoothness=Modelica.Blocks.Types.Smoothness.ConstantSegments)
    "Reader for \"IndirectAbsorptionChiller.IDF\" energy plus example results"
      annotation (Placement(transformation(extent={{-140,60},{-120,80}})));

  Controls.OBC.UnitConversions.From_degC TConEnt1
    "Block that converts entering water temperature of the condenser"
    annotation (Placement(transformation(extent={{-100,60},{-80,80}})));
  Modelica.Blocks.Sources.RealExpression QGen_EP(y=datRea.y[40])
    "EnergyPlus results: generator heat flow rate"
    annotation (Placement(transformation(extent={{-136,-58},{-116,-38}})));
  Controls.OBC.UnitConversions.From_degC TEvaSet
    "Block that converts setpoint water temperature of the evaporator"
    annotation (Placement(transformation(extent={{-20,-40},{0,-20}})));
  Controls.OBC.UnitConversions.From_degC TEvaEnt
    "Block that converts entering water temperature to the evaporator"
    annotation (Placement(transformation(extent={{-20,-90},{0,-70}})));
  Modelica.Blocks.Sources.RealExpression QCon_EP(y=datRea.y[36])
    "EnergyPlus results: condenser heat flow rate"
    annotation (Placement(transformation(extent={{-136,-40},{-116,-20}})));
  Modelica.Blocks.Sources.RealExpression QEva_EP(y=-1*datRea.y[32])
    "EnergyPlus results: evaporator heat flow rate"
    annotation (Placement(transformation(extent={{-136,-24},{-116,-4}})));
equation
  connect(evaPum.ports[1], absChi.port_a2) annotation (Line(points={{70,-8},{70,
          -6},{46,-6}},          color={0,127,255}));
  connect(res1.port_a, absChi.port_b1) annotation (Line(points={{60,40},{58,40},
          {58,6},{46,6}}, color={0,127,255}));
  connect(res2.port_b, absChi.port_b2) annotation (Line(points={{-40,-60},{-28,
          -60},{-28,-6},{27.8182,-6}},
                            color={0,127,255}));
  connect(res2.port_a, cooVol.ports[1])
   annotation (Line(points={{-60,-60},{-80,-60}}, color={0,127,255}));
  connect(res1.port_b, heaVol.ports[1])
   annotation (Line(points={{80,40},{80,70},{100,70}},
                                              color={0,127,255}));
  connect(absChi.on, realToBoolean.y) annotation (Line(points={{26.8182,0.1},{0,
          0.1},{0,0},{-39,0}}, color={255,0,255}));
  connect(conPum.ports[1], absChi.port_a1) annotation (Line(points={{-20,50},{
          20,50},{20,6},{27.8182,6}},
                              color={0,127,255}));
  connect(datRea.y[37], TConEnt1.u)
    annotation (Line(points={{-119,70},{-102,70}}, color={0,0,127}));
  connect(TConEnt1.y, conPum.T_in) annotation (Line(points={{-79,70},{-60,70},{-60,
          46},{-42,46}}, color={0,0,127}));
  connect(datRea.y[34], TEvaSet.u) annotation (Line(points={{-119,70},{-110,70},
          {-110,-30},{-22,-30}}, color={0,0,127}));
  connect(TEvaSet.y, absChi.TEvaSet) annotation (Line(points={{1,-30},{20,-30},
          {20,-8.9},{26.8182,-8.9}}, color={0,0,127}));
  connect(datRea.y[33], TEvaEnt.u) annotation (Line(points={{-119,70},{-110,70},
          {-110,-80},{-22,-80}}, color={0,0,127}));
  connect(TEvaEnt.y, evaPum.T_in) annotation (Line(points={{1,-80},{112,-80},{
          112,-12.8},{96.4,-12.8}}, color={0,0,127}));
  connect(datRea.y[31], realToBoolean.u) annotation (Line(points={{-119,70},{-110,
          70},{-110,0},{-62,0}}, color={0,0,127}));
  connect(datRea.y[35], evaPum.m_flow_in) annotation (Line(points={{-119,70},{
          -110,70},{-110,-46},{106,-46},{106,-17.6},{96.4,-17.6}}, color={0,0,
          127}));
   annotation (Icon(coordinateSystem(preserveAspectRatio=false, extent={{-100,
            -100},{100,100}}), graphics={
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
        coordinateSystem(preserveAspectRatio=false, extent={{-140,-100},{120,100}})),
                 __Dymola_Commands(file= "modelica://Buildings/Resources/Scripts/Dymola/Fluid/Chillers/Validation/AbsorptionIndirect_EnergyPlus.mos"
        "Simulate and plot"),
    experiment(StopTime=86400, Tolerance=1e-06),
  Documentation(info="<html>
<p>
This model validates the model
<a href=\"Buildings.Fluid.Chillers.AbsorptionIndirectChiller\">
Buildings.Fluid.Chillers.AbsorptionIndirectChiller</a>.
<p>
The EnergyPlus results were generated using the example file <code>IndirectAbsorptionChiller.idf</code>
from EnergyPlus 9.1, with a nominal cooling capacity of <i>10000</i> Watts.
</p>
</html>", revisions="<html>
<ul>
<li>
July 4, 2019, by Hagar Elarga:<br/>
First implementation.
</li>
</ul>
</html>"));
end AbsorptionIndirect_EnergyPlus;
