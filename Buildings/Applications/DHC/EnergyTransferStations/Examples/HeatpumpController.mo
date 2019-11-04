within Buildings.Applications.DHC.EnergyTransferStations.Examples;
model HeatpumpController
  "Reverse heatpump controller operates in heating mode only"
  package Medium = Buildings.Media.Water "Medium model";

  parameter Modelica.SIunits.MassFlowRate mSou_flow_nominal=per.mSou_flow_nominal
   "Source heat exchanger nominal mass flow rate";
  parameter Modelica.SIunits.MassFlowRate mLoa_flow_nominal=per.mLoa_flow_nominal
   "Load heat exchanger nominal mass flow rate";
  parameter Fluid.HeatPumps.Data.ReverseWaterToWater.Trane_Axiom_EXW240 per
    "Performance data"
    annotation (Placement(transformation(extent={{30,70},{50,90}})));
  parameter Real scaling_factor=1
   "Scaling factor for heatpump capacity";

  Buildings.Applications.DHC.EnergyTransferStations.Control.HeatPumpController heaPumCon
    annotation (Placement(transformation(extent={{-20,-10},{0,8}})));

  Fluid.HeatPumps.ReverseWaterToWater heaPum(
    m1_flow_nominal=mLoa_flow_nominal,
    m2_flow_nominal=mSou_flow_nominal,
    per=per,
    scaling_factor=scaling_factor,
    redeclare package Medium1 = Medium,
    redeclare package Medium2 = Medium,
    reverseCycle=false,
    energyDynamics=Modelica.Fluid.Types.Dynamics.FixedInitial,
    massDynamics=Modelica.Fluid.Types.Dynamics.FixedInitial,
    show_T=true,
    dp1_nominal=200,
    dp2_nominal=200,
    T1_start = 273.15+40,
    T2_start = 273.15+7)
    annotation (Placement(transformation(extent={{20,-10},{40,10}})));


  Buildings.Controls.OBC.CDL.Continuous.Sources.Sine     TLoaEnt(
    amplitude=5,
    freqHz=1/3000,
    offset=30 + 273.15)
    "Load heat exchanger entering water temperature"
    annotation (Placement(transformation(extent={{-92,70},{-72,90}})));
  Fluid.Sources.MassFlowSource_T loaPum(
    use_m_flow_in=false,
    m_flow=mLoa_flow_nominal,
    use_T_in=true,
    redeclare package Medium = Medium,
    nPorts=1)
   "Load Side water pump"
   annotation (Placement(transformation(
      extent={{10,-10},{-10,10}},
      rotation=180,
      origin={-44,84})));
  Modelica.Blocks.Sources.BooleanConstant heaMod(k=false) "Step control"
    annotation (Placement(transformation(extent={{-60,42},{-40,62}})));
  Modelica.Blocks.Sources.BooleanConstant CooMod "Step control"
    annotation (Placement(transformation(extent={{-58,-60},{-38,-40}})));
  Fluid.FixedResistances.PressureDrop res2(
    redeclare package Medium = Medium,
    m_flow_nominal=mSou_flow_nominal,
    dp_nominal=6000)
   "Flow resistance"
   annotation (Placement(transformation(extent={{10,-90},{-10,-70}})));
  Modelica.Fluid.Sources.FixedBoundary souVol(redeclare package Medium = Medium,
      nPorts=1) "Volume for source heat exchnager side"
   annotation (Placement(transformation(extent={{-44,-90},{-24,-70}})));
  Fluid.Sensors.TemperatureTwoPort TSouLvg(
    redeclare final package Medium = Medium,
    allowFlowReversal=false,
    m_flow_nominal=mSou_flow_nominal,
    tau=30) "Source heat exchanger side leaving water temperature"
    annotation (Placement(transformation(extent={{10,-10},{-10,10}},
      rotation=90,
      origin={16,-52})));
  Modelica.Blocks.Sources.Constant TCooSet(k=7 + 273.15)
    "Cooling setpoint temperature"
    annotation (Placement(transformation(extent={{-92,-90},{-72,-70}})));
  Fluid.FixedResistances.PressureDrop
                                res1(
    redeclare package Medium = Medium,
    m_flow_nominal=mLoa_flow_nominal,
    dp_nominal=6000)
   "Flow resistance"
   annotation (Placement(transformation(extent={{52,50},{72,70}})));
  Modelica.Fluid.Sources.FixedBoundary loaVol(redeclare package Medium = Medium,
      nPorts=1)
   "Volume for the load side"
   annotation (Placement(transformation(extent={{98,50},{78,70}})));
  Fluid.Sources.MassFlowSource_T souPum(
    m_flow=mSou_flow_nominal,
    use_T_in=true,
    redeclare package Medium = Medium,
    nPorts=1)
   "Source side water pump"
   annotation (Placement(transformation(
      extent={{-10,-10},{10,10}},
      rotation=180,
      origin={76,-6})));
  Buildings.Controls.OBC.CDL.Continuous.Sources.Sine TSouEnt(
    amplitude=3,
    freqHz=1/3000,
    offset=12 + 273.15,
    startTime=0) "Entering water temperature at the source side"
    annotation (Placement(transformation(extent={{60,-60},{80,-40}})));
  Modelica.Blocks.Sources.Constant THeaSet(k=40 + 273.15)
    "Heating set point temperature"
    annotation (Placement(transformation(extent={{-92,-50},{-72,-30}})));
  Modelica.Blocks.Sources.Constant THeaSetMax(k=55 + 273.15)
    "Maximum heating set point temperature"
    annotation (Placement(transformation(extent={{-90,2},{-70,22}})));
  Modelica.Blocks.Sources.Constant TSetHeaMin(k=25)
    "Minimum heating set point temperature"
    annotation (Placement(transformation(extent={{-90,34},{-70,54}})));
equation
  connect(loaPum.T_in,TLoaEnt. y)
    annotation (Line(points={{-56,80},{-70,80}}, color={0,0,127}));
  connect(heaPumCon.yHeaPumMod, heaPum.uMod) annotation (Line(points={{1.4,
          2.22045e-16},{10,2.22045e-16},{10,0},{19,0}},
                                color={255,127,0}));
  connect(heaPumCon.TSetHeaPum, heaPum.THeaLoaSet) annotation (Line(points={{1.4,4.2},
          {6,4.2},{6,9},{18.6,9}},          color={0,0,127}));
  connect(loaPum.ports[1], heaPum.port_a1) annotation (Line(points={{-34,84},{
          10,84},{10,6},{20,6}},
                              color={0,127,255}));
  connect(heaPumCon.ReqHea, heaMod.y) annotation (Line(points={{-21.4,9},{-28,9},
          {-28,52},{-39,52}}, color={255,0,255}));
  connect(heaPumCon.ReqCoo, CooMod.y) annotation (Line(points={{-21.4,6},{-28,6},
          {-28,-50},{-37,-50}}, color={255,0,255}));
  connect(TSouLvg.port_b,res2. port_a)
    annotation (Line(points={{16,-62},{16,-80},{10,-80}},color={0,127,255}));
  connect(souVol.ports[1],res2. port_b)
    annotation (Line(points={{-24,-80},{-10,-80}}, color={0,127,255}));
  connect(heaPum.port_b2,TSouLvg. port_a)
    annotation (Line(points={{20,-6},{16,-6},{16,-42}}, color={0,127,255}));
  connect(res1.port_b,loaVol. ports[1])
    annotation (Line(points={{72,60},{78,60}},color={0,127,255}));
  connect(TSouEnt.y, souPum.T_in) annotation (Line(points={{82,-50},{96,-50},{
          96,-10},{88,-10}}, color={0,0,127}));
  connect(heaPum.port_a2, souPum.ports[1])
    annotation (Line(points={{40,-6},{66,-6}}, color={0,127,255}));
  connect(res1.port_a, heaPum.port_b1) annotation (Line(points={{52,60},{46,60},
          {46,6},{40,6}}, color={0,127,255}));
  connect(THeaSetMax.y, heaPumCon.TSetHeaMax) annotation (Line(points={{-69,12},
          {-66,12},{-66,-3.8},{-21,-3.8}},color={0,0,127}));
  connect(heaPumCon.TSetCoo, TCooSet.y) annotation (Line(points={{-21,-8},{-60,
          -8},{-60,-80},{-71,-80}}, color={0,0,127}));
  connect(TSetHeaMin.y, heaPumCon.TSetHeaMin) annotation (Line(points={{-69,44},
          {-62,44},{-62,-1.4},{-21,-1.4}}, color={0,0,127}));
  connect(THeaSet.y, heaPumCon.TSetHea) annotation (Line(points={{-71,-40},{-64,
          -40},{-64,-6},{-21,-6}}, color={0,0,127}));
  connect(TSouLvg.T, heaPumCon.TSouLvg) annotation (Line(
      points={{5,-52},{-26,-52},{-26,-10},{-21,-10}},
      color={0,0,127},
      pattern=LinePattern.Dot));
  annotation (Icon(coordinateSystem(preserveAspectRatio=false), graphics={
        Ellipse(lineColor = {75,138,73},
                fillColor={255,255,255},
                fillPattern = FillPattern.Solid,
                extent={{-98,-100},{98,98}}),
        Polygon(lineColor = {0,0,255},
                fillColor = {75,138,73},
                pattern = LinePattern.None,
                fillPattern = FillPattern.Solid,
                points={{-30,64},{70,4},{-30,-56},{-30,64}})}),  Diagram(coordinateSystem(preserveAspectRatio=false, extent={
            {-100,-140},{100,100}}),
        graphics={Line(points={{-22,22}}, color={28,108,200})}),
    experiment(StopTime=14400),
    __Dymola_Commands(
  file="modelica://Buildings/Resources/Scripts/Dymola/Applications/DHC/EnergyTransferStations/Control/HeatpumpController.mos"
        "Simulate and plot"),
         experiment(Tolerance=1e-6, StopTime=14400));
end HeatpumpController;
