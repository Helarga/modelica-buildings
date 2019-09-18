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


  Buildings.Controls.OBC.CDL.Continuous.Sources.Constant TLoaEnt(k=273.15 + 40)
    "Load heat exchanger entering water temperature"
    annotation (Placement(transformation(extent={{-90,70},{-70,90}})));
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
    annotation (Placement(transformation(extent={{-60,40},{-40,60}})));
  Modelica.Blocks.Sources.BooleanConstant CooMod "Step control"
    annotation (Placement(transformation(extent={{-60,-60},{-40,-40}})));
  Fluid.FixedResistances.PressureDrop res2(
    redeclare package Medium = Medium,
    m_flow_nominal=mSou_flow_nominal,
    dp_nominal=6000)
   "Flow resistance"
   annotation (Placement(transformation(extent={{10,-92},{-10,-72}})));
  Modelica.Fluid.Sources.FixedBoundary souVol(redeclare package Medium = Medium,
      nPorts=1) "Volume for source heat exchnager side"
   annotation (Placement(transformation(extent={{-44,-94},{-24,-74}})));
  Fluid.Sensors.TemperatureTwoPort TSouLvg(
    redeclare final package Medium = Medium,
    allowFlowReversal=false,
    m_flow_nominal=mSou_flow_nominal,
    tau=30) "Source heat exchanger side leaving water temperature"
    annotation (Placement(transformation(extent={{10,-10},{-10,10}},
      rotation=90,
      origin={16,-54})));
  Modelica.Blocks.Sources.Constant TCooSet(k=7 + 273.15)
    "Cooling setpoint temperature"
    annotation (Placement(transformation(extent={{-90,-32},{-70,-12}})));
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
  Fluid.Sensors.TemperatureTwoPort TLoaLvg(
    redeclare final package Medium = Medium,
    allowFlowReversal=false,
    m_flow_nominal=mLoa_flow_nominal,
    tau=30)
          "Load heat exchanger leaving water temperature"
    annotation (Placement(
      transformation(
      extent={{10,10},{-10,-10}},
      rotation=270,
      origin={50,34})));
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
  Buildings.Controls.OBC.CDL.Continuous.Sources.Sine sin(
    amplitude=3,
    freqHz=1/3000,
    offset=10 + 273.15,
    startTime=0)  "Sine source block"
    annotation (Placement(transformation(extent={{60,-60},{80,-40}})));
  Modelica.Blocks.Sources.Constant THeaSet(k=45 + 273.15)
   "Heating set point temperature"
    annotation (Placement(transformation(extent={{-90,12},{-70,32}})));
equation
  connect(loaPum.T_in,TLoaEnt. y)
    annotation (Line(points={{-56,80},{-68,80}}, color={0,0,127}));
  connect(heaPumCon.heaPumMod, heaPum.uMod) annotation (Line(points={{1.4,2.22045e-16},
          {6,2.22045e-16},{6,0},{19,0}}, color={255,127,0}));
  connect(heaPumCon.TSetCon, heaPum.THeaLoaSet) annotation (Line(points={{1.4,4.2},
          {6,4.2},{6,9},{18.6,9}}, color={0,0,127}));
  connect(loaPum.ports[1], heaPum.port_a1) annotation (Line(points={{-34,84},{10,
          84},{10,6},{20,6}}, color={0,127,255}));
  connect(heaPumCon.ReqHea, heaMod.y) annotation (Line(points={{-21.4,9},{-32,9},
          {-32,50},{-39,50}}, color={255,0,255}));
  connect(heaPumCon.ReqCoo, CooMod.y) annotation (Line(points={{-21.4,-9},{-30,-9},
          {-30,-50},{-39,-50}}, color={255,0,255}));
  connect(TSouLvg.port_b,res2. port_a)
    annotation (Line(points={{16,-64},{16,-82},{10,-82}},color={0,127,255}));
  connect(souVol.ports[1],res2. port_b)
    annotation (Line(points={{-24,-84},{-18,-84},{-18,-82},{-10,-82}},
                                                   color={0,127,255}));
  connect(heaPum.port_b2,TSouLvg. port_a)
    annotation (Line(points={{20,-6},{16,-6},{16,-44}}, color={0,127,255}));
  connect(heaPumCon.TSetCoo, TCooSet.y) annotation (Line(points={{-21,-5.8},{-54,
          -5.8},{-54,-22},{-69,-22}}, color={0,0,127}));
  connect(res1.port_b,loaVol. ports[1])
    annotation (Line(points={{72,60},{78,60}},color={0,127,255}));
  connect(res1.port_a,TLoaLvg. port_b)
    annotation (Line(points={{52,60},{50,60},{50,44}}, color={0,127,255}));
  connect(heaPum.port_b1, TLoaLvg.port_a)
    annotation (Line(points={{40,6},{50,6},{50,24}}, color={0,127,255}));
  connect(sin.y,souPum. T_in) annotation (Line(points={{82,-50},{96,-50},{96,-10},
          {88,-10}},
                   color={0,0,127}));
  connect(heaPum.port_a2, souPum.ports[1])
    annotation (Line(points={{40,-6},{66,-6}}, color={0,127,255}));
  connect(heaPumCon.TSetHea, THeaSet.y) annotation (Line(points={{-21,4},{-54,4},
          {-54,22},{-69,22}}, color={0,0,127}));
  connect(TLoaEnt.y, heaPumCon.TConEnt) annotation (Line(
      points={{-68,80},{-64,80},{-64,0.8},{-21,0.8}},
      color={0,0,127},
      pattern=LinePattern.Dot));
  connect(TLoaLvg.T, heaPumCon.TConLvg) annotation (Line(
      points={{39,34},{-24,34},{-24,-1.6},{-21,-1.6}},
      color={0,0,127},
      pattern=LinePattern.Dot));
  connect(TSouLvg.T, heaPumCon.TEvaLvg) annotation (Line(
      points={{5,-54},{-36,-54},{-36,-4},{-21,-4}},
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
                points={{-30,64},{70,4},{-30,-56},{-30,64}})}),  Diagram(coordinateSystem(preserveAspectRatio=false),
        graphics={Line(points={{-22,22}}, color={28,108,200})}),
    experiment(StopTime=14400),
    __Dymola_Commands(
  file="modelica://Buildings/Resources/Scripts/Dymola/Applications/DHC/EnergyTransferStations/Control/HeatpumpController.mos"
        "Simulate and plot"),
         experiment(Tolerance=1e-6, StopTime=14400));
end HeatpumpController;
