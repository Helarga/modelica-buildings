within Buildings.Applications.DHC.EnergyTransferStations.Examples;
model HeatpumpControllerHeaOnly
  "Reverse heatpump controller operates in heating mode only"
  package Medium = Buildings.Media.Water "Medium model";

  parameter Modelica.SIunits.MassFlowRate mSou_flow_nominal=1.89
   "Source heat exchanger nominal mass flow rate";
  parameter Modelica.SIunits.MassFlowRate mLoa_flow_nominal=1.89
   "Load heat exchanger nominal mass flow rate";
  parameter Real scaling_factor=1
   "Scaling factor for heatpump capacity";

  parameter Modelica.SIunits.MassFlowRate mSecHea_flow_nominal=1.0
   "Secondary(building side) heatig water nominal mass flow rate";
  parameter Modelica.SIunits.MassFlowRate mSecCoo_flow_nominal=1.0
   "Secondary(building side) cooling water mass flow rate";

    parameter Modelica.SIunits.MassFlowRate valEva_flow_nominal= mSou_flow_nominal
   "Evaporator three way valve nominal mass flow rate";
  parameter Modelica.SIunits.PressureDifference dpSou_nominal= per.dpHeaSou_nominal
      "Pressure difference accross the condenser"
      annotation (Dialog(tab="WSHP system"));
  parameter Boolean show_T=true
      "= true, if actual temperature at port is computed"
      annotation (Dialog(group="Advanced"));

  Control.HeatPumpController
    heaPumCon annotation (Placement(transformation(extent={{-34,-10},{-14,8}})));

  Fluid.HeatPumps.EquationFitReversible heaPum(
    per=per,
    scaling_factor=scaling_factor,
    redeclare package Medium1 = Medium,
    redeclare package Medium2 = Medium,
    energyDynamics=Modelica.Fluid.Types.Dynamics.FixedInitial,
    massDynamics=Modelica.Fluid.Types.Dynamics.FixedInitial,
    show_T=true,
    T1_start = 273.15+40,
    T2_start = 273.15+7)
    annotation (Placement(transformation(extent={{20,-10},{40,10}})));

  Buildings.Controls.OBC.CDL.Continuous.Sources.Sine TConEntInput(
    amplitude=1,
    freqHz=1/3000,
    offset=36 + 273.15) "Load heat exchanger entering water temperature"
    annotation (Placement(transformation(extent={{-100,70},{-80,90}})));
  Fluid.Sources.MassFlowSource_T mConEnt(
    use_m_flow_in=false,
    m_flow=1.89,
    use_T_in=true,
    redeclare package Medium = Medium,
    nPorts=1) "Load Side water pump" annotation (Placement(transformation(
        extent={{10,-10},{-10,10}},
        rotation=180,
        origin={-44,84})));
  Modelica.Blocks.Sources.BooleanConstant heaMod(k=true)  "Step control"
    annotation (Placement(transformation(extent={{-60,40},{-40,60}})));
  Modelica.Blocks.Sources.BooleanConstant CooMod(k=false)
                                                 "Step control"
    annotation (Placement(transformation(extent={{-60,10},{-40,30}})));
  Fluid.Sources.Boundary_pT sin(redeclare package Medium = Medium, nPorts=1)
    "Volume for source heat exchnager side"
    annotation (Placement(transformation(extent={{-12,-126},{8,-106}})));
  Fluid.Sensors.TemperatureTwoPort TEvaLvg(
    redeclare final package Medium = Medium,
    allowFlowReversal=false,
    m_flow_nominal=mSou_flow_nominal,
    tau=30) "Evaporator leaving water temperature."
    annotation (Placement(transformation(extent={{10,-10},{-10,10}},
      rotation=0,
      origin={46,-114})));
  Modelica.Blocks.Sources.Constant TCooSet(k=7 + 273.15)
    "Cooling setpoint temperature"
    annotation (Placement(transformation(extent={{-100,-28},{-80,-8}})));
  Modelica.Fluid.Sources.FixedBoundary sin2(redeclare package Medium = Medium,
      nPorts=1) "Volume for the load side"
    annotation (Placement(transformation(extent={{138,64},{118,84}})));
  Modelica.Blocks.Sources.Constant THeaSet(k=40 + 273.15)
    "Heating set point temperature"
    annotation (Placement(transformation(extent={{-100,2},{-80,22}})));
  Modelica.Blocks.Sources.Constant THeaSetMax(k=55 + 273.15)
    "Maximum heating set point temperature"
    annotation (Placement(transformation(extent={{-100,-60},{-80,-40}})));
  Modelica.Blocks.Sources.Constant TMaxEvaEnt(k=15 + 273.15)
    "Minimum heating set point temperature"
    annotation (Placement(transformation(extent={{-100,-90},{-80,-70}})));
  Fluid.Sensors.TemperatureTwoPort TConLvg(
    redeclare final package Medium = Medium,
    allowFlowReversal=false,
    m_flow_nominal=mSou_flow_nominal,
    tau=30) "Condenser leaving water temperature." annotation (Placement(
        transformation(
        extent={{-10,-10},{10,10}},
        rotation=0,
        origin={82,74})));
  Fluid.Sensors.TemperatureTwoPort TConEnt(
    redeclare final package Medium = Medium,
    allowFlowReversal=false,
    m_flow_nominal=mSou_flow_nominal,
    tau=30) "Condenser entering water temperature." annotation (Placement(
        transformation(
        extent={{10,-10},{-10,10}},
        rotation=90,
        origin={2,54})));
  Fluid.Sensors.TemperatureTwoPort TEvaEnt(
    redeclare final package Medium = Medium,
    allowFlowReversal=false,
    m_flow_nominal=mSou_flow_nominal,
    tau=30) "Evaporator entering water temperature." annotation (Placement(
        transformation(
        extent={{10,10},{-10,-10}},
        rotation=0,
        origin={54,-6})));
  Modelica.Blocks.Sources.Constant TMinConEnt(k=25 + 273.15)
    "Minimum heating set point temperature"
    annotation (Placement(transformation(extent={{-100,-130},{-80,-110}})));
    Fluid.Actuators.Valves.ThreeWayEqualPercentageLinear valEva(
    redeclare package Medium = Medium,
    energyDynamics=Modelica.Fluid.Types.Dynamics.FixedInitial,
    m_flow_nominal=mSou_flow_nominal,
    l={0.01,0.01},
    dpValve_nominal=6000)
    "Three way valve modulated to control the entering water temperature to the evaporator."
     annotation (Placement(transformation(extent={{-10,10},{10,-10}},
                                          rotation=180,
                                          origin={114,-2})));
    Fluid.FixedResistances.Junction splVal2(
    final dp_nominal=200*{2,2,-1},
    from_dp=false,
    tau=1,
    m_flow_nominal={mSou_flow_nominal,-mSou_flow_nominal,-mSou_flow_nominal},
    redeclare package Medium = Medium,
    energyDynamics=Modelica.Fluid.Types.Dynamics.SteadyState)
      "Flow splitter" annotation (Placement(transformation(
          extent={{-10,10},{10,-10}},
          rotation=270,
          origin={64,-50})));
   Fluid.HeatPumps.Data.EquationFitReversible.Trane_Axiom_EXW240 per
    "Heat pump performance data"
    annotation (Placement(transformation(extent={{24,66},{44,86}})));
  Fluid.Movers.SpeedControlled_y pumEva1(
    redeclare package Medium = Medium,
    energyDynamics=Modelica.Fluid.Types.Dynamics.SteadyState,
    T_start=308.15,
    addPowerToMedium=false,
    show_T=show_T,
    per(pressure(dp={2*dpSou_nominal,0}, V_flow={0,1*mSou_flow_nominal/1000})),
    allowFlowReversal=false,
    use_inputFilter=false,
    riseTime=10) "Evaporator variable speed pump-primary circuit" annotation (
      Placement(transformation(
        extent={{10,-10},{-10,10}},
        rotation=0,
        origin={82,-2})));

  Fluid.Sources.Boundary_pT            sou(
    redeclare package Medium = Medium,
    T=291.15,
    nPorts=1)   "Source volume"
    annotation (Placement(transformation(extent={{198,8},{178,-12}})));
  Buildings.Controls.Continuous.LimPID valCon2(
    controllerType=Modelica.Blocks.Types.SimpleController.PI,
    yMax=1,
    yMin=0.1,
    reset=Buildings.Types.Reset.Disabled,
    y_reset=0,
    k=1,
    Ti(displayUnit="s") = 300,
    reverseAction=false)
                        "Condenser three way valve PI control signal "
    annotation (Placement(transformation(extent={{158,36},{138,56}})));
  Buildings.Controls.OBC.CDL.Continuous.Sources.Constant mEvaFlo(k=1.89)
    "Evaporator mass flow rate."
    annotation (Placement(transformation(extent={{196,36},{176,56}})));
  Fluid.Sensors.MassFlowRate priHeaFlo(redeclare package Medium = Media.Water)
    "Primary circuit condenser side heating water flow rate" annotation (
      Placement(transformation(
        extent={{-10,10},{10,-10}},
        rotation=180,
        origin={148,-2})));
equation
  connect(mConEnt.T_in, TConEntInput.y)
    annotation (Line(points={{-56,80},{-78,80}}, color={0,0,127}));
  connect(heaPumCon.yHeaPumMod, heaPum.uMod) annotation (Line(points={{-12.6,0.08},
          {10,0.08},{10,0},{19,0}},
                                color={255,127,0}));
  connect(THeaSetMax.y, heaPumCon.TSetHeaMax) annotation (Line(points={{-79,-50},
          {-58,-50},{-58,-2},{-48,-2},{-48,-2.08},{-35,-2.08}},
                                          color={0,0,127}));
  connect(heaPumCon.TSetCoo, TCooSet.y) annotation (Line(points={{-35,-0.1},{-60,
          -0.1},{-60,-18},{-79,-18}},
                                    color={0,0,127}));
  connect(THeaSet.y, heaPumCon.TSetHea) annotation (Line(points={{-79,12},{-64,12},
          {-64,1.7},{-35,1.7}},    color={0,0,127}));
  connect(TEvaLvg.T,heaPumCon.TEvaLvg)  annotation (Line(
      points={{46,-103},{46,-94},{-42,-94},{-42,-8.56},{-35,-8.56}},
      color={0,0,127},
      pattern=LinePattern.Dot));
  connect(heaMod.y, heaPumCon.reqHea) annotation (Line(points={{-39,50},{-36,50},
          {-36,7.1},{-35.4,7.1}}, color={255,0,255}));
  connect(heaPumCon.reqCoo, CooMod.y) annotation (Line(points={{-35.4,4.4},{-38,
          4.4},{-38,20},{-39,20}}, color={255,0,255}));
  connect(mConEnt.ports[1], TConEnt.port_a)
    annotation (Line(points={{-34,84},{2,84},{2,64}},   color={0,127,255}));
  connect(heaPum.port_a1, TConEnt.port_b)
    annotation (Line(points={{20,6},{2,6},{2,44}},   color={0,127,255}));
  connect(heaPumCon.TConEnt, TConEnt.T) annotation (Line(
      points={{-35,-9.82},{-42,-9.82},{-42,-44},{-8,-44},{-8,6},{-9,6},{-9,54}},
      color={0,0,127},
      pattern=LinePattern.Dot));

  connect(heaPum.port_a2, TEvaEnt.port_b)
    annotation (Line(points={{40,-6},{44,-6}}, color={0,127,255}));
  connect(TEvaEnt.T, heaPumCon.TEvaEnt) annotation (Line(
      points={{54,-17},{54,-26},{-46,-26},{-46,-6.94},{-35,-6.94}},
      color={0,0,127},
      pattern=LinePattern.Dot));
  connect(TMaxEvaEnt.y, heaPumCon.TMaxEvaEnt) annotation (Line(points={{-79,-80},
          {-54,-80},{-54,-6},{-44,-6},{-44,-5.14},{-35,-5.14}},
                                              color={0,0,127}));
  connect(TMinConEnt.y, heaPumCon.TMinConEnt) annotation (Line(points={{-79,-120},
          {-48,-120},{-48,-3.52},{-35,-3.52}}, color={0,0,127}));
  connect(sin2.ports[1], TConLvg.port_b)
    annotation (Line(points={{118,74},{92,74}}, color={0,127,255}));
  connect(heaPum.port_b1, TConLvg.port_a) annotation (Line(points={{40,6},{56,6},
          {56,74},{72,74}}, color={0,127,255}));
  connect(sin.ports[1], TEvaLvg.port_b) annotation (Line(points={{8,-116},{10,-116},
          {10,-114},{36,-114}}, color={0,127,255}));
  connect(heaPumCon.TSetHeaPum, heaPum.TSet) annotation (Line(points={{-12.6,3.14},
          {6,3.14},{6,9},{18.6,9}}, color={0,0,127}));
  connect(valEva.port_2, pumEva1.port_a)
    annotation (Line(points={{104,-2},{92,-2}}, color={0,127,255}));
  connect(TEvaEnt.port_a, pumEva1.port_b) annotation (Line(points={{64,-6},{68,-6},
          {68,-2},{72,-2}}, color={0,127,255}));
  connect(valCon2.u_s,mEvaFlo. y)
    annotation (Line(points={{160,46},{174,46}}, color={0,0,127}));
  connect(valCon2.y, pumEva1.y)
    annotation (Line(points={{137,46},{82,46},{82,10}}, color={0,0,127}));
  connect(valEva.port_1, priHeaFlo.port_b)
    annotation (Line(points={{124,-2},{138,-2}}, color={0,127,255}));
  connect(sou.ports[1], priHeaFlo.port_a)
    annotation (Line(points={{178,-2},{158,-2}}, color={0,127,255}));
  connect(priHeaFlo.m_flow, valCon2.u_m)
    annotation (Line(points={{148,9},{148,34}}, color={0,0,127}));
  connect(TEvaLvg.port_a, splVal2.port_2) annotation (Line(points={{56,-114},{64,
          -114},{64,-60}}, color={0,127,255}));
  connect(heaPum.port_b2, splVal2.port_1) annotation (Line(points={{20,-6},{14,-6},
          {14,-30},{64,-30},{64,-40}}, color={0,127,255}));
  connect(splVal2.port_3, valEva.port_3) annotation (Line(points={{74,-50},{114,
          -50},{114,-12}}, color={0,127,255}));
  connect(heaPumCon.yValEva, valEva.y) annotation (Line(points={{-12.6,-3.16},{10,
          -3.16},{10,24},{114,24},{114,10}}, color={0,0,127}));
  annotation (Icon(coordinateSystem(preserveAspectRatio=false, extent={{-100,-100},
            {100,100}}),                                        graphics={
        Ellipse(lineColor = {75,138,73},
                fillColor={255,255,255},
                fillPattern = FillPattern.Solid,
                extent={{-98,-100},{98,98}}),
        Polygon(lineColor = {0,0,255},
                fillColor = {75,138,73},
                pattern = LinePattern.None,
                fillPattern = FillPattern.Solid,
                points={{-30,64},{70,4},{-30,-56},{-30,64}})}),  Diagram(coordinateSystem(preserveAspectRatio=false, extent={{-140,
            -140},{200,100}}),
        graphics={Line(points={{-22,22}}, color={28,108,200})}),
    experiment(StopTime=5000, Tolerance=1e-06),
    __Dymola_Commands(
  file="modelica://Buildings/Resources/Scripts/Dymola/Applications/DHC/EnergyTransferStations/Control/HeatpumpControllerHeaOnly.mos"
        "Simulate and plot"),
         experiment(Tolerance=1e-6, StopTime=14400));
end HeatpumpControllerHeaOnly;
