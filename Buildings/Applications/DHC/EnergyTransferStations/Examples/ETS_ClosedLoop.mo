within Buildings.Applications.DHC.EnergyTransferStations.Examples;
model ETS_ClosedLoop "ETS example using EIRchiller and constant speed pumps"
  package Medium = Buildings.Media.Water "Medium model";

  parameter Modelica.SIunits.MassFlowRate mEva_flow_nominal=datChi.mEva_flow_nominal
    "Source heat exchanger nominal mass flow rate"
    annotation (Dialog(group="WSHP system"));
  parameter Modelica.SIunits.MassFlowRate mCon_flow_nominal=datChi.mCon_flow_nominal
    "Load heat exchanger nominal mass flow rate"
    annotation (Dialog(group="WSHP system"));
  parameter Modelica.SIunits.PressureDifference dpCon_nominal=33530
    "Pressure difference accross the condenser"
      annotation (Dialog(group="WSHP system"));
  parameter Modelica.SIunits.PressureDifference dpEva_nominal=32460
    "Pressure difference accross the evaporator"
      annotation (Dialog(group="WSHP system"));
  parameter Modelica.SIunits.MassFlowRate mSecHea_flow_nominal=15
   "Secondary(building side) heatig water nominal mass flow rate";
  parameter Modelica.SIunits.MassFlowRate mSecCoo_flow_nominal=8
   "Secondary(building side) cooling water mass flow rate";
  parameter Modelica.SIunits.MassFlowRate mDis_flow_nominal = 3
   "District circuit water mass flow rate";

   SubstationWithConstPrimPum_OnOffChiller_twoVal ETS(
    datChi=datChi,
    mCon_flow_nominal=mCon_flow_nominal,
    mEva_flow_nominal=mEva_flow_nominal,
    dpCon_nominal=dpCon_nominal,
    dpEva_nominal=dpEva_nominal,
    mSecHea_flow_nominal=mSecHea_flow_nominal,
    mSecCoo_flow_nominal=mSecCoo_flow_nominal,
    dTChi=2,
    dTGeo=2,
    dTHex=5,
    xBorFie=datGeo.lBorFie[1],
    yBorFie=datGeo.wBorFie[1],
    dpBorFie_nominal=datGeo.dpBor_nominal,
    THys=1.5)
    "Energy transfer station for the 5th generation of district heating and cooling"
    annotation (Placement(transformation(extent={{-10,-10},{10,10}})));

  Buildings.Controls.OBC.CDL.Continuous.Sources.Constant TSetCooMin(k=5 + 273.15)
    "Minimum cooling setpoint temperature"
   annotation (Placement(transformation(extent={{20,122},{0,142}})));
  Modelica.Blocks.Sources.Constant TBorMaxEnt(k=40 + 273.15)
    "Cooling setpoint temperature"
    annotation (Placement(transformation(extent={{-70,58},{-50,78}})));
  Modelica.Blocks.Sources.Constant TMaxEvaEnt(k=17 + 273.15)
    "Maximum heating set point temperature"
    annotation (Placement(transformation(extent={{-72,90},{-52,110}})));
  Modelica.Blocks.Sources.Constant TMinConEnt(k=12 + 273.15)
    "Minimum heating set point temperature"
    annotation (Placement(transformation(extent={{-70,122},{-50,142}})));

  Fluid.Sources.Boundary_pT disLoa(
      redeclare package Medium = Medium,
      nPorts=1) "Volume for the district system"
    annotation (Placement(transformation(extent={{120,-120},{100,-100}})));
  Fluid.Sources.MassFlowSource_T disPum(
    m_flow=mDis_flow_nominal,
    use_T_in=true,
    redeclare package Medium = Medium,
    nPorts=1) "District system water pump" annotation (Placement(transformation(
        extent={{10,-10},{-10,10}},
        rotation=180,
        origin={-30,-110})));
  Buildings.Controls.OBC.CDL.Continuous.Sources.Constant TDisEnt(k=16 + 273.15)
    "District entering water temperature"
    annotation (Placement(transformation(extent={{-120,-120},{-100,-100}})));

  Buildings.Applications.DHC.EnergyTransferStations.Data.DesignDataGeothermal
    datGeo(lBorFie={70,90,40,70,120}*0.5, wBorFie={44,50,40,40,40})
    "Borfield system performance data"
    annotation (Placement(transformation(extent={{100,134},{120,154}})));
  Buildings.Fluid.Chillers.Data.ElectricEIR.ElectricEIRChiller_McQuay_WSC_816kW_6_74COP_Vanes datChi(
  PLRMinUnl=
       1, PLRMin=1)
     annotation (Placement(transformation(extent={{100,106},{120,126}})));
  Buildings.Controls.OBC.CDL.Continuous.Sources.Constant TSetHea(k=27 + 273.15)
    "Heating setpoint temperature"
    annotation (Placement(transformation(extent={{20,90},{0,110}})));
  Buildings.Controls.OBC.CDL.Continuous.Sources.Constant TSetCoo(k=10 + 273.15)
    "Cooling setpoint temperature"
    annotation (Placement(transformation(extent={{20,58},{0,78}})));
  Fluid.FixedResistances.PressureDrop disPD(
    redeclare final package Medium = Medium,
    final m_flow_nominal=mDis_flow_nominal,
    final deltaM=0.3,
    final show_T=false,
    final dp_nominal=200) "Flow resistance"
    annotation (Placement(transformation(extent={{20,-120},{40,-100}})));
  Fluid.Movers.FlowControlled_m_flow
                                 pumCoo(
    redeclare package Medium = Medium,
    energyDynamics=Modelica.Fluid.Types.Dynamics.SteadyState,
    m_flow_nominal=7,
    addPowerToMedium=false,
    show_T=true,
    allowFlowReversal=false,
    nominalValuesDefineDefaultPressureCurve=true,
    use_inputFilter=false,
    riseTime=60,
    dp_nominal=500000)
    "Cooling constant speed pump-secondary circuit"
    annotation (Placement(transformation(extent={{-78,20},{-58,40}})));
  Buildings.Controls.OBC.CDL.Continuous.Sources.Pulse mSecCooRet(
    amplitude=7,
    width=0.75,
    period=2*3600,
    offset=0,
    startTime=2*3600) "Secondary (building side) cooling water flow rate"
    annotation (Placement(transformation(extent={{-160,40},{-140,60}})));
  Fluid.MixingVolumes.MixingVolume cooVol(
    redeclare package Medium = Medium,
    energyDynamics=Modelica.Fluid.Types.Dynamics.SteadyState,
    T_start=285.15,
    m_flow_nominal=7,
    V=0.1*3600*7/1000,
    nPorts=2) "Mixing volume mimics a room to be cooled" annotation (Placement(
        transformation(
        extent={{-10,10},{10,-10}},
        rotation=0,
        origin={-30,-70})));
  Modelica.Blocks.Math.Gain Q_flow_Coo(k=1*4200) "Heat added to the volume."
    annotation (Placement(transformation(extent={{-122,-80},{-102,-60}})));
  HeatTransfer.Sources.PrescribedHeatFlow heaFloPos
    "Prescribed added heat flow rate(positive)"
    annotation (Placement(transformation(extent={{-80,-80},{-60,-60}})));
  Fluid.Movers.FlowControlled_m_flow
                                 pumHea(
    redeclare package Medium = Medium,
    energyDynamics=Modelica.Fluid.Types.Dynamics.SteadyState,
    m_flow_nominal=15,
    addPowerToMedium=false,
    show_T=true,
    allowFlowReversal=false,
    nominalValuesDefineDefaultPressureCurve=true,
    use_inputFilter=false,
    riseTime=10,
    init=Modelica.Blocks.Types.Init.SteadyState,
    dp_nominal=500000)
                 "Heating constant speed pump-secondary circuit"
    annotation (Placement(transformation(extent={{80,18},{60,38}})));
  Buildings.Controls.OBC.CDL.Continuous.Sources.Pulse mSecHeaRet(
    amplitude=15,
    width=0.3,
    period=3*3600,
    offset=0)
    "Secondary (building side) heating water flow rate"
    annotation (Placement(transformation(extent={{140,38},{120,58}})));
  Fluid.MixingVolumes.MixingVolume heaVol(
    redeclare package Medium = Medium,
    energyDynamics=Modelica.Fluid.Types.Dynamics.SteadyState,
    T_start=298.15,
    m_flow_nominal=15,
    V=0.1*3600*15/1000,
    nPorts=2) "Mixing volume mimics zones to be heated" annotation (Placement(
        transformation(
        extent={{10,10},{-10,-10}},
        rotation=0,
        origin={30,-70})));
  Modelica.Blocks.Math.Gain Q_flow_Hea(k=-1*4200)
    "Heat extracted from the volume" annotation (Placement(transformation(
        extent={{10,-10},{-10,10}},
        rotation=0,
        origin={90,-70})));
  HeatTransfer.Sources.PrescribedHeatFlow heaFloNeg
    "Prescribed extracted heat flow rate (negative)"
    annotation (Placement(transformation(extent={{68,-80},{48,-60}})));
  Modelica.Fluid.Sources.FixedBoundary pre1(redeclare package Medium = Medium,
      nPorts=1) "Pressure source" annotation (Placement(transformation(
        extent={{-10,-10},{10,10}},
        rotation=180,
        origin={50,-10})));
    Fluid.Sensors.TemperatureTwoPort retChiWat(
    redeclare final package Medium = Medium,
    allowFlowReversal=false,
    m_flow_nominal=mEva_flow_nominal,
    tau=30) "Evaporator leaving water temperature"
    annotation (Placement(transformation(extent={{-80,-50},{-100,-30}})));
    Fluid.Sensors.TemperatureTwoPort supChiWat(
    redeclare final package Medium = Medium,
    allowFlowReversal=false,
    m_flow_nominal=mEva_flow_nominal,
    tau=30) "Evaporator leaving water temperature" annotation (Placement(
        transformation(
        extent={{10,-10},{-10,10}},
        rotation=90,
        origin={-18,-30})));
    Fluid.Sensors.TemperatureTwoPort retHeaWat(
    redeclare final package Medium = Medium,
    allowFlowReversal=false,
    m_flow_nominal=mEva_flow_nominal,
    tau=30) annotation (Placement(transformation(extent={{80,-42},{100,-22}})));
    Fluid.Sensors.TemperatureTwoPort supHeaWat(
    redeclare final package Medium = Medium,
    allowFlowReversal=false,
    m_flow_nominal=mEva_flow_nominal,
    tau=30) annotation (Placement(transformation(
        extent={{10,-10},{-10,10}},
        rotation=90,
        origin={14,-30})));
  Modelica.Fluid.Sources.FixedBoundary pre(redeclare package Medium = Medium)
    "Pressure source" annotation (Placement(transformation(
        extent={{10,-10},{-10,10}},
        rotation=180,
        origin={-70,-10})));
equation
  connect(TDisEnt.y, disPum.T_in)
  annotation (Line(points={{-98,-110},{-70,-110},{-70,-114},{-42,-114}},
                                                  color={0,0,127}));
  connect(disLoa.ports[1], disPD.port_b) annotation (Line(points={{100,-110},{
          40,-110}},                  color={0,127,255}));
  connect(TBorMaxEnt.y, ETS.TMaxBorEnt) annotation (Line(points={{-49,68},{-42,
          68},{-42,-2},{-10.8,-2}},
                                 color={0,0,127}));
  connect(TMaxEvaEnt.y, ETS.TMaxEvaEnt) annotation (Line(points={{-51,100},{-36,
          100},{-36,0.4},{-10.8,0.4}},
                                     color={0,0,127}));
  connect(TMinConEnt.y, ETS.TMinConEnt) annotation (Line(points={{-49,132},{-30,
          132},{-30,5.8},{-10.8,5.8}},
                                   color={0,0,127}));
  connect(TSetCooMin.y, ETS.TSetCooMin) annotation (Line(points={{-2,132},{-26,
          132},{-26,7.4},{-10.8,7.4}},
                                   color={0,0,127}));
  connect(TSetCoo.y, ETS.TSetCoo) annotation (Line(points={{-2,68},{-14,68},{
          -14,10.2},{-10.8,10.2}},
                              color={0,0,127}));
  connect(TSetHea.y, ETS.TSetHea) annotation (Line(points={{-2,100},{-20,100},{
          -20,8.8},{-10.8,8.8}},
                              color={0,0,127}));
  connect(ETS.disWatRet, disPD.port_a) annotation (Line(points={{1.2,-11},{1.2,
          -110},{20,-110}},
                      color={0,127,255}));
  connect(disPum.ports[1], ETS.disWatSup) annotation (Line(points={{-20,-110},{
          -1.2,-110},{-1.2,-11}},
                            color={0,127,255}));
  connect(heaFloPos.port, cooVol.heatPort)
    annotation (Line(points={{-60,-70},{-40,-70}}, color={191,0,0}));
  connect(Q_flow_Coo.y, heaFloPos.Q_flow)
    annotation (Line(points={{-101,-70},{-80,-70}}, color={0,0,127}));
  connect(mSecCooRet.y, Q_flow_Coo.u) annotation (Line(points={{-138,50},{-128,
          50},{-128,-70},{-124,-70}},  color={0,0,127}));
  connect(heaVol.heatPort, heaFloNeg.port)
    annotation (Line(points={{40,-70},{48,-70}}, color={191,0,0}));
  connect(heaFloNeg.Q_flow, Q_flow_Hea.y)
    annotation (Line(points={{68,-70},{79,-70}}, color={0,0,127}));
  connect(mSecHeaRet.y, Q_flow_Hea.u) annotation (Line(points={{118,48},{110,48},
          {110,-70},{102,-70}}, color={0,0,127}));
  connect(mSecHeaRet.y, pumHea.m_flow_in)
    annotation (Line(points={{118,48},{70,48},{70,40}},    color={0,0,127}));
  connect(retChiWat.port_b, pumCoo.port_a) annotation (Line(points={{-100,-40},
          {-100,30},{-78,30}}, color={0,127,255}));
  connect(retChiWat.port_a, cooVol.ports[1]) annotation (Line(points={{-80,-40},
          {-34,-40},{-34,-60},{-32,-60}}, color={0,127,255}));
  connect(supChiWat.port_b, cooVol.ports[2]) annotation (Line(points={{-18,-40},
          {-18,-60},{-28,-60}}, color={0,127,255}));
  connect(ETS.hotWatSup, supHeaWat.port_a) annotation (Line(points={{11,-7.2},{
          14,-7.2},{14,-20}}, color={0,127,255}));
  connect(heaVol.ports[1], supHeaWat.port_b)
    annotation (Line(points={{32,-60},{14,-60},{14,-40}}, color={0,127,255}));
  connect(pumCoo.m_flow_in, mSecCooRet.y)
    annotation (Line(points={{-68,42},{-68,50},{-138,50}}, color={0,0,127}));
  connect(ETS.chiWatSup, supChiWat.port_a) annotation (Line(points={{-11,-7.2},
          {-18,-7.2},{-18,-20}}, color={0,127,255}));
  connect(heaVol.ports[2], retHeaWat.port_a) annotation (Line(points={{28,-60},
          {34,-60},{34,-32},{80,-32}}, color={0,127,255}));
  connect(retHeaWat.port_b, pumHea.port_a)
    annotation (Line(points={{100,-32},{100,28},{80,28}}, color={0,127,255}));
  connect(pumHea.port_b, ETS.hotWatRet) annotation (Line(points={{60,28},{14,28},
          {14,-4.8},{11,-4.8}}, color={0,127,255}));
  connect(pumCoo.port_b, ETS.chiWatRet) annotation (Line(points={{-58,30},{-46,
          30},{-46,-5},{-11,-5}}, color={0,127,255}));
  connect(ETS.chiWatSup, pre.ports[1]) annotation (Line(points={{-11,-7.2},{-36,
          -7.2},{-36,-10},{-60,-10}}, color={0,127,255}));
  connect(ETS.hotWatSup, pre1.ports[1]) annotation (Line(points={{11,-7.2},{24,
          -7.2},{24,-10},{40,-10}}, color={0,127,255}));
   annotation (Dialog(tab="Borefield"),
              Icon(coordinateSystem(preserveAspectRatio=false,
              extent={{-100,-100},{100,100}}),
              graphics={
                  Ellipse(lineColor = {75,138,73},
                          fillColor={255,255,255},
                          fillPattern = FillPattern.Solid,
                          extent={{-98,-100},{98,98}}),
                  Polygon(lineColor = {0,0,255},
                          fillColor = {75,138,73},
                          pattern = LinePattern.None,
                          fillPattern = FillPattern.Solid,
                          points={{-30,64},{70,4},{-30,-56},{-30,64}})}),
                  Diagram(coordinateSystem(
                            preserveAspectRatio=false,
                            extent={{-180,-140},{160,160}}),
                            graphics={Line(points={{-22,22}}, color={28,108,200})}),
              __Dymola_Commands(file="modelica://Buildings/Resources/Scripts/Dymola/Applications/DHC/EnergyTransferStations/Examples/ETS_ClosedLoop.mos"
                  "Simulate and plot"),
                  experiment(StopTime=18000,Tolerance=1e-06,__Dymola_Algorithm="Cvode"));
end ETS_ClosedLoop;
