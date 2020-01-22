within Buildings.Applications.DHC.EnergyTransferStations.Examples;
model ETSChillerConstantSpeedPumpTwoValves
  "ETS example using EIRchiller and constant speed pumps"
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
    THys=2)
    "Energy transfer station for the 5th generation of district heating and cooling"
    annotation (Placement(transformation(extent={{0,-20},{20,0}})));

  Buildings.Controls.OBC.CDL.Continuous.Sources.Constant TSetCooMin(k=5 + 273.15)
    "Minimum cooling setpoint temperature"
   annotation (Placement(transformation(extent={{-120,128},{-100,148}})));
  Modelica.Blocks.Sources.Constant TBorMaxEnt(k=40 + 273.15)
    "Cooling setpoint temperature"
    annotation (Placement(transformation(extent={{-120,16},{-100,36}})));
  Modelica.Blocks.Sources.Constant TMaxEvaEnt(k=17 + 273.15)
    "Maximum heating set point temperature"
    annotation (Placement(transformation(extent={{-120,48},{-100,68}})));
  Modelica.Blocks.Sources.Constant TMinConEnt(k=12 + 273.15)
    "Minimum heating set point temperature"
    annotation (Placement(transformation(extent={{-120,88},{-100,108}})));

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
    annotation (Placement(transformation(extent={{-80,-122},{-60,-102}})));

  Buildings.Applications.DHC.EnergyTransferStations.Data.DesignDataGeothermal
    datGeo(lBorFie={70,90,40,70,120}*0.5, wBorFie={44,50,40,40,40})
    "Borfield system performance data"
    annotation (Placement(transformation(extent={{100,120},{120,140}})));
  Buildings.Fluid.Chillers.Data.ElectricEIR.ElectricEIRChiller_McQuay_WSC_816kW_6_74COP_Vanes datChi(
  PLRMinUnl=
       1, PLRMin=1)
     annotation (Placement(transformation(extent={{100,92},{120,112}})));
  Buildings.Controls.OBC.CDL.Continuous.Sources.Constant TSetHea(k=30 + 273.15)
    "Heating setpoint temperature"
    annotation (Placement(transformation(extent={{20,80},{0,100}})));
  Buildings.Controls.OBC.CDL.Continuous.Sources.Constant TSetCoo(k=10 + 273.15)
    "Cooling setpoint temperature"
    annotation (Placement(transformation(extent={{20,40},{0,60}})));
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
    riseTime=10,
    dp_nominal=500000)
    "Cooling constant speed pump-secondary circuit"
    annotation (Placement(transformation(extent={{-80,-42},{-60,-22}})));
  Buildings.Controls.OBC.CDL.Continuous.Sources.Pulse mSecCooRet(
    amplitude=7,
    width=0.75,
    period=2*3600,
    offset=0,
    startTime=2*3600) "Secondary (building side) cooling water flow rate"
    annotation (Placement(transformation(extent={{-136,-20},{-116,0}})));
  Fluid.Sources.MassFlowSource_T heaPum1(
    use_m_flow_in=true,
    use_T_in=true,
    redeclare package Medium = Medium,
    nPorts=1) "Heating load water pump." annotation (Placement(transformation(
        extent={{-10,-10},{10,10}},
        rotation=180,
        origin={56,40})));
  Buildings.Controls.OBC.CDL.Continuous.Sources.Pulse    TSecHeaEnt(
    amplitude=2,
    width=0.3,
    period=3*3600,
    offset=20 + 273.15)
    "Secondary (building side) return heating water temperature"
    annotation (Placement(transformation(extent={{114,56},{94,76}})));
  Buildings.Controls.OBC.CDL.Continuous.Sources.Pulse mSecHeaRet(
    amplitude=15,
    width=0.3,
    period=3*3600,
    offset=0.0005)    "Secondary (building side) heating water flow rate"
    annotation (Placement(transformation(extent={{104,-4},{84,16}})));
  Fluid.Sources.Boundary_pT disLoa1(redeclare package Medium = Medium, nPorts=1)
    "Volume for the district system"
    annotation (Placement(transformation(extent={{138,-60},{118,-40}})));
  Fluid.FixedResistances.PressureDrop disPD1(
    redeclare final package Medium = Medium,
    final m_flow_nominal=mDis_flow_nominal,
    final deltaM=0.3,
    final show_T=false,
    final dp_nominal=200) "Flow resistance"
    annotation (Placement(transformation(extent={{38,-60},{58,-40}})));
  Fluid.Sources.Boundary_pT sinCoo(redeclare package Medium = Medium)
    "Volume for the district system"
    annotation (Placement(transformation(extent={{-120,-80},{-100,-60}})));
  Fluid.Sources.Boundary_pT disLoa3(redeclare package Medium = Medium, nPorts=1)
    "Volume for the district system"
    annotation (Placement(transformation(extent={{-114,-46},{-94,-26}})));
equation
 /* 
  connect(TDisEnt.y, disPum.T_in)
  annotation (Line(points={{-58,-112},{-48,-112},{-48,-114},{-42,-114}},
                                                  color={0,0,127}));
  connect(disLoa.ports[1], disPD.port_b) annotation (Line(points={{100,-110},{40,
          -110}},                     color={0,127,255}));
  connect(ETS.yRejHeaInd, yRejHeaInd) annotation (Line(points={{15.4,-12.8},{26.7,
          -12.8},{26.7,88},{150,88}}, color={255,127,0}));
  connect(TBorMaxEnt.y, ETS.TMaxBorEnt) annotation (Line(points={{-99,26},{-42,26},
          {-42,-16},{-6.8,-16}}, color={0,0,127}));
  connect(TMaxEvaEnt.y, ETS.TMaxEvaEnt) annotation (Line(points={{-99,58},{-36,58},
          {-36,-13.6},{-6.8,-13.6}}, color={0,0,127}));
  connect(TMinConEnt.y, ETS.TMinConEnt) annotation (Line(points={{-99,98},{-30,98},
          {-30,-8.2},{-6.8,-8.2}}, color={0,0,127}));
  connect(TSetCooMin.y, ETS.TSetCooMin) annotation (Line(points={{-98,138},{-26,
          138},{-26,-6.6},{-6.8,-6.6}},
                                   color={0,0,127}));
  connect(TSetCoo.y, ETS.TSetCoo) annotation (Line(points={{-2,50},{-14,50},{-14,
          -3.8},{-6.8,-3.8}}, color={0,0,127}));
  connect(TSetHea.y, ETS.TSetHea) annotation (Line(points={{-2,90},{-20,90},{-20,
          -5.2},{-6.8,-5.2}}, color={0,0,127}));
  connect(pumCoo.port_b, ETS.chiWatRet) annotation (Line(points={{-60,-32},{-40,
          -32},{-40,-19},{-7,-19}},     color={0,127,255}));
  connect(ETS.disWatRet, disPD.port_a) annotation (Line(points={{5.2,-25},{5.2,-110},
          {20,-110}}, color={0,127,255}));
  connect(disPum.ports[1], ETS.disWatSup) annotation (Line(points={{-20,-110},{2.8,
          -110},{2.8,-25}}, color={0,127,255}));
  connect(mSecCooRet.y, pumCoo.m_flow_in) annotation (Line(points={{-114,-10},{-70,
          -10},{-70,-20}}, color={0,0,127}));
  connect(TSecHeaEnt.y,heaPum1. T_in) annotation (Line(points={{92,66},{88,66},{
          88,36},{68,36}},  color={0,0,127}));
  connect(heaPum1.m_flow_in,mSecHeaRet. y) annotation (Line(points={{68,32},{74,
          32},{74,6},{82,6}},   color={0,0,127}));
  connect(ETS.hotWatRet,heaPum1. ports[1]) annotation (Line(points={{15,-18.8},{
          32,-18.8},{32,40},{46,40}},  color={0,127,255}));
  connect(disLoa1.ports[1], disPD1.port_b)
    annotation (Line(points={{118,-50},{58,-50}}, color={0,127,255}));
  connect(disPD1.port_a, ETS.hotWatSup) annotation (Line(points={{38,-50},{22,-50},
          {22,-21.2},{15,-21.2}}, color={0,127,255}));
  connect(pumCoo.port_a, disLoa3.ports[1]) annotation (Line(points={{-80,-32},{-88,
          -32},{-88,-36},{-94,-36}}, color={0,127,255}));
  connect(ETS.chiWatSup, sinCoo.ports[1]) annotation (Line(points={{-7,-21.2},{-32,
  -21.2},{-32,-70},{-100,-70}}, color={0,127,255}));
  */


  connect(disLoa3.ports[1], pumCoo.port_a) annotation (Line(points={{-94,-36},{-88,
          -36},{-88,-32},{-80,-32}}, color={0,127,255}));
  connect(pumCoo.port_b, ETS.chiWatRet) annotation (Line(points={{-60,-32},{-30,
          -32},{-30,-15},{-1,-15}}, color={0,127,255}));
  connect(mSecCooRet.y, pumCoo.m_flow_in) annotation (Line(points={{-114,-10},{-70,
          -10},{-70,-20}}, color={0,0,127}));
  connect(TBorMaxEnt.y, ETS.TMaxBorEnt) annotation (Line(points={{-99,26},{-58,26},
          {-58,-12},{-0.8,-12}}, color={0,0,127}));
  connect(TMaxEvaEnt.y, ETS.TMaxEvaEnt) annotation (Line(points={{-99,58},{-52,58},
          {-52,-9.6},{-0.8,-9.6}}, color={0,0,127}));
  connect(TMinConEnt.y, ETS.TMinConEnt) annotation (Line(points={{-99,98},{-46,98},
          {-46,-4.2},{-0.8,-4.2}}, color={0,0,127}));
  connect(TSetCooMin.y, ETS.TSetCooMin) annotation (Line(points={{-98,138},{-40,
          138},{-40,-2.6},{-0.8,-2.6}}, color={0,0,127}));
  connect(TSetCoo.y, ETS.TSetCoo) annotation (Line(points={{-2,50},{-10,50},{-10,
          0.2},{-0.8,0.2}}, color={0,0,127}));
  connect(TSetHea.y, ETS.TSetHea) annotation (Line(points={{-2,90},{-20,90},{-20,
          -1.2},{-0.8,-1.2}}, color={0,0,127}));
  connect(TDisEnt.y, disPum.T_in) annotation (Line(points={{-58,-112},{-52,-112},
          {-52,-114},{-42,-114}}, color={0,0,127}));
  connect(disPum.ports[1], ETS.disWatSup) annotation (Line(points={{-20,-110},{8.8,
          -110},{8.8,-21}}, color={0,127,255}));
  connect(ETS.disWatRet, disPD.port_a) annotation (Line(points={{11.2,-21},{11.2,
          -110},{20,-110}}, color={0,127,255}));
  connect(disPD.port_b, disLoa.ports[1])
    annotation (Line(points={{40,-110},{100,-110}}, color={0,127,255}));
  connect(disLoa1.ports[1], disPD1.port_b)
    annotation (Line(points={{118,-50},{58,-50}}, color={0,127,255}));
  connect(disPD1.port_a, ETS.hotWatSup) annotation (Line(points={{38,-50},{34,-50},
          {34,-20},{21,-20},{21,-17.2}}, color={0,127,255}));
  connect(mSecHeaRet.y, heaPum1.m_flow_in)
    annotation (Line(points={{82,6},{76,6},{76,32},{68,32}}, color={0,0,127}));
  connect(TSecHeaEnt.y, heaPum1.T_in) annotation (Line(points={{92,66},{80,66},{
          80,36},{68,36}}, color={0,0,127}));
  connect(heaPum1.ports[1], ETS.hotWatRet) annotation (Line(points={{46,40},{32,
          40},{32,-14.8},{21,-14.8}}, color={0,127,255}));
  connect(ETS.chiWatSup, sinCoo.ports[1]) annotation (Line(points={{-1,-17.2},{-20,
          -17.2},{-20,-70},{-100,-70}}, color={0,127,255}));
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
                            extent={{-140,-140},{140,160}}),
                            graphics={Line(points={{-22,22}}, color={28,108,200})}),
              __Dymola_Commands(file="modelica://Buildings/Resources/Scripts/Dymola/Applications/DHC/EnergyTransferStations/Examples/ETSChillerConstantSpeedPumpTwoValvesPumps.mos"
                  "Simulate and plot"),
                  experiment(StopTime=18000,Tolerance=1e-06,__Dymola_Algorithm="Cvode"));
end ETSChillerConstantSpeedPumpTwoValves;
