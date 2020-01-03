within Buildings.Applications.DHC.EnergyTransferStations.Examples;
model StartifiedTank
  "Example of four ways stratified tank implementation."
  package Medium = Buildings.Media.Water "Medium model";

    parameter Modelica.SIunits.MassFlowRate mEva_flow_nominal= 10
     "Source heat exchanger nominal mass flow rate";
    parameter Modelica.SIunits.MassFlowRate mSecCoo_flow_nominal=5;
    parameter Integer nSeg=10
     "Number of volume segments";
    parameter Boolean show_T=true
    "= true, if actual temperature at port is computed"
    annotation(Dialog(tab="Advanced",group="Diagnostics"));
    BaseClasses.StratifiedTank colBufTan(
    redeclare package Medium = Medium,
    VTan=5000,
    hTan=10,
    dIns=0.3,
    nSeg=nSeg,
    show_T=true,
    m_flow_nominal=mEva_flow_nominal)
      "Cold Buffer tank"
      annotation (Placement(transformation(extent={{-12,-10},{12,14}})));
    Modelica.Thermal.HeatTransfer.Sensors.TemperatureSensor topCooTan
      "Cold tank top temperature"
      annotation (Placement(transformation(extent={{-20,40},{-40,60}})));
    Modelica.Thermal.HeatTransfer.Sensors.TemperatureSensor botCooTan
      "Cold tank bottom temperature"
      annotation (Placement(transformation(extent={{-20,-82},{-40,-62}})));
  /* 
   Medium.ThermodynamicState sta_a=
      Medium.setState_phX(port_a.p,
                          noEvent(actualStream(port_a.h_outflow)),
                          noEvent(actualStream(port_a.Xi_outflow))) if 
         show_T "Medium properties in port_a";

  Medium.ThermodynamicState sta_b=
      Medium.setState_phX(port_b.p,
                          noEvent(actualStream(port_b.h_outflow)),
                          noEvent(actualStream(port_b.Xi_outflow))) if 
          show_T "Medium properties in port_b";

  Medium.ThermodynamicState sta_a1=
      Medium.setState_phX(port_a1.p,
                          noEvent(actualStream(port_a1.h_outflow)),
                          noEvent(actualStream(port_a1.Xi_outflow))) if 
         show_T "Medium properties in port_a";

  Medium.ThermodynamicState sta_b1=
      Medium.setState_phX(port_b1.p,
                          noEvent(actualStream(port_b1.h_outflow)),
                          noEvent(actualStream(port_b1.Xi_outflow))) if 
                       show_T "Medium properties in port_b";
*/
  Fluid.Sources.MassFlowSource_T BuiChiWRet(
    use_m_flow_in=true,
    m_flow=mSecCoo_flow_nominal,
    use_T_in=true,
    redeclare package Medium = Medium,
    nPorts=1) "Cooling load water pump." annotation (Placement(transformation(
        extent={{10,-10},{-10,10}},
        rotation=180,
        origin={-48,10})));
  Modelica.Fluid.Sources.FixedBoundary cooLoa(redeclare package Medium = Medium,
      nPorts=1) "Volume for the cooling load"
    annotation (Placement(transformation(extent={{-98,-40},{-78,-20}})));
  Fluid.FixedResistances.PressureDrop cooPD(
    redeclare final package Medium = Medium,
    final m_flow_nominal=20,
    final deltaM=0.3,
    final show_T=false,
    final dp_nominal=200) "Flow resistance of fluid 2"
    annotation (Placement(transformation(extent={{-42,-40},{-62,-20}})));
  Fluid.Sources.MassFlowSource_T chiWatSup(
    use_m_flow_in=true,
    m_flow=mSecCoo_flow_nominal,
    use_T_in=true,
    redeclare package Medium = Medium,
    nPorts=1) "Cooling load water pump." annotation (Placement(transformation(
        extent={{10,-10},{-10,10}},
        rotation=0,
        origin={50,-34})));
  Modelica.Fluid.Sources.FixedBoundary toChi(redeclare package Medium = Medium,
      nPorts=1) "Volume for the cooling load"
    annotation (Placement(transformation(extent={{90,0},{70,20}})));
  Fluid.FixedResistances.PressureDrop cooPD1(
    redeclare final package Medium = Medium,
    final m_flow_nominal=20,
    final deltaM=0.3,
    final show_T=false,
    final dp_nominal=200) "Flow resistance of fluid 2"
    annotation (Placement(transformation(extent={{44,0},{64,20}})));
  Buildings.Controls.OBC.CDL.Continuous.Sources.Constant TSecCooRet(k=20 + 273.15)
    "Secondary (building side) return cooling water temperature"
    annotation (Placement(transformation(extent={{-96,42},{-76,62}})));
  Buildings.Controls.OBC.CDL.Continuous.Sources.Constant mSecCoo(k=10)
    "Secondary (building side) return heating water temperature"
    annotation (Placement(transformation(extent={{-98,-4},{-78,16}})));
  Buildings.Controls.OBC.CDL.Continuous.Sources.Constant TChiSup(k=5 + 273.15)
    "Chiller supply water temperature"
    annotation (Placement(transformation(extent={{94,-78},{74,-58}})));
  Buildings.Controls.OBC.CDL.Continuous.Sources.Constant mChiSup(k=10)
                           "mEva_flow_nominal"
    annotation (Placement(transformation(extent={{96,-36},{76,-16}})));
   Fluid.Sensors.TemperatureTwoPort TTanOutTop(
    redeclare final package Medium = Medium,
    allowFlowReversal=false,
    tau=10,
    m_flow_nominal=10)
    "Entering water tmperature to the district heat exchanger" annotation (
      Placement(transformation(
        extent={{10,10},{-10,-10}},
        rotation=180,
        origin={30,38})));
   Fluid.Sensors.TemperatureTwoPort TTanOutBot(
    redeclare final package Medium = Medium,
    allowFlowReversal=false,
    tau=10,
    m_flow_nominal=10)
    "Entering water tmperature to the district heat exchanger" annotation (
      Placement(transformation(
        extent={{-10,10},{10,-10}},
        rotation=180,
        origin={-26,-30})));
equation
  connect(colBufTan.heaPorVol[1],topCooTan. port)
    annotation (Line(points={{0,2},{0,50},{-20,50}},        color={191,0,0},
      pattern=LinePattern.Dash));
  connect(cooLoa.ports[1],cooPD. port_b) annotation (Line(points={{-78,-30},{-62,
          -30}},                         color={0,127,255}));
  connect(colBufTan.port_a, BuiChiWRet.ports[1]) annotation (Line(points={{-12,2},
          {-36,2},{-36,10},{-38,10}}, color={0,127,255}));
  connect(colBufTan.port_a1, chiWatSup.ports[1]) annotation (Line(points={{12,-7.6},
          {20,-7.6},{20,-34},{40,-34}}, color={0,127,255}));
  connect(toChi.ports[1], cooPD1.port_b)
    annotation (Line(points={{70,10},{64,10}}, color={0,127,255}));
  connect(BuiChiWRet.T_in, TSecCooRet.y) annotation (Line(points={{-60,6},{-68,6},
          {-68,52},{-74,52}},     color={0,0,127}));
  connect(BuiChiWRet.m_flow_in, mSecCoo.y) annotation (Line(points={{-60,2},{-70,
          2},{-70,6},{-76,6}},  color={0,0,127}));
  connect(mChiSup.y, chiWatSup.m_flow_in)
    annotation (Line(points={{74,-26},{62,-26}}, color={0,0,127}));
  connect(TChiSup.y, chiWatSup.T_in) annotation (Line(points={{72,-68},{68,-68},
          {68,-30},{62,-30}}, color={0,0,127}));
  connect(colBufTan.heaPorVol[nSeg], botCooTan.port)
    annotation (Line(points={{0,2},{0,-72},{-20,-72}},
                                     color={191,0,0},
      pattern=LinePattern.Dash));
  connect(cooPD1.port_a, TTanOutTop.port_b) annotation (Line(points={{44,10},{42,
          10},{42,38},{40,38}}, color={0,127,255}));
  connect(colBufTan.port_b, TTanOutTop.port_a) annotation (Line(points={{12,2},{
          16,2},{16,38},{20,38}}, color={0,127,255}));
  connect(colBufTan.port_b1, TTanOutBot.port_a) annotation (Line(points={{-12,-7.6},
          {-14,-7.6},{-14,-30},{-16,-30}}, color={0,127,255}));
  connect(cooPD.port_a, TTanOutBot.port_b) annotation (Line(points={{-42,-30},{-36,
          -30}},                     color={0,127,255}));
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
    experiment(StopTime=31104000, __Dymola_Algorithm="Dassl"));
end StartifiedTank;
