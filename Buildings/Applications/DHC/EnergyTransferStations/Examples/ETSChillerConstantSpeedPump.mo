within Buildings.Applications.DHC.EnergyTransferStations.Examples;
model ETSChillerConstantSpeedPump
  "ETS example using EIRchiller and constant speed pumps"
  package Medium = Buildings.Media.Water "Medium model";

  parameter Modelica.SIunits.MassFlowRate mEva_flow_nominal=datChi.mEva_flow_nominal
    "Source heat exchanger nominal mass flow rate";
  parameter Modelica.SIunits.MassFlowRate mCon_flow_nominal=datChi.mCon_flow_nominal
    "Load heat exchanger nominal mass flow rate";
  parameter Modelica.SIunits.MassFlowRate mSecHea_flow_nominal=15
   "Secondary(building side) heatig water nominal mass flow rate";
  parameter Modelica.SIunits.MassFlowRate mSecCoo_flow_nominal=8
   "Secondary(building side) cooling water mass flow rate";
  parameter Modelica.SIunits.MassFlowRate mDis_flow_nominal = 3
   "District circuit water mass flow rate";

   SubstationWithConstPrimPum_OnOffChiller ETS(
    datChi=datChi,
    mCon_flow_nominal=mCon_flow_nominal,
    mEva_flow_nominal=mEva_flow_nominal,
    mSecHea_flow_nominal=mSecHea_flow_nominal,
    mSecCoo_flow_nominal=mSecCoo_flow_nominal,
    dTGeo=2,
    dTHex=2,
    dTHeaPum=2,
    xBorFie=datGeo.lBorFie[1],
    yBorFie=datGeo.wBorFie[1],
    dpBorFie_nominal=datGeo.dpBor_nominal,
    THys=1)
    "Energy transfer station for the 5th generation of district heating and cooling"
    annotation (Placement(transformation(extent={{-6,-24},{14,-4}})));
    //dpCon_nominal=chiDat.dpHeaLoa_nominal,
    //dpEva_nominal=chiDat.dpHeaSou_nominal,
  Buildings.Controls.OBC.CDL.Continuous.Sources.Constant TSetCooMin(k=4 +
        273.15)
    "Minimum cooling setpoint temperature"
   annotation (Placement(transformation(extent={{-120,84},{-100,104}})));
  Modelica.Blocks.Sources.Constant TBorMaxEnt(k=40 + 273.15)
    "Cooling setpoint temperature"
    annotation (Placement(transformation(extent={{-120,-10},{-100,10}})));
  Modelica.Fluid.Sources.FixedBoundary heaLoa(
    redeclare package Medium = Medium, nPorts=1)
    "Volume for the heating load"
   annotation (Placement(transformation(extent={{100,-60},{80,-40}})));
  Fluid.Sources.MassFlowSource_T heaPum(
    use_m_flow_in=true,
    use_T_in=true,
    redeclare package Medium = Medium,
    nPorts=1) "Heating load water pump."
   annotation (Placement(transformation(
      extent={{-10,-10},{10,10}},
      rotation=180,
      origin={68,54})));
  Modelica.Blocks.Sources.Constant TMaxEvaEnt(k=17 + 273.15)
    "Maximum heating set point temperature"
    annotation (Placement(transformation(extent={{-120,22},{-100,42}})));
  Modelica.Blocks.Sources.Constant TMinConEnt(k=12 + 273.15)
    "Minimum heating set point temperature"
    annotation (Placement(transformation(extent={{-120,52},{-100,72}})));

  Modelica.Fluid.Sources.FixedBoundary cooLoa(redeclare package Medium = Medium,
      nPorts=1) "Volume for the cooling load"
    annotation (Placement(transformation(extent={{-80,-40},{-60,-20}})));
  Modelica.Fluid.Sources.Boundary_pT   disLoa(redeclare package Medium = Medium,
      nPorts=1) "Volume for the district system"
    annotation (Placement(transformation(extent={{80,-120},{60,-100}})));
  Fluid.Sources.MassFlowSource_T disPum(
    m_flow=mDis_flow_nominal,
    use_T_in=true,
    redeclare package Medium = Medium,
    nPorts=1) "District system water pump" annotation (Placement(transformation(
        extent={{10,-10},{-10,10}},
        rotation=180,
        origin={-30,-106})));
  Buildings.Controls.OBC.CDL.Continuous.Sources.Constant TDisEnt(k=16 + 273.15)
    "District entering water temperature"
    annotation (Placement(transformation(extent={{-120,-120},{-100,-100}})));
  Buildings.Controls.OBC.CDL.Continuous.Sources.Pulse    TSecHeaEnt(
    amplitude=1,
    period=86400*2,
    offset=33 + 273.15)
    "Secondary (building side) return heating water temperature"
    annotation (Placement(transformation(extent={{120,52},{100,72}})));
  Buildings.Controls.OBC.CDL.Continuous.Sources.Pulse    TSecCooEnt(
    amplitude=0,
    period=500,
    offset=12 + 273.15,
    startTime=0)
    "Secondary (building side) return Chilled water temperature"
    annotation (Placement(transformation(extent={{-120,-50},{-100,-30}})));

  Buildings.Applications.DHC.EnergyTransferStations.Data.DesignDataGeothermal datGeo(
    lBorFie={70,90,40,70,120}*0.5,
    wBorFie={44,50,40,40,40})
    "Borfield system performance data"
    annotation (Placement(transformation(extent={{100,120},{120,140}})));
  Buildings.Fluid.Chillers.Data.ElectricEIR.ElectricEIRChiller_McQuay_WSC_816kW_6_74COP_Vanes datChi(PLRMinUnl=
       1, PLRMin=1)
     annotation (Placement(transformation(extent={{100,92},{120,112}})));
  Fluid.Sources.MassFlowSource_T cooPum(
    use_m_flow_in=true,
    m_flow=0,
    use_T_in=true,
    redeclare package Medium = Medium,
    nPorts=1) "Cooling load water pump." annotation (Placement(transformation(
        extent={{10,-10},{-10,10}},
        rotation=180,
        origin={-50,-70})));
  Buildings.Controls.OBC.CDL.Continuous.Sources.Constant TSetHea(k=26 + 273.15)
    "Heating setpoint temperature"
    annotation (Placement(transformation(extent={{20,80},{0,100}})));
  Buildings.Controls.OBC.CDL.Continuous.Sources.Constant TSetCoo(k=12 + 273.15)
    "Cooling setpoint temperature"
    annotation (Placement(transformation(extent={{20,20},{0,40}})));
  Fluid.FixedResistances.PressureDrop heaPD(
    redeclare final package Medium = Medium,
    final m_flow_nominal=mSecHea_flow_nominal,
    final deltaM=0.3,
    final show_T=false,
    final dp_nominal=200)
    "Flow resistance "
    annotation (Placement(transformation(extent={{40,-40},{60,-60}})));
  Fluid.FixedResistances.PressureDrop cooPD(
    redeclare final package Medium = Medium,
    final m_flow_nominal=mSecCoo_flow_nominal,
    final deltaM=0.3,
    final show_T=false,
    final dp_nominal=200)
    "Flow resistance of fluid 2"
    annotation (Placement(transformation(extent={{-20,-40},{-40,-20}})));
  Fluid.FixedResistances.PressureDrop disPD(
    redeclare final package Medium = Medium,
    final m_flow_nominal=mDis_flow_nominal,
    final deltaM=0.3,
    final show_T=false,
    final dp_nominal=200) "Flow resistance"
    annotation (Placement(transformation(extent={{12,-120},{32,-100}})));
  Buildings.Controls.OBC.CDL.Interfaces.IntegerOutput yRejHeaInd
    "Heat rejection index"      annotation (Placement(transformation(extent={{140,-20},
            {160,0}}),      iconTransformation(extent={{100,-2},{128,26}})));
  Buildings.Controls.OBC.CDL.Continuous.Sources.Pulse mSecHea(
    amplitude=0,
    period=86400*2,
    offset=0)  "Secondary (building side) heating water flow rate"
    annotation (Placement(transformation(extent={{120,16},{100,36}})));
  Buildings.Controls.OBC.CDL.Continuous.Sources.Pulse mSecCoo(
    amplitude=10,
    period=500,
    offset=1,
    startTime=0)
              "Secondary (building side) cooling water flow rate"
    annotation (Placement(transformation(extent={{-120,-80},{-100,-60}})));
equation
  connect(TDisEnt.y, disPum.T_in)
  annotation (Line(points={{-98,-110},{-42,-110}},color={0,0,127}));
  connect(TSecHeaEnt.y, heaPum.T_in)
    annotation (Line(points={{98,62},{90,62},{90,50},{80,50}},
                                                            color={0,0,127}));
  connect(TSecCooEnt.y, cooPum.T_in) annotation (Line(points={{-98,-40},{-86,
          -40},{-86,-74},{-62,-74}},
                                color={0,0,127}));
  connect(ETS.disWatSup, disPum.ports[1]) annotation (Line(points={{2.8,-25},{2.8,
          -106},{-20,-106}}, color={0,127,255}));
  connect(ETS.disWatRet, disPD.port_a) annotation (Line(points={{5.2,-25},{8,-25},
          {8,-110},{12,-110}},      color={0,127,255}));
  connect(disLoa.ports[1], disPD.port_b) annotation (Line(points={{60,-110},{32,
          -110}},                     color={0,127,255}));
  connect(ETS.yRejHeaInd, yRejHeaInd) annotation (Line(points={{15.4,-12.8},{
          20.7,-12.8},{20.7,-10},{150,-10}},
                                      color={255,127,0}));
  connect(TBorMaxEnt.y, ETS.TMaxBorEnt) annotation (Line(points={{-99,0},{-42,0},
          {-42,-16},{-6.8,-16}}, color={0,0,127}));
  connect(TMaxEvaEnt.y, ETS.TMaxEvaEnt) annotation (Line(points={{-99,32},{-36,32},
          {-36,-13.6},{-6.8,-13.6}}, color={0,0,127}));
  connect(TMinConEnt.y, ETS.TMinConEnt) annotation (Line(points={{-99,62},{-30,62},
          {-30,-8.2},{-6.8,-8.2}}, color={0,0,127}));
  connect(TSetCooMin.y, ETS.TSetCooMin) annotation (Line(points={{-98,94},{-26,94},
          {-26,-6.6},{-6.8,-6.6}}, color={0,0,127}));
  connect(TSetCoo.y, ETS.TSetCoo) annotation (Line(points={{-2,30},{-14,30},{-14,
          -3.8},{-6.8,-3.8}}, color={0,0,127}));
  connect(TSetHea.y, ETS.TSetHea) annotation (Line(points={{-2,90},{-20,90},{-20,
          -5.2},{-6.8,-5.2}}, color={0,0,127}));
  connect(cooPD.port_b, cooLoa.ports[1])
    annotation (Line(points={{-40,-30},{-60,-30}}, color={0,127,255}));
  connect(heaPum.m_flow_in, mSecHea.y) annotation (Line(points={{80,46},{90,46},
          {90,26},{98,26}},
                          color={0,0,127}));
  connect(mSecCoo.y, cooPum.m_flow_in) annotation (Line(points={{-98,-70},{-88,
          -70},{-88,-78},{-62,-78}},
                                color={0,0,127}));
  connect(heaLoa.ports[1], heaPD.port_b)
    annotation (Line(points={{80,-50},{60,-50}}, color={0,127,255}));
  connect(heaPD.port_a, ETS.hotWatSup) annotation (Line(points={{40,-50},{26,-50},
          {26,-21.2},{15,-21.2}}, color={0,127,255}));
  connect(ETS.hotWatRet, heaPum.ports[1]) annotation (Line(points={{15,-18.8},{26,
          -18.8},{26,54},{58,54}}, color={0,127,255}));
  connect(ETS.chiWatRet, cooPum.ports[1]) annotation (Line(points={{-7,-21.2},{
          -11.5,-21.2},{-11.5,-70},{-40,-70}}, color={0,127,255}));
  connect(ETS.chiWatSup, cooPD.port_a) annotation (Line(points={{-6.8,-19},{-16,
          -19},{-16,-30},{-20,-30}}, color={0,127,255}));
   annotation (Dialog(tab="Borefield"),
              Icon(coordinateSystem(preserveAspectRatio=false, extent={{-100,-100},
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
            -140},{140,160}}),
        graphics={Line(points={{-22,22}}, color={28,108,200})}),
    __Dymola_Commands(
  file="modelica://Buildings/Resources/Scripts/Dymola/Applications/DHC/EnergyTransferStations/Control/ETSChillerConstantSpeedPump.mos"
        "Simulate and plot"),
        experiment(
      StopTime=345600,
      Tolerance=1e-06,
      __Dymola_Algorithm="Cvode"));
end ETSChillerConstantSpeedPump;
