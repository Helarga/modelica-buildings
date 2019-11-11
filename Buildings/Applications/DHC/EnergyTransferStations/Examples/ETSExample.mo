within Buildings.Applications.DHC.EnergyTransferStations.Examples;
model ETSExample "ETS example first try"
  package Medium = Buildings.Media.Water "Medium model";

  parameter Modelica.SIunits.MassFlowRate mEva_flow_nominal=heaPumDat.hea.mSou_flow
   "Source heat exchanger nominal mass flow rate";
  parameter Modelica.SIunits.MassFlowRate mCon_flow_nominal=heaPumDat.hea.mLoa_flow
   "Load heat exchanger nominal mass flow rate";
  /*parameter Real scaling_factor=1
   "Scaling factor for heatpump capacity"; */
  parameter Modelica.SIunits.MassFlowRate mSecHea_flow_nominal= 2
   "Secondary(building side) heatig water nominal mass flow rate";
  parameter Modelica.SIunits.MassFlowRate mSecCoo_flow_nominal= 2
   "Secondary(building side) cooling water mass flow rate";
  parameter Modelica.SIunits.MassFlowRate mDis_flow_nominal = 5
   "District circuit water mass flow rate";

  parameter Modelica.SIunits.Temperature THeaWatSup_nominal=314.15
      "Nominal heating supply water temperature"
      annotation (Dialog(group="Design parameter"));
  parameter Modelica.SIunits.Temperature THeaWatRet_nominal=303.15
    "Nominal heating Return water temperature"
    annotation (Dialog(group="Design parameter"));
  parameter Modelica.SIunits.Temperature TCooWatSup_max=288.15
    "Maximum cooling water supply temperature"
    annotation (Dialog(group="Design parameter"));
  parameter Modelica.SIunits.Temperature TCooWatSup_min=277.15
    "Minimum cooling water supply temperature"
    annotation (Dialog(group="Design parameter"));
  parameter Modelica.SIunits.TemperatureDifference dTCooWat=4
    "Cooling water supply and return temperature difference"
    annotation (Dialog(group="Design parameter"));

   Fluid.HeatPumps.Data.EquationFitReversible.Trane_Axiom_EXW240 heaPumDat
   annotation (Placement(transformation(extent={{100,98},{120,118}})));

  Substation ETS(
    mCon_flow_nominal=mCon_flow_nominal,
    mEva_flow_nominal=mEva_flow_nominal,
    dpCon_nominal=heaPumDat.dpHeaLoa_nominal,
    dpEva_nominal=heaPumDat.dpHeaSou_nominal,
    heaPumDat=heaPumDat,
    mGeo_flow_nominal=3,
    dTGeo= 5,
    mHex_flow_nominal=3,
    dTHex= 2,
    xBorFie=datGeo.lBorFie[1],
    yBorFie=datGeo.wBorFie[1],
    dpBorFie_nominal=datGeo.dpBor_nominal)
  "Energy transfer station for the 5th generation of district heating and cooling"
   annotation (Placement(transformation(extent={{-12,-24},{8,-4}})));


  Buildings.Controls.OBC.CDL.Continuous.Sources.Constant TSetHeaMax(k=40 + 273.15)
   annotation (Placement(transformation(extent={{-120,120},{-100,140}})));
  Fluid.FixedResistances.PressureDrop resDis(
    redeclare package Medium = Medium,
    m_flow_nominal=mDis_flow_nominal,
    dp_nominal=6000) "Flow resistance"
    annotation (Placement(transformation(extent={{12,-120},{32,-100}})));
  Modelica.Blocks.Sources.Constant TBorMaxEnt(k=35 + 273.15)
    "Cooling setpoint temperature"
    annotation (Placement(transformation(extent={{-120,0},{-100,20}})));
  Modelica.Fluid.Sources.FixedBoundary heaLoa(redeclare package Medium = Medium)
                "Volume for the heating load"
   annotation (Placement(transformation(extent={{80,-60},{60,-40}})));
  Fluid.Sources.MassFlowSource_T heaPum(
    use_m_flow_in=true,
    m_flow=mSecHea_flow_nominal,
    use_T_in=true,
    redeclare package Medium = Medium,
    nPorts=1) "heating load water pump"
   annotation (Placement(transformation(
      extent={{-10,-10},{10,10}},
      rotation=180,
      origin={70,-8})));
  Modelica.Blocks.Sources.Constant TMaxEvaEnt(k=20 + 273.15)
    "Maximum heating set point temperature"
    annotation (Placement(transformation(extent={{-120,40},{-100,60}})));
  Modelica.Blocks.Sources.Constant TMinConEnt(k=25 + 273.15)
    "Minimum heating set point temperature"
    annotation (Placement(transformation(extent={{-120,78},{-100,98}})));

  Modelica.Fluid.Sources.FixedBoundary cooLoa(redeclare package Medium = Medium,
      nPorts=1) "Volume for the cooling load"
    annotation (Placement(transformation(extent={{-80,-60},{-60,-40}})));
  Fluid.FixedResistances.PressureDrop resCoo(
    redeclare package Medium = Medium,
    m_flow_nominal=mSecCoo_flow_nominal,
    dp_nominal=6000) "Flow resistance"
    annotation (Placement(transformation(extent={{-30,-58},{-50,-38}})));
  Modelica.Fluid.Sources.FixedBoundary disLoa(redeclare package Medium = Medium,
      nPorts=1) "Volume for the district system"
    annotation (Placement(transformation(extent={{60,-120},{40,-100}})));
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
  Buildings.Controls.OBC.CDL.Continuous.Sources.Constant TSecHeaEnt(k=35 + 273.15)
    "Secondary (building side) return heating water temperature"
    annotation (Placement(transformation(extent={{120,0},{100,20}})));
  Buildings.Controls.OBC.CDL.Continuous.Sources.Constant TSecCooEnt(k=12 + 273.15)
    "Secondary (building side) return Chilled water temperature"
    annotation (Placement(transformation(extent={{-120,-40},{-100,-20}})));

  SidewalkQuayside.Data.DesignDataGeothermal datGeo
    annotation (Placement(transformation(extent={{100,124},{120,144}})));
  BoundaryConditions.WeatherData.ReaderTMY3 weaDat(filNam=
        Modelica.Utilities.Files.loadResource(
        "modelica://Buildings/Resources/weatherdata/USA_IL_Chicago-OHare.Intl.AP.725300_TMY3.mos"),
      computeWetBulbTemperature=false)
    annotation (Placement(transformation(extent={{120,58},{100,78}})));
  BoundaryConditions.WeatherData.Bus weaBus
    annotation (Placement(transformation(extent={{40,58},{60,78}}),
        iconTransformation(extent={{-38,0},{-18,20}})));
  Fluid.Sources.MassFlowSource_T cooPum(
    use_m_flow_in=true,
    m_flow=mSecCoo_flow_nominal,
    use_T_in=true,
    redeclare package Medium = Medium,
    nPorts=1) "Cooling load water pump." annotation (Placement(transformation(
        extent={{10,-10},{-10,10}},
        rotation=180,
        origin={-70,-10})));
  Buildings.Controls.OBC.CDL.Continuous.Sources.Constant mSecHea(k=2)
    "Secondary (building side)  heating water flow rate"
    annotation (Placement(transformation(extent={{120,-40},{100,-20}})));
  Buildings.Controls.OBC.CDL.Continuous.Sources.Constant mSecCoo(k=0)
    "Secondary (building side) cooling water flow rate."
    annotation (Placement(transformation(extent={{-120,-74},{-100,-54}})));
equation
  connect(ETS.chiWatRet, resCoo.port_a) annotation (Line(points={{-13,-21.2},{-26,
          -21.2},{-26,-48},{-30,-48}}, color={0,127,255}));
  connect(disPum.ports[1], ETS.disWatSup) annotation (Line(points={{-20,-106},{-3.2,
          -106},{-3.2,-25}},color={0,127,255}));
  connect(resDis.port_a, ETS.disWatRet) annotation (Line(points={{12,-110},{-0.8,
          -110},{-0.8,-25}},color={0,127,255}));
  connect(TSetHeaMax.y, ETS.TSetHeaMax) annotation (Line(points={{-98,130},{-16,
          130},{-16,-7.8},{-12.8,-7.8}}, color={0,0,127}));
  connect(TMinConEnt.y, ETS.TMinConEnt) annotation (Line(points={{-99,88},{-18,88},
          {-18,-10.8},{-12.8,-10.8}}, color={0,0,127}));
  connect(TMaxEvaEnt.y, ETS.TMaxEvaEnt) annotation (Line(points={{-99,50},{-20,50},
          {-20,-13.6},{-12.8,-13.6}}, color={0,0,127}));
  connect(TBorMaxEnt.y, ETS.TMaxBorEnt) annotation (Line(points={{-99,10},{-22,10},
          {-22,-16},{-12.8,-16}}, color={0,0,127}));
  connect(TDisEnt.y, disPum.T_in)
    annotation (Line(points={{-98,-110},{-42,-110}},
                                                   color={0,0,127}));
  connect(TSecHeaEnt.y, heaPum.T_in)
    annotation (Line(points={{98,10},{90,10},{90,-12},{82,-12}},
                                                 color={0,0,127}));
  connect(weaDat.weaBus,weaBus)  annotation (Line(
      points={{100,68},{50,68}},
      color={255,204,51},
      thickness=0.5,
      smooth=Smooth.None), Text(
      textString="%second",
      index=1,
      extent={{6,3},{6,3}}));
  connect(ETS.weaBus, weaBus) annotation (Line(
      points={{-13,-4.4},{-13,68},{50,68}},
      color={255,204,51},
      thickness=0.5), Text(
      string="%second",
      index=1,
      extent={{-3,6},{-3,6}},
      horizontalAlignment=TextAlignment.Right));
  connect(resDis.port_b, disLoa.ports[1])
    annotation (Line(points={{32,-110},{40,-110}}, color={0,127,255}));
  connect(cooLoa.ports[1], resCoo.port_b) annotation (Line(points={{-60,-50},{-60,
          -48},{-50,-48}}, color={0,127,255}));
  connect(cooPum.ports[1], ETS.chiWatSup) annotation (Line(points={{-60,-10},{-26,
          -10},{-26,-18.8},{-13,-18.8}}, color={0,127,255}));
  connect(mSecHea.y, heaPum.m_flow_in) annotation (Line(points={{98,-30},{90,-30},
          {90,-16},{82,-16}}, color={0,0,127}));
  connect(TSecCooEnt.y, cooPum.T_in) annotation (Line(points={{-98,-30},{-92,-30},
          {-92,-14},{-82,-14}}, color={0,0,127}));
  connect(mSecCoo.y, cooPum.m_flow_in) annotation (Line(points={{-98,-64},{-88,-64},
          {-88,-18},{-82,-18}}, color={0,0,127}));
  connect(heaPum.ports[1], ETS.hotWatSup) annotation (Line(points={{60,-8},{20,-8},
          {20,-18.8},{9,-18.8}}, color={0,127,255}));
  connect(heaLoa.ports[1], ETS.hotWatRet) annotation (Line(points={{60,-50},{20,
          -50},{20,-21.2},{9,-21.2}}, color={0,127,255}));
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
    experiment(StopTime=14400),
    __Dymola_Commands(
  file="modelica://Buildings/Resources/Scripts/Dymola/Applications/DHC/EnergyTransferStations/Control/HeatpumpController.mos"
        "Simulate and plot"),
         experiment(Tolerance=1e-6, StopTime=14400));
end ETSExample;
