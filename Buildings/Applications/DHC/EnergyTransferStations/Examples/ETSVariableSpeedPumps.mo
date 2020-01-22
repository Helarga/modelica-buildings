within Buildings.Applications.DHC.EnergyTransferStations.Examples;
model ETSVariableSpeedPumps "ETS example first try"
  package Medium = Buildings.Media.Water "Medium model";

  parameter Modelica.SIunits.MassFlowRate mEva_flow_nominal=heaPumDat.hea.mSou_flow
   "Source heat exchanger nominal mass flow rate";
  parameter Modelica.SIunits.MassFlowRate mCon_flow_nominal=heaPumDat.hea.mLoa_flow
   "Load heat exchanger nominal mass flow rate";
  parameter Modelica.SIunits.MassFlowRate mSecHea_flow_nominal=1.5
   "Secondary(building side) heatig water nominal mass flow rate";
  parameter Modelica.SIunits.MassFlowRate mSecCoo_flow_nominal=1.5
   "Secondary(building side) cooling water mass flow rate";

  parameter Modelica.SIunits.MassFlowRate mDis_flow_nominal = 3
   "District circuit water mass flow rate";

   Fluid.HeatPumps.Data.EquationFitReversible.Trane_Axiom_EXW240 heaPumDat
   annotation (Placement(transformation(extent={{100,98},{120,118}})));

  Substation ETS(
    scaling_factor=2,
    mCon_flow_nominal=mCon_flow_nominal,
    mEva_flow_nominal=mEva_flow_nominal,
    mSecHea_flow_nominal=mSecHea_flow_nominal,
    mSecCoo_flow_nominal=mSecCoo_flow_nominal,
    dpCon_nominal=heaPumDat.dpHeaLoa_nominal,
    dpEva_nominal=heaPumDat.dpHeaSou_nominal,
    dTGeo= 2,
    dTHex= 2,
    dTHeaPum=2,
    xBorFie=datGeo.lBorFie[1],
    yBorFie=datGeo.wBorFie[1],
    dpBorFie_nominal=datGeo.dpBor_nominal,
    heaPumDat=heaPumDat,
    THys=1)
  "Energy transfer station for the 5th generation of district heating and cooling"
   annotation (Placement(transformation(extent={{-6,-24},{14,-4}})));
  Buildings.Controls.OBC.CDL.Continuous.Sources.Constant TSetHeaMax(k=45 +
        273.15)
   annotation (Placement(transformation(extent={{-120,120},{-100,140}})));
  Modelica.Blocks.Sources.Constant TBorMaxEnt(k=35 + 273.15)
    "Cooling setpoint temperature"
    annotation (Placement(transformation(extent={{-120,-10},{-100,10}})));
  Modelica.Fluid.Sources.FixedBoundary heaLoa(
    redeclare package Medium = Medium, nPorts=1)
    "Volume for the heating load"
   annotation (Placement(transformation(extent={{80,-62},{60,-42}})));
  Fluid.Sources.MassFlowSource_T heaPum(
    use_m_flow_in=true,
    m_flow=mSecHea_flow_nominal,
    use_T_in=true,
    redeclare package Medium = Medium,
    nPorts=1) "Heating load water pump."
   annotation (Placement(transformation(
      extent={{-10,-10},{10,10}},
      rotation=180,
      origin={70,12})));
  Modelica.Blocks.Sources.Constant TMaxEvaEnt(k=15 + 273.15)
    "Maximum heating set point temperature"
    annotation (Placement(transformation(extent={{-120,22},{-100,42}})));
  Modelica.Blocks.Sources.Constant TMinConEnt(k=25 + 273.15)
    "Minimum heating set point temperature"
    annotation (Placement(transformation(extent={{-120,88},{-100,108}})));

  Modelica.Fluid.Sources.FixedBoundary cooLoa(redeclare package Medium = Medium,
      nPorts=1) "Volume for the cooling load"
    annotation (Placement(transformation(extent={{-60,-62},{-40,-42}})));
  Modelica.Fluid.Sources.Boundary_pT   disLoa(redeclare package Medium = Medium,
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
  Buildings.Controls.OBC.CDL.Continuous.Sources.Constant TSecHeaEnt(k=40 +
        273.15)
    "Secondary (building side) return heating water temperature"
    annotation (Placement(transformation(extent={{120,20},{100,40}})));
  Buildings.Controls.OBC.CDL.Continuous.Sources.Constant TSecCooEnt(k=16 +
        273.15)
    "Secondary (building side) return Chilled water temperature"
    annotation (Placement(transformation(extent={{-120,-40},{-100,-20}})));

  Fluid.Sources.MassFlowSource_T cooPum(
    use_m_flow_in=true,
    m_flow=mSecCoo_flow_nominal,
    use_T_in=true,
    redeclare package Medium = Medium,
    nPorts=1) "Cooling load water pump." annotation (Placement(transformation(
        extent={{10,-10},{-10,10}},
        rotation=180,
        origin={-66,-20})));
  Buildings.Controls.OBC.CDL.Continuous.Sources.Constant mSecHea(k=1)
    "Secondary (building side)  heating water flow rate"
    annotation (Placement(transformation(extent={{120,-20},{100,0}})));
  Buildings.Controls.OBC.CDL.Continuous.Sources.Constant mSecCoo(k=1)
    "Secondary (building side) cooling water flow rate."
    annotation (Placement(transformation(extent={{-120,-74},{-100,-54}})));
  Modelica.Blocks.Sources.Constant conMinFlo(k=0.4)
    "Condenser minimum flow rate."
    annotation (Placement(transformation(extent={{-90,40},{-70,60}})));
  Modelica.Blocks.Sources.Constant evaFloMin(k=0.4)
    "Evaporator minimum flow rate."
    annotation (Placement(transformation(extent={{-90,70},{-70,90}})));
  Buildings.Controls.OBC.CDL.Continuous.Sources.Constant
                                                     TSetHea(k=40 + 273.15)
    "Heating setpoint temperature"
    annotation (Placement(transformation(extent={{20,80},{0,100}})));
  Buildings.Controls.OBC.CDL.Continuous.Sources.Constant
                                                     TSetCoo(k=16 + 273.15)
                       "Cooling setpoint temperature"
    annotation (Placement(transformation(extent={{20,20},{0,40}})));
  Fluid.FixedResistances.PressureDrop heaPD(
    redeclare final package Medium = Medium,
    final m_flow_nominal=mSecHea_flow_nominal,
    final deltaM=0.3,
    final show_T=false,
    final dp_nominal=200) "Flow resistance "
    annotation (Placement(transformation(extent={{28,-42},{48,-62}})));
  Fluid.FixedResistances.PressureDrop cooPD(
    redeclare final package Medium = Medium,
    final m_flow_nominal=mSecCoo_flow_nominal,
    final deltaM=0.3,
    final show_T=false,
    final dp_nominal=200) "Flow resistance of fluid 2"
    annotation (Placement(transformation(extent={{-12,-66},{-32,-46}})));
  Fluid.FixedResistances.PressureDrop disPD(
    redeclare final package Medium = Medium,
    final m_flow_nominal=mDis_flow_nominal,
    final deltaM=0.3,
    final show_T=false,
    final dp_nominal=200) "Flow resistance"
    annotation (Placement(transformation(extent={{12,-118},{32,-98}})));
  Data.DesignDataGeothermal datGeo(lBorFie={70,90,40,70,120}*0.1, wBorFie={44,
        50,40,40,40}*0.1)
    annotation (Placement(transformation(extent={{100,124},{120,144}})));
equation
  connect(TSetHeaMax.y, ETS.TSetHeaMax) annotation (Line(points={{-98,130},{-16,
          130},{-16,-6.6},{-6.8,-6.6}},  color={0,0,127}));
  connect(TMinConEnt.y, ETS.TMinConEnt) annotation (Line(points={{-99,98},{-18,
          98},{-18,-8.2},{-6.8,-8.2}},color={0,0,127}));
  connect(TMaxEvaEnt.y, ETS.TMaxEvaEnt) annotation (Line(points={{-99,32},{-24,
          32},{-24,-13.6},{-6.8,-13.6}},
                                      color={0,0,127}));
  connect(TBorMaxEnt.y, ETS.TMaxBorEnt) annotation (Line(points={{-99,0},{-26,0},
          {-26,-16},{-6.8,-16}},  color={0,0,127}));
  connect(TDisEnt.y, disPum.T_in)
    annotation (Line(points={{-98,-110},{-42,-110}},
                                                   color={0,0,127}));
  connect(TSecHeaEnt.y, heaPum.T_in)
    annotation (Line(points={{98,30},{90,30},{90,8},{82,8}},
                                                 color={0,0,127}));
  connect(mSecHea.y, heaPum.m_flow_in) annotation (Line(points={{98,-10},{90,-10},
          {90,4},{82,4}},     color={0,0,127}));
  connect(TSecCooEnt.y, cooPum.T_in) annotation (Line(points={{-98,-30},{-92,-30},
          {-92,-24},{-78,-24}}, color={0,0,127}));
  connect(mSecCoo.y, cooPum.m_flow_in) annotation (Line(points={{-98,-64},{-88,-64},
          {-88,-28},{-78,-28}}, color={0,0,127}));
  connect(conMinFlo.y, ETS.conFloMin) annotation (Line(points={{-69,50},{-22,50},
          {-22,-11.6},{-6.8,-11.6}},  color={0,0,127}));
  connect(evaFloMin.y, ETS.evaFloMin) annotation (Line(points={{-69,80},{-20,80},
          {-20,-9.8},{-6.8,-9.8}},  color={0,0,127}));
  connect(ETS.TSetCoo, TSetCoo.y) annotation (Line(points={{-6.8,-3.8},{-8,-3.8},
          {-8,30},{-2,30}}, color={0,0,127}));
  connect(ETS.TSetHea, TSetHea.y) annotation (Line(points={{-6.8,-5.2},{-12,
          -5.2},{-12,90},{-2,90}}, color={0,0,127}));
  connect(cooPum.ports[1], ETS.chiWatSup) annotation (Line(points={{-56,-20},{-14,
          -20},{-14,-18.8},{-7,-18.8}}, color={0,127,255}));
  connect(ETS.hotWatSup, heaPum.ports[1]) annotation (Line(points={{15,-18.8},{40,
          -18.8},{40,12},{60,12}}, color={0,127,255}));
  connect(ETS.disWatSup, disPum.ports[1]) annotation (Line(points={{2.8,-25},{2.8,
          -106},{-20,-106}}, color={0,127,255}));
  connect(ETS.hotWatRet, heaPD.port_a) annotation (Line(points={{15,-21.2},{22,
          -21.2},{22,-52},{28,-52}}, color={0,127,255}));
  connect(heaLoa.ports[1], heaPD.port_b)
    annotation (Line(points={{60,-52},{48,-52}}, color={0,127,255}));
  connect(ETS.chiWatRet, cooPD.port_a) annotation (Line(points={{-7,-21.2},{-10,
          -21.2},{-10,-56},{-12,-56}}, color={0,127,255}));
  connect(cooLoa.ports[1], cooPD.port_b) annotation (Line(points={{-40,-52},{
          -36,-52},{-36,-56},{-32,-56}}, color={0,127,255}));
  connect(ETS.disWatRet, disPD.port_a) annotation (Line(points={{5.2,-25},{8,
          -25},{8,-108},{12,-108}}, color={0,127,255}));
  connect(disLoa.ports[1], disPD.port_b) annotation (Line(points={{40,-110},{36,
          -110},{36,-108},{32,-108}}, color={0,127,255}));
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
  file="modelica://Buildings/Resources/Scripts/Dymola/Applications/DHC/EnergyTransferStations/Control/ETSExample.mos"
        "Simulate and plot"),
         experiment(Tolerance=1e-6,StopTime=31536000, __Dymola_Algorithm="Cvode"));
end ETSVariableSpeedPumps;
