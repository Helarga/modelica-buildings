within Buildings.Applications.DHC.Loads;
model TimeSeriesMassFlow_Temp
  "Heating and Cooling Water massflowrates and temperatures from building on EP."
 extends Modelica.Icons.Example;

  package MediumW = Buildings.Media.Water;

  parameter String filNam= "modelica://Buildings/Resources/Data/Applications/DHC/Examples/FourthGeneration/modelica.csv";

  parameter Modelica.SIunits.MassFlowRate mChW_flow_nominal=
    Buildings.Experimental.DistrictHeatingCooling.SubStations.VaporCompression.BaseClasses.getPeakMassFlowRate(
    string="#Nominal chilled water mass flow rate",
    filNam=Modelica.Utilities.Files.loadResource(filNam))
    "Design cooling water flow rate"
    annotation (Dialog(group="Design parameter"));
  parameter Modelica.SIunits.MassFlowRate mHW_flow_nominal=
    Buildings.Experimental.DistrictHeatingCooling.SubStations.VaporCompression.BaseClasses.getPeakMassFlowRate(
    string="#Nominal heating water mass flow rate",
    filNam=Modelica.Utilities.Files.loadResource(filNam))
    "Design heating water flow rate"
    annotation (Dialog(group="Design parameter"));
  parameter Modelica.SIunits.TemperatureDifference delTBuiChW=7
    "Nominal chilled water temperature differnce(building side)";
  parameter Modelica.SIunits.TemperatureDifference delTBuiHW=15
    "Nominal heating water temperature difference(building side) ";
  parameter Modelica.SIunits.TemperatureDifference delTDisChW(displayUnit=
        "degC")=9
    "Nominal chilled water temperature differnce(district side)";
  parameter Modelica.SIunits.TemperatureDifference delTDisHW=20
    "Nominal heating water temperature difference(district side) ";

  Modelica.Blocks.Sources.CombiTimeTable buiMasTem(
    tableOnFile=true,
    tableName="modelica",
    fileName=Modelica.Utilities.Files.loadResource(filNam),
    extrapolation=Modelica.Blocks.Types.Extrapolation.Periodic,
    columns=2:7,
    smoothness=Modelica.Blocks.Types.Smoothness.LinearSegments)
    annotation (Placement(transformation(extent={{110,0},{90,20}})));
  Buildings.Fluid.Sources.MassFlowSource_T supChiWat(
    redeclare package Medium = MediumW,
    use_m_flow_in=true,
    use_T_in=true,
    nPorts=1) "Chilled water supply" annotation (Placement(transformation(
      extent={{10,-10},{-10,10}},
      rotation=0,
      origin={20,22})));
  Buildings.Fluid.Sources.Boundary_pT disCooSin(
    redeclare package Medium = MediumW,
    use_T_in=false,
    T=288.15,
    nPorts=1) "District cooling sink." annotation (Placement(transformation(
        extent={{10,-10},{-10,10}},
        rotation=0,
        origin={22,60})));
  Buildings.Applications.DHC.EnergyTransferStations.CoolingIndirect cooETS(
    allowFlowReversal1=false,
    allowFlowReversal2=false,
    redeclare package Medium = MediumW,
    mBui_flow_nominal=mChW_flow_nominal,
    mDis_flow_nominal=mChW_flow_nominal,
    dpValve_nominal=6000,
    dp1_nominal=500,
    dp2_nominal=500,
    use_Q_flow_nominal=true,
    Q_flow_nominal=mChW_flow_nominal*4187*delTBuiChW,
    T_a1_nominal(displayUnit="degC") = 278.15,
    T_a2_nominal(displayUnit="degC") = 291.15)
    annotation (Placement(transformation(extent={{-26,18},{-6,38}})));
  Buildings.Fluid.Sources.Boundary_pT buiCooSin(
    redeclare package Medium = MediumW,
    use_T_in=false,
    nPorts=1) "Building cooling sink." annotation (Placement(transformation(
        extent={{-10,-10},{10,10}},
        rotation=0,
        origin={-70,20})));
  EnergyTransferStations.Heating.HeatingIndirect heaETS(
    allowFlowReversal1=false,
    allowFlowReversal2=false,
    redeclare package Medium = MediumW,
    mBui_flow_nominal=mHW_flow_nominal,
    mDis_flow_nominal=mHW_flow_nominal*delTBuiHW/delTDisHW,
    dpValve_nominal=6000,
    dp1_nominal=500,
    dp2_nominal=500,
    use_Q_flow_nominal=true,
    Q_flow_nominal=mHW_flow_nominal*4187*delTBuiHW,
    T_a1_nominal=60 + 273.15,
    T_a2_nominal=38 + 273.15,
    reverseActing=true)
    annotation (Placement(transformation(extent={{-32,-48},{-12,-28}})));
  Buildings.Fluid.Sources.MassFlowSource_T supHeaWat(
    redeclare package Medium = MediumW,
    use_m_flow_in=true,
    use_T_in=true,
    nPorts=1) "Heating water supply" annotation (Placement(transformation(
        extent={{10,-10},{-10,10}},
        rotation=0,
        origin={24,-70})));
  Buildings.Fluid.Sources.Boundary_pT buiHeaSin(
    redeclare package Medium = MediumW,
    use_T_in=false,
    nPorts=1) "Building heating sink." annotation (Placement(transformation(
        extent={{-10,-10},{10,10}},
        rotation=0,
        origin={-70,-70})));
  Buildings.Fluid.Sources.Boundary_pT disHeaSou(
    redeclare package Medium = MediumW,
    use_T_in=false,
    T=313.15,
    nPorts=1) "Heating water supply" annotation (Placement(transformation(
        extent={{10,-10},{-10,10}},
        rotation=0,
        origin={20,-30})));
  Buildings.Controls.OBC.UnitConversions.From_degC TChWR
    annotation (Placement(transformation(extent={{72,30},{52,50}})));
  Buildings.Controls.OBC.UnitConversions.From_degC THWR
    "Heating water temperature."
    annotation (Placement(transformation(extent={{78,-60},{58,-40}})));
  Buildings.Controls.OBC.UnitConversions.From_degC TChWSET
    annotation (Placement(transformation(extent={{0,70},{-20,90}})));
  Buildings.Controls.OBC.UnitConversions.From_degC THWSET
    annotation (Placement(transformation(extent={{0,-96},{-20,-76}})));
  Fluid.Sources.MassFlowSource_T supDisChWat(
    redeclare package Medium = MediumW,
    use_m_flow_in=true,
    use_T_in=false,
    T=278.15,
    nPorts=1) "Chilled water supply district side." annotation (Placement(
        transformation(
        extent={{-10,-10},{10,10}},
        rotation=0,
        origin={-70,50})));
  Modelica.Blocks.Sources.RealExpression mDisChW(y=buiMasTem.y[6]*(delTBuiChW/
        delTDisChW))
    annotation (Placement(transformation(extent={{-40,70},{-60,90}})));
  Modelica.Blocks.Sources.RealExpression mDisHW(y=buiMasTem.y[5]*delTDisHW/
        delTBuiHW)
    annotation (Placement(transformation(extent={{-40,-8},{-60,12}})));
  Modelica.Blocks.Sources.RealExpression mBuiHW(y=buiMasTem.y[5])
    annotation (Placement(transformation(extent={{76,-34},{56,-14}})));
  Fluid.Sources.MassFlowSource_T      disHeaSin(
    redeclare package Medium = MediumW,
    use_m_flow_in=true,
    use_T_in=false,
    T=331.15,
    nPorts=1) "District heating sink." annotation (Placement(transformation(
        extent={{-10,-10},{10,10}},
        rotation=0,
        origin={-70,-30})));
  Modelica.Blocks.Sources.RealExpression mBuiChW(y=buiMasTem.y[6])
    annotation (Placement(transformation(extent={{74,52},{54,72}})));
equation
  connect(cooETS.port_a2, supChiWat.ports[1]) annotation (Line(points={{-6,22},
          {10,22}},              color={0,127,255}));
  connect(cooETS.port_b1, disCooSin.ports[1]) annotation (Line(points={{-6,34},
          {0,34},{0,60},{12,60}}, color={0,127,255}));
  connect(buiCooSin.ports[1], cooETS.port_b2) annotation (Line(points={{-60,20},
          {-52,20},{-52,22},{-26,22}},
                                     color={0,127,255}));
  connect(THWSET.y, heaETS.TSetBuiSup) annotation (Line(points={{-22,-86},{-40,
          -86},{-40,-38},{-34,-38}},
                                color={0,0,127}));
  connect(TChWSET.y, cooETS.TSetBuiSup) annotation (Line(points={{-22,80},{-32,
          80},{-32,28},{-28,28}},
                              color={0,0,127}));
  connect(TChWR.y, supChiWat.T_in) annotation (Line(points={{50,40},{42,40},{42,
          26},{32,26}}, color={0,0,127}));
  connect(THWR.y, supHeaWat.T_in) annotation (Line(points={{56,-50},{54,-50},{
          54,-66},{36,-66}},
                        color={0,0,127}));
  connect(supDisChWat.ports[1], cooETS.port_a1) annotation (Line(points={{-60,50},
          {-52,50},{-52,34},{-26,34}}, color={0,127,255}));
  connect(mDisChW.y, supDisChWat.m_flow_in) annotation (Line(points={{-61,80},{
          -88,80},{-88,58},{-82,58}},
                                  color={0,0,127}));
  connect(buiHeaSin.ports[1], heaETS.port_b2) annotation (Line(points={{-60,-70},
          {-48,-70},{-48,-44},{-32,-44}}, color={0,127,255}));
  connect(supHeaWat.ports[1], heaETS.port_a2) annotation (Line(points={{14,-70},
          {0,-70},{0,-44},{-12,-44}}, color={0,127,255}));
  connect(disHeaSou.ports[1], heaETS.port_b1) annotation (Line(points={{10,-30},
          {0,-30},{0,-32},{-12,-32}}, color={0,127,255}));
  connect(mBuiHW.y, supHeaWat.m_flow_in) annotation (Line(points={{55,-24},{46,-24},
          {46,-62},{36,-62}}, color={0,0,127}));
  connect(disHeaSin.ports[1], heaETS.port_a1) annotation (Line(points={{-60,-30},
          {-42,-30},{-42,-32},{-32,-32}}, color={0,127,255}));
  connect(mDisHW.y, disHeaSin.m_flow_in) annotation (Line(points={{-61,2},{-88,
          2},{-88,-22},{-82,-22}}, color={0,0,127}));
  connect(buiMasTem.y[1], THWR.u) annotation (Line(points={{89,10},{86,10},{86,-50},
          {80,-50}}, color={0,0,127}));
  connect(buiMasTem.y[2], THWSET.u) annotation (Line(points={{89,10},{86,10},{86,
          -86},{2,-86}}, color={0,0,127}));
  connect(buiMasTem.y[3], TChWR.u) annotation (Line(points={{89,10},{86,10},{86,
          40},{74,40}}, color={0,0,127}));
  connect(buiMasTem.y[4], TChWSET.u) annotation (Line(points={{89,10},{86,10},{86,
          80},{2,80}}, color={0,0,127}));
  connect(mBuiChW.y, supChiWat.m_flow_in) annotation (Line(points={{53,62},{38,62},
          {38,30},{32,30}}, color={0,0,127}));
  annotation (Diagram(coordinateSystem(extent={{-100,-100},{120,100}})), Icon(
        coordinateSystem(extent={{-100,-100},{120,100}})),
    experiment(StopTime=31534200, __Dymola_Algorithm="Dassl"));
end TimeSeriesMassFlow_Temp;
