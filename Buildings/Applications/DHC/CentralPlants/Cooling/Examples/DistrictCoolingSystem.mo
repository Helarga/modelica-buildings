within Buildings.Applications.DHC.CentralPlants.Cooling.Examples;
model DistrictCoolingSystem "Example to test the district cooling system."
  extends Modelica.Icons.Example;

  package Medium = Buildings.Media.Water "Medium model for water";

  // chiller and cooling tower
  redeclare parameter Buildings.Fluid.Chillers.Data.ElectricEIR.ElectricEIRChiller_York_YT_1055kW_5_96COP_Vanes
    perChi;
  parameter Modelica.SIunits.MassFlowRate mCHW_flow_nominal=18.3
    "Nominal chilled water mass flow rate";
  parameter Modelica.SIunits.MassFlowRate mCW_flow_nominal=34.7
    "Nominal condenser water mass flow rate";
  parameter Modelica.SIunits.PressureDifference dpCHW_nominal=44.8*1000
    "Nominal chilled water side pressure";
  parameter Modelica.SIunits.PressureDifference dpCW_nominal=46.2*1000
    "Nominal condenser water side pressure";
  parameter Modelica.SIunits.Power QEva_nominal = mCHW_flow_nominal*4200*(6.67-18.56)
    "Nominal cooling capaciaty (Negative means cooling)";
  parameter Modelica.SIunits.MassFlowRate mMin_flow = 0.03
    "Minimum mass flow rate of single chiller";

  // control settings
  parameter Modelica.SIunits.Pressure dpSetPoi=68900
    "Differential pressure setpoint";
  parameter Modelica.SIunits.Temperature TCHWSet = 273.15 + 8
    "Chilled water temperature setpoint";
  parameter Modelica.SIunits.Time tWai=30 "Waiting time";

  // pumps
  parameter Buildings.Fluid.Movers.Data.Generic perCHWPum(
    pressure=Buildings.Fluid.Movers.BaseClasses.Characteristics.flowParameters(
      V_flow=mCHW_flow_nominal/1000*{0.2,0.6,1.0,1.2},
      dp=(dpCHW_nominal+dpSetPoi+18000+30000)*{1.5,1.3,1.0,0.6}))
    "Performance data for chilled water pumps";
  parameter Buildings.Fluid.Movers.Data.Generic perCWPum(
    pressure=Buildings.Fluid.Movers.BaseClasses.Characteristics.flowParameters(
      V_flow=mCW_flow_nominal/1000*{0.2,0.6,1.0,1.2},
      dp=(dpCW_nominal+60000+6000)*{1.2,1.1,1.0,0.6}))
    "Performance data for condenser water pumps";
  parameter Modelica.SIunits.Pressure dpCHWPum_nominal=6000
    "Nominal pressure drop of chilled water pumps";
  parameter Modelica.SIunits.Pressure dpCWPum_nominal=6000
    "Nominal pressure drop of chilled water pumps";

  // building
  parameter Modelica.SIunits.Power Q_flow_nominal=-50E3
    "Nominal heat flow rate, negative";
  parameter Modelica.SIunits.Power QCooLoa[:, :]= [0, -200E3; 6, -300E3; 12, -500E3; 18, -300E3; 24, -200E3]
    "Cooling load table matrix, negative";

  BoundaryConditions.WeatherData.ReaderTMY3           weaDat(final
      computeWetBulbTemperature=true, filNam=
        Modelica.Utilities.Files.loadResource("modelica://Buildings/Resources/weatherdata/USA_CA_San.Francisco.Intl.AP.724940_TMY3.mos"))
    annotation (Placement(transformation(extent={{-108,-70},{-88,-50}})));

  BoundaryConditions.WeatherData.Bus           weaBus "Weather data bus"
    annotation (Placement(transformation(extent={{-90,-70},{-70,-50}})));

  Modelica.Blocks.Sources.BooleanConstant on "On signal of the plant"
    annotation (Placement(transformation(extent={{-98,32},{-78,52}})));

  Plant pla(
    perChi=perChi,
    perCHWPum=perCHWPum,
    perCWPum=perCWPum,
    mCHW_flow_nominal=mCHW_flow_nominal,
    dpCHW_nominal=dpCHW_nominal,
    QEva_nominal=QEva_nominal,
    mMin_flow=mMin_flow,
    mCW_flow_nominal=mCW_flow_nominal,
    dpCW_nominal=dpCW_nominal,
    TAirInWB_nominal=298.7,
    TCW_nominal=308.15,
    dT_nominal=5.56,
    TMin=288.15,
    PFan_nominal=5000,
    dpCHWPum_nominal=dpCHWPum_nominal,
    dpCWPum_nominal=dpCWPum_nominal,
    tWai=tWai,
    dpSetPoi=dpSetPoi,
    energyDynamics=Modelica.Fluid.Types.Dynamics.FixedInitial)
    "District cooling plant"
    annotation (Placement(transformation(extent={{-52,-22},{-32,-2}})));

  Modelica.Blocks.Sources.Constant TCHWSupSet(k=TCHWSet)
    "Chilled water supply temperature setpoint"
    annotation (Placement(transformation(extent={{-98,0},{-78,20}})));

  inner Modelica.Fluid.System system
    "System properties and default values"
    annotation (Placement(transformation(extent={{-96,72},{-76,92}})));
  Loads.Validation.BaseClasses.Distribution2Pipe
                                dis(
    redeclare final package Medium = Medium1,
    nCon=nLoa,
    allowFlowReversal=false,
    iConDpSen=nLoa,
    mDis_flow_nominal={sum(terUniHea[i:nLoa].mHeaWat_flow_nominal) for i in 1:
        nLoa},
    mCon_flow_nominal=terUniHea.mHeaWat_flow_nominal,
    dpDis_nominal=dpDis_nominal) "Distribution network"
    annotation (Placement(transformation(extent={{-20,36},{2,46}})));
equation
  connect(weaDat.weaBus, weaBus) annotation (Line(
      points={{-88,-60},{-80,-60}},
      color={255,204,51},
      thickness=0.5));
  connect(weaBus.TWetBul, pla.TWetBul) annotation (Line(
      points={{-80,-60},{-60,-60},{-60,-20},{-54,-20}},
      color={255,204,51},
      thickness=0.5));
  connect(TCHWSupSet.y, pla.TCHWSupSet) annotation (Line(points={{-77,10},{-66,
          10},{-66,-9},{-54,-9}},
                              color={0,0,127}));
  connect(on.y, pla.on) annotation (Line(points={{-77,42},{-62,42},{-62,-4},{
          -54,-4}}, color={255,0,255}));
  connect(pla.port_b, dis.port_aDisSup) annotation (Line(points={{-32,-17},{-26,
          -17},{-26,41},{-20,41}}, color={0,127,255}));
  connect(dis.port_bDisRet, pla.port_a) annotation (Line(points={{-20,38},{-24,
          38},{-24,-7},{-32,-7}}, color={0,127,255}));
  annotation (Icon(coordinateSystem(preserveAspectRatio=false)), Diagram(
        coordinateSystem(preserveAspectRatio=false)),
    experiment(StopTime=86400, Tolerance=1e-06));
end DistrictCoolingSystem;
