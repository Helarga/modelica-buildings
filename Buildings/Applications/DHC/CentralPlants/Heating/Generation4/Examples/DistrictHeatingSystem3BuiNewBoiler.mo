within Buildings.Applications.DHC.CentralPlants.Heating.Generation4.Examples;
model DistrictHeatingSystem3BuiNewBoiler
  "Example to test the district heating system."
  extends Modelica.Icons.Example;

  package MediumW = Buildings.Media.Water "Medium model for water";
  inner parameter
    Buildings.Applications.DHC.Examples.Combined.Generation5.Unidirectional.Data.DesignDataParallel4GDC
    datDes(
    nBui=nBui,
    mDis_flow_nominal=sum({bui_ETS[i].ets.mDis_flow_nominal for i in 1:nBui}),
    mCon_flow_nominal={bui_ETS[i].ets.mDis_flow_nominal for i in 1:nBui},
    lDis=fill(15, nBui),
    lCon=fill(20, nBui),
    dhDis={0.05,0.025},
    dhCon=fill(0.025, nBui))
                            "Design data"
    annotation (Placement(transformation(extent={{-80,20},{-60,40}})));
   //sum({bui_ETS[i].ets.mDis_flow_nominal for i in 1:nBui})*1.01,
  parameter Integer nBui=2
  "number of coonected buildings";
  parameter Modelica.SIunits.PressureDifference dpDis_nominal[nBui](
    each min=0, each displayUnit="Pa")= 1/2 .* cat(1, {dp_nominal*0.05}, fill(dp_nominal*0.95 / (nBui-1), nBui-1))
    "Pressure drop between each connected building at nominal conditions (supply line)";
    //1/2 .* fill(dp_nominal/ (nBui), nBui)

  parameter Modelica.SIunits.HeatFlowRate Q_flow_nominal=50000
    "Heating load";
  parameter Modelica.SIunits.PressureDifference dp_nominal= dpSetPoi + nBui * 7000;

  parameter Modelica.SIunits.MassFlowRate mHW_flow_nominal=mBoi_flow_nominal * heaPla.numBoi
    "Nominal chilled water mass flow rate";                 //Q_flow_nominal/(4200*heaPla.delT_nominal)                                                        //cooPla.mulChiSys.per[1].mEva_flow_nominal//40
  parameter Modelica.SIunits.MassFlowRate mBoi_flow_nominal= QBoi_nominal/(4200*heaPla.delT_nominal)
    "Nominal chilled water mass flow rate";
  parameter Modelica.SIunits.PressureDifference dpHW_nominal=45000
    "Nominal chilled water side pressure";
  parameter Modelica.SIunits.Power QBoi_nominal=Q_flow_nominal/heaPla.numBoi
    "Nominal cooling capaciaty (Negative means cooling)";
  parameter Modelica.SIunits.MassFlowRate mMin_flow= 0.2*mHW_flow_nominal/heaPla.numBoi
    "Minimum mass flow rate of single chiller";
  parameter Boolean allowFlowReversal = false;
  // control settings
  parameter Modelica.SIunits.Pressure dpSetPoi=70000
    "Differential pressure setpoint";
  parameter Modelica.SIunits.Pressure pumDP=(heaPla.dpBoi_nominal+dpSetPoi+20000);

  parameter Modelica.SIunits.Time tWai=30 "Waiting time";
  // pumps
  parameter Buildings.Fluid.Movers.Data.Generic perHWPum(
    pressure=Buildings.Fluid.Movers.BaseClasses.Characteristics.flowParameters(
      V_flow=mBoi_flow_nominal/1000 *{0.1,1.5},
      dp=pumDP*{1.5,0.1}))
    "Performance data for chilled water pumps"; //datDes.mDis_flow_nominal

  PlantHeatingNewBoiler heaPla(
    mHW_flow_nominal= mHW_flow_nominal,
    dpHW_nominal=dpHW_nominal,
    QBoi_flow_nominal=QBoi_nominal,
    mMin_flow=mMin_flow,
    mBoi_flow_nominal=mBoi_flow_nominal,
    dpBoi_nominal=10000,
    delT_nominal(displayUnit="degC") = 10,
    perHWPum=perHWPum,
    tWai=tWai,
    dpSetPoi=dpSetPoi,
    energyDynamics=Modelica.Fluid.Types.Dynamics.FixedInitial)
    "District heating plant."
    annotation (Placement(transformation(extent={{-40,-40},{-20,-20}})));

  Buildings.Applications.DHC.Examples.FifthGeneration.Unidirectional.Loads.BuildingSpawnZ6WithHeatingIndirectETS bui_ETS[nBui]
    annotation (Placement(transformation(extent={{20,60},{40,80}})));
    //THeaWatSup_nominal=328.15,
   // THeaWatRet_nominal=313.15)
    //dp_nominal=100000)
  Buildings.Fluid.Sources.Boundary_pT cooSou(
    redeclare package Medium = MediumW,
    T=280.15,
    nPorts=nBui) "Cooling source."
    annotation (Placement(transformation(extent={{84,54},{64,74}})));
  Buildings.Fluid.Sources.Boundary_pT cooSin(redeclare package Medium = MediumW,
    T=287.15,
      nPorts=nBui) "Cooling sink"
    annotation (Placement(transformation(extent={{-30,54},{-10,74}})));
  Loads.Validation.BaseClasses.Distribution2Pipe disNet(
    redeclare final package Medium = MediumW,
    final nCon=nBui,
    iConDpSen=1,
    final mDis_flow_nominal=datDes.mDis_flow_nominal,
    final mCon_flow_nominal=datDes.mCon_flow_nominal,
    final allowFlowReversal=allowFlowReversal,
    dpDis_nominal=dpDis_nominal)               "Distribution network."
    annotation (Placement(transformation(extent={{16,20},{44,32}})));
  Modelica.Blocks.Sources.RealExpression TSetHeaWatSup[nBui](each y=50 + 273.15)
    "Heating water supply temperature set point."
    annotation (Placement(transformation(extent={{-90,70},{-70,90}})));
  Fluid.Sensors.TemperatureTwoPort           senTDisSup(redeclare final package
      Medium = MediumW, final m_flow_nominal=mHW_flow_nominal)
    "District-side (primary) supply temperature sensor"
    annotation (Placement(transformation(extent={{-10,-10},{10,10}},
        rotation=90,
        origin={14,-20})));
  Modelica.Blocks.Sources.BooleanConstant mPum_flow(k=true)
    "Total heating water pump mass flow rate"
    annotation (Placement(transformation(extent={{-20,0},{-40,20}})));
  Modelica.Blocks.Sources.RealExpression TDisSetHeaWat(each y=55 + 273.15)
    "Distrcit side heating water supply temperature set point."
    annotation (Placement(transformation(extent={{-90,-50},{-70,-30}})));
equation

 for i in 1:nBui loop
  connect(disNet.ports_bCon[i], bui_ETS[i].port_a1) annotation (Line(points={{21.6,32},
            {0,32},{0,76},{20,76}},
                                  color={0,127,255}));
  connect(disNet.ports_aCon[i], bui_ETS[i].port_b1) annotation (Line(points={{38.4,32},
          {52,32},{52,76},{40,76}}, color={0,127,255}));
  connect(cooSou.ports[i], bui_ETS[i].port_a2)  annotation (Line(points={{64,64},
            {40,64}},                                                                      color={0,127,255}));
  connect(cooSin.ports[i], bui_ETS[i].port_b2)   annotation (Line(points={{-10,64},{20,64}}, color={0,127,255}));

 end for;
  connect(TSetHeaWatSup.y, bui_ETS.TSetWat) annotation (Line(points={{-69,80},{12,
          80},{12,73},{19,73}}, color={0,0,127}));
  connect(heaPla.port_a, disNet.port_bDisRet) annotation (Line(points={{-20,-25},
          {0,-25},{0,22.4},{16,22.4}}, color={0,127,255}));

  connect(disNet.dp, heaPla.dpMea) annotation (Line(points={{44.7,27.8},{64,27.8},
          {64,-66},{-60,-66},{-60,-33},{-42,-33}}, color={0,0,127}));
  connect(heaPla.port_b, senTDisSup.port_a) annotation (Line(points={{-20,-35},
          {0,-35},{0,-34},{14,-34},{14,-30}}, color={0,127,255}));
  connect(senTDisSup.port_b, disNet.port_aDisSup) annotation (Line(points={{14,
          -10},{14,0},{2,0},{2,26},{16,26}}, color={0,127,255}));
  connect(mPum_flow.y, heaPla.on) annotation (Line(points={{-41,10},{-60,10},{
          -60,-22},{-42,-22}},
                           color={255,0,255}));
  connect(TDisSetHeaWat.y, heaPla.THeaSet) annotation (Line(points={{-69,-40},{-56,
          -40},{-56,-38.4},{-42,-38.4}}, color={0,0,127}));
  annotation (Icon(coordinateSystem(preserveAspectRatio=false)), Diagram(
        coordinateSystem(preserveAspectRatio=false)),
    experiment(
      StartTime=3600,
      StopTime=86400,
      Tolerance=1e-06,
      __Dymola_Algorithm="Dassl"));
end DistrictHeatingSystem3BuiNewBoiler;
