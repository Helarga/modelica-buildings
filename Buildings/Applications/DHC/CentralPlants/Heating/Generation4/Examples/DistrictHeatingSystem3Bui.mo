within Buildings.Applications.DHC.CentralPlants.Heating.Generation4.Examples;
model DistrictHeatingSystem3Bui
  "Example to test the district heating system."
  extends Modelica.Icons.Example;

  package MediumW = Buildings.Media.Water "Medium model for water";
  inner parameter
    Buildings.Applications.DHC.Examples.Combined.Generation5.Unidirectional.Data.DesignDataParallel4GDC
    datDes(
    nBui=nBui,
    mDis_flow_nominal=mCHW_flow_nominal,
    mCon_flow_nominal={bui_ETS[i].ets.mDis_flow_nominal for i in 1:nBui},
    lDis=fill(15, nBui),
    lCon=fill(20, nBui),
    dhDis={0.07,0.06,0.05},
    dhCon=fill(0.05, nBui)) "Design data"
    annotation (Placement(transformation(extent={{-80,40},{-60,60}})));
                                        //sum({bui_ETS[i].ets.mDis_flow_nominal for i in 1:nBui})*1.01,
  parameter Integer nBui=3
  "number of coonected buildings";
  parameter Modelica.SIunits.PressureDifference dpDis_nominal[nBui](
    each min=0, each displayUnit="Pa")= 1/2 .* cat(1, {dp_nominal*0.05}, fill(dp_nominal*0.95 / (nBui-1), nBui-1))
    "Pressure drop between each connected building at nominal conditions (supply line)";
    //1/2 .* fill(dp_nominal/ (nBui), nBui)

  parameter Modelica.SIunits.PressureDifference dp_nominal= dpSetPoi + nBui * 7000;
  parameter Modelica.SIunits.MassFlowRate mCHW_flow_nominal=heaPla.numChi*heaPla.perChi.mEva_flow_nominal
    "Nominal chilled water mass flow rate";
  parameter Modelica.SIunits.MassFlowRate mCW_flow_nominal=heaPla.perChi.mCon_flow_nominal
    "Nominal condenser water mass flow rate";                                                                                                                             //cooPla.mulChiSys.per[1].mEva_flow_nominal//40
  parameter Modelica.SIunits.PressureDifference dpCHW_nominal=44.8*1000
    "Nominal chilled water side pressure";
  parameter Modelica.SIunits.PressureDifference dpCW_nominal=46.2*1000
    "Nominal condenser water side pressure";
  parameter Modelica.SIunits.Power QEva_nominal=mCHW_flow_nominal*4200*(5 - 14)
    "Nominal cooling capaciaty (Negative means cooling)";
  parameter Modelica.SIunits.MassFlowRate mMin_flow= 0.2*mCHW_flow_nominal/heaPla.numChi
    "Minimum mass flow rate of single chiller";
  parameter Boolean allowFlowReversal = false;
  // control settings
  parameter Modelica.SIunits.Pressure dpSetPoi=70000
    "Differential pressure setpoint";
  parameter Modelica.SIunits.Pressure pumDP=dpSetPoi+dpCHW_nominal;
  parameter Modelica.SIunits.Temperature TCHWSet=273.15 + 5
    "Chilled water temperature setpoint";
  parameter Modelica.SIunits.Time tWai=30 "Waiting time";
  // pumps
  parameter Buildings.Fluid.Movers.Data.Generic perCHWPum(
    pressure=Buildings.Fluid.Movers.BaseClasses.Characteristics.flowParameters(
      V_flow={0,mCHW_flow_nominal/heaPla.numChi}/1000,
      dp={pumDP,0}))
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

  Modelica.Blocks.Sources.BooleanConstant on "On signal of the plant"
    annotation (Placement(transformation(extent={{-100,0},{-80,20}})));

  PlantHeating heaPla(
    mMin_flow=mMin_flow,
    tWai=tWai,
    dpSetPoi=dpSetPoi,
    energyDynamics=Modelica.Fluid.Types.Dynamics.FixedInitial)
    "District heating plant."
    annotation (Placement(transformation(extent={{-34,-46},{-14,-26}})));

  Buildings.Applications.DHC.Examples.FifthGeneration.Unidirectional.Loads.BuildingSpawnZ6WithHeatingIndirectETS bui_ETS[nBui](
      TChiWatSup_nominal=280.15)
    annotation (Placement(transformation(extent={{20,60},{40,80}})));
    //dp_nominal=100000)
  Buildings.Fluid.Sources.Boundary_pT cooSou(
    redeclare package Medium = MediumW,
    T=280.15,
    nPorts=nBui) "Cooling source."
    annotation (Placement(transformation(extent={{82,54},{62,74}})));
  Buildings.Fluid.Sources.Boundary_pT cooSin(redeclare package Medium = MediumW,
      nPorts=nBui) "Cooling sink"
    annotation (Placement(transformation(extent={{-30,54},{-10,74}})));
  Loads.Validation.BaseClasses.Distribution2Pipe disNet(
    redeclare final package Medium = MediumW,
    final nCon=nBui,
    iConDpSen=nBui,
    final mDis_flow_nominal=datDes.mDis_flow_nominal,
    final mCon_flow_nominal=datDes.mCon_flow_nominal,
    final allowFlowReversal=allowFlowReversal,
    dpDis_nominal=dpDis_nominal)               "Distribution network."
    annotation (Placement(transformation(extent={{16,20},{44,32}})));
  Modelica.Blocks.Sources.RealExpression TSetHeaWatSup[nBui](y=bui_ETS.bui.T_aHeaWat_nominal)
    "Heating water supply temperature set point."
    annotation (Placement(transformation(extent={{-84,70},{-64,90}})));
  Modelica.Blocks.Continuous.FirstOrder firOrdDel(
    T=60,
    initType=Modelica.Blocks.Types.Init.InitialOutput,
    y_start=70000) "Delay to mimic ETS signal delay."
    annotation (Placement(transformation(extent={{32,-46},{52,-26}})));
equation
  connect(on.y,heaPla. on) annotation (Line(points={{-79,10},{-44,10},{-44,-28},
          {-36,-28}}, color={255,0,255}));
  connect(disNet.port_bDisRet,heaPla. port_a) annotation (Line(points={{16,22.4},
          {0,22.4},{0,-31},{-14,-31}}, color={0,127,255}));
  connect(heaPla.port_b, disNet.port_aDisSup) annotation (Line(points={{-14,-41},
          {-4,-41},{-4,26},{16,26}}, color={0,127,255}));
  for i in 1:nBui loop
  end for;
  connect(TSetHeaWatSup.y, bui_ETS.TSetWat) annotation (Line(points={{-63,80},{12,
          80},{12,73},{19,73}}, color={0,0,127}));
  connect(disNet.dp, firOrdDel.u) annotation (Line(points={{44.7,27.8},{62,27.8},
          {62,-6},{18,-6},{18,-36},{30,-36}}, color={0,0,127}));
  connect(firOrdDel.y,heaPla. dpMea) annotation (Line(points={{53,-36},{68,-36},
          {68,-68},{-50,-68},{-50,-39},{-36,-39}}, color={0,0,127}));
  connect(cooSou.ports, bui_ETS.port_a2)
    annotation (Line(points={{62,64},{40,64}}, color={0,127,255}));
  connect(cooSin.ports, bui_ETS.port_b2)
    annotation (Line(points={{-10,64},{20,64}}, color={0,127,255}));
  connect(disNet.ports_bCon, bui_ETS.port_a1) annotation (Line(points={{21.6,32},
          {2,32},{2,76},{20,76}}, color={0,127,255}));
  connect(disNet.ports_aCon, bui_ETS.port_b1) annotation (Line(points={{38.4,32},
          {52,32},{52,76},{40,76}}, color={0,127,255}));
  annotation (Icon(coordinateSystem(preserveAspectRatio=false)), Diagram(
        coordinateSystem(preserveAspectRatio=false)),
    experiment(
      StartTime=15724800,
      StopTime=15897600,
      Tolerance=1e-06,
      __Dymola_Commands(file="modelica://Buildings/Resources/Scripts/Dymola/Applications/DHC/CentralPlants/Heating/Generation4/Validation/MappingBoilerPLRToMassFlow.mos"
         "Simulate and plot")));
end DistrictHeatingSystem3Bui;
