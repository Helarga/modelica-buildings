within Buildings.Applications.DHC.CentralPlants.Heating.Generation4.Validation;
model CentralPlantHeating4G
  "The vaildation of the fourth generation central heating plant."
  extends Modelica.Icons.Example;

  package Medium = Buildings.Media.Water;

  parameter Integer nBui= 2;
  parameter Modelica.SIunits.HeatFlowRate Q_flow_nominal=100000
    "Heating load";
  parameter Modelica.SIunits.HeatFlowRate QBoi_flow_nominal= Q_flow_nominal/numBoi
    "Boiler heat flow rate";
  parameter Modelica.SIunits.MassFlowRate mHW_flow_nominal = Q_flow_nominal/(4200*delTDis)
    "Heating water flow rate";                               //mBoi_flow_nominal*numBoi
  parameter Modelica.SIunits.MassFlowRate mBoi_flow_nominal= QBoi_flow_nominal/(4200*5)
    "Heating water flow rate";
  parameter Modelica.SIunits.TemperatureDifference delTDis=5
    "Nominal heating water temperature difference (district side)";
                                                            //15
  parameter Integer numBoi= 2
    "Number of boilers";
  parameter Modelica.SIunits.PressureDifference dpHW_nominal=(dpBoi_nominal+dpSetPoi+20000)
    "Heating circuit ressure drop including 50% as a factor of safey";
  parameter Modelica.SIunits.PressureDifference dpBoi_nominal=10000
    "Boiler pressure drop";
  parameter Modelica.SIunits.PressureDifference dpSetPoi=70000
    "Setpoint pressure drop";
  redeclare parameter Buildings.Fluid.Movers.Data.Generic perHWPum(
    pressure=Buildings.Fluid.Movers.BaseClasses.Characteristics.flowParameters(
      V_flow=mBoi_flow_nominal/1000*{0.1,1.5},
      dp=(dpHW_nominal)*{1.5,0.1}))
    "Performance data for chilled water pumps";
 parameter Modelica.SIunits.PressureDifference dpDis_nominal[nBui](
    each min=0, each displayUnit="Pa")= 1/2 .* cat(1, {dp_nominal*0.05}, fill(dp_nominal*0.95 / (nBui-1), nBui-1))
    "Pressure drop between each connected building at nominal conditions (supply line)";
 parameter Modelica.SIunits.PressureDifference dp_nominal= dpSetPoi + nBui * 7000;

  PlantHeating    heaPla(
    show_T=true,
    mHW_flow_nominal=mHW_flow_nominal,
    dpHW_nominal = dpHW_nominal,
    QBoi_flow_nominal=QBoi_flow_nominal,
    mMin_flow=0.2*mBoi_flow_nominal,
    mBoi_flow_nominal=mBoi_flow_nominal,
    dpBoi_nominal=dpBoi_nominal,
    delT_nominal(displayUnit="degC") = 5,
    perHWPum=perHWPum,
    tWai=100,
    dpSetPoi=dpSetPoi,
    energyDynamics=Modelica.Fluid.Types.Dynamics.FixedInitial)
    "The fourth generation disrict heating plant."
    annotation (Placement(transformation(extent={{-36,4},{-16,24}})));
  Modelica.Blocks.Sources.BooleanConstant on
    "Central heating plant on signal."
    annotation (Placement(transformation(extent={{-80,40},{-60,60}})));

  Modelica.Blocks.Sources.Constant  dpMea(k=0.5*dpSetPoi)
                 "Measured pressure difference"
    annotation (Placement(transformation(extent={{-94,-36},{-74,-16}})));
  EnergyTransferStations.Heating.HeatingIndirect heaETS[2](
    show_T=true,
    redeclare package Medium = Medium,
    mDis_flow_nominal=0.8*mBoi_flow_nominal,
    mBui_flow_nominal=mBoi_flow_nominal,
    dpValve_nominal=6000,
    dp1_nominal=1000,
    dp2_nominal=1000,
    use_Q_flow_nominal=true,
    Q_flow_nominal=QBoi_flow_nominal,
    T_a1_nominal(displayUnit="degC") = 333.15,
    T_a2_nominal(displayUnit="degC") = 318.15,
    eta=1)   annotation (Placement(transformation(extent={{20,60},{40,80}})));
  Fluid.Sources.MassFlowSource_T           souHea(
    redeclare package Medium = Medium,
    use_m_flow_in=false,
    m_flow=0.5*mBoi_flow_nominal,
    T=318.15,
    nPorts=2) annotation (Placement(transformation(extent={{88,54},{68,74}})));
  Fluid.Sources.Boundary_pT sinHea(redeclare package Medium = Medium, nPorts=2)
    annotation (Placement(transformation(extent={{-20,40},{0,60}})));
  Modelica.Blocks.Sources.RealExpression Tset[nBui](y=50 + 273.15)
    annotation (Placement(transformation(extent={{-20,70},{0,90}})));
  inner parameter
    Buildings.Applications.DHC.Examples.Combined.Generation5.Unidirectional.Data.DesignDataParallel4GDC
    datDes(
    nBui=nBui,
    mDis_flow_nominal=sum(datDes.mCon_flow_nominal[i] for i in 1:nBui),
    mCon_flow_nominal={heaETS[i].mDis_flow_nominal for i in 1:nBui},
    lDis=fill(3, nBui),
    lCon=fill(3, nBui),
    lEnd=1,
    dhDis={0.05,0.03},
    dhCon=fill(0.03, nBui)) "Design data"
    annotation (Placement(transformation(extent={{-82,74},{-62,94}})));
  Loads.Validation.BaseClasses.Distribution2Pipe disNet(
    redeclare package Medium = Medium,
    nCon=nBui,
    mDis_flow_nominal=2*mBoi_flow_nominal,
    each mCon_flow_nominal={mBoi_flow_nominal,mBoi_flow_nominal},
    energyDynamics=Modelica.Fluid.Types.Dynamics.FixedInitial,
    dpDis_nominal=dpDis_nominal)
    annotation (Placement(transformation(extent={{16,8},{44,22}})));
  Modelica.Blocks.Continuous.FirstOrder firOrdDel(
    T=60,
    initType=Modelica.Blocks.Types.Init.InitialOutput,
    y_start=70000) "Delay to mimic ETS signal delay."
    annotation (Placement(transformation(extent={{60,-40},{40,-20}})));
equation
 for i in 1:numBoi loop
  connect(on.y, heaETS[i].reSet) annotation (Line(points={{-59,50},{-46,50},{-46,
          66.8},{18,66.8}}, color={255,0,255}));
 end for;
  connect(on.y, heaPla.on) annotation (Line(points={{-59,50},{-46,50},{-46,22},
          {-38,22}}, color={255,0,255}));
  connect(Tset.y, heaETS.TSetBuiSup) annotation (Line(points={{1,80},{10,80},{10,
          70},{18,70}},    color={0,0,127}));
  connect(sinHea.ports, heaETS.port_b2) annotation (Line(points={{0,50},{6,50},{
          6,64},{20,64}}, color={0,127,255}));
  connect(heaETS.port_a2, souHea.ports)
    annotation (Line(points={{40,64},{68,64}}, color={0,127,255}));
  connect(heaPla.port_b, disNet.port_aDisSup) annotation (Line(points={{-16,9},
          {-2,9},{-2,15},{16,15}}, color={0,127,255}));
  connect(disNet.port_bDisRet, heaPla.port_a) annotation (Line(points={{16,10.8},
          {-2,10.8},{-2,19},{-16,19}}, color={0,127,255}));
  connect(disNet.ports_bCon, heaETS.port_a1) annotation (Line(points={{21.6,22},
          {14,22},{14,76},{20,76}}, color={0,127,255}));
  connect(disNet.ports_aCon, heaETS.port_b1) annotation (Line(points={{38.4,22},
          {52,22},{52,76},{40,76}}, color={0,127,255}));
  connect(disNet.dp, firOrdDel.u) annotation (Line(points={{44.7,17.1},{80,17.1},
          {80,-30},{62,-30}}, color={0,0,127}));
  connect(firOrdDel.y, heaPla.dpMea) annotation (Line(points={{39,-30},{-60,-30},
          {-60,11},{-38,11}}, color={0,0,127}));
  annotation (Icon(coordinateSystem(preserveAspectRatio=false,grid={2,2})),
       Diagram(coordinateSystem(preserveAspectRatio=false, grid={2,2})),
     experiment(
      StopTime=86400,
      Tolerance=1e-06,
      __Dymola_Algorithm="Dassl"),
      __Dymola_Commands(file="modelica://Buildings/Resources/Scripts/Dymola/Applications/DHC/CentralPlants/Heating/Generation4/Validation/MappingBoilerPLRToMassFlow.mos"
          "Simulate and plot"));
end CentralPlantHeating4G;
