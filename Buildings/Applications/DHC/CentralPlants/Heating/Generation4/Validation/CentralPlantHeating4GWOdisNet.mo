within Buildings.Applications.DHC.CentralPlants.Heating.Generation4.Validation;
model CentralPlantHeating4GWOdisNet
  "The vaildation of the fourth generation central heating plant."
  extends Modelica.Icons.Example;

  package Medium = Buildings.Media.Water;

  parameter Integer nBui= 2;
  parameter Modelica.SIunits.HeatFlowRate Q_flow_nominal=18000
    "Heating load";
  parameter Modelica.SIunits.HeatFlowRate QBoi_flow_nominal= Q_flow_nominal/numBoi
    "Boiler heat flow rate";
  parameter Modelica.SIunits.MassFlowRate mHW_flow_nominal = mBoi_flow_nominal*numBoi
    "Heating water flow rate";
  //Q_flow_nominal/(4200*delTDis)                            //
  parameter Modelica.SIunits.MassFlowRate mBoi_flow_nominal= QBoi_flow_nominal/(4200*5)
    "Heating water flow rate";
  parameter Modelica.SIunits.TemperatureDifference delTDis=15
    "Nominal heating water temperature difference (district side)";
                                                            //15
  parameter Integer numBoi= 2
    "Number of boilers";
  parameter Modelica.SIunits.PressureDifference dpHW_nominal=(dpBoi_nominal+dpSetPoi+100000)
    "Heating circuit ressure drop including 50% as a factor of safey";
  parameter Modelica.SIunits.PressureDifference dpBoi_nominal=10000
    "Boiler pressure drop";
  parameter Modelica.SIunits.PressureDifference dpSetPoi=70000
    "Setpoint pressure drop";
  redeclare parameter Buildings.Fluid.Movers.Data.Generic perHWPum(
    pressure=Buildings.Fluid.Movers.BaseClasses.Characteristics.flowParameters(
      V_flow=mHW_flow_nominal/1000*{0.1,1.5},
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
    mMin_flow=0.1*mBoi_flow_nominal,
    mBoi_flow_nominal=mBoi_flow_nominal,
    dpBoi_nominal=dpBoi_nominal,
    delT_nominal(displayUnit="degC") = 5,
    perHWPum=perHWPum,
    tWai=100,
    dpSetPoi=dpSetPoi,
    energyDynamics=Modelica.Fluid.Types.Dynamics.FixedInitial)
    "The fourth generation disrict heating plant."
    annotation (Placement(transformation(extent={{-36,6},{-16,26}})));
  Modelica.Blocks.Sources.BooleanConstant on
    "Central heating plant on signal."
    annotation (Placement(transformation(extent={{-80,40},{-60,60}})));

  Modelica.Blocks.Sources.Pulse     dpMea(
    amplitude=-0.8*dpSetPoi,
    width=50,
    period=7200,
    offset=dpSetPoi)
                 "Measured pressure difference"
    annotation (Placement(transformation(extent={{-80,-38},{-60,-18}})));
  EnergyTransferStations.Heating.HeatingIndirect heaETS(
    show_T=true,
    redeclare package Medium = Medium,
    mDis_flow_nominal=0.8*mBoi_flow_nominal,
    mBui_flow_nominal=mBoi_flow_nominal,
    dpValve_nominal=6000,
    dp1_nominal=600,
    dp2_nominal=600,
    use_Q_flow_nominal=true,
    Q_flow_nominal=QBoi_flow_nominal,
    T_a1_nominal(displayUnit="degC") = 333.15,
    T_a2_nominal(displayUnit="degC") = 318.15,
    eta=0.8) annotation (Placement(transformation(extent={{32,58},{52,78}})));
  Fluid.Sources.MassFlowSource_T           souHea(
    redeclare package Medium = Medium,
    use_m_flow_in=false,
    m_flow=mBoi_flow_nominal,
    T=313.15,
    nPorts=1) annotation (Placement(transformation(extent={{96,-20},{76,0}})));
  Fluid.Sources.Boundary_pT sinHea(redeclare package Medium = Medium, nPorts=1)
    annotation (Placement(transformation(extent={{42,-40},{22,-20}})));
  Modelica.Blocks.Sources.RealExpression Tset(y=50 + 273.15)
    annotation (Placement(transformation(extent={{-46,62},{-26,82}})));
equation
  connect(on.y, heaPla.on) annotation (Line(points={{-59,50},{-46,50},{-46,24},{
          -38,24}},  color={255,0,255}));
  connect(dpMea.y, heaPla.dpMea) annotation (Line(points={{-59,-28},{-46,-28},{-46,
          13},{-38,13}},                     color={0,0,127}));
  connect(Tset.y, heaETS.TSetBuiSup) annotation (Line(points={{-25,72},{2,72},{2,
          68},{30,68}}, color={0,0,127}));
  connect(heaPla.port_b, heaETS.port_a1) annotation (Line(points={{-16,11},{8,11},
          {8,74},{32,74}}, color={0,127,255}));
  connect(heaETS.port_b1, heaPla.port_a) annotation (Line(points={{52,74},{52,
          88},{6,88},{6,20},{-16,20},{-16,21}},
                              color={0,127,255}));
  connect(heaETS.port_a2, souHea.ports[1]) annotation (Line(points={{52,62},{64,
          62},{64,-10},{76,-10}}, color={0,127,255}));
  connect(heaETS.port_b2, sinHea.ports[1]) annotation (Line(points={{32,62},{12,
          62},{12,-30},{22,-30}}, color={0,127,255}));
  connect(on.y, heaETS.reSet) annotation (Line(points={{-59,50},{-46,50},{-46,
          64.8},{30,64.8}}, color={255,0,255}));
  annotation (Icon(coordinateSystem(preserveAspectRatio=false,grid={2,2})),
       Diagram(coordinateSystem(preserveAspectRatio=false, grid={2,2})),
     experiment(
      StopTime=36000,
      Tolerance=1e-06,
      __Dymola_Algorithm="Dassl"),
      __Dymola_Commands(file="modelica://Buildings/Resources/Scripts/Dymola/Applications/DHC/CentralPlants/Heating/Generation4/Validation/MappingBoilerPLRToMassFlow.mos"
          "Simulate and plot"));
end CentralPlantHeating4GWOdisNet;
