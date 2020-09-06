within Buildings.Applications.DHC.CentralPlants.Heating.Generation4.Validation;
model CentralPlantHeating4GSec
  "The vaildation of the fourth generation central heating plant."
  extends Modelica.Icons.Example;

  package Medium = Buildings.Media.Water;

  parameter Modelica.SIunits.HeatFlowRate Q_flow_nominal=50000
    "Heating load";
  parameter Modelica.SIunits.HeatFlowRate QBoi_flow_nominal= Q_flow_nominal/numBoi
    "Boiler heat flow rate";
  parameter Modelica.SIunits.MassFlowRate mHW_flow_nominal= mBoi_flow_nominal*numBoi
    "Heating water flow rate";
  parameter Modelica.SIunits.MassFlowRate mBoi_flow_nominal= QBoi_flow_nominal/(4200*delTDis)
    "Heating water flow rate";
  parameter Modelica.SIunits.TemperatureDifference delTDis=5
    "Nominal heating water temperature difference (district side)";
  parameter Integer numBoi= 2
    "Number of boilers";
  parameter Modelica.SIunits.PressureDifference dpHW_nominal=(dpBoi_nominal+dpSetPoi)*3
    "Heating circuit ressure drop including 50% as a factor of safey";
  parameter Modelica.SIunits.PressureDifference dpBoi_nominal=50000
    "Boiler pressure drop";
  parameter Modelica.SIunits.PressureDifference dpSetPoi=7000
    "Setpoint pressure drop";
  redeclare parameter Buildings.Fluid.Movers.Data.Generic perHWPum(
    pressure=Buildings.Fluid.Movers.BaseClasses.Characteristics.flowParameters(
      V_flow=mBoi_flow_nominal*10/1000*{0.1,1},
      dp=(dpHW_nominal)*{1,0.1}))
    "Performance data for chilled water pumps";

  PlantHeating heaPla(
    numBoi=2,
    show_T=true,
    mHW_flow_nominal=mHW_flow_nominal,
    dpHW_nominal = dpHW_nominal,
    QBoi_flow_nominal=QBoi_flow_nominal,
    mMin_flow=0.1*mBoi_flow_nominal,
    mBoi_flow_nominal=mBoi_flow_nominal,
    dpBoi_nominal=dpBoi_nominal,
    delT_nominal(displayUnit="degC") = 15,
    perHWPum=perHWPum,
    tWai=100,
    dpSetPoi=7000,
    energyDynamics=Modelica.Fluid.Types.Dynamics.FixedInitial)
    "The fourth generation disrict heating plant."
    annotation (Placement(transformation(extent={{-36,6},{-16,26}})));
  Modelica.Blocks.Sources.BooleanConstant on
    "Central heating plant on signal."
    annotation (Placement(transformation(extent={{-86,46},{-66,66}})));

  Modelica.Blocks.Sources.RealExpression
                                    dpMea(y=3000)
                 "Measured pressure difference"
    annotation (Placement(transformation(extent={{-86,-34},{-66,-14}})));
  Fluid.Sources.Boundary_pT           watSin(redeclare package Medium = Medium,
      nPorts=1) "Water sink"
    annotation (Placement(transformation(extent={{58,-30},{38,-10}})));
  Fluid.Sources.Boundary_pT                watSou(
    redeclare package Medium = Medium,
    T=318.15,
    nPorts=1) "Water source"
    annotation (Placement(transformation(extent={{56,10},{36,30}})));
  Fluid.FixedResistances.PressureDrop           res(
    dp_nominal=6000,
    redeclare package Medium = Medium,
    m_flow_nominal=mBoi_flow_nominal*2)
    "Flow resistance"
    annotation (Placement(transformation(extent={{8,-30},{28,-10}})));
  Modelica.Blocks.Sources.Trapezoid dpMea1(
    amplitude=0.5*dpSetPoi,
    rising=300,
    width=300,
    falling=300,
    period=1200,
    offset=0.5*dpSetPoi,
    startTime=0) "Measured pressure difference"
    annotation (Placement(transformation(extent={{-80,-72},{-60,-52}})));
  Fluid.FixedResistances.PressureDrop           res1(
    dp_nominal=6000,
    redeclare package Medium = Medium,
    m_flow_nominal=mBoi_flow_nominal*2)
    "Flow resistance"
    annotation (Placement(transformation(extent={{20,10},{0,30}})));
equation
  connect(on.y, heaPla.on) annotation (Line(points={{-65,56},{-46,56},{-46,24},{
          -38,24}},  color={255,0,255}));
  connect(res.port_b,watSin. ports[1]) annotation (Line(points={{28,-20},{38,
          -20}},                                                                    color={0,127,255}));
  connect(res.port_a, heaPla.port_b) annotation (Line(points={{8,-20},{-4,-20},
          {-4,11},{-16,11}}, color={0,127,255}));
  connect(dpMea.y, heaPla.dpMea) annotation (Line(points={{-65,-24},{-46,-24},{
          -46,13},{-38,13}}, color={0,0,127}));
  connect(watSou.ports[1], res1.port_a)
    annotation (Line(points={{36,20},{20,20}}, color={0,127,255}));
  connect(heaPla.port_a, res1.port_b) annotation (Line(points={{-16,21},{-8,21},
          {-8,20},{0,20}}, color={0,127,255}));
  annotation (Icon(coordinateSystem(preserveAspectRatio=false,grid={2,2})),
       Diagram(coordinateSystem(preserveAspectRatio=false, grid={2,2})),
     experiment(
      StopTime=86400,
      Tolerance=1e-06,
      __Dymola_Algorithm="Dassl"),
      __Dymola_Commands(file="modelica://Buildings/Resources/Scripts/Dymola/Applications/DHC/CentralPlants/Heating/Generation4/Validation/MappingBoilerPLRToMassFlow.mos"
          "Simulate and plot"));
end CentralPlantHeating4GSec;
