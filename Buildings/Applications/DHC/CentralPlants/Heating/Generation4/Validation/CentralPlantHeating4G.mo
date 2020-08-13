within Buildings.Applications.DHC.CentralPlants.Heating.Generation4.Validation;
model CentralPlantHeating4G
  "The vaildation of the fourth generation central heating plant."
  extends Modelica.Icons.Example;

  package Medium = Buildings.Media.Water;

  parameter Modelica.SIunits.HeatFlowRate Q_flow_nominal=20000;
  parameter Integer numBoi= 2;

  Fluid.Sources.MassFlowSource_T boundary(
    redeclare package Medium = Medium,
    use_m_flow_in=true,
    m_flow=1.5*boi.Q_flow_nominal/(4200*5),
    T=308.15,
    nPorts=1)
    annotation (Placement(transformation(extent={{40,40},{20,60}})));

  Fluid.Sources.Boundary_pT boundary1(
    redeclare package Medium = Medium, nPorts=1)
    annotation (Placement(transformation(extent={{40,-40},{20,-20}})));

  Modelica.Blocks.Sources.Trapezoid mHea(
    amplitude=1.5*Q_flow_nominal/(4200*5),
    rising=100,
    width=100,
    falling=100,
    period=400,
    offset=0.5*Q_flow_nominal/(4200*5)) "Heating water mass flow rate."
    annotation (Placement(transformation(extent={{80,40},{60,60}})));
  PlantHeating heaPla(
    show_T=true,
    mHW_flow_nominal=Q_flow_nominal/(4200*5),
    dpHW_nominal(displayUnit="bar") = 80000,
    QBoi_flow_nominal=1000,
    mMin_flow=0.1*mHW_flow_nominal,
    dp_nominalBoi=70000,
    tWai=100,
    dpSetPoi=7000,
    energyDynamics=Modelica.Fluid.Types.Dynamics.FixedInitial)
    "The fourth generation district heating plant."
    annotation (Placement(transformation(extent={{-20,0},{0,20}})));
  Modelica.Blocks.Sources.BooleanConstant on "Central heating plant on signal."
    annotation (Placement(transformation(extent={{-80,40},{-60,60}})));
  Modelica.Blocks.Sources.RealExpression dpMea(y=7000)
    "Measured pressure drop."
    annotation (Placement(transformation(extent={{-80,-40},{-60,-20}})));
equation

 for i in 1: numBoi loop
 end for;

  connect(mHea.y, boundary.m_flow_in) annotation (Line(points={{59,50},{53,50},{
          53,58},{42,58}},  color={0,0,127}));
  connect(on.y, heaPla.on) annotation (Line(points={{-59,50},{-40,50},{-40,18},
          {-22,18}}, color={255,0,255}));
  connect(boundary.ports[1], heaPla.port_a) annotation (Line(points={{20,50},{
          20,52},{12,52},{12,15},{0,15}}, color={0,127,255}));
  connect(heaPla.port_b, boundary1.ports[1]) annotation (Line(points={{0,5},{12,
          5},{12,-30},{20,-30}}, color={0,127,255}));
  connect(dpMea.y, heaPla.dpMea) annotation (Line(points={{-59,-30},{-40,-30},{
          -40,7},{-22,7}}, color={0,0,127}));
  annotation (Icon(coordinateSystem(preserveAspectRatio=false,grid={2,2})),
       Diagram(coordinateSystem(preserveAspectRatio=false, grid={2,2})),
     experiment(
        StopTime=3600,
        Tolerance=1e-06),
      __Dymola_Commands(file="modelica://Buildings/Resources/Scripts/Dymola/Applications/DHC/CentralPlants/Heating/Generation4/Validation/MappingBoilerPLRToMassFlow.mos"
          "Simulate and plot"));
end CentralPlantHeating4G;
