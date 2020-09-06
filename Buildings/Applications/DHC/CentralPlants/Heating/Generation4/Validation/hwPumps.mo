within Buildings.Applications.DHC.CentralPlants.Heating.Generation4.Validation;
model hwPumps
    extends Modelica.Icons.Example;

   package Medium = Buildings.Media.Water;

 parameter Buildings.Fluid.Movers.Data.Generic perHWPum(
    pressure=Buildings.Fluid.Movers.BaseClasses.Characteristics.flowParameters(
      V_flow=0.5
               /1000*{0,1},
      dp=(300000)*{1,0}))
    "Performance data for chilled water pumps";

  Buildings.Applications.DataCenters.ChillerCooled.Equipment.FlowMachine_y flowMachine_y(
    redeclare package Medium = Medium,
    show_T=true,
     per=fill(perHWPum, 2),
    m_flow_nominal=0.5,
    dpValve_nominal=7000,
    num=2,
    riseTimeValve=50,
    energyDynamics=Modelica.Fluid.Types.Dynamics.FixedInitial)
           annotation (Placement(transformation(extent={{-38,32},{-18,52}})));
  Modelica.Blocks.Sources.RealExpression pum1(y=1)
    "Chilled water bypass valve mass flow rate"
    annotation (Placement(transformation(extent={{-82,54},{-102,74}})));
  Modelica.Blocks.Sources.RealExpression pum2(y=1)
    "Chilled water bypass valve mass flow rate"
    annotation (Placement(transformation(extent={{-78,-4},{-98,16}})));
  EnergyTransferStations.Heating.HeatingIndirect heaETS(
    redeclare package Medium = Medium,
    mDis_flow_nominal=2,
    mBui_flow_nominal=2*0.8,
    dpValve_nominal=6000,
    dp1_nominal=600,
    dp2_nominal=600,
    use_Q_flow_nominal=true,
    Q_flow_nominal=2*4200*5,
    T_a1_nominal(displayUnit="degC") = 328.15,
    T_a2_nominal(displayUnit="degC") = 313.15,
    eta=0.8,
    initType=Modelica.Blocks.Types.InitPID.InitialOutput,
    xi_start=1)
             annotation (Placement(transformation(extent={{52,6},{72,26}})));
  Fluid.Sources.MassFlowSource_T           souHea(
    redeclare package Medium = Medium,
    use_m_flow_in=false,
    m_flow=0.79,
    T=308.15,
    nPorts=1) annotation (Placement(transformation(extent={{114,-22},{94,-2}})));
  Fluid.Sources.Boundary_pT                souHea1(
    redeclare package Medium = Medium,
    nPorts=1) annotation (Placement(transformation(extent={{66,-40},{46,-20}})));
  Modelica.Blocks.Sources.RealExpression Tset(y=45 + 273.15)
    annotation (Placement(transformation(extent={{-6,66},{14,86}})));
  Fluid.Sources.Boundary_pT preSou(redeclare package Medium = Medium, nPorts=1)
    annotation (Placement(transformation(extent={{0,-30},{-20,-10}})));
  Fluid.Boilers.BoilerPolynomial boi(
    redeclare package Medium = Medium,
    m_flow_nominal=0.79,
    dp_nominal(displayUnit="bar") = 50000,
    Q_flow_nominal=50000,
    fue=Fluid.Data.Fuels.NaturalGasLowerHeatingValue())
    annotation (Placement(transformation(extent={{2,4},{22,24}})));
equation
  connect(pum1.y, flowMachine_y.u[1]) annotation (Line(points={{-103,64},{-120,64},
          {-120,45},{-40,45}}, color={0,0,127}));
  connect(pum2.y, flowMachine_y.u[2]) annotation (Line(points={{-99,6},{-120,6},
          {-120,47},{-40,47}}, color={0,0,127}));
  connect(heaETS.port_a2,souHea. ports[1]) annotation (Line(points={{72,10},{84,
          10},{84,-12},{94,-12}}, color={0,127,255}));
  connect(heaETS.port_b2,souHea1. ports[1]) annotation (Line(points={{52,10},{
          36,10},{36,-30},{46,-30}},
                                 color={0,127,255}));
  connect(Tset.y,heaETS. TSetBuiSup) annotation (Line(points={{15,76},{38,76},{
          38,16},{50,16}},
                        color={0,0,127}));
  connect(pum2.y, boi.y) annotation (Line(points={{-99,6},{-106,6},{-106,22},{0,
          22}},  color={0,0,127}));
  connect(flowMachine_y.port_b, boi.port_a) annotation (Line(points={{-18,42},{
          -14,42},{-14,14},{2,14}}, color={0,127,255}));
  connect(boi.port_b, heaETS.port_a1) annotation (Line(points={{22,14},{28,14},
          {28,22},{52,22}}, color={0,127,255}));
  connect(heaETS.port_b1, flowMachine_y.port_a) annotation (Line(points={{72,22},
          {92,22},{92,62},{-74,62},{-74,42},{-38,42}}, color={0,127,255}));
  connect(flowMachine_y.port_a, preSou.ports[1]) annotation (Line(points={{-38,
          42},{-38,-20},{-20,-20}}, color={0,127,255}));
  annotation (Icon(coordinateSystem(preserveAspectRatio=false)), Diagram(
        coordinateSystem(preserveAspectRatio=false)),
    experiment(StopTime=3600, __Dymola_Algorithm="Dassl"));
end hwPumps;
