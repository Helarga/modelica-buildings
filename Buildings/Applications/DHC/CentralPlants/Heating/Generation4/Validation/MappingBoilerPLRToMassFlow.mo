within Buildings.Applications.DHC.CentralPlants.Heating.Generation4.Validation;
model MappingBoilerPLRToMassFlow
  "The example tests mapping the boiler capacity to the heating water mass flow rate."
  extends Modelica.Icons.Example;

  package Medium = Buildings.Media.Water;

  parameter Modelica.SIunits.HeatFlowRate Q_flow_nominal=20000;
  parameter Integer numBoi= 2;

  BoilerParallel boi(
    redeclare package Medium = Medium,
    m_flow_nominal=Q_flow_nominal/(4200*5),
    show_T=true,
    dp_nominal(displayUnit="Pa") = 500,
    num=numBoi,
    l=0.001,
    Q_flow_nominal=Q_flow_nominal)
    annotation (Placement(transformation(extent={{14,-10},{34,10}})));
  Controls.HeatingWaterPumpSpeed heaWatPumCon(
    numPum=numBoi,
    dpSetPoi=5000,
    tWai=10,
    m_flow_nominal=boi.Q_flow_nominal/(4200*5)) "Heating water pumps control."
    annotation (Placement(transformation(extent={{-12,36},{8,56}})));
  Fluid.Sources.MassFlowSource_T boundary(
    redeclare package Medium = Medium,
    use_m_flow_in=true,
    m_flow=1.5*boi.Q_flow_nominal/(4200*5),
    T=308.15,
    nPorts=1)
    annotation (Placement(transformation(extent={{-64,-10},{-44,10}})));

  Fluid.Sources.Boundary_pT boundary1(
    redeclare package Medium = Medium,
    nPorts=1)
    annotation (Placement(transformation(extent={{98,-10},{78,10}})));
  Modelica.Blocks.Sources.RealExpression realExpression(y=7000)
    annotation (Placement(transformation(extent={{-60,32},{-40,52}})));
  Fluid.Sensors.MassFlowRate senMasFlo(redeclare package Medium = Medium)
    "Chilled water return mass flow"
    annotation (Placement(transformation(extent={{-34,-10},{-14,10}})));

  Modelica.Blocks.Tables.CombiTable1Ds combiTable1Ds(table=[0,0,0; 1,1,0; 2,1,1])
    "Determine which pump should be on - rotation control is not considered here"
    annotation (Placement(transformation(extent={{38,58},{58,78}})));
  Modelica.Blocks.Math.Product pro1[numBoi] "Product" annotation (Placement(
        transformation(
        extent={{-10,10},{10,-10}},
        rotation=-90,
        origin={38,30})));
  Modelica.Blocks.Sources.Pulse          OnOff_y(
    amplitude=1,
    width=50,
    period=300,
    offset=1) "On off signal for parallel boilers."
    annotation (Placement(transformation(extent={{-18,70},{2,90}})));
  Modelica.Blocks.Sources.Pulse mHea(amplitude=0.5*Q_flow_nominal/(4200*5),
    width=50,
    period=300,
    offset=Q_flow_nominal/(4200*5)) "Heating water mass flow rate."
    annotation (Placement(transformation(extent={{-98,14},{-78,34}})));
equation

  connect(boi.port_b, boundary1.ports[1])
    annotation (Line(points={{34,0},{78,0}}, color={0,127,255}));
  connect(heaWatPumCon.dpMea, realExpression.y)
    annotation (Line(points={{-14,42},{-39,42}}, color={0,0,127}));
  connect(boundary.ports[1], senMasFlo.port_a)
    annotation (Line(points={{-44,0},{-34,0}}, color={0,127,255}));
  connect(boi.port_a, senMasFlo.port_b)
    annotation (Line(points={{14,0},{-14,0}},  color={0,127,255}));
  connect(senMasFlo.m_flow, heaWatPumCon.masFloPum) annotation (Line(points={{-24,
          11},{-24,52.6},{-14,52.6}}, color={0,0,127}));
 for i in 1: numBoi loop
 end for;

  connect(combiTable1Ds.u, OnOff_y.y)
    annotation (Line(points={{36,68},{28,68},{28,80},{3,80}},color={0,0,127}));
  connect(combiTable1Ds.y, pro1.u2) annotation (Line(points={{59,68},{64,68},{64,
          52},{44,52},{44,42}},    color={0,0,127}));
  connect(pro1.y, boi.PLR) annotation (Line(points={{38,19},{38,16},{2,16},{2,4},
          {13,4}},         color={0,0,127}));
  connect(heaWatPumCon.PLR, pro1[1].u1)
    annotation (Line(points={{9,52},{32,52},{32,42}}, color={0,0,127}));
  connect(heaWatPumCon.PLR, pro1[2].u1)
    annotation (Line(points={{9,52},{32,52},{32,42}}, color={0,0,127}));
  connect(mHea.y, boundary.m_flow_in) annotation (Line(points={{-77,24},{-72,24},
          {-72,8},{-66,8}}, color={0,0,127}));
  annotation (Icon(coordinateSystem(preserveAspectRatio=false,grid={4,4})),
       Diagram(coordinateSystem(preserveAspectRatio=false, grid={4,4})),
     experiment(
        StopTime=3600,
        Tolerance=1e-06),
      __Dymola_Commands(file="modelica://Buildings/Resources/Scripts/Dymola/Applications/DHC/CentralPlants/Heating/Generation4/Validation/MappingBoilerPLRToMassFlow.mos"
          "Simulate and plot"));
end MappingBoilerPLRToMassFlow;
