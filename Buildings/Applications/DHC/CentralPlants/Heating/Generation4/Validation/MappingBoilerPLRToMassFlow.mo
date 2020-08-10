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
    annotation (Placement(transformation(extent={{-10,-10},{10,10}})));
  Controls.HeatingWaterPumpSpeed heatingWaterPumpSpeed(
    numPum=numBoi,
    dpSetPoi=5000,
    tWai=10,
    m_flow_nominal=boi.Q_flow_nominal/(4200*5))
    annotation (Placement(transformation(extent={{-36,36},{-16,56}})));
  Fluid.Sources.MassFlowSource_T boundary(
    redeclare package Medium = Medium,
    m_flow=1.5*boi.Q_flow_nominal/(4200*5),
    T=309.15,
    nPorts=1)
    annotation (Placement(transformation(extent={{-88,-10},{-68,10}})));

  Fluid.Sources.Boundary_pT boundary1(
    redeclare package Medium = Medium,
    nPorts=1)
    annotation (Placement(transformation(extent={{74,-10},{54,10}})));
  Modelica.Blocks.Sources.RealExpression realExpression(y=7000)
    annotation (Placement(transformation(extent={{-84,32},{-64,52}})));
  Fluid.Sensors.MassFlowRate senMasFlo(redeclare package Medium = Medium)
    "Chilled water return mass flow"
    annotation (Placement(transformation(extent={{-58,-10},{-38,10}})));

  Modelica.Blocks.Tables.CombiTable1Ds combiTable1Ds(table=[0,0,0; 1,1,0; 2,1,1])
    "Determine which pump should be on - rotation control is not considered here"
    annotation (Placement(transformation(extent={{14,58},{34,78}})));
  Modelica.Blocks.Math.Product pro1[numBoi] "Product" annotation (Placement(
        transformation(
        extent={{-10,10},{10,-10}},
        rotation=-90,
        origin={14,30})));
  Modelica.Blocks.Sources.RealExpression OnOff_y(y=2)
    "On off signal for parallel boilers."
    annotation (Placement(transformation(extent={{-26,62},{-6,82}})));
equation

  connect(boi.port_b, boundary1.ports[1])
    annotation (Line(points={{10,0},{54,0}}, color={0,127,255}));
  connect(heatingWaterPumpSpeed.dpMea, realExpression.y)
    annotation (Line(points={{-38,42},{-63,42}}, color={0,0,127}));
  connect(boundary.ports[1], senMasFlo.port_a)
    annotation (Line(points={{-68,0},{-58,0}}, color={0,127,255}));
  connect(boi.port_a, senMasFlo.port_b)
    annotation (Line(points={{-10,0},{-38,0}}, color={0,127,255}));
  connect(senMasFlo.m_flow, heatingWaterPumpSpeed.masFloPum)
    annotation (Line(points={{-48,11},{-48,52.6},{-38,52.6}},
                                                          color={0,0,127}));
 for i in 1: numBoi loop
 end for;

  connect(combiTable1Ds.u, OnOff_y.y)
    annotation (Line(points={{12,68},{4,68},{4,72},{-5,72}}, color={0,0,127}));
  connect(combiTable1Ds.y, pro1.u2) annotation (Line(points={{35,68},{40,68},{
          40,52},{20,52},{20,42}}, color={0,0,127}));
  connect(pro1.y, boi.PLR) annotation (Line(points={{14,19},{14,16},{-22,16},{
          -22,4},{-11,4}}, color={0,0,127}));
  connect(heatingWaterPumpSpeed.PLR, pro1[1].u1)
    annotation (Line(points={{-15,52},{8,52},{8,42}},     color={0,0,127}));
  connect(heatingWaterPumpSpeed.PLR, pro1[2].u1)
    annotation (Line(points={{-15,52},{8,52},{8,42}},     color={0,0,127}));
  annotation (Icon(coordinateSystem(preserveAspectRatio=false, grid={4,4})),
                                                                 Diagram(
        coordinateSystem(preserveAspectRatio=false, grid={4,4})));
end MappingBoilerPLRToMassFlow;
