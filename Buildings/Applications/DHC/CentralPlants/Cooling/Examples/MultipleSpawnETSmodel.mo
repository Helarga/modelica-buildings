within Buildings.Applications.DHC.CentralPlants.Cooling.Examples;
model MultipleSpawnETSmodel
 extends Modelica.Icons.Example;

  package MediumW = Buildings.Media.Water "Medium model for water";

  parameter Integer nBui=3
    "Number of buildings";

  parameter Modelica.SIunits.MassFlowRate mDisTot_flow_nominal=sum(bui.bui.disFloCoo.m_flow_nominal .* (bui.bui.delTBuiCoo/delTDisCoo))
    "Nominal mass flow rate of primary (district) district cooling side";

  parameter Modelica.SIunits.TemperatureDifference delTDisCoo=8
    "Nominal building supply and return water temperature difference";

  Buildings.Applications.DHC.Examples.FifthGeneration.Unidirectional.Loads.BuildingSpawnZ6WithCoolingIndirectETS bui[nBui]
    annotation (Placement(transformation(extent={{-10,-10},{10,10}})));

   // m_flow_nominal=100)

  Buildings.Controls.OBC.CDL.Continuous.Sources.Constant TSetChiWatSup[3](
    k=bui.TChiWatSup_nominal)
    "Chilled water supply temperature set point"
    annotation (Placement(transformation(extent={{-60,-10},{-40,10}})));
  Buildings.Fluid.Sources.MassFlowSource_T boundary(
    redeclare package Medium = MediumW,
    m_flow=mDisTot_flow_nominal,
    T=278.15,
    nPorts=3) annotation (Placement(transformation(extent={{60,-48},{40,-28}})));
  Modelica.Fluid.Sources.Boundary_pT  boundary3(
    redeclare package Medium = MediumW, nPorts=3)
    annotation (Placement(transformation(extent={{-62,-48},{-42,-28}})));
  Buildings.Fluid.Sources.Boundary_pT boundary1(
    redeclare package Medium = MediumW,
    T=313.15,
    nPorts=3) annotation (Placement(transformation(extent={{-60,30},{-40,50}})));
  Buildings.Fluid.Sources.Boundary_pT boundary2(
    redeclare package Medium = MediumW, nPorts=3)
    annotation (Placement(transformation(extent={{60,30},{40,50}})));
equation
  connect(TSetChiWatSup.y, bui.TSetChiWat) annotation (Line(points={{-38,0},{-28,
          0},{-28,4},{-11,4}},    color={0,0,127}));
  connect(bui.port_a1, boundary1.ports[1:3]) annotation (Line(points={{-10,6},{
          -20,6},{-20,37.3333},{-40,37.3333}},  color={0,127,255}));
  connect(bui.port_b1, boundary2.ports[1:3]) annotation (Line(points={{10,6},{
          20,6},{20,37.3333},{40,37.3333}},
                                         color={0,127,255}));
  connect(boundary.ports[1:3], bui.port_a2) annotation (Line(points={{40,
          -40.6667},{20,-40.6667},{20,-6},{10,-6}},
                                          color={0,127,255}));
  connect(bui.port_b2, boundary3.ports[1:3]) annotation (Line(points={{-10,-6},
          {-20,-6},{-20,-40.6667},{-42,-40.6667}},color={0,127,255}));
  annotation (
      experiment(
      StopTime=100,
      __Dymola_NumberOfIntervals=50,
      Tolerance=1e-06,
      __Dymola_Algorithm="Cvode"));
end MultipleSpawnETSmodel;
