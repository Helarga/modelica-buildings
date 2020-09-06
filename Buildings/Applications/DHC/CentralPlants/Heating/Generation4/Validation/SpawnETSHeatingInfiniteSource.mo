within Buildings.Applications.DHC.CentralPlants.Heating.Generation4.Validation;
model SpawnETSHeatingInfiniteSource
  "Indirect heating ETS connected to Spawn buildings."
    extends Modelica.Icons.Example;

   package Medium = Buildings.Media.Water;

 parameter Buildings.Fluid.Movers.Data.Generic perHWPum(
    pressure=Buildings.Fluid.Movers.BaseClasses.Characteristics.flowParameters(
      V_flow=0.5 /1000*{0,1},
      dp=(300000)*{1,0}))
    "Performance data for chilled water pumps";

  Fluid.Sources.Boundary_pT buiCoo(
    redeclare package Medium = Medium,
    T=280.15,
    nPorts=1) annotation (Placement(transformation(extent={{60,0},{40,20}})));
  Fluid.Sources.Boundary_pT sinCoo(redeclare package Medium = Medium, nPorts=1)
    annotation (Placement(transformation(extent={{-70,-50},{-50,-30}})));
  Modelica.Blocks.Sources.RealExpression Tset(y=45 + 273.15)
    annotation (Placement(transformation(extent={{-58,70},{-38,90}})));
  Buildings.Applications.DHC.Examples.FifthGeneration.Unidirectional.Loads.BuildingSpawnZ6WithHeatingIndirectETS
    buiETS(
    TChiWatSup_nominal=278.15,
    THeaWatSup_nominal=328.15,
    THeaWatRet_nominal=313.15)
    annotation (Placement(transformation(extent={{-10,30},{10,50}})));
  Fluid.Sources.Boundary_pT heaSou(redeclare package Medium = Medium,
    p=330000,
    T=328.15,
    nPorts=1)
    annotation (Placement(transformation(extent={{-88,28},{-68,48}})));
  Fluid.Sources.Boundary_pT heaSin(
    redeclare package Medium = Medium,
    p=300000,
    T=313.15,
    nPorts=1) annotation (Placement(transformation(extent={{56,32},{36,52}})));
equation
  connect(Tset.y, buiETS.TSetWat) annotation (Line(points={{-37,80},{-30,80},{
          -30,43},{-11,43}}, color={0,0,127}));
  connect(buiCoo.ports[1], buiETS.port_a2) annotation (Line(points={{40,10},{30,
          10},{30,34},{10,34}}, color={0,127,255}));
  connect(buiETS.port_b2, sinCoo.ports[1]) annotation (Line(points={{-10,34},{
          -26,34},{-26,-40},{-50,-40}}, color={0,127,255}));
  connect(heaSou.ports[1], buiETS.port_a1) annotation (Line(points={{-68,38},{-50,
          38},{-50,46},{-10,46}}, color={0,127,255}));
  connect(heaSin.ports[1], buiETS.port_b1) annotation (Line(points={{36,42},{24,
          42},{24,46},{10,46}}, color={0,127,255}));
  annotation (Icon(coordinateSystem(preserveAspectRatio=false)), Diagram(
        coordinateSystem(preserveAspectRatio=false)),
    experiment(StopTime=3600, __Dymola_Algorithm="Dassl"));
end SpawnETSHeatingInfiniteSource;
