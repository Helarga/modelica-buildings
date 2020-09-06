within Buildings.Applications.DHC.CentralPlants.Heating.Generation4.Validation;
model SpawnETSHeating "Indirect heating ETS connected to Spawn buildings."
    extends Modelica.Icons.Example;

   package Medium = Buildings.Media.Water;

 parameter Buildings.Fluid.Movers.Data.Generic perHWPum(
    pressure=Buildings.Fluid.Movers.BaseClasses.Characteristics.flowParameters(
      V_flow=0.5
               /1000*{0,1},
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
  Fluid.Movers.FlowControlled_m_flow buiHea(
    redeclare package Medium = Medium,
    energyDynamics=Modelica.Fluid.Types.Dynamics.FixedInitial,
    m_flow_nominal=buiETS.bui.disFloHea.m_flow_nominal,
    dp_nominal(displayUnit="bar") = 300000)
    annotation (Placement(transformation(extent={{-70,26},{-50,46}})));
  Buildings.Applications.DHC.Examples.FifthGeneration.Unidirectional.Loads.BuildingSpawnZ6WithHeatingIndirectETS
    buiETS(
    TChiWatSup_nominal=278.15,
    THeaWatSup_nominal=328.15,
    THeaWatRet_nominal=313.15)
    annotation (Placement(transformation(extent={{-10,30},{10,50}})));
  Modelica.Blocks.Sources.RealExpression mReq(y=buiETS.bui.disFloHea.mReqTot_flow)
    annotation (Placement(transformation(extent={{-84,52},{-64,72}})));
  Fluid.Sources.Boundary_pT preSou(redeclare package Medium = Medium,
    T=328.15,                                                         nPorts=1)
    annotation (Placement(transformation(extent={{-100,-12},{-80,8}})));
equation
  connect(Tset.y, buiETS.TSetWat) annotation (Line(points={{-37,80},{-30,80},{
          -30,43},{-11,43}}, color={0,0,127}));
  connect(buiCoo.ports[1], buiETS.port_a2) annotation (Line(points={{40,10},{30,
          10},{30,34},{10,34}}, color={0,127,255}));
  connect(buiETS.port_b2, sinCoo.ports[1]) annotation (Line(points={{-10,34},{
          -26,34},{-26,-40},{-50,-40}}, color={0,127,255}));
  connect(buiHea.port_a, buiETS.port_b1) annotation (Line(points={{-70,36},{-92,
          36},{-92,96},{48,96},{48,46},{10,46}}, color={0,127,255}));
  connect(buiHea.port_b, buiETS.port_a1) annotation (Line(points={{-50,36},{-30,
          36},{-30,46},{-10,46}}, color={0,127,255}));
  connect(mReq.y, buiHea.m_flow_in)
    annotation (Line(points={{-63,62},{-60,62},{-60,48}}, color={0,0,127}));
  connect(buiHea.port_a, preSou.ports[1]) annotation (Line(points={{-70,36},{
          -76,36},{-76,-2},{-80,-2}}, color={0,127,255}));
  annotation (Icon(coordinateSystem(preserveAspectRatio=false)), Diagram(
        coordinateSystem(preserveAspectRatio=false)),
    experiment(StopTime=3600, __Dymola_Algorithm="Dassl"));
end SpawnETSHeating;
