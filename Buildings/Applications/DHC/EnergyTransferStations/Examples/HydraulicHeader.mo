within Buildings.Applications.DHC.EnergyTransferStations.Examples;
model HydraulicHeader "Example of the hydraulic header implementation"
   package Medium = Buildings.Media.Water "Medium model";

  BaseClasses.HydraulicHeader HeaSupHed(
    redeclare package Medium = Medium,
    m_flow_nominal=1,
    show_T=true,
    allowFlowReversal=true,
    nPorts_a=2,
    nPorts_b=2)
    annotation (Placement(transformation(extent={{-6,-10},{14,10}})));
  Modelica.Blocks.Sources.Constant m_flowPumVal(k=1)
    annotation (Placement(transformation(extent={{94,-60},{74,-40}})));
  Modelica.Blocks.Sources.Pulse pulse(
    amplitude=1,
    width=50,
    period=100,
    nperiod=-1)
     annotation (Placement(transformation(extent={{-80,60},{-60,80}})));
  Fluid.Sources.MassFlowSource_T conPum(
    redeclare package Medium = Medium,
    use_m_flow_in=true,
    T=353.15,
    nPorts=1) "Condenser water Pump"
    annotation (Placement(transformation(
        extent={{10,-10},{-10,10}},
        rotation=180,
        origin={-30,70})));
  Fluid.Sources.MassFlowSource_T solPum(
    redeclare package Medium = Medium,
    use_m_flow_in=true,
    T=338.15,
    nPorts=1)
    "Solar water Pump"
     annotation (Placement(transformation(
        extent={{-10,10},{10,-10}},
        rotation=180,
        origin={50,-50})));
  Modelica.Fluid.Sources.FixedBoundary hotTan(
    redeclare package Medium = Medium,
    T=283.15,
    nPorts=1)
     "hot buffer tank"
     annotation (Placement(transformation(extent={{98,46},{78,26}})));
  Fluid.Sources.MassFlowSource_T borPum(
    redeclare package Medium = Medium,
    use_m_flow_in=true,
    T=363.15,
    nPorts=1) "Condenser water Pump" annotation (Placement(transformation(
        extent={{-10,-10},{10,10}},
        rotation=180,
        origin={-50,-50})));
  Fluid.Sensors.MassFlowRate senHotTan(redeclare package Medium = Medium)
    annotation (Placement(transformation(extent={{40,26},{20,46}})));
  Modelica.Blocks.Sources.Pulse pulse1(
    amplitude=-2,
    width=50,
    period=100,
    nperiod=-1)
     annotation (Placement(transformation(extent={{10,-10},{-10,10}},
        rotation=0,
        origin={-10,-50})));
  Fluid.Sensors.MassFlowRate senCon(redeclare package Medium = Medium)
    annotation (Placement(transformation(
        extent={{10,-10},{-10,10}},
        rotation=90,
        origin={-8,48})));
equation
  connect(pulse.y, conPum.m_flow_in)
    annotation (Line(points={{-59,70},{-58,70},{-58,62},{-42,62}},
                                                 color={0,0,127}));
  connect(solPum.m_flow_in, m_flowPumVal.y)
    annotation (Line(points={{62,-42},{68,-42},{68,-50},{73,-50}},
                                                 color={0,0,127}));
  connect(borPum.m_flow_in, pulse1.y)
    annotation (Line(points={{-38,-58},{-30,-58},{-30,-50},{-21,-50}},
                                                   color={0,0,127}));
  connect(conPum.ports[1], senCon.port_a)
    annotation (Line(points={{-20,70},{-8,70},{-8,58}}, color={0,127,255}));
  connect(solPum.ports[1], HeaSupHed.ports_b[1]) annotation (Line(points={{40,-50},
          {28,-50},{28,-2},{14,-2}},         color={0,127,255}));
  connect(HeaSupHed.ports_b[2], senHotTan.port_b) annotation (Line(points={{14,2},{
          14,36},{20,36}},          color={0,127,255}));
  connect(senCon.port_b, HeaSupHed.ports_a[1]) annotation (Line(points={{-8,38},
          {-8,2},{-6,2}},         color={0,127,255}));
  connect(HeaSupHed.ports_a[2], borPum.ports[1]) annotation (Line(points={{-6,-2},
          {-70,-2},{-70,-50},{-60,-50}}, color={0,127,255}));
  connect(senHotTan.port_a, hotTan.ports[1]) annotation (Line(points={{40,36},{60,
          36},{60,36},{78,36}}, color={0,127,255}));
  annotation (Icon(coordinateSystem(preserveAspectRatio=false),
     graphics={
        Ellipse(lineColor = {75,138,73},
                fillColor={255,255,255},
                fillPattern = FillPattern.Solid,
                extent={{-98,-100},{98,98}}),
        Polygon(lineColor = {0,0,255},
                fillColor = {75,138,73},
                pattern = LinePattern.None,
                fillPattern = FillPattern.Solid,
                points={{-30,64},{70,4},{-30,-56},{-30,64}})}),
                Diagram(coordinateSystem(preserveAspectRatio=false)),
    __Dymola_Commands(file="modelica://Buildings/Resources/Scripts/Dymola/Applications/DHC/EnergyTransferStations/Examples/HydraulicHeader.mos"
                           "Simulate and plot"),
                            experiment(Tolerance=1e-6, StopTime=500));
end HydraulicHeader;
