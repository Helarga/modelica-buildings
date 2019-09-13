within Buildings.Applications.DHC.EnergyTransferStations.Examples;
model HeatingSupplyHeader_PortsExample "heating supply header"
   package Medium = Buildings.Media.Water "Medium model";

  parameter Modelica.SIunits.MassFlowRate m_flow_nominal=1;

  EnergyTransferStation.BaseClasses.HeatingSupplyHeader HeaSupHed(
    redeclare package Medium = Medium,
    m_flow_nominal=m_flow_nominal,
    nPorts_a=2,
    nPorts_b=2)
    annotation (Placement(transformation(extent={{-6,-10},{14,10}})));
  Modelica.Blocks.Sources.Constant m_flowPumVal(k=2)
    annotation (Placement(transformation(extent={{96,-54},{76,-34}})));
  Modelica.Blocks.Sources.Pulse    pulse(
    amplitude=1,
    width=50,
    period=100,
    nperiod=-1)
     annotation (Placement(transformation(extent={{-90,56},{-70,76}})));
  Fluid.Sources.MassFlowSource_T conPum(
    redeclare package Medium = Medium,
    use_m_flow_in=true,
    T=313.15,
    nPorts=1) "Condenser water Pump"
    annotation (Placement(transformation(
        extent={{10,-10},{-10,10}},
        rotation=180,
        origin={-36,74})));
  Fluid.Sources.MassFlowSource_T solPum(
    redeclare package Medium = Medium,
    use_m_flow_in=true,
    T=338.15,
    nPorts=1)
    "Solar water Pump"
     annotation (Placement(transformation(
        extent={{-10,10},{10,-10}},
        rotation=180,
        origin={50,-52})));
  Modelica.Fluid.Sources.FixedBoundary hotTan(
    redeclare package Medium = Medium,
    T=338.15,
    nPorts=1)
     "hot buffer tank"
     annotation (Placement(transformation(extent={{98,46},{78,26}})));
  Fluid.Sources.MassFlowSource_T borPum(
    redeclare package Medium = Medium,
    use_m_flow_in=true,
    T=313.15,
    nPorts=1) "Condenser water Pump" annotation (Placement(transformation(
        extent={{-10,-10},{10,10}},
        rotation=180,
        origin={-52,-30})));
  Fluid.Sensors.TemperatureTwoPort senTem1(redeclare package Medium = Medium,
      m_flow_nominal=1)
    annotation (Placement(transformation(extent={{-66,-8},{-46,12}})));
  Fluid.Sensors.TemperatureTwoPort senTem2(redeclare package Medium = Medium,
      m_flow_nominal=1)
    annotation (Placement(transformation(extent={{64,26},{44,46}})));
  Fluid.Sensors.MassFlowRate senHotTan(redeclare package Medium = Medium)
    annotation (Placement(transformation(extent={{40,26},{20,46}})));
  Modelica.Blocks.Sources.Pulse    pulse1(
    amplitude=-2,
    width=50,
    period=100,
    nperiod=-1)
     annotation (Placement(transformation(extent={{10,-10},{-10,10}},
        rotation=0,
        origin={-10,-38})));
  Fluid.Sensors.MassFlowRate senCon(redeclare package Medium = Medium)
    annotation (Placement(transformation(
        extent={{10,-10},{-10,10}},
        rotation=90,
        origin={-8,48})));
equation
  connect(pulse.y, conPum.m_flow_in)
    annotation (Line(points={{-69,66},{-48,66}}, color={0,0,127}));
  connect(solPum.m_flow_in, m_flowPumVal.y)
    annotation (Line(points={{62,-44},{75,-44}}, color={0,0,127}));
  connect(senTem1.port_a, borPum.ports[1]) annotation (Line(points={{-66,2},{
          -70,2},{-70,-30},{-62,-30}},
                                    color={0,127,255}));
  connect(senTem2.port_a, hotTan.ports[1])
    annotation (Line(points={{64,36},{78,36}}, color={0,127,255}));
  connect(senTem2.port_b, senHotTan.port_a)
    annotation (Line(points={{44,36},{40,36}}, color={0,127,255}));
  connect(borPum.m_flow_in, pulse1.y)
    annotation (Line(points={{-40,-38},{-21,-38}}, color={0,0,127}));
  connect(HeaSupHed.ports_b[1], senHotTan.port_b) annotation (Line(points={{15,2},
          {16,2},{16,36},{20,36}}, color={0,127,255}));
  connect(HeaSupHed.ports_b[2], solPum.ports[1]) annotation (Line(points={{15,-2},
          {22,-2},{22,-54},{40,-54},{40,-52}}, color={0,127,255}));
  connect(HeaSupHed.ports_a[1], senTem1.port_b)
    annotation (Line(points={{-6.8,2},{-46,2}}, color={0,127,255}));
  connect(HeaSupHed.ports_a[2], senCon.port_b)
    annotation (Line(points={{-6.8,-2},{-8,-2},{-8,38}}, color={0,127,255}));
  connect(conPum.ports[1], senCon.port_a)
    annotation (Line(points={{-26,74},{-8,74},{-8,58}}, color={0,127,255}));
  annotation (Icon(coordinateSystem(preserveAspectRatio=false), graphics={
        Ellipse(lineColor = {75,138,73},
                fillColor={255,255,255},
                fillPattern = FillPattern.Solid,
                extent={{-98,-100},{98,98}}),
        Polygon(lineColor = {0,0,255},
                fillColor = {75,138,73},
                pattern = LinePattern.None,
                fillPattern = FillPattern.Solid,
                points={{-30,64},{70,4},{-30,-56},{-30,64}})}),  Diagram(coordinateSystem(preserveAspectRatio=false)));
end HeatingSupplyHeader_PortsExample;
