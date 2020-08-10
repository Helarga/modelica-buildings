within Buildings.Applications.DHC.Loads;
model TimeSeriesMassFlow_Temp
  "Heating and Cooling Water massflowrates and temperatures from building on EP."
 extends Modelica.Icons.Example;
  Modelica.Blocks.Sources.CombiTimeTable buiMasTem(
    tableOnFile=true,
    tableName="tab1",
    fileName=Modelica.Utilities.Files.loadResource(filNam),
    extrapolation=Modelica.Blocks.Types.Extrapolation.Periodic,
    y(each unit="W"),
    offset={0,0,0},
    columns={2,3,4},
    smoothness=Modelica.Blocks.Types.Smoothness.MonotoneContinuousDerivative1)
    "Reader for thermal loads (y[1] is cooling load, y[2] is heating load)"
    annotation (Placement(transformation(extent={{80,-20},{60,0}})));
  Buildings.Fluid.Sources.MassFlowSource_T
                            supHeaWat(
    redeclare package Medium = Medium1,
    use_m_flow_in=true,
    use_T_in=true,
    nPorts=1) "Heating water supply" annotation (Placement(transformation(
      extent={{10,-10},{-10,10}},
      rotation=0,
      origin={30,-10})));
  Buildings.Fluid.Sources.Boundary_pT disSin(
    redeclare package Medium = Medium1,
    use_T_in=false,
    nPorts=1) "Heating water supply" annotation (Placement(transformation(
        extent={{10,-10},{-10,10}},
        rotation=0,
        origin={30,50})));
  Buildings.Applications.DHC.EnergyTransferStations.CoolingIndirect coo
    annotation (Placement(transformation(extent={{-24,10},{-4,30}})));
  Buildings.Fluid.Sources.Boundary_pT disSou(
    redeclare package Medium = Medium1,
    p=340000,
    use_T_in=false,
    nPorts=1) "Heating water supply" annotation (Placement(transformation(
        extent={{-10,-10},{10,10}},
        rotation=0,
        origin={-70,50})));
  Buildings.Fluid.Sources.Boundary_pT buiSin(
    redeclare package Medium = Medium1,
    p=340000,
    use_T_in=false,
    nPorts=1) "Heating water supply" annotation (Placement(transformation(
        extent={{-10,-10},{10,10}},
        rotation=0,
        origin={-70,-12})));
equation
  connect(coo.port_a2, supHeaWat.ports[1]) annotation (Line(points={{-4,14},{6,
          14},{6,-10},{20,-10}}, color={0,127,255}));
  connect(buiMasTem.y[1], supHeaWat.m_flow_in) annotation (Line(points={{59,-10},
          {52,-10},{52,-2},{42,-2}}, color={0,0,127}));
  connect(buiMasTem.y[2], supHeaWat.T_in) annotation (Line(points={{59,-10},{52,
          -10},{52,-6},{42,-6}}, color={0,0,127}));
  connect(buiMasTem.y[3], coo.TSetBuiSup) annotation (Line(points={{59,-10},{52,
          -10},{52,38},{-52,38},{-52,20},{-26,20}}, color={0,0,127}));
  connect(disSou.ports[1], coo.port_a1) annotation (Line(points={{-60,50},{-34,
          50},{-34,26},{-24,26}}, color={0,127,255}));
  connect(coo.port_b1, disSin.ports[1]) annotation (Line(points={{-4,26},{8,26},
          {8,50},{20,50}}, color={0,127,255}));
  connect(buiSin.ports[1], coo.port_b2) annotation (Line(points={{-60,-12},{-52,
          -12},{-52,14},{-24,14}}, color={0,127,255}));
  annotation ();
end TimeSeriesMassFlow_Temp;
