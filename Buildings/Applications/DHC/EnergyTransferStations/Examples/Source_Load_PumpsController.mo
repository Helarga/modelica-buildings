within Buildings.Applications.DHC.EnergyTransferStations.Examples;
model Source_Load_PumpsController "Source and load pumps controller"

  Buildings.Controls.OBC.CDL.Continuous.Sources.Constant yPumMinLoa(k=0.2)
    "Minimum speed for the load side pump"
    annotation (Placement(transformation(extent={{32,20},{52,40}})));
  Modelica.Blocks.Sources.BooleanPulse    heaMod(
    width=70,
    period=200,
    startTime=0)                                          "Step control"
    annotation (Placement(transformation(extent={{28,60},{48,80}})));
  Modelica.Blocks.Sources.BooleanPulse    CooMod(width=70, period=200)
                                                 "Step control"
    annotation (Placement(transformation(extent={{30,-80},{50,-60}})));
  Modelica.Blocks.Sources.Constant TSouEntMin(k=4 + 273.15)
    "Cooling setpoint temperature"
    annotation (Placement(transformation(extent={{-90,-46},{-70,-26}})));
  Modelica.Blocks.Sources.Constant THeaSet(k=40 + 273.15)
    "Heating set point temperature"
    annotation (Placement(transformation(extent={{2,20},{22,40}})));
  Modelica.Blocks.Sources.Constant yPumMinSou(k=0.2)
    "Minimum speed for the source side pump"
    annotation (Placement(transformation(extent={{28,-46},{48,-26}})));
  Control.Source_Load_PumpsController pumCon
    annotation (Placement(transformation(extent={{60,-10},{80,10}})));
  Modelica.Blocks.Sources.Pulse TLoaLvg(
    amplitude=5,
    width=50,
    period=200,
    offset=35 + 273.15) "Load side leaving water temperature"
    annotation (Placement(transformation(extent={{-30,20},{-10,40}})));
  Modelica.Blocks.Sources.Constant TLoaEntMax(k=55 + 273.15)
    "Maximum entering water temperature at the load side"
    annotation (Placement(transformation(extent={{-90,20},{-70,40}})));
  Modelica.Blocks.Sources.Pulse    TSouEnt(
    amplitude=4,
    period=200,
    offset=12 + 273.15)
    "Source side entering water temperature"
    annotation (Placement(transformation(extent={{-62,-46},{-42,-26}})));
  Modelica.Blocks.Sources.Pulse    TSouLvg(
    amplitude=2,
    width=50,
    period=200,
    offset=9 + 273.15)
    "Source side leaving water temperature"
    annotation (Placement(transformation(extent={{-32,-46},{-12,-26}})));
  Modelica.Blocks.Sources.Constant TCooSet(k=280.15)
    "Cooling setpoint temperature"
    annotation (Placement(transformation(extent={{0,-46},{20,-26}})));
  Modelica.Blocks.Sources.Pulse TLoaEnt(
    amplitude=20,
    width=50,
    period=200,
    offset=35 + 273.15) "Load side enetring water temperature"
    annotation (Placement(transformation(extent={{-58,20},{-38,40}})));
equation
  connect(pumCon.ReqHea, heaMod.y) annotation (Line(points={{58.6,9.4},{56,9.4},
          {56,70},{49,70}}, color={255,0,255}));
  connect(CooMod.y, pumCon.ReqCoo) annotation (Line(points={{51,-70},{56,-70},{56,
          -9.8},{58.6,-9.8}}, color={255,0,255}));
  connect(pumCon.pumConMin, yPumMinLoa.y)
    annotation (Line(points={{59.3,7.5},{54,7.5},{54,30}}, color={0,0,127}));
  connect(pumCon.TSetHea, THeaSet.y) annotation (Line(points={{59.3,5.9},{28,5.9},
          {28,30},{23,30}},color={0,0,127}));
  connect(TLoaLvg.y, pumCon.TLoaLvg) annotation (Line(points={{-9,30},{-4,30},{-4,
          4.3},{59.3,4.3}},      color={0,0,127}));
  connect(TLoaEntMax.y, pumCon.TLoaEntMax) annotation (Line(points={{-69,30},{-64,
          30},{-64,1.5},{59.3,1.5}}, color={0,0,127}));
  connect(pumCon.pumEvaMin, yPumMinSou.y) annotation (Line(points={{59.3,-7.5},{
          54,-7.5},{54,-36},{49,-36}}, color={0,0,127}));
  connect(TSouEntMin.y, pumCon.TSouEntMin) annotation (Line(points={{-69,-36},{-64,
          -36},{-64,-1.1},{59.3,-1.1}},
                                      color={0,0,127}));
  connect(TSouLvg.y, pumCon.TSouLvg) annotation (Line(points={{-11,-36},{-4,-36},
          {-4,-4.1},{59.3,-4.1}},  color={0,0,127}));
  connect(pumCon.TSetCoo, TCooSet.y) annotation (Line(points={{59.3,-5.9},{26,-5.9},
          {26,-36},{21,-36}}, color={0,0,127}));
  connect(pumCon.TSouEnt, TSouEnt.y) annotation (Line(points={{59.3,-2.5},{-34,-2.5},
          {-34,-36},{-41,-36}}, color={0,0,127}));
  connect(TLoaEnt.y, pumCon.TLoaEnt) annotation (Line(points={{-37,30},{-34,30},
          {-34,2.9},{59.3,2.9}}, color={0,0,127}));
  annotation (Icon(coordinateSystem(preserveAspectRatio=false), graphics={
        Ellipse(lineColor = {75,138,73},
                fillColor={255,255,255},
                fillPattern = FillPattern.Solid,
                extent={{-98,-100},{98,98}}),
        Polygon(lineColor = {0,0,255},
                fillColor = {75,138,73},
                pattern = LinePattern.None,
                fillPattern = FillPattern.Solid,
                points={{-30,64},{70,4},{-30,-56},{-30,64}})}),  Diagram(coordinateSystem(preserveAspectRatio=false, extent={
            {-100,-100},{160,100}}),
        graphics={Line(points={{-22,22}}, color={28,108,200})}),
    experiment(StopTime=2000),
    __Dymola_Commands(
  file="modelica://Buildings/Resources/Scripts/Dymola/Applications/DHC/EnergyTransferStations/Control/Source_LoadPumpsController.mos"
        "Simulate and plot"),
         experiment(Tolerance=1e-6, StopTime=14400));
end Source_Load_PumpsController;
