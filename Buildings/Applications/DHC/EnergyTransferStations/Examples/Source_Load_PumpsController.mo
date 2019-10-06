within Buildings.Applications.DHC.EnergyTransferStations.Examples;
model Source_Load_PumpsController "Source and load pumps controller"

  Buildings.Controls.OBC.CDL.Continuous.Sources.Constant yPumMinLoa(k=0.2)
    "Minimum speed for the load side pump"
    annotation (Placement(transformation(extent={{32,20},{52,40}})));
  Modelica.Blocks.Sources.BooleanPulse    heaMod(
    width=70,
    period=200,
    startTime=0)                                          "Step control"
    annotation (Placement(transformation(extent={{60,40},{80,60}})));
  Modelica.Blocks.Sources.BooleanPulse    CooMod(width=70, period=200)
                                                 "Step control"
    annotation (Placement(transformation(extent={{60,-80},{80,-60}})));
  Modelica.Blocks.Sources.Constant TSouEntMin(k=4 + 273.15)
    "Cooling setpoint temperature"
    annotation (Placement(transformation(extent={{-94,20},{-74,40}})));
  Modelica.Blocks.Sources.Constant mSecCoo(k=40 + 273.15)
    "Mass flow rate of the secondary pump at the cold tank side"
    annotation (Placement(transformation(extent={{0,-40},{20,-20}})));
  Modelica.Blocks.Sources.Constant mSecHea(k=0.2)
    "Mass flow rate of the secondary pump at the hot tank side"
    annotation (Placement(transformation(extent={{-40,-40},{-20,-20}})));
  Control.Source_Load_PumpsController pumCon
    annotation (Placement(transformation(extent={{90,-10},{110,10}})));
  Modelica.Blocks.Sources.Pulse mLoa(
    amplitude=5,
    width=50,
    period=200,
    offset=35 + 273.15) "Load side water flow rate"
    annotation (Placement(transformation(extent={{0,20},{20,40}})));
  Modelica.Blocks.Sources.Constant TLoaEntMax(k=55 + 273.15)
    "Maximum entering water temperature at the load side"
    annotation (Placement(transformation(extent={{-62,20},{-42,40}})));
  Modelica.Blocks.Sources.Pulse    TSouEnt(
    amplitude=4,
    period=200,
    offset=12 + 273.15)
    "Source side entering water temperature"
    annotation (Placement(transformation(extent={{-80,-40},{-60,-20}})));
  Modelica.Blocks.Sources.Pulse mSou(
    amplitude=2,
    width=50,
    period=200,
    offset=9 + 273.15) "Source side water flow rate"
    annotation (Placement(transformation(extent={{40,-40},{60,-20}})));
  Modelica.Blocks.Sources.Pulse TLoaEnt(
    amplitude=20,
    width=50,
    period=200,
    offset=35 + 273.15) "Load side enetring water temperature"
    annotation (Placement(transformation(extent={{-32,20},{-12,40}})));
equation
  connect(pumCon.ReqHea, heaMod.y) annotation (Line(points={{88.6,9.4},{88,9.4},
          {88,50},{81,50}}, color={255,0,255}));
  connect(CooMod.y, pumCon.ReqCoo) annotation (Line(points={{81,-70},{86,-70},{
          86,-9.8},{88.6,-9.8}},
                              color={255,0,255}));
  connect(pumCon.yheaPumMin, yPumMinLoa.y)
    annotation (Line(points={{89.3,6.1},{54,6.1},{54,30}}, color={0,0,127}));
  connect(mLoa.y, pumCon.mLoa) annotation (Line(points={{21,30},{24,30},{24,4.1},
          {89.3,4.1}}, color={0,0,127}));
  connect(TLoaEntMax.y,pumCon.TLoaEntMax)  annotation (Line(points={{-41,30},{
          -36,30},{-36,-0.1},{89.3,-0.1}},
                                     color={0,0,127}));
  connect(TSouEntMin.y, pumCon.TSouEntMin) annotation (Line(points={{-73,30},{
          -70,30},{-70,-1.9},{89.3,-1.9}},
                                      color={0,0,127}));
  connect(pumCon.TSouEnt, TSouEnt.y) annotation (Line(points={{89.3,-3.5},{-46,
          -3.5},{-46,-30},{-59,-30}},
                                color={0,0,127}));
  connect(TLoaEnt.y, pumCon.TLoaEnt) annotation (Line(points={{-11,30},{-4,30},
          {-4,2.1},{89.3,2.1}},  color={0,0,127}));
  connect(pumCon.mSou, mSou.y) annotation (Line(points={{89.3,-8.3},{66,-8.3},{
          66,-30},{61,-30}}, color={0,0,127}));
  connect(mSecCoo.y, pumCon.mSecCoo) annotation (Line(points={{21,-30},{26,-30},
          {26,-6.7},{89.3,-6.7}}, color={0,0,127}));
  connect(mSecHea.y, pumCon.mSecHot) annotation (Line(points={{-19,-30},{-14,
          -30},{-14,-5.1},{89.3,-5.1}}, color={0,0,127}));
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
