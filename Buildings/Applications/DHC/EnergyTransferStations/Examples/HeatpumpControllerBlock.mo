within Buildings.Applications.DHC.EnergyTransferStations.Examples;
model HeatpumpControllerBlock
  "Reverse heatpump controller operates in heating mode only"
  package Medium = Buildings.Media.Water "Medium model";

  Buildings.Applications.DHC.EnergyTransferStations.Control.HeatPumpController heaPumCon
    annotation (Placement(transformation(extent={{40,0},{60,18}})));

  Modelica.Blocks.Sources.BooleanPulse heaMod(
    width=50,
    period=500)
    "Step control"
    annotation (Placement(transformation(extent={{0,60},{20,80}})));
  Modelica.Blocks.Sources.BooleanPulse    CooMod(
    width=50,
    period=500,
    startTime=250)
    "Step control"
    annotation (Placement(transformation(extent={{0,30},{20,50}})));
  Modelica.Blocks.Sources.Constant TCooSet(k=7 + 273.15)
    "Cooling setpoint temperature"
    annotation (Placement(transformation(extent={{-60,-36},{-40,-16}})));
  Modelica.Blocks.Sources.Constant THeaSet(k=40 + 273.15)
    "Heating set point temperature"
    annotation (Placement(transformation(extent={{-60,70},{-40,90}})));
  Modelica.Blocks.Sources.Constant THeaSetMax(k=55 + 273.15)
    "Maximum heating set point temperature"
    annotation (Placement(transformation(extent={{-60,38},{-40,58}})));
  Modelica.Blocks.Sources.Constant minFloHeaPum(k=0.2)
    "Minimum flow rate for the heat pump"
    annotation (Placement(transformation(extent={{-60,-68},{-40,-48}})));
  Modelica.Blocks.Sources.Ramp mLoa(
    height=0.9,
    duration=500,
    offset=0.1)
   "Mass flow rate at the load side"
    annotation (Placement(transformation(extent={{-60,-98},{-40,-78}})));
  Modelica.Blocks.Sources.Ramp mSou(
    height=1,
    duration=500,
    offset=0.3)
   "Mass flow rate at the source side"
    annotation (Placement(transformation(extent={{-60,-130},{-40,-110}})));
  Modelica.Blocks.Sources.Constant TSouLvg(k=55 + 273.15)
    "Maximum heating set point temperature"
    annotation (Placement(transformation(extent={{-60,0},{-40,20}})));
equation
  connect(heaPumCon.ReqHea, heaMod.y) annotation (Line(points={{38.6,19},{30,19},
          {30,70},{21,70}},   color={255,0,255}));
  connect(heaPumCon.ReqCoo, CooMod.y) annotation (Line(points={{38.6,16},{26,16},
          {26,40},{21,40}},     color={255,0,255}));
  connect(heaPumCon.TSetCoo, TCooSet.y) annotation (Line(points={{39,8},{-30,8},
          {-30,-26},{-39,-26}},       color={0,0,127}));
  connect(heaPumCon.TSetHea, THeaSet.y) annotation (Line(points={{39,14},{-20,
          14},{-20,80},{-39,80}},
                              color={0,0,127}));
  connect(THeaSetMax.y, heaPumCon.TSetHeaMax) annotation (Line(points={{-39,48},
          {-30,48},{-30,12},{39,12}},     color={0,0,127}));
  connect(minFloHeaPum.y, heaPumCon.minFloHeaPum) annotation (Line(points={{-39,-58},
          {-28,-58},{-28,4},{39,4}},            color={0,0,127}));
  connect(mLoa.y, heaPumCon.mFloLoaHeaPum) annotation (Line(points={{-39,-88},{
          -24,-88},{-24,2},{39,2}},
                                  color={0,0,127}));
  connect(mSou.y, heaPumCon.mFloSouHeaPum) annotation (Line(points={{-39,-120},
          {-20,-120},{-20,0},{39,0}},    color={0,0,127}));
  connect(TSouLvg.y, heaPumCon.TEvaLvg)
    annotation (Line(points={{-39,10},{39,10}}, color={0,0,127}));
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
            {-100,-140},{100,100}}),
        graphics={Line(points={{-22,22}}, color={28,108,200})}),
    experiment(StopTime=14400),
    __Dymola_Commands(
  file="modelica://Buildings/Resources/Scripts/Dymola/Applications/DHC/EnergyTransferStations/Control/HeatpumpController.mos"
        "Simulate and plot"),
         experiment(Tolerance=1e-6, StopTime=14400));
end HeatpumpControllerBlock;
