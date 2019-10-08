within Buildings.Applications.DHC.EnergyTransferStations.Examples;
model HeatpumpControllerBlock
  "Reverse heatpump controller operates in heating mode only"
  package Medium = Buildings.Media.Water "Medium model";

  Buildings.Applications.DHC.EnergyTransferStations.Control.HeatPumpController heaPumCon
    annotation (Placement(transformation(extent={{40,0},{60,18}})));

  Modelica.Blocks.Sources.BooleanConstant
                                       heaMod(k=false)
    "Step control"
    annotation (Placement(transformation(extent={{0,60},{20,80}})));
  Modelica.Blocks.Sources.BooleanPulse    CooMod(width=50, period=1000)
    "Step control"
    annotation (Placement(transformation(extent={{0,30},{20,50}})));
  Modelica.Blocks.Sources.Constant TCooSet(k=7 + 273.15)
    "Cooling setpoint temperature"
    annotation (Placement(transformation(extent={{-60,-78},{-40,-58}})));
  Modelica.Blocks.Sources.Constant THeaSetMin(k=25 + 273.15)
    "Minimum heating set point temperature"
    annotation (Placement(transformation(extent={{-60,54},{-40,74}})));
  Modelica.Blocks.Sources.Constant THeaSetMax(k=50 + 273.15)
    "Maximum heating set point temperature"
    annotation (Placement(transformation(extent={{-60,-12},{-40,8}})));
  Modelica.Blocks.Sources.Constant TSouLvg(k=10 + 273.15)
    "Maximum heating set point temperature"
    annotation (Placement(transformation(extent={{-60,-42},{-40,-22}})));
  Modelica.Blocks.Sources.Constant THeaSet(k=30 + 273.15)
    "Heating set point temperature"
    annotation (Placement(transformation(extent={{-60,20},{-40,40}})));
equation
  connect(heaPumCon.ReqHea, heaMod.y) annotation (Line(points={{38.6,19},{30,19},
          {30,70},{21,70}},   color={255,0,255}));
  connect(heaPumCon.ReqCoo, CooMod.y) annotation (Line(points={{38.6,16},{26,16},
          {26,40},{21,40}},     color={255,0,255}));
  connect(heaPumCon.TSetCoo, TCooSet.y) annotation (Line(points={{39,0.8},{36,
          0.8},{36,-68},{-39,-68}},   color={0,0,127}));
  connect(heaPumCon.TSetHeaMin, THeaSetMin.y) annotation (Line(points={{39,8.6},
          {-20,8.6},{-20,64},{-39,64}}, color={0,0,127}));
  connect(THeaSetMax.y, heaPumCon.TSetHeaMax) annotation (Line(points={{-39,-2},
          {-28,-2},{-28,4.8},{39,4.8}},   color={0,0,127}));
  connect(TSouLvg.y, heaPumCon.TSouLvg) annotation (Line(points={{-39,-32},{22,
          -32},{22,2.8},{39,2.8}}, color={0,0,127}));
  connect(THeaSet.y, heaPumCon.TSetHea) annotation (Line(points={{-39,30},{-28,
          30},{-28,6.8},{39,6.8}}, color={0,0,127}));
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
    experiment(StopTime=3500),
    __Dymola_Commands(
  file="modelica://Buildings/Resources/Scripts/Dymola/Applications/DHC/EnergyTransferStations/Control/HeatpumpController.mos"
        "Simulate and plot"),
         experiment(Tolerance=1e-6, StopTime=14400));
end HeatpumpControllerBlock;
