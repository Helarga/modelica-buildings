within Buildings.Applications.DHC.EnergyTransferStations.Examples;
model HeatpumpController
  "Reverse heatpump controller operates in heating mode only"




  ETSControl.HeatPumpController heaPumCon
    annotation (Placement(transformation(extent={{36,-8},{56,10}})));
    Controls.OBC.CDL.Continuous.Sources.Ramp TEvaLvg(
    height=3.5,
    duration(displayUnit="h") = 14400,
    offset=7 + 273.15,
    startTime=0) "Evaporator leaving water temperature"
    annotation (Placement(transformation(extent={{-70,-10},{-50,10}})));
  Modelica.Blocks.Sources.Constant CooSet(k=7 + 273.15)
    annotation (Placement(transformation(extent={{-40,-40},{-20,-20}})));
  Modelica.Blocks.Sources.Constant heaSet(k=45 + 273.15)
    annotation (Placement(transformation(extent={{-40,20},{-20,40}})));
  Modelica.Blocks.Sources.BooleanPulse stepCoo(
    width=50,
    startTime=0,
    period=200) "Step control"
    annotation (Placement(transformation(extent={{-20,-78},{0,-58}})));
  Modelica.Blocks.Sources.BooleanPulse stepHea(
    width=50,
    startTime=0,
    period=400) "Step control"
    annotation (Placement(transformation(extent={{-20,52},{0,72}})));
  Fluid.HeatPumps.EquationFitWaterToWaterReverse heaPum(reverseCycle=true)
    annotation (Placement(transformation(extent={{64,60},{84,80}})));
equation
  connect(CooSet.y, heaPumCon.TSetCoo) annotation (Line(points={{-19,-30},{8,
          -30},{8,-3.6},{35,-3.6}}, color={0,0,127}));
  connect(TEvaLvg.y, heaPumCon.TEvaLvg)
    annotation (Line(points={{-49,0},{35,0}}, color={0,0,127}));
  connect(heaSet.y, heaPumCon.TSetHea) annotation (Line(points={{-19,30},{8,30},
          {8,6.2},{35,6.2}}, color={0,0,127}));
  connect(stepCoo.y, heaPumCon.ReqCoo) annotation (Line(points={{1,-68},{20,-68},
          {20,-7},{34.6,-7}}, color={255,0,255}));
  connect(heaPumCon.ReqHea, stepHea.y) annotation (Line(points={{34.6,11},{20,
          11},{20,62},{1,62}}, color={255,0,255}));
  annotation (Icon(coordinateSystem(preserveAspectRatio=false), graphics={
        Ellipse(lineColor = {75,138,73},
                fillColor={255,255,255},
                fillPattern = FillPattern.Solid,
                extent={{-98,-100},{98,98}}),
        Polygon(lineColor = {0,0,255},
                fillColor = {75,138,73},
                pattern = LinePattern.None,
                fillPattern = FillPattern.Solid,
                points={{-30,64},{70,4},{-30,-56},{-30,64}})}),  Diagram(coordinateSystem(preserveAspectRatio=false)),
    experiment(StopTime=14400));
end HeatpumpController;
