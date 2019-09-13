within Buildings.Applications.DHC.EnergyTransferStations.BaseClasses;
model CoolingWaterSupplyTemperature
  "Space cooling water supply temperature setpoint"
  parameter Modelica.SIunits.Temperature TSup_max=288.15
    "Maximum cooling supply temperature";
  parameter Modelica.SIunits.Temperature TSup_min=277.15
    "Minimum cooling supply temperature";
  parameter Modelica.SIunits.Temperature TSup_ini=280.15
    "Cooling supply temperature when it starts to do dehumidification";
  parameter Modelica.SIunits.MassFraction Xi_ini=7.5e-3
    "Humidity ratio when it starts to do dehumidification";
  parameter Modelica.SIunits.MassFraction dXi=0.5e-3;

  Modelica.Blocks.Interfaces.RealInput uXi "Humidity ratio, g/kg"
    annotation (Placement(transformation(extent={{-140,-20},{-100,20}}),
      iconTransformation(extent={{-140,-20},{-100,20}})));
  Modelica.Blocks.Interfaces.RealOutput TSup(
    final unit="K") "Cooling supply water temperature"
    annotation (Placement(transformation(extent={{100,-10},{120,10}}),
      iconTransformation(extent={{100,-10},{120,10}})));

  Buildings.Controls.OBC.CDL.Continuous.Line lin
    annotation (Placement(transformation(extent={{40,-10},{60,10}})));
  Modelica.Blocks.Sources.Constant xi_max(k=Xi_ini + dXi)
    annotation (Placement(transformation(extent={{-60,-50},{-40,-30}})));
  Modelica.Blocks.Sources.Constant minTSup(k=TSup_min)
    "Minimum cooling supply temperature"
    annotation (Placement(transformation(extent={{-40,-80},{-20,-60}})));
  Modelica.Blocks.Sources.Constant xi_min(
    k=Xi_ini - dXi*(TSup_max - TSup_ini)/(TSup_ini - TSup_min))
    annotation (Placement(transformation(extent={{-60,50},{-40,70}})));
  Modelica.Blocks.Sources.Constant maxTSup(k=TSup_max)
    "Maximum cooling supply temperature"
    annotation (Placement(transformation(extent={{-40,20},{-20,40}})));

equation
  connect(uXi, lin.u)
    annotation (Line(points={{-120,0},{38,0}}, color={0,0,127}));
  connect(xi_min.y, lin.x1) annotation (Line(points={{-39,60},{20,60},{20,8},{38,
          8}}, color={0,0,127}));
  connect(maxTSup.y, lin.f1)
    annotation (Line(points={{-19,30},{0,30},{0,4},{38,4}}, color={0,0,127}));
  connect(xi_max.y, lin.x2) annotation (Line(points={{-39,-40},{0,-40},{0,-4},{38,
          -4}}, color={0,0,127}));
  connect(minTSup.y, lin.f2) annotation (Line(points={{-19,-70},{20,-70},{20,-8},
          {38,-8}}, color={0,0,127}));
  connect(lin.y, TSup)
    annotation (Line(points={{61,0},{110,0}}, color={0,0,127}));
  annotation (Diagram(
        coordinateSystem(preserveAspectRatio=false)), Icon(graphics={
                 Rectangle(
        extent={{-100,-100},{100,100}},
        lineColor={0,0,127},
        fillColor={255,255,255},
        fillPattern=FillPattern.Solid),
        Polygon(
          points={{-80,92},{-88,70},{-72,70},{-80,92}},
          lineColor={192,192,192},
          fillColor={192,192,192},
          fillPattern=FillPattern.Solid),
        Line(points={{-80,-80},{-80,72}},
             color={192,192,192}),
        Line(points={{-80,40},{-46,40}},
          color={238,46,47},
          thickness=0.5,
          visible=limitBelow),
        Line(points={{-46,40},{50,-44}},
          color={0,0,0},
          thickness=0.5),
        Line(points={{52,-44},{86,-44}},
          color={238,46,47},
          thickness=0.5,
          visible=limitAbove),
        Line(points={{-88,-8},{76,-8}},
             color={192,192,192}),
        Polygon(
          points={{92,-8},{70,0},{70,-16},{92,-8}},
          lineColor={192,192,192},
          fillColor={192,192,192},
          fillPattern=FillPattern.Solid),
                                        Text(
        extent={{-118,140},{100,100}},
        lineColor={0,0,255},
          textString="%name"),
        Ellipse(
          extent={{-52,46},{-40,34}},
          pattern=LinePattern.None,
          lineColor={0,0,0},
          fillColor={0,0,0},
          fillPattern=FillPattern.Solid),
        Line(points={{-80,-44},{50,-44}},
             color={192,192,192},
          pattern=LinePattern.Dash),
        Text(
          extent={{-70,96},{-40,68}},
          lineColor={135,135,135},
          fillColor={0,0,0},
          fillPattern=FillPattern.Solid,
          textString="TSup"),
        Text(
          extent={{-98,52},{-68,40}},
          lineColor={135,135,135},
          fillColor={0,0,0},
          fillPattern=FillPattern.Solid,
          textString="TSup_max"),
        Line(points={{50,-42},{50,4}},
             color={192,192,192},
          pattern=LinePattern.Dash),
        Text(
          extent={{-98,-32},{-68,-44}},
          lineColor={135,135,135},
          fillColor={175,175,175},
          fillPattern=FillPattern.Solid,
          textString="TSup_min"),
        Text(
          extent={{-98,4},{-70,-8}},
          lineColor={135,135,135},
          fillColor={0,0,0},
          fillPattern=FillPattern.Solid,
          textString="TSup_ini"),
        Text(
          extent={{-12,-14},{8,-22}},
          lineColor={135,135,135},
          fillColor={0,0,0},
          fillPattern=FillPattern.Solid,
          textString="Xi_ini"),
        Text(
          extent={{84,2},{98,-6}},
          lineColor={135,135,135},
          fillColor={0,0,0},
          fillPattern=FillPattern.Solid,
          textString="Xi"),
        Ellipse(
          extent={{44,-38},{56,-50}},
          pattern=LinePattern.None,
          lineColor={0,0,0},
          fillColor={0,0,0},
          fillPattern=FillPattern.Solid),
        Text(
          extent={{20,10},{38,4}},
          lineColor={135,135,135},
          fillColor={0,0,0},
          fillPattern=FillPattern.Solid,
          textString="dXi"),
    Polygon(
      points={{50,2},{44,4},{44,0},{50,2}},
      lineColor={95,95,95},
      fillColor={95,95,95},
      fillPattern=FillPattern.Solid),
        Line(points={{14,2},{44,2}},
             color={192,192,192},
          pattern=LinePattern.Dash),
    Polygon(
      points={{8,2},{14,4},{14,0},{8,2}},
      lineColor={95,95,95},
      fillColor={95,95,95},
      fillPattern=FillPattern.Solid),
        Line(points={{8,-8},{8,6}},
             color={192,192,192},
          pattern=LinePattern.Dash),
        Ellipse(
          extent={{2,-2},{14,-14}},
          pattern=LinePattern.None,
          lineColor={0,0,0},
          fillColor={0,0,127},
          fillPattern=FillPattern.Solid)}));
end CoolingWaterSupplyTemperature;
