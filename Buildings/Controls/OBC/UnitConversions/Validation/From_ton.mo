within Buildings.Controls.OBC.UnitConversions.Validation;
model From_ton "Validation model for unit conversion from ton of refrigeration to watt"
  extends Modelica.Icons.Example;

  Buildings.Controls.OBC.CDL.Reals.Subtract sub
    "Difference between the calculated and expected conversion output"
    annotation (Placement(transformation(extent={{20,40},{40,60}})));
  Buildings.Controls.OBC.CDL.Reals.Subtract sub1
    "Difference between the calculated and expected conversion output"
    annotation (Placement(transformation(extent={{20,-40},{40,-20}})));

protected
  parameter Real kin = 1. "Validation input";
  parameter Real kin1 = 100. "Validation input 1";
  parameter Real kout = 3517. "Validation output";
  parameter Real kout1 = 351700. "Validation output 1";

  Buildings.Controls.OBC.UnitConversions.From_ton from_ton
  "Unit converter from ton of refrigeration to watt "
    annotation (Placement(transformation(extent={{-20,40},{0,60}})));
  Buildings.Controls.OBC.UnitConversions.From_ton from_ton1
  "Unit converter from ton of refrigeration to watt "
    annotation (Placement(transformation(extent={{-20,-40},{0,-20}})));

  Buildings.Controls.OBC.CDL.Reals.Sources.Constant value(
    final k=kin)
    "Value to convert"
    annotation (Placement(transformation(extent={{-60,40},{-40,60}})));
  Buildings.Controls.OBC.CDL.Reals.Sources.Constant value1(
    final k=kin1)
    "Value to convert"
    annotation (Placement(transformation(extent={{-60,-40},{-40,-20}})));
  Buildings.Controls.OBC.CDL.Reals.Sources.Constant result(
    final k=kout)
    "Expected converted value"
    annotation (Placement(transformation(extent={{-20,10},{0,30}})));
  Buildings.Controls.OBC.CDL.Reals.Sources.Constant result1(
    final k=kout1)
    "Expected converted value"
    annotation (Placement(transformation(extent={{-20,-70},{0,-50}})));

equation
  connect(result.y, sub.u2)
    annotation (Line(points={{2,20},{10,20},{10,44},{18,44}}, color={0,0,127}));
  connect(result1.y, sub1.u2)
    annotation (Line(points={{2,-60},{10,-60},{10,-36},{18,-36}}, color={0,0,127}));
  connect(value1.y,from_ton1.u)
    annotation (Line(points={{-38,-30},{-22,-30}}, color={0,0,127}));
  connect(from_ton1.y, sub1.u1)
    annotation (Line(points={{2,-30},{8,-30},{8,-24},{18,-24}}, color={0,0,127}));
  connect(from_ton.y, sub.u1)
    annotation (Line(points={{2,50},{10,50},{10,56},{18,56}}, color={0,0,127}));
  connect(value.y,from_ton.u)
    annotation (Line(points={{-38,50}, {-22,50}}, color={0,0,127}));
  annotation (Icon(graphics={
        Ellipse(lineColor = {75,138,73},
                fillColor={255,255,255},
                fillPattern = FillPattern.Solid,
                extent = {{-100,-100},{100,100}}),
        Polygon(lineColor = {0,0,255},
                fillColor = {75,138,73},
                pattern = LinePattern.None,
                fillPattern = FillPattern.Solid,
                points = {{-36,60},{64,0},{-36,-60},{-36,60}})}),
                Diagram(coordinateSystem( preserveAspectRatio=false)),
            experiment(StopTime=10.0, Tolerance=1e-06),
  __Dymola_Commands(file="modelica://Buildings/Resources/Scripts/Dymola/Controls/OBC/UnitConversions/Validation/From_ton.mos"
    "Simulate and plot"),
    Documentation(
    info="<html>
<p>
This model validates power unit conversion from ton of refrigeration to watt.
</p>
</html>",
revisions="<html>
<ul>
<li>
November 29, 2021, by Michael Wetter:<br/>
Regenerated files with <code>Text</code> annotation using now the <code>textColor</code> attribute
rather than the deprecated <code>lineColor</code> attribute.
</li>
<li>
July 05, 2018, Milica Grahovac<br/>
Generated with <code>Buildings/Resources/src/Controls/OBC/UnitConversions/unit_converters.py</code>.<br/>
First implementation.
</li>
</ul>
</html>"));
end From_ton;
