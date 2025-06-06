within Buildings.Controls.OBC.CDL.Reals;
block Cos "Output the cosine of the input"
  Buildings.Controls.OBC.CDL.Interfaces.RealInput u(
    final unit="rad",
    displayUnit="deg")
    "Input for the cosine function"
    annotation (Placement(transformation(extent={{-140,-20},{-100,20}})));
  Buildings.Controls.OBC.CDL.Interfaces.RealOutput y
    "Cosine of the input"
    annotation (Placement(transformation(extent={{100,-20},{140,20}})));

equation
  y=Modelica.Math.cos(u);
  annotation (
    defaultComponentName="cos",
    Icon(
      coordinateSystem(
        preserveAspectRatio=true,
        extent={{-100,-100},{100,100}}),
      graphics={
        Rectangle(
          extent={{-100,-100},{100,100}},
          lineColor={0,0,127},
          fillColor={255,255,255},
          fillPattern=FillPattern.Solid),
        Line(
          points={{-80,80},{-74.4,78.1},{-68.7,72.3},{-63.1,63},{-56.7,48.7},{-48.6,26.6},{-29.3,-32.5},{-22.1,-51.7},{-15.7,-65.3},{-10.1,-73.8},{-4.42,-78.8},{1.21,-79.9},{6.83,-77.1},{12.5,-70.6},{18.1,-60.6},{24.5,-45.7},{32.6,-23},{50.3,31.3},{57.5,50.7},{63.9,64.6},{69.5,73.4},{75.2,78.6},{80,80}},
          smooth=Smooth.Bezier),
        Text(
          textColor={0,0,255},
          extent={{-150,110},{150,150}},
          textString="%name"),
        Polygon(
          points={{-80,90},{-88,68},{-72,68},{-80,90}},
          lineColor={192,192,192},
          fillColor={192,192,192},
          fillPattern=FillPattern.Solid),
        Line(
          points={{-80,-80},{-80,68}},
          color={192,192,192}),
        Line(
          points={{-90,0},{68,0}},
          color={192,192,192}),
        Polygon(
          points={{90,0},{68,8},{68,-8},{90,0}},
          lineColor={192,192,192},
          fillColor={192,192,192},
          fillPattern=FillPattern.Solid),
        Text(
          extent={{-36,80},{36,32}},
          textColor={192,192,192},
          textString="cos"),
        Text(
          extent={{226,60},{106,10}},
          textColor={0,0,0},
          textString=DynamicSelect("",String(y,
            leftJustified=false,
            significantDigits=3)))}),
    Documentation(
      info="<html>
<p>
Block that outputs <code>y = cos(u)</code>,
where
<code>u</code> is an input.
</p>

<p align=\"center\">
<img src=\"modelica://Buildings/Resources/Images/Controls/OBC/CDL/Reals/Cos.png\"
     alt=\"cos.png\" />
</p>

</html>",
      revisions="<html>
<ul>
<li>
November 8, 2024, by Michael Wetter:<br/>
Added <code>final</code> keyword to unit declaration as block is only valid for this unit.<br/>
Also added <code>displayUnit</code> keyword.
</li>
<li>
March 7, 2023, by Jianjun Hu:<br/>
Added unit <code>rad</code> to the input.<br/>
This is for
<a href=\"https://github.com/lbl-srg/modelica-buildings/issues/3277\">Buildings, issue 3277</a>.
</li>
<li>
March 2, 2020, by Michael Wetter:<br/>
Changed icon to display dynamically the output value.
</li>
<li>
January 3, 2017, by Michael Wetter:<br/>
First implementation, based on the implementation of the
Modelica Standard Library.
</li>
</ul>
</html>"));
end Cos;
