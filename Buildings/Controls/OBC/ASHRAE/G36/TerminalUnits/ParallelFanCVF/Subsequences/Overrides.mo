within Buildings.Controls.OBC.ASHRAE.G36.TerminalUnits.ParallelFanCVF.Subsequences;
block Overrides "Software switches to override setpoints"

  Buildings.Controls.OBC.CDL.Interfaces.IntegerInput oveDamPos
    "Index of overriding damper position, 1: set to close; 2: set to open"
    annotation (Placement(transformation(extent={{-180,120},{-140,160}}),
        iconTransformation(extent={{-140,70},{-100,110}})));
  Buildings.Controls.OBC.CDL.Interfaces.RealInput uDam(
    final min=0,
    final unit="1")
    "Damper commanded position"
    annotation (Placement(transformation(extent={{-180,30},{-140,70}}),
        iconTransformation(extent={{-140,50},{-100,90}})));
  Buildings.Controls.OBC.CDL.Interfaces.BooleanInput uHeaOff
    "Override heating valve position, true: close heating valve"
    annotation (Placement(transformation(extent={{-180,0},{-140,40}}),
        iconTransformation(extent={{-140,-10},{-100,30}})));
  Buildings.Controls.OBC.CDL.Interfaces.RealInput uVal(
    final min=0,
    final unit="1")
    "Heating valve commanded position"
    annotation (Placement(transformation(extent={{-180,-40},{-140,0}}),
        iconTransformation(extent={{-140,-30},{-100,10}})));
  Buildings.Controls.OBC.CDL.Interfaces.IntegerInput oveFan
    "Index of overriding fan status, 1: turn fan off; 2: turn fan on"
    annotation (Placement(transformation(extent={{-180,-90},{-140,-50}}),
        iconTransformation(extent={{-140,-90},{-100,-50}})));
  Buildings.Controls.OBC.CDL.Interfaces.BooleanInput u1Fan
    "Terminal fan status"
    annotation (Placement(transformation(extent={{-180,-180},{-140,-140}}),
        iconTransformation(extent={{-140,-110},{-100,-70}})));
  Buildings.Controls.OBC.CDL.Interfaces.RealOutput yDam(
    final min=0,
    final unit="1")
    "Damper commanded position, after considering override"
    annotation (Placement(transformation(extent={{140,60},{180,100}}),
        iconTransformation(extent={{100,40},{140,80}})));
  Buildings.Controls.OBC.CDL.Interfaces.RealOutput yVal(
    final min=0,
    final unit="1")
    "Heating valve commanded position, after considering override"
    annotation (Placement(transformation(extent={{140,-20},{180,20}}),
        iconTransformation(extent={{100,-20},{140,20}})));
  Buildings.Controls.OBC.CDL.Interfaces.BooleanOutput y1Fan "Fan command on"
    annotation (Placement(transformation(extent={{140,-90},{180,-50}}),
        iconTransformation(extent={{100,-80},{140,-40}})));

  Buildings.Controls.OBC.CDL.Integers.Sources.Constant conInt3(
    final k=1)
    "Constant 1"
    annotation (Placement(transformation(extent={{-120,110},{-100,130}})));
  Buildings.Controls.OBC.CDL.Integers.Equal intEqu3
    "Check if forcing damper to full close"
    annotation (Placement(transformation(extent={{-80,130},{-60,150}})));
  Buildings.Controls.OBC.CDL.Integers.Equal intEqu4
    "Check if forcing damper to full open"
    annotation (Placement(transformation(extent={{-80,90},{-60,110}})));
  Buildings.Controls.OBC.CDL.Integers.Sources.Constant conInt4(
    final k=2)
    "Constant 2"
    annotation (Placement(transformation(extent={{-120,70},{-100,90}})));
  Buildings.Controls.OBC.CDL.Conversions.BooleanToReal cloDam(
    final realTrue=0) "Full closed damper position"
    annotation (Placement(transformation(extent={{-40,130},{-20,150}})));
  Buildings.Controls.OBC.CDL.Conversions.BooleanToReal opeDam(
    final realTrue=1) "Full open damper position"
    annotation (Placement(transformation(extent={{-40,90},{-20,110}})));
  Buildings.Controls.OBC.CDL.Reals.Add add3 "Add up inputs"
    annotation (Placement(transformation(extent={{20,110},{40,130}})));
  Buildings.Controls.OBC.CDL.Logical.Or or2
    "Check if the damper setpoint position should be overrided"
    annotation (Placement(transformation(extent={{0,70},{20,90}})));
  Buildings.Controls.OBC.CDL.Reals.Switch swi1
    "Damper setpoint position after considering override"
    annotation (Placement(transformation(extent={{80,70},{100,90}})));
  Buildings.Controls.OBC.CDL.Reals.Multiply pro
    "Valve position setpoint after considering override"
    annotation (Placement(transformation(extent={{80,-10},{100,10}})));
  Buildings.Controls.OBC.CDL.Conversions.BooleanToReal booToRea(
    final realTrue=0,
    final realFalse=1)
    "Convert boolean true to real 0"
    annotation (Placement(transformation(extent={{-80,10},{-60,30}})));
  Buildings.Controls.OBC.CDL.Integers.Sources.Constant conInt5(
    final k=1)
    "Constant 1"
    annotation (Placement(transformation(extent={{-120,-100},{-100,-80}})));
  Buildings.Controls.OBC.CDL.Integers.Equal intEqu1
    "Check if forcing fan to turn  off"
    annotation (Placement(transformation(extent={{-60,-80},{-40,-60}})));
  Buildings.Controls.OBC.CDL.Integers.Equal intEqu2
    "Check if forcing fan to turn on"
    annotation (Placement(transformation(extent={{-60,-130},{-40,-110}})));
  Buildings.Controls.OBC.CDL.Integers.Sources.Constant conInt6(
    final k=2)
    "Constant 2"
    annotation (Placement(transformation(extent={{-120,-150},{-100,-130}})));
  Buildings.Controls.OBC.CDL.Logical.Switch logSwi
    "Fan status after considering override"
    annotation (Placement(transformation(extent={{80,-80},{100,-60}})));
  Buildings.Controls.OBC.CDL.Logical.Sources.Constant con(
    final k=false)
    "Turn fan off"
    annotation (Placement(transformation(extent={{0,-50},{20,-30}})));
  Buildings.Controls.OBC.CDL.Logical.Switch logSwi1
    "Logical switch"
    annotation (Placement(transformation(extent={{20,-130},{40,-110}})));
  Buildings.Controls.OBC.CDL.Logical.Sources.Constant con1(
    final k=true)
    "Turn fan on"
    annotation (Placement(transformation(extent={{-20,-100},{0,-80}})));

equation
  connect(conInt3.y, intEqu3.u2) annotation (Line(points={{-98,120},{-90,120},{-90,
          132},{-82,132}}, color={255,127,0}));
  connect(conInt4.y,intEqu4. u2) annotation (Line(points={{-98,80},{-90,80},{-90,
          92},{-82,92}}, color={255,127,0}));
  connect(oveDamPos, intEqu3.u1)
    annotation (Line(points={{-160,140},{-82,140}}, color={255,127,0}));
  connect(oveDamPos, intEqu4.u1) annotation (Line(points={{-160,140},{-130,140},
          {-130,100},{-82,100}}, color={255,127,0}));
  connect(intEqu3.y, cloDam.u)
    annotation (Line(points={{-58,140},{-42,140}},  color={255,0,255}));
  connect(intEqu4.y, opeDam.u)
    annotation (Line(points={{-58,100},{-42,100}}, color={255,0,255}));
  connect(cloDam.y, add3.u1) annotation (Line(points={{-18,140},{0,140},{0,126},
          {18,126}}, color={0,0,127}));
  connect(opeDam.y, add3.u2) annotation (Line(points={{-18,100},{0,100},{0,114},
          {18,114}}, color={0,0,127}));
  connect(intEqu3.y, or2.u1) annotation (Line(points={{-58,140},{-50,140},{-50,80},
          {-2,80}},   color={255,0,255}));
  connect(intEqu4.y, or2.u2) annotation (Line(points={{-58,100},{-50,100},{-50,72},
          {-2,72}},   color={255,0,255}));
  connect(add3.y, swi1.u1) annotation (Line(points={{42,120},{60,120},{60,88},{78,
          88}},     color={0,0,127}));
  connect(or2.y, swi1.u2)
    annotation (Line(points={{22,80},{78,80}}, color={255,0,255}));
  connect(uDam, swi1.u3) annotation (Line(points={{-160,50},{60,50},{60,72},{78,
          72}}, color={0,0,127}));
  connect(swi1.y, yDam)
    annotation (Line(points={{102,80},{160,80}}, color={0,0,127}));
  connect(uHeaOff, booToRea.u)
    annotation (Line(points={{-160,20},{-82,20}},   color={255,0,255}));
  connect(booToRea.y, pro.u1) annotation (Line(points={{-58,20},{60,20},{60,6},{
          78,6}},    color={0,0,127}));
  connect(uVal, pro.u2) annotation (Line(points={{-160,-20},{60,-20},{60,-6},{78,
          -6}},      color={0,0,127}));
  connect(pro.y, yVal)
    annotation (Line(points={{102,0},{160,0}},     color={0,0,127}));
  connect(conInt5.y,intEqu1. u2) annotation (Line(points={{-98,-90},{-80,-90},{-80,
          -78},{-62,-78}},        color={255,127,0}));
  connect(conInt6.y,intEqu2. u2) annotation (Line(points={{-98,-140},{-80,-140},
          {-80,-128},{-62,-128}}, color={255,127,0}));
  connect(oveFan, intEqu2.u1) annotation (Line(points={{-160,-70},{-130,-70},{-130,
          -120},{-62,-120}},      color={255,127,0}));
  connect(oveFan, intEqu1.u1)
    annotation (Line(points={{-160,-70},{-62,-70}},   color={255,127,0}));
  connect(intEqu1.y, logSwi.u2)
    annotation (Line(points={{-38,-70},{78,-70}},   color={255,0,255}));
  connect(con.y, logSwi.u1) annotation (Line(points={{22,-40},{60,-40},{60,-62},
          {78,-62}},  color={255,0,255}));
  connect(intEqu2.y, logSwi1.u2)
    annotation (Line(points={{-38,-120},{18,-120}}, color={255,0,255}));
  connect(con1.y, logSwi1.u1) annotation (Line(points={{2,-90},{10,-90},{10,-112},
          {18,-112}}, color={255,0,255}));
  connect(u1Fan, logSwi1.u3) annotation (Line(points={{-160,-160},{0,-160},{0,-128},
          {18,-128}}, color={255,0,255}));
  connect(logSwi1.y, logSwi.u3) annotation (Line(points={{42,-120},{60,-120},{60,
          -78},{78,-78}},   color={255,0,255}));
  connect(logSwi.y, y1Fan)
    annotation (Line(points={{102,-70},{160,-70}},   color={255,0,255}));

annotation (defaultComponentName="ove",
  Icon(coordinateSystem(preserveAspectRatio=false, extent={{-100,-100},{100,100}}),
        graphics={
        Rectangle(
        extent={{-100,-100},{100,100}},
        lineColor={0,0,127},
        fillColor={255,255,255},
        fillPattern=FillPattern.Solid),
        Text(
        extent={{-100,140},{100,100}},
        textString="%name",
        textColor={0,0,255}),
        Text(
          extent={{-100,76},{-70,64}},
          textColor={0,0,127},
          pattern=LinePattern.Dash,
          textString="uDam"),
        Text(
          extent={{-98,98},{-48,84}},
          textColor={255,127,0},
          pattern=LinePattern.Dash,
          textString="oveDamPos"),
        Text(
          extent={{60,66},{100,54}},
          textColor={0,0,127},
          pattern=LinePattern.Dash,
          textString="yDam"),
        Text(
          extent={{-100,-2},{-72,-14}},
          textColor={0,0,127},
          pattern=LinePattern.Dash,
          textString="uVal"),
        Text(
          extent={{-100,16},{-60,4}},
          textColor={255,0,255},
          pattern=LinePattern.Dash,
          textString="uHeaOff"),
        Text(
          extent={{60,8},{100,-4}},
          textColor={0,0,127},
          pattern=LinePattern.Dash,
          textString="yVal"),
        Text(
          extent={{-98,-82},{-72,-96}},
          textColor={255,0,255},
          pattern=LinePattern.Dash,
          textString="u1Fan"),
        Text(
          extent={{-96,-62},{-62,-76}},
          textColor={255,127,0},
          pattern=LinePattern.Dash,
          textString="oveFan"),
        Text(
          extent={{56,-54},{96,-66}},
          textColor={255,0,255},
          pattern=LinePattern.Dash,
          textString="y1Fan")}),
  Diagram(coordinateSystem(preserveAspectRatio=false, extent={{-140,-180},{140,180}})),
Documentation(info="<html>
<p>
This block considers the overrides to the setpoints for parallel fan-powered unit
with constant-volume fan.
The implementation is according to the Section 5.7.7 of ASHRAE Guideline 36,
May 2020.
</p>
<p>
Provide software switches that interlock to a system-level point to:
</p>
<ol>
<li>
when <code>oveDamPos</code> equals to 1, force the damper to full closed by setting
<code>yDam</code> to 0,
</li>
<li>
when <code>oveDamPos</code> equals to 2, force the damper to full open by setting
<code>yDam</code> to 1.
</li>
<li>
when <code>uHeaOff</code> equals to <code>true</code>, force the heating valve to
full closed by setting <code>yVal</code> to 0.
</li>
<li>
When <code>oveFan</code> equals to 1, force the fan to turn OFF.
</li>
<li>
When <code>oveFan</code> equals to 2, force the fan to turn ON.
</li>
</ol>
</html>",revisions="<html>
<ul>
<li>
August 1, 2020, by Jianjun Hu:<br/>
First implementation.
</li>
</ul>
</html>"));
end Overrides;
