within Buildings.Applications.DHC.EnergyTransferStations.Control;
model ColdSideControlleUO
  "Controller for valves on cold side, and heat demand on heat pump"
     extends Modelica.Blocks.Icons.Block;
  extends
    Buildings.Applications.DHC.EnergyTransferStations.Control.HotColdSideControllerUO(
      redeclare model Inequality =
        Buildings.Controls.OBC.CDL.Continuous.LessEqual,
          addPar(p=-2*THys),
          addPar1(p=-THys),
          addPar2(p=-THys),
          addPar3(p=-0.5*THys));

  Modelica.Blocks.Interfaces.BooleanOutput reqCoo
    "True if cooling is required from heat pump, false otherwise"
    annotation (Placement(
        transformation(extent={{140,132},{160,152}}),
                                                    iconTransformation(extent={{100,80},{120,100}})));
  Buildings.Controls.OBC.CDL.Logical.OnOffController frePro(bandwidth=1)
    "Freeze protection override"
    annotation (Placement(transformation(extent={{50,-38},{70,-18}})));
  Buildings.Controls.OBC.CDL.Continuous.Sources.Constant con(k=273.15 + 3.5)
    annotation (Placement(transformation(extent={{10,-20},{30,0}})));
  Buildings.Controls.OBC.CDL.Logical.Or or1
    annotation (Placement(transformation(extent={{88,-30},{108,-10}})));
  Buildings.Controls.OBC.CDL.Continuous.Max max
    annotation (Placement(transformation(extent={{-100,-110},{-80,-90}})));
equation
  connect(runHP.active, reqCoo) annotation (Line(points={{6,109},{6,108},{24,
          108},{24,142},{150,142}},color={255,0,255}));
  connect(con.y, frePro.reference) annotation (Line(points={{32,-10},{40,-10},{
          40,-22},{48,-22}}, color={0,0,127}));
  connect(or1.u2, frePro.y)
    annotation (Line(points={{86,-28},{72,-28}}, color={255,0,255}));
  connect(or1.u1, rejFulLoa.active) annotation (Line(points={{86,-20},{76,-20},
          {76,12},{56,12},{56,49}}, color={255,0,255}));
  connect(or1.y, rejFulHexBor) annotation (Line(points={{110,-20},{116,-20},{
          116,-48},{150,-48}}, color={255,0,255}));
  connect(or1.y, or2.u1) annotation (Line(points={{110,-20},{116,-20},{116,-48},
          {40,-48},{40,-80},{58,-80}}, color={255,0,255}));
  connect(yVal,booToRea. y) annotation (Line(points={{150,-100},{122,-100}},
                     color={0,0,127}));
  connect(valSta,or2. y)
    annotation (Line(points={{150,-80},{82,-80}}, color={255,0,255}));
  connect(or2.u2, rejParLoa.active)
    annotation (Line(points={{58,-88},{-6,-88},{-6,69}},   color={255,0,255}));
  connect(booToRea.u,or2. y) annotation (Line(points={{98,-100},{90,-100},{90,
          -80},{82,-80}}, color={255,0,255}));
  connect(or1.y,or2. u1) annotation (Line(points={{110,-20},{116,-20},{116,-48},
          {40,-48},{40,-80},{58,-80}}, color={255,0,255}));
  connect(max.u1, TTanTop) annotation (Line(points={{-102,-94},{-112,-94},{-112,
          60},{-160,60}},
                     color={0,0,127}));
  connect(frePro.u, TTanTop) annotation (Line(points={{48,-34},{44,-34},{44,-44},
          {-66,-44},{-66,22},{-112,22},{-112,60},{-160,60}}, color={0,0,127}));
  connect(TTanTop, greEqu1.u1) annotation (Line(points={{-160,60},{-112,60},{-112,
          22},{-66,22},{-66,8},{-62,8}}, color={0,0,127}));
  connect(TTanTop, greEqu2.u1) annotation (Line(points={{-160,60},{-112,60},{-112,
          22},{-66,22},{-66,-22},{-62,-22}}, color={0,0,127}));
  connect(TTanTop, greEqu3.u2) annotation (Line(points={{-160,60},{-112,60},{-112,
          22},{-66,22},{-66,-68},{-62,-68}}, color={0,0,127}));
  connect(max.y, greEqu4.u1) annotation (Line(points={{-78,-100},{-70,-100},{-70,
          -110},{-62,-110}},     color={0,0,127}));
  connect(max.u2, TTanBot) annotation (Line(points={{-102,-106},{-120,-106},{
          -120,-60},{-160,-60}},
                            color={0,0,127}));
  connect(TTanBot, greEqu5.u2) annotation (Line(points={{-160,-60},{-120,-60},{
          -120,-160},{-70,-160},{-70,-148},{-62,-148}},
                                                   color={0,0,127}));
  connect(TTanBot, greEqu.u2) annotation (Line(points={{-160,-60},{-120,-60},{-120,
          32},{-102,32}}, color={0,0,127}));
  annotation (
  defaultComponentName="conColSid",
  Diagram(coordinateSystem(extent={{-140,-160},{140,160}}), graphics={Text(
          extent={{-36,-32},{40,-40}},
          lineColor={28,108,200},
          textString="reject full load if tank exceeds setpoint
by 3*THys",
          horizontalAlignment=TextAlignment.Left)}),
  Documentation(info="<html>
<p>
This block is a finite stat machine controller which transitions the <a href=\"Buildings.DistrictHeatingCooling.EnergyTransferStations.EnergyTransferStation.Substation\">
Buildings.DistrictHeatingCooling.EnergyTransferStations.EnergyTransferStation.Substation</a> operational modes:
<ul>
<li>
Heatpump on and off.
</li>
</ul>
<ul>
<li>
Reject part load to the borefiled system on and off.
</li>
</ul>
<ul>
<li>
Reject full load to both the borefiled and the district system on and off.
</li>
</ul>
</p>
<p>
An on-off override controller was used to start the full load rejection once the water inside the tank reaches 3.5degC to avoid frezzing.
</p>
</html>", revisions="<html>
<ul>
<li>
 <br/>

</li>
</ul>
</html>"),
    Icon(coordinateSystem(extent={{-100,-100},{100,100}})));
end ColdSideControlleUO;
