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
    addPar2(p=-THys));

  Modelica.Blocks.Interfaces.BooleanOutput reqCoo
    "True if cooling is required from heat pump, false otherwise" annotation (Placement(
        transformation(extent={{140,70},{160,90}}), iconTransformation(extent={{100,80},{120,100}})));
  Buildings.Controls.OBC.CDL.Continuous.Max max
    annotation (Placement(transformation(extent={{-100,-110},{-80,-90}})));
  Buildings.Controls.OBC.CDL.Logical.OnOffController frePro(bandwidth=1)
    "Freeze protection override"
    annotation (Placement(transformation(extent={{50,-38},{70,-18}})));
  Buildings.Controls.OBC.CDL.Continuous.Sources.Constant con(k=273.15 + 3.5)
    annotation (Placement(transformation(extent={{10,-20},{30,0}})));
  Buildings.Controls.OBC.CDL.Logical.Or or1
    annotation (Placement(transformation(extent={{88,-30},{108,-10}})));
equation
  connect(runHP.active, reqCoo) annotation (Line(points={{6,109},{6,106},{4,106},
          {4,142},{120,142},{120,80},{150,80}},
                                   color={255,0,255}));
  connect(max.u1, TTanTop) annotation (Line(points={{-102,-94},{-112,-94},{-112,
          60},{-160,60}},
                     color={0,0,127}));
  connect(max.u2, TTanBot) annotation (Line(points={{-102,-106},{-120,-106},{
          -120,-110},{-160,-110}},
                            color={0,0,127}));
  connect(con.y, frePro.reference) annotation (Line(points={{32,-10},{40,-10},{40,
          -22},{48,-22}},    color={0,0,127}));
  connect(frePro.u, TTanTop) annotation (Line(points={{48,-34},{40,-34},{40,-40},
          {-66,-40},{-66,20},{-112,20},{-112,60},{-160,60}}, color={0,0,127}));
  connect(or1.u2, frePro.y)
    annotation (Line(points={{86,-28},{72,-28}}, color={255,0,255}));
  connect(or1.u1, rejFulLoa.active) annotation (Line(points={{86,-20},{76,-20},
          {76,12},{56,12},{56,49}}, color={255,0,255}));
  connect(or1.y, rejFulHexBor) annotation (Line(points={{110,-20},{116,-20},{
          116,-48},{150,-48}}, color={255,0,255}));
  connect(or1.y, or2.u1) annotation (Line(points={{110,-20},{116,-20},{116,-50},
          {40,-50},{40,-90},{62,-90}}, color={255,0,255}));
  connect(TTanBot, t1On.u2) annotation (Line(points={{-160,-110},{-120,-110},{
          -120,32},{-102,32}}, color={0,0,127}));
  connect(TTanTop, t3On.u1) annotation (Line(points={{-160,60},{-112,60},{-112,
          20},{-66,20},{-66,-22},{-62,-22}}, color={0,0,127}));
  connect(TTanTop, t5On.u1) annotation (Line(points={{-160,60},{-112,60},{-112,
          20},{-66,20},{-66,-64},{-62,-64}}, color={0,0,127}));
  connect(TTanTop, greEqu3.u2) annotation (Line(points={{-160,60},{-112,60},{-112,
          20},{-66,20},{-66,-68},{-62,-68}}, color={0,0,127}));
  connect(max.y, t2Off.u1) annotation (Line(points={{-78,-100},{-72,-100},{-72,
          -14},{-56,-14}},   color={0,0,127}));
  connect(TTanBot, t4Off.u2) annotation (Line(points={{-160,-110},{-120,-110},{
          -120,-160},{-70,-160},{-70,-148},{-62,-148}}, color={0,0,127}));
  annotation (
  defaultComponentName="conColSid",
  Diagram(coordinateSystem(extent={{-140,-160},{140,160}})),
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
</html>"));
end ColdSideControlleUO;
