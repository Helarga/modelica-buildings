within Buildings.Applications.DHC.EnergyTransferStations.Control;
block HotSideControllerUO
  "Controller for valves on hot side, and heat demand on heat pump"
 extends
    Buildings.Applications.DHC.EnergyTransferStations.Control.HotColdSideControllerUO(
    addPar(p=THys, k=1),
    addPar1(p=THys, k=1),
    addPar2(p=THys + 0.5, k=1));
  Modelica.Blocks.Interfaces.BooleanOutput reqHea
    "True if heat is required from heat pump, false otherwise" annotation (
      Placement(transformation(extent={{140,12},{160,32}}), iconTransformation(
          extent={{100,80},{120,100}})));
  Buildings.Controls.OBC.CDL.Continuous.Min min
    annotation (Placement(transformation(extent={{-100,-4},{-80,16}})));
  Buildings.Controls.OBC.CDL.Interfaces.BooleanInput           ReqHea
    "Heating is required Boolean signal"
    annotation (Placement(transformation(extent={{-166,26},{-138,54}}),
        iconTransformation(extent={{-128,76},{-100,104}})));
  Modelica.Blocks.Interfaces.RealInput TBorOut(final unit="K")
    "Water temperature at borfield outlet" annotation (Placement(transformation(
          extent={{-164,-140},{-140,-116}}), iconTransformation(extent={{-120,-110},
            {-100,-90}})));
  Modelica.Blocks.Interfaces.RealInput TBorHeaSet(final unit="K")
    "Setpoint heating temperature of the borefield " annotation (Placement(
        transformation(extent={{-164,-160},{-140,-136}}), iconTransformation(
          extent={{-120,-110},{-100,-90}})));
  Modelica.Blocks.Interfaces.RealInput TTanTop(final unit="K", displayUnit="degC")
    "Temperature at top of tank"
    annotation (Placement(transformation(extent={{-168,-2},{-140,26}}),
        iconTransformation(extent={{-140,20},{-100,60}})));
  Modelica.Blocks.Interfaces.RealInput TTanBot(final unit="K", displayUnit="degC")
    "Temperature at bottom of tank"
    annotation (Placement(transformation(extent={{-168,54},{-140,82}}),
        iconTransformation(extent={{-140,-80},{-100,-40}})));
  Buildings.Controls.OBC.CDL.Logical.Not not1
    annotation (Placement(transformation(extent={{-76,18},{-56,38}})));
  Buildings.Controls.OBC.CDL.Interfaces.BooleanInput ReqCoo
    "Cooling is required Boolean signal" annotation (Placement(transformation(
          extent={{-160,-82},{-140,-62}}), iconTransformation(extent={{-128,76},
            {-100,104}})));
  Modelica.Blocks.MathBoolean.And
                              and2(nu=3)
    annotation (Placement(transformation(extent={{-10,-10},{10,10}},
        rotation=90,
        origin={6,-30})));
  Modelica.Blocks.Interfaces.RealInput TBorIn(final unit="K")
    "Water temperature at borfield inlet" annotation (Placement(transformation(
          extent={{-162,-52},{-140,-30}}), iconTransformation(extent={{-120,-110},
            {-100,-90}})));
  Modelica.Blocks.Interfaces.BooleanOutput rejBor
    "Reject heating load using borefield" annotation (Placement(transformation(
          extent={{140,-138},{160,-118}}), iconTransformation(extent={{100,50},
            {120,70}})));
equation
  connect(min.u2, TTanBot) annotation (Line(points={{-102,0},{-128,0},{-128,68},
          {-154,68}},       color={0,0,127}));
  connect(min.u1, TTanTop) annotation (Line(points={{-102,12},{-154,12}},
                     color={0,0,127}));
  connect(rejFulLoa.active, rejFulHexBor)
    annotation (Line(points={{56,49},{56,-48},{150,-48}}, color={255,0,255}));
  connect(rejFulLoa.active, or2.u1)
    annotation (Line(points={{56,49},{56,-90},{62,-90}}, color={255,0,255}));
  connect(t4Off.y, t4.condition) annotation (Line(
      points={{-30,-90},{24,-90},{24,82},{24,82}},
      color={255,0,255},
      pattern=LinePattern.Dot));
  connect(t5On.y, t5.condition) annotation (Line(
      points={{-32,-138},{36,-138},{36,48}},
      color={255,0,255},
      pattern=LinePattern.Dot));
  connect(t2Off.y, t2.condition) annotation (Line(
      points={{-32,-14},{44,-14},{44,108}},
      color={255,0,255},
      pattern=LinePattern.Dot));
  connect(rejParLoa.active, or2.u2) annotation (Line(points={{-6,69},{-6,12},{
          48,12},{48,-98},{62,-98}},
                                  color={255,0,255}));
  connect(runHP.active, reqHea)
    annotation (Line(points={{6,109},{6,22},{150,22}}, color={255,0,255}));
  connect(t1On.y, t1.condition) annotation (Line(
      points={{-54,68},{-48,68},{-48,100},{-24,100},{-24,108}},
      color={255,0,255},
      pattern=LinePattern.Dot));
  connect(ReqHea, not1.u) annotation (Line(
      points={{-152,40},{-116,40},{-116,28},{-78,28}},
      color={255,0,255},
      pattern=LinePattern.Dot));
  connect(not1.y, t6.condition) annotation (Line(
      points={{-54,28},{76,28},{76,48}},
      color={255,0,255},
      pattern=LinePattern.Dot));
  connect(min.y, t2Off.u1) annotation (Line(points={{-78,6},{-70,6},{-70,-14},{-56,
          -14}}, color={0,0,127}));
  connect(TTanTop, t1On.u2) annotation (Line(points={{-154,12},{-122,12},{-122,
          54},{-86,54},{-86,60},{-78,60}},
                                       color={0,0,127}));
  connect(t3On.y, and2.u[1]) annotation (Line(points={{-32,-52},{1.33333,-52},{1.33333,
          -40}}, color={255,0,255}));
  connect(ReqCoo, and2.u[2])
    annotation (Line(points={{-150,-72},{6,-72},{6,-40}}, color={255,0,255}));
  connect(and2.y, t3.condition) annotation (Line(
      points={{6,-18.5},{6,8},{-24,8},{-24,68}},
      color={255,0,255},
      pattern=LinePattern.Dot));
  connect(TBorHeaSet, addPar1.u) annotation (Line(points={{-152,-148},{-116,
          -148},{-116,-94},{-102,-94}},
                                  color={0,0,127}));
  connect(TBorOut, t4Off.u1) annotation (Line(points={{-152,-128},{-128,-128},{-128,
          -74},{-64,-74},{-64,-90},{-54,-90}}, color={0,0,127}));
  connect(TBorHeaSet, addPar.u) annotation (Line(points={{-152,-148},{-116,-148},
          {-116,-60},{-102,-60}}, color={0,0,127}));
  connect(TBorIn, t3On.u1) annotation (Line(points={{-151,-41},{-68,-41},{-68,-52},
          {-56,-52}}, color={0,0,127}));
  connect(TBorHeaSet, addPar2.u)
    annotation (Line(points={{-152,-148},{-102,-148}}, color={0,0,127}));
  connect(addPar2.y, t5On.u2) annotation (Line(points={{-78,-148},{-68,-148},{-68,
          -146},{-56,-146}}, color={0,0,127}));
  connect(TBorOut, t5On.u1) annotation (Line(points={{-152,-128},{-68,-128},{-68,
          -138},{-56,-138}}, color={0,0,127}));
  connect(t2Off.y, and2.u[3]) annotation (Line(points={{-32,-14},{-20,-14},{-20,
          -46},{10.6667,-46},{10.6667,-40}},
                                         color={255,0,255},
      pattern=LinePattern.Dot));
  connect(rejParLoa.active, rejBor) annotation (Line(points={{-6,69},{-6,12},{
          48,12},{48,-128},{150,-128}}, color={255,0,255}));
  annotation (
  defaultComponentName="conHotSid",
  Diagram(coordinateSystem(extent={{-140,-160},{140,160}})),
    Documentation(info="<html>
<p>
This block is a finite state machine controller which transitions hot side operational modes for
<a href=\"Buildings.DistrictHeatingCooling.EnergyTransferStations.EnergyTransferStation.Substation\">
Buildings.DistrictHeatingCooling.EnergyTransferStations.EnergyTransferStation.Substation</a> by generating the status of
<ol>
<li>
The boolean output signal<code>reqHea</code>, when it is true means that the top level water temperature of the hot buffer tank <code>T<sub>HeaTop</sub></code> is
lower than the heating setpoint temperature <code>T<sub>HeaSet</sub></code>
</li>
<li>
The boolean output signal <code>valSta</code>, when it is true means that the rejection of surplus heating energy
is required either to the borefiled or to the district system.
</li>
<li>
The rejection to the borefield signal <code> </code> is constrained by three simultaneous conditions<code>reqCoo</code> is true which indicates that necessity to reject heat through the heatpump condenser,
the minimum temperature inside the hot buffer tank is higher than the set point temperature<code>T<sub>HeaSet</sub></code>
and finally the borfield water inlet temperature is lower than heating charging setpoint temperature
<code>T<sub>BorHeaSet</code>.
</li>


<li>



<li>
Reject surplus load to both the borefiled and the district system on and off.
</li>
</ol>


</html>", revisions="<html>

<li>
 <br/>

</li>
</ul>
</html>"));
end HotSideControllerUO;
