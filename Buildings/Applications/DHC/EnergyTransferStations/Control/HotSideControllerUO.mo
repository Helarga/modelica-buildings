within Buildings.Applications.DHC.EnergyTransferStations.Control;
block HotSideControllerUO
  "Controller for valves on hot side, and heat demand on heat pump"
  extends
    Buildings.Applications.DHC.EnergyTransferStations.Control.HotColdSideControllerUO;
  Modelica.Blocks.Interfaces.BooleanOutput reqHea
    "True if heat is required from heat pump, false otherwise" annotation (
      Placement(transformation(extent={{140,128},{160,148}}),
                                                            iconTransformation(
          extent={{100,80},{120,100}})));
  Buildings.Controls.OBC.CDL.Continuous.Min min
    annotation (Placement(transformation(extent={{-100,-110},{-80,-90}})));
equation
  connect(rejFulLoa.active, rejFulHexBor)
    annotation (Line(points={{56,49},{56,-48},{150,-48}}, color={255,0,255}));
  connect(min.u1, TTanTop) annotation (Line(points={{-102,-94},{-120,-94},{-120,
          60},{-160,60}},
                     color={0,0,127}));
  connect(TTanTop, greEqu.u2) annotation (Line(points={{-160,60},{-120,60},{
          -120,30},{-100,30}},
                          color={0,0,127}));
  connect(TTanTop, greEqu5.u2) annotation (Line(points={{-160,60},{-120,60},{-120,
          -160},{-70,-160},{-70,-148},{-62,-148}}, color={0,0,127}));
  connect(min.u2, TTanBot) annotation (Line(points={{-102,-106},{-112,-106},{
          -112,-60},{-160,-60}},
                            color={0,0,127}));
  connect(TTanBot, greEqu1.u1) annotation (Line(points={{-160,-60},{-112,-60},{
          -112,18},{-66,18},{-66,10},{-60,10}},
                                         color={0,0,127}));
  connect(TTanBot, greEqu2.u1) annotation (Line(points={{-160,-60},{-112,-60},{
          -112,18},{-66,18},{-66,-24},{-60,-24}},
                                             color={0,0,127}));
  connect(TTanBot, greEqu3.u2) annotation (Line(points={{-160,-60},{-112,-60},{
          -112,18},{-66,18},{-66,-70},{-62,-70}},
                                             color={0,0,127}));
  connect(min.y, greEqu4.u1) annotation (Line(points={{-78,-100},{-70,-100},{
          -70,-112},{-62,-112}}, color={0,0,127}));
  connect(runHP.active, reqHea) annotation (Line(points={{6,109},{6,106},{24,
          106},{24,138},{150,138}}, color={255,0,255}));
  connect(rejFulLoa.active, or2.u1)
    annotation (Line(points={{56,49},{56,-80},{58,-80}}, color={255,0,255}));
  annotation (
  defaultComponentName="conHotSid",
  Diagram(coordinateSystem(extent={{-140,-180},{140,160}})),
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
</html>"),
    Icon(coordinateSystem(extent={{-100,-100},{100,100}})));
end HotSideControllerUO;
