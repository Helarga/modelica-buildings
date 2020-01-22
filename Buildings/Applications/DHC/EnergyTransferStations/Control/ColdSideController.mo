within Buildings.Applications.DHC.EnergyTransferStations.Control;
model ColdSideController "State machine controls the operation of the EIR chiller, two way cooling valve, borfield and district pumps "

  extends Buildings.Applications.DHC.EnergyTransferStations.BaseClasses.HotColdSideController(
    THys=THys,
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
  connect(or1.u1, rejFulLoaSta.active) annotation (Line(points={{86,-20},{76,-20},
          {76,12},{56,12},{56,49}}, color={255,0,255}));
  connect(or1.y, rejFulLoa) annotation (Line(points={{110,-20},{116,-20},{116,-48},
          {150,-48}}, color={255,0,255}));
  connect(or1.y, or2.u1) annotation (Line(points={{110,-20},{116,-20},{116,-48},
          {40,-48},{40,-80},{58,-80}}, color={255,0,255}));
  connect(yVal,booToRea. y) annotation (Line(points={{150,-100},{122,-100}},
                     color={0,0,127}));
  connect(valSta,or2. y)
    annotation (Line(points={{150,-80},{82,-80}}, color={255,0,255}));
  connect(or2.u2, rejParLoaSta.active)
    annotation (Line(points={{58,-88},{-6,-88},{-6,69}}, color={255,0,255}));
  connect(booToRea.u,or2. y) annotation (Line(points={{98,-100},{90,-100},{90,
          -80},{82,-80}}, color={255,0,255}));
  connect(or1.y,or2. u1) annotation (Line(points={{110,-20},{116,-20},{116,-48},
          {40,-48},{40,-80},{58,-80}}, color={255,0,255}));
  connect(max.u1, TTop) annotation (Line(points={{-102,-94},{-112,-94},{-112,60},
          {-160,60}}, color={0,0,127}));
  connect(frePro.u, TTop) annotation (Line(points={{48,-34},{44,-34},{44,-44},{
          -66,-44},{-66,22},{-112,22},{-112,60},{-160,60}}, color={0,0,127}));
  connect(TTop, greEqu1.u1) annotation (Line(points={{-160,60},{-112,60},{-112,22},{-66,
          22},{-66,8},{-62,8}}, color={0,0,127}));
  connect(TTop, greEqu2.u1) annotation (Line(points={{-160,60},{-112,60},{-112,
          22},{-66,22},{-66,-22},{-62,-22}}, color={0,0,127}));
  connect(TTop, greEqu3.u2) annotation (Line(points={{-160,60},{-112,60},{-112,
          22},{-66,22},{-66,-68},{-62,-68}}, color={0,0,127}));
  connect(max.y, greEqu4.u1) annotation (Line(points={{-78,-100},{-70,-100},{-70,
          -110},{-62,-110}},     color={0,0,127}));
  connect(max.u2, TBot) annotation (Line(points={{-102,-106},{-120,-106},{-120,
          -60},{-160,-60}}, color={0,0,127}));
  connect(TBot, greEqu5.u2) annotation (Line(points={{-160,-60},{-120,-60},{-120,
          -160},{-70,-160},{-70,-148},{-62,-148}}, color={0,0,127}));
  connect(TBot, greEqu.u2) annotation (Line(points={{-160,-60},{-120,-60},{-120,
          32},{-102,32}}, color={0,0,127}));
  annotation (
  defaultComponentName="conColSid",
  Diagram(coordinateSystem(extent={{-140,-160},{140,160}}), graphics={Text(
          extent={{-36,-32},{40,-40}},
          lineColor={28,108,200},
          textString="reject full load if tank exceeds setpoint by 3*THys",
          horizontalAlignment=TextAlignment.Left)}),
          Icon(coordinateSystem(extent={{-100,-100},{100,100}})),
Documentation(info="<html>
  
<p>
  This block is a state machine controller which transitions the <a href=\"Buildings.Applications.DHC.EnergyTransferStations.SubstationWithConstPrimPum_OnOffChiller\">
Buildings.Applications.DHC.EnergyTransferStations.SubstationWithConstPrimPum_OnOffChiller</a> operational modes:
<ul>
<li>
Cooling generation i.e. EIR chiller/ heat pump on and off.
</li>
</ul>
<ul>
<li>
First stage reject part load i.e. to the borefield system.
</li>
</ul>
<ul>
<li>
Second stage reject full load i.e. to both the borefield and the district system.
</li>
</ul>
</p>
<h4> Controller elaboration</h4> 
<p>
The scheme below represents the substation cold side state machine which generates the status of
<ol>
<li>
The Boolean output signal <code>reqCoo</code>, true when the bottom level water temperature of the cold buffer tank <code>T<sub>Bot</sub></code> is
higher than or equal to the cooling setpoint temperature <code>T<sub>Set</sub></code>.
</li>
<li>
The Boolean/real output signals <code>valSta</code>, true when the top level water temperature of the cold buffer tank <code>T<sub>Top</sub></code> is
lower than the cooling setpoint temperature <code>T<sub>Set</sub></code> minus the defined hysteresis. In addition,
it indicates that the rejection of surplus cooling energy is partially required to the borefield.
</li>
<li>
The Boolean output signal <code>rejFulHexBor </code>, true when the top level water temperature of the cold buffer tank <code>T<sub>Top</sub></code> is
lower than or equal to the cooling setpoint temperature <code>T<sub>Set</sub></code> minus the defined hysteresis. 
The full heat rejection indicates two simultaneous steps
<ul>
<li>
The borefield pump runs on its maximum flow rate.
</li>
<li>
The district heat exchanger pump switches on.
</li> 

</ol>
<p align= \"center\">
<img alt=\"State finite machine for the hot side\"
src=\"modelica://Buildings/Resources/Images/Applications/DHC/EnergyTransferStations/colTanCon.png\"/>
</p>      

The table clarifies the states and associated actions
<table class=\"releaseTable\" summary=\"summary\" border=\"1\" cellspacing=0 cellpadding=2>
     <tr><td align=\"center\"><b>State</b> 
        </td>
        <td align=\"center\"><b>Action</b>
        </td>
        </tr>
    <tr><td align=\"center\">reqCoo:true
        </td>
        <td align=\"center\">Cooling generation:On
        </td>
        </tr>
    <tr><td align=\"center\">rejCooParLoa:true
        </td>
        <td align=\"center\">yVal:true, BorPum:On
        </td>
        </tr>
    <tr><td align=\"center\">rejCooFulLoa:true
        </td>
        <td align=\"center\">yVal:true, BorPum:On, DisPum:On
        </td>
        </tr>     
        </table>
<h4>Note</h4>
<ul>
<li>
<p>      
The parameter &Delta;T is the implemented hysteresis to transit from state to another. 
</p>
</li>
<li>
<p>
An on-off override controller used to start the full load rejection once the water inside the tank reaches 3.5degC to avoid freezing.
</p>
</li>
</ul>   
</html>", revisions="<html>
<ul>
<li>
November 2, 2019, by Hagar Elarga:<br/>
Added the info section.
</li>
</ul>
</html>"));
end ColdSideController;
