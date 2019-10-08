within Buildings.Applications.DHC.EnergyTransferStations.Control;
model HeatPumpController "The control block of the heatpump on heating mode"
     extends Modelica.Blocks.Icons.Block;

  Buildings.Controls.OBC.CDL.Interfaces.BooleanInput ReqHea
    "Heating is required Boolean signal"
    annotation (Placement(transformation(extent={{-128,62},{-100,90}}),
        iconTransformation(extent={{-128,76},{-100,104}})));
  Buildings.Controls.OBC.CDL.Interfaces.BooleanInput ReqCoo
    "Cooling is required Boolean signal"
    annotation (Placement(transformation(extent={{-128,24},{-100,52}}),
        iconTransformation(extent={{-128,46},{-100,74}})));

  Buildings.Controls.OBC.CDL.Interfaces.RealInput TSetHeaMin(final unit="K",
      displayUnit="degC")
    "Minimum setpoint for heating supply water to space loads" annotation (
      Placement(transformation(extent={{-130,-100},{-102,-72}}),
        iconTransformation(extent={{-120,-24},{-100,-4}})));
  Buildings.Controls.OBC.CDL.Interfaces.RealInput           TSetCoo(final unit="K",
      displayUnit="degC") "Setpoint for cooling supply water to space loads"
                                                       annotation (Placement(transformation(extent={{-128,
            -160},{-100,-132}}),
                     iconTransformation(extent={{-120,-102},{-100,-82}})));
  Modelica.Blocks.Logical.Or or1
    annotation (Placement(transformation(extent={{-46,42},{-26,62}})));
  Buildings.Controls.OBC.CDL.Logical.Switch swi1
    annotation (Placement(transformation(extent={{2,42},{22,62}})));
  Buildings.Controls.OBC.CDL.Continuous.Sources.Constant heaModHeaPum(k=1)
    "Heating mode signal for the heatpump =1"
    annotation (Placement(transformation(extent={{-46,72},{-26,92}})));
  Buildings.Controls.OBC.CDL.Continuous.Sources.Constant shuOffSig(k=0)
    "HeatPump shut off signal =0"
    annotation (Placement(transformation(extent={{-46,16},{-26,36}})));
  Modelica.Blocks.Math.RealToInteger realToInteger
    annotation (Placement(transformation(extent={{58,42},{78,62}})));
  Buildings.Controls.OBC.CDL.Interfaces.IntegerOutput yHeaPumMod
    "Heatpump operational mode" annotation (Placement(transformation(extent={{102,46},
            {122,66}}),     iconTransformation(extent={{100,-14},{128,14}})));
  Buildings.Controls.OBC.CDL.Interfaces.RealOutput TSetHeaPum(final unit="K",
      displayUnit="degC") "Setpint temperture for the heatpump" annotation (
      Placement(transformation(extent={{102,-68},{122,-48}}),
        iconTransformation(extent={{100,28},{128,56}})));
  Buildings.Controls.OBC.CDL.Logical.Switch swi2
    annotation (Placement(transformation(extent={{70,-70},{90,-50}})));
  Buildings.Controls.OBC.CDL.Interfaces.RealInput TSouLvg(final unit="K", displayUnit=
        "degC") "Source side leaving water temperature"
                                                       annotation (Placement(
        transformation(extent={{-128,-184},{-100,-156}}), iconTransformation(
          extent={{-120,-82},{-100,-62}})));
  Modelica.Blocks.Logical.And simHeaCoo "Simultaneous heating and cooling mode"
    annotation (Placement(transformation(extent={{-86,-38},{-66,-18}})));
  Buildings.Controls.Continuous.LimPID PI(
    controllerType=Modelica.Blocks.Types.SimpleController.PI,
    yMax=1,
    yMin=0,
    reset=Buildings.Types.Reset.Parameter,
    y_reset=0,
    k=0.1,
    Ti(displayUnit="s") = 300,
    reverseAction=true)
    "Resetting of heating set point tempearture in case reqCoo or (reqCoo and reqHea) are true."
    annotation (Placement(transformation(extent={{-80,-156},{-60,-136}})));
  Buildings.Controls.OBC.CDL.Continuous.Line mapFun
    "Mapping control function to reset the TsetHea"
    annotation (Placement(transformation(extent={{38,-156},{58,-136}})));
  Buildings.Controls.OBC.CDL.Continuous.Sources.Constant X1(k=0)
    "PI minimum error"
    annotation (Placement(transformation(extent={{0,-130},{20,-110}})));
  Buildings.Controls.OBC.CDL.Continuous.Sources.Constant X2(k=1)
    "PI maximum error"
    annotation (Placement(transformation(extent={{0,-180},{20,-160}})));
  Buildings.Controls.OBC.CDL.Interfaces.RealInput TSetHeaMax(final unit="K",
      displayUnit="degC") "Maximum setpoint for heating water " annotation (
      Placement(transformation(extent={{-128,-208},{-100,-180}}),
        iconTransformation(extent={{-120,-62},{-100,-42}})));
  Buildings.Controls.OBC.CDL.Interfaces.RealInput TSetHea(final unit="K",
      displayUnit="degC") "Setpoint for heating supply water to space loads"
    annotation (Placement(transformation(extent={{-130,-76},{-102,-48}}),
        iconTransformation(extent={{-120,-42},{-100,-22}})));
  Buildings.Controls.OBC.CDL.Logical.Switch swi4
    annotation (Placement(transformation(extent={{0,-80},{20,-60}})));
  Buildings.Controls.OBC.CDL.Logical.Not not1
    annotation (Placement(transformation(extent={{-8,-32},{12,-12}})));
  Modelica.Blocks.Logical.And heaOnl "Heating only mode"
    annotation (Placement(transformation(extent={{34,-16},{54,4}})));
equation

  connect(ReqCoo, or1.u2)
    annotation (Line(points={{-114,38},{-78,38},{-78,44},{-48,44}},
                                                  color={255,0,255}));
  connect(ReqHea, or1.u1)
    annotation (Line(points={{-114,76},{-56,76},{-56,52},{-48,52}},
                     color={255,0,255}));
  connect(or1.y, swi1.u2)
    annotation (Line(points={{-25,52},{0,52}},  color={255,0,255}));
  connect(swi1.u1, heaModHeaPum.y)
    annotation (Line(points={{0,60},{-16,60},{-16,82},{-24,82}},
                             color={0,0,127}));
  connect(swi1.u3, shuOffSig.y)
    annotation (Line(points={{0,44},{-16,44},{-16,26},{-24,26}},
                         color={0,0,127}));
  connect(realToInteger.y, yHeaPumMod)
    annotation (Line(points={{79,52},{96,52},{96,56},{112,56}},
                                                color={255,127,0}));
  connect(swi1.y, realToInteger.u)
    annotation (Line(points={{24,52},{56,52}}, color={0,0,127}));
  connect(swi2.y, TSetHeaPum)
    annotation (Line(points={{92,-60},{104,-60},{104,-58},{112,-58}},
                                                  color={0,0,127}));
  connect(ReqCoo, simHeaCoo.u2) annotation (Line(points={{-114,38},{-96,38},{
          -96,-36},{-88,-36}}, color={255,0,255}));
  connect(ReqHea, simHeaCoo.u1) annotation (Line(points={{-114,76},{-92,76},{
          -92,-28},{-88,-28}}, color={255,0,255}));
  connect(PI.u_s, TSetCoo)
    annotation (Line(points={{-82,-146},{-114,-146}},
                                                    color={0,0,127}));
  connect(TSouLvg, PI.u_m) annotation (Line(points={{-114,-170},{-70,-170},{-70,
          -158}}, color={0,0,127}));
  connect(ReqCoo, PI.trigger) annotation (Line(points={{-114,38},{-96,38},{-96,
          -158},{-78,-158}},      color={255,0,255}));
  connect(X1.y, mapFun.x1) annotation (Line(points={{22,-120},{22,-138},{36,
          -138}},     color={0,0,127}));
  connect(PI.y, mapFun.u)
    annotation (Line(points={{-59,-146},{36,-146}},color={0,0,127}));
  connect(TSetHeaMax, mapFun.f2) annotation (Line(points={{-114,-194},{26,-194},
          {26,-154},{36,-154}}, color={0,0,127}));
  connect(X2.y, mapFun.x2) annotation (Line(points={{22,-170},{22,-150},{36,
          -150}},     color={0,0,127}));
  connect(simHeaCoo.y, swi4.u2) annotation (Line(points={{-65,-28},{-54,-28},{
          -54,-70},{-2,-70}}, color={255,0,255}));
  connect(TSetHea, swi4.u1)
    annotation (Line(points={{-116,-62},{-2,-62}}, color={0,0,127}));
  connect(TSetHea, swi2.u1) annotation (Line(points={{-116,-62},{-88,-62},{-88,
          -52},{68,-52}}, color={0,0,127}));
  connect(swi4.y, mapFun.f1) annotation (Line(points={{22,-70},{30,-70},{30,
          -142},{36,-142}}, color={0,0,127}));
  connect(TSetHeaMin, swi4.u3) annotation (Line(points={{-116,-86},{-14,-86},{
          -14,-78},{-2,-78}}, color={0,0,127}));
  connect(mapFun.y, swi2.u3) annotation (Line(points={{60,-146},{66,-146},{66,
          -68},{68,-68}}, color={0,0,127}));
  connect(ReqHea, heaOnl.u1) annotation (Line(points={{-114,76},{-56,76},{-56,
          -6},{32,-6}}, color={255,0,255}));
  connect(ReqCoo, not1.u) annotation (Line(points={{-114,38},{-60,38},{-60,-22},
          {-10,-22}}, color={255,0,255}));
  connect(heaOnl.y, swi2.u2) annotation (Line(points={{55,-6},{62,-6},{62,-60},
          {68,-60}}, color={255,0,255}));
  connect(not1.y, heaOnl.u2) annotation (Line(points={{14,-22},{20,-22},{20,-14},
          {32,-14}}, color={255,0,255}));
  annotation (defaultComponentName="heaPumCon",Icon(coordinateSystem(preserveAspectRatio=false, extent={{-100,
            -100},{100,80}})),                                   Diagram(
        coordinateSystem(preserveAspectRatio=false, extent={{-100,-240},{100,
            100}}),
        graphics={
        Rectangle(
          extent={{-98,100},{102,18}},
          lineColor={28,108,200},
          fillColor={215,215,215},
          fillPattern=FillPattern.Solid),
        Text(
          extent={{-14,106},{96,82}},
          lineColor={0,0,255},
          fillColor={215,215,215},
          fillPattern=FillPattern.None,
          textString="Heatpump operational mode"),
        Text(
          extent={{-42,-228},{102,-246}},
          lineColor={0,0,255},
          fillColor={215,215,215},
          fillPattern=FillPattern.None,
          textString="Reset of water setpoint temperature")}),
                Documentation(info="<html>
<p>
The block is applied for the water to water heat pump control. It outputs the heat pump status
and resets the water temperature setpoint input signal <based on the operational mode.
</p>
<h4>Heat pump status</h4>
<p>
If either <code>reqHea</code> or <code>reqCoo</code> is true, the controller outputs the integer output
<code>yHeaPumMod</code> to switch on the heat pump, other wise it switches off. 

<h4>Modes of operation</h4> 
<ul>
<li> 
Heating-only mode, the leaving heating water from the load side is determined by the heating set point<code>TSetHea</code>
and the leaving chilled water from source side floats depending on the source water temperature and flow rate.
</li>
<li> 
cooling-only and simultaneous cooling and heating modes, the sequence resets the heating set point<code>TSetHea</code> till the the leaving chilled water 
from the source side meets the cooling set point<code>TSetCoo</code> as shown below in the figure
</p>
<p align=\"center\">
<img alt=\"Image PI controller to reset the TSetHea\"
src=\"modelica://Buildings/Resources/Images/Applications/DHC/EnergyTransferStations/resetTsetHea.png\"/>
</p> 

<p>
Hence, according to the below figure, when the source leaving water temperature is higher than <code>TSetCoo</code>, TSetHea is increased.
</p>
<p align=\"center\">
<img alt=\"Image Control Mapping function of resetting TsetHea\"
src=\"modelica://Buildings/Resources/Images/Applications/DHC/EnergyTransferStations/controlMappingFunction.png\"/>
</p>  


See <a href=\"Buildings.Fluid.HeatPumps.EquationFitReversible\">
Buildings.Fluid.HeatPumps.EquationFitReversible</a> for detailed description of the heatpump theory of operation.

</html>", revisions="<html>
<ul>
<li>
 <br/>
</li>
</ul>
</html>"));
end HeatPumpController;
