within Buildings.Applications.DHC.EnergyTransferStations.Control;
model HeatPumpController "The control block of the heatpump on heating mode"
     extends Modelica.Blocks.Icons.Block;

  Buildings.Controls.OBC.CDL.Interfaces.BooleanInput ReqHea
    "Heating is required Boolean signal"
    annotation (Placement(transformation(extent={{-128,54},{-100,82}}),
        iconTransformation(extent={{-128,76},{-100,104}})));
  Buildings.Controls.OBC.CDL.Interfaces.BooleanInput ReqCoo
  "Cooling is required Boolean signal"
    annotation (Placement(transformation(extent={{-128,16},{-100,44}}),
        iconTransformation(extent={{-128,-104},{-100,-76}})));

  Buildings.Controls.OBC.CDL.Interfaces.RealInput           TSetHea(final unit="K",
      displayUnit="degC")
    "Setpoint for heating supply water to space loads" annotation (Placement(transformation(extent={{-128,
            -48},{-100,-20}}),
                    iconTransformation(extent={{-120,30},{-100,50}})));
  Buildings.Controls.OBC.CDL.Interfaces.RealInput           TSetCoo(final unit="K",
      displayUnit="degC") "Setpoint for cooling supply water to space loads"
                                                       annotation (Placement(transformation(extent={{-128,
            -130},{-100,-102}}),
                     iconTransformation(extent={{-120,-68},{-100,-48}})));
  Modelica.Blocks.Logical.Or or1
    annotation (Placement(transformation(extent={{-40,28},{-20,48}})));
  Buildings.Controls.OBC.CDL.Logical.Switch swi1
    annotation (Placement(transformation(extent={{0,28},{20,48}})));
  Buildings.Controls.OBC.CDL.Continuous.Sources.Constant heaModHeaPum(k=1)
    "Heating mode signal for the heatpump =1"
    annotation (Placement(transformation(extent={{-50,54},{-30,74}})));
  Buildings.Controls.OBC.CDL.Continuous.Sources.Constant shuOffSig(k=0)
    "HeatPump shut off signal =0"
    annotation (Placement(transformation(extent={{-48,2},{-28,22}})));
  Modelica.Blocks.Math.RealToInteger realToInteger
    annotation (Placement(transformation(extent={{60,28},{80,48}})));
  Buildings.Controls.OBC.CDL.Interfaces.IntegerOutput heaPumMod
  "Heatpump operational mode"
   annotation (Placement(transformation(extent={{100,28},{120,48}}),
        iconTransformation(extent={{100,-14},{128,14}})));
  Buildings.Controls.OBC.CDL.Interfaces.RealOutput TSetCon( final unit="K", displayUnit=
        "degC")
    "Setpint temperture for the heatpump" annotation (Placement(transformation(
          extent={{100,-52},{120,-32}}), iconTransformation(extent={{100,28},{128,
            56}})));
  Buildings.Controls.OBC.CDL.Logical.Switch swi2
    annotation (Placement(transformation(extent={{76,-52},{96,-32}})));
  Buildings.Controls.OBC.CDL.Logical.Switch swi3
    annotation (Placement(transformation(extent={{34,-60},{54,-40}})));
  Buildings.Controls.OBC.CDL.Continuous.Sources.Constant MinSetTem(k=25 + 273.15)
    "Minimum setpoint condenser leaving water temperature"
    annotation (Placement(transformation(extent={{-20,-166},{0,-146}})));
  Buildings.Controls.OBC.CDL.Interfaces.RealInput TEvaLvg(final unit="K", displayUnit=
        "degC") "Evaporator leaving water temperature" annotation (Placement(
        transformation(extent={{-128,-162},{-100,-134}}), iconTransformation(
          extent={{-120,-50},{-100,-30}})));
  Modelica.Blocks.Logical.And and1
    annotation (Placement(transformation(extent={{-52,-30},{-32,-10}})));
  Modelica.Blocks.Logical.Or or2
    annotation (Placement(transformation(extent={{-8,-30},{12,-10}})));
  Buildings.Controls.Continuous.LimPID pumConCon(
    controllerType=Modelica.Blocks.Types.SimpleController.PI,
    yMax=1,
    yMin=-1,
    reset=Buildings.Types.Reset.Parameter,
    y_reset=-1,
    k=1,
    Ti(displayUnit="s") = 300,
    reverseAction=true) "Controller for heatpump mode"
   annotation (Placement(transformation(extent={{-80,-126},{-60,-106}})));
  Modelica.Blocks.Math.Product product
    annotation (Placement(transformation(extent={{-40,-120},{-20,-100}})));
  Buildings.Controls.OBC.CDL.Interfaces.RealInput TConLvg(final unit="K", displayUnit=
        "degC") "Condenser leaving water temperature" annotation (Placement(
        transformation(extent={{-128,-80},{-100,-52}}), iconTransformation(
          extent={{-120,-26},{-100,-6}})));
  Buildings.Controls.OBC.CDL.Interfaces.RealInput TConEnt(final unit="K", displayUnit=
        "degC") "Condenser entering water temperature" annotation (Placement(
        transformation(extent={{-128,-106},{-100,-78}}), iconTransformation(
          extent={{-120,-2},{-100,18}})));
  Modelica.Blocks.Math.Add add(k2=-1)
    annotation (Placement(transformation(extent={{-80,-96},{-60,-76}})));
  Modelica.Blocks.Math.Add add1
    annotation (Placement(transformation(extent={{-8,-82},{12,-62}})));
equation

  connect(ReqCoo, or1.u2)
    annotation (Line(points={{-114,30},{-42,30}}, color={255,0,255}));
  connect(ReqHea, or1.u1)
    annotation (Line(points={{-114,68},{-62,68},{-62,38},
          {-42,38}}, color={255,0,255}));
  connect(or1.y, swi1.u2)
    annotation (Line(points={{-19,38},{-2,38}}, color={255,0,255}));
  connect(swi1.u1, heaModHeaPum.y)
    annotation (Line(points={{-2,46},{-16,46},{
          -16,64},{-28,64}}, color={0,0,127}));
  connect(swi1.u3, shuOffSig.y)
    annotation (Line(points={{-2,30},{-16,30},{-16,
          12},{-26,12}}, color={0,0,127}));
  connect(realToInteger.y,heaPumMod)
    annotation (Line(points={{81,38},{110,38}},
                                             color={255,127,0}));
  connect(swi1.y, realToInteger.u)
    annotation (Line(points={{22,38},{58,38}}, color={0,0,127}));
  connect(TSetHea, swi2.u1)
    annotation (Line(points={{-114,-34},{74,-34}}, color={0,0,127}));
  connect(ReqCoo, swi3.u2)
    annotation (Line(points={{-114,30},{-96,30},{-96,-50},
          {32,-50}}, color={255,0,255}));
  connect(swi3.u3, MinSetTem.y)
    annotation (Line(points={{32,-58},{24,-58},{24,
          -156},{2,-156}}, color={0,0,127}));
  connect(swi3.y, swi2.u3)
    annotation (Line(points={{56,-50},{74,-50}},
                     color={0,0,127}));
  connect(swi2.y, TSetCon)
    annotation (Line(points={{98,-42},{110,-42}}, color={0,0,127}));
  connect(ReqCoo, and1.u2)
    annotation (Line(points={{-114,30},{-96,30},{-96,-28},
          {-54,-28}}, color={255,0,255}));
  connect(ReqHea, and1.u1)
    annotation (Line(points={{-114,68},{-92,68},{-92,-20},
          {-54,-20}}, color={255,0,255}));
  connect(ReqHea, or2.u1)
    annotation (Line(points={{-114,68},{-92,68},{-92,-4},
          {-16,-4},{-16,-20},{-10,-20}}, color={255,0,255}));
  connect(and1.y, or2.u2)
    annotation (Line(points={{-31,-20},{-26,-20},{-26,-28},
          {-10,-28}}, color={255,0,255}));
  connect(or2.y, swi2.u2)
    annotation (Line(points={{13,-20},{70,-20},{70,-42},{
          74,-42}}, color={255,0,255}));
  connect(pumConCon.u_s, TSetCoo)
    annotation (Line(points={{-82,-116},{-114,-116}}, color={0,0,127}));
  connect(TEvaLvg, pumConCon.u_m)
    annotation (Line(points={{-114,-148},{-70,
          -148},{-70,-128}}, color={0,0,127}));
  connect(pumConCon.y, product.u2)
    annotation (Line(points={{-59,-116},{-42,
          -116}},                       color={0,0,127}));
  connect(ReqCoo, pumConCon.trigger)
    annotation (Line(points={{-114,30},{-96,30},
          {-96,-140},{-78,-140},{-78,-128}}, color={255,0,255}));
  connect(TConLvg, add.u1)
    annotation (Line(points={{-114,-66},{-96,-66},{-96,
          -80},{-82,-80}}, color={0,0,127}));
  connect(TConEnt, add.u2)
    annotation (Line(points={{-114,-92},{-82,-92}}, color={0,0,127}));
  connect(add.y, product.u1)
    annotation (Line(points={{-59,-86},{-46,-86},{-46,
          -104},{-42,-104}}, color={0,0,127}));
  connect(TConLvg, add1.u1)
    annotation (Line(points={{-114,-66},{-10,-66}}, color={0,0,127}));
  connect(add1.y, swi3.u1)
    annotation (Line(points={{13,-72},{18,-72},{18,-42},
          {32,-42}}, color={0,0,127}));
  connect(product.y, add1.u2)
    annotation (Line(points={{-19,-110},{-16,-110},{
          -16,-78},{-10,-78}}, color={0,0,127}));
  annotation (defaultComponentName="heaPumCon",Icon(coordinateSystem(preserveAspectRatio=false, extent={{-100,
            -100},{100,80}})),                                   Diagram(
        coordinateSystem(preserveAspectRatio=false, extent={{-100,-180},{100,80}}),
        graphics={
        Rectangle(
          extent={{-100,80},{100,-2}},
          lineColor={28,108,200},
          fillColor={215,215,215},
          fillPattern=FillPattern.Solid),
        Text(
          extent={{28,84},{96,66}},
          lineColor={0,0,255},
          fillColor={215,215,215},
          fillPattern=FillPattern.None,
          textString="Heatpump operational mode"),
        Text(
          extent={{-36,-170},{92,-184}},
          lineColor={0,0,255},
          fillColor={215,215,215},
          fillPattern=FillPattern.None,
          textString="Reset of heatpump condenser leaving water tempearture")}),
                Documentation(info="<html>
<p>
The controller outputs the heatpump operational mode status based on two Boolean inputs,<code>reqHea</code> when heating is required and
<code>reqCoo</code> when cooling is required. It considers three operational modes
</p>
<ol>
<li>
Only the <code>reqHea</code>=true, in that case the controller generates an output integer signal equals to 1 which
denotes that the condenser shall operate to satisfy the heating setpoint temperature <code>THeaSet</code>
</li>
<li>
Only the <code>reqCoo</code>=true, in that case the controller generates an output integer signal equals to -1 which
denoted that the evaporator shall operate
to satisfy the cooling setpoint temperature<code>TCooSet</code>.
</li>
<li>
Both the <code>reqHea</code> and <code>reqCoo</code>= true instantaneousheating and cooling, in this case the controller prioritize the heating mode,
so that the condenser operates to satisfy first the <code>THeaSet</code> then once the <code>reqHea</code> turns to false, the heatpump
evaporator operates to satisfy the cooling set point temperature <code>TCooSet</code> if the Boolean signal <code>reqCoo</code> is still true.
</li>
</ol>
<p>
It is important to highlight that prioritizing the heating mode is not an obligatory choice, the user may choose to prioritize the cooling mode if
both heating and cooling are required.
See <a href=\"Buildings.Fluid.HeatPumps.EquationFitWaterToWater\">
Buildings.Fluid.HeatPumps.EquationFitWaterToWater</a> for detailed description of the heatpump theory of operation.

</html>", revisions="<html>
<ul>
<li>
 <br/>
</li>
</ul>
</html>"));
end HeatPumpController;
