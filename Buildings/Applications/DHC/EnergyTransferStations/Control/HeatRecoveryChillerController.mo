within Buildings.Applications.DHC.EnergyTransferStations.Control;
model HeatRecoveryChillerController
  "The control block of the heatrecovery chiller"
     extends Modelica.Blocks.Icons.Block;

  Buildings.Controls.OBC.CDL.Interfaces.BooleanInput ReqHea
    "Heating is required Boolean signal"
    annotation (Placement(transformation(extent={{-128,6},{-100,34}}),
        iconTransformation(extent={{-128,76},{-100,104}})));
  Buildings.Controls.OBC.CDL.Interfaces.BooleanInput ReqCoo
  "Cooling is required Boolean signal"
    annotation (Placement(transformation(extent={{-128,-64},{-100,-36}}),
        iconTransformation(extent={{-128,-104},{-100,-76}})));
  Buildings.Controls.OBC.CDL.Interfaces.IntegerOutput heaPumMod
  "Heatpump operational mode"
   annotation (Placement(transformation(extent={{100,-14},{128,14}}),
        iconTransformation(extent={{100,-14},{128,14}})));
  Modelica.Blocks.Logical.Or or1
    annotation (Placement(transformation(extent={{-20,10},{0,30}})));
  Controls.OBC.CDL.Logical.Switch swi1
    annotation (Placement(transformation(extent={{36,-10},{56,10}})));
  Controls.OBC.CDL.Logical.Switch swi2
    annotation (Placement(transformation(extent={{-20,-60},{0,-40}})));
  Controls.OBC.CDL.Continuous.Sources.Constant cooModHeaPum(k=-1)
    "Heating mode signal for the heatpump =-1"
    annotation (Placement(transformation(extent={{-60,-40},{-40,-20}})));
  Modelica.Blocks.Logical.And and1
    annotation (Placement(transformation(extent={{-60,-10},{-40,10}})));
  Controls.OBC.CDL.Continuous.Sources.Constant heaModHeaPum(k=1)
    "Heating mode signal for the heatpump =1"
    annotation (Placement(transformation(extent={{0,40},{20,60}})));
  Modelica.Blocks.Math.RealToInteger realToInteger
    annotation (Placement(transformation(extent={{68,-10},{88,10}})));
  Controls.OBC.CDL.Continuous.Sources.Constant shuOffSig(k=0)
    "HeatPump shut off signal =0"
    annotation (Placement(transformation(extent={{-60,-80},{-40,-60}})));

equation

  connect(ReqCoo,swi2. u2)
   annotation (Line(points={{-114,-50},{-22,-50}},
                           color={255,0,255}));
  connect(ReqHea, and1.u1)
   annotation (Line(points={{-114,20},{-70,20},{-70,0},{-62,0}},
                   color={255,0,255}));
  connect(ReqCoo, and1.u2)
   annotation (Line(points={{-114,-50},{-70,-50},{-70,
          -8},{-62,-8}},
                     color={255,0,255}));
  connect(ReqHea, or1.u1)
   annotation (Line(points={{-114,20},{-22,20}},
                    color={255,0,255}));
  connect(swi1.u2, or1.y)
   annotation (Line(points={{34,0},{10,0},{10,20},{1,20}},
        color={255,0,255}));
  connect(or1.u2, and1.y)
   annotation (Line(points={{-22,12},{-30,12},{-30,0},{
          -39,0}},
               color={255,0,255}));
  connect(swi1.u3,swi2. y)
   annotation (Line(points={{34,-8},{26,-8},{26,-50},{1,
          -50}}, color={0,0,127}));
  connect(swi1.u1, heaModHeaPum.y)
   annotation (Line(points={{34,8},{26,8},{26,50},{21,50}}, color={0,0,127}));
  connect(swi1.y, realToInteger.u)
   annotation (Line(points={{57,0},{66,0}}, color={0,0,127}));
  connect(swi2.u1, cooModHeaPum.y)
   annotation (Line(points={{-22,-42},{-38,-42},
          {-38,-30},{-39,-30}},color={0,0,127}));
  connect(swi2.u3, shuOffSig.y)
   annotation (Line(points={{-22,-58},{-38,-58},{-38,-70},{-39,-70}},
                               color={0,0,127}));
  connect(realToInteger.y, heaPumMod)
   annotation (Line(points={{89,0},{114,0}}, color={255,127,0}));
  annotation (defaultComponentName="heaPumCon",Icon(coordinateSystem(preserveAspectRatio=false, extent={{-100,
            -100},{100,80}})),                                   Diagram(
        coordinateSystem(preserveAspectRatio=false, extent={{-100,-100},{100,80}})),
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
end HeatRecoveryChillerController;
