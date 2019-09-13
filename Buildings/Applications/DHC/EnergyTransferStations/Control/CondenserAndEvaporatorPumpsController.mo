within Buildings.Applications.DHC.EnergyTransferStations.Control;
model CondenserAndEvaporatorPumpsController
  "The control block of the condenser and the evaporator water pumps"
     extends Modelica.Blocks.Icons.Block;
  Buildings.Controls.OBC.CDL.Interfaces.RealInput TEvaLvg
  "Evaporator leaving water temperature"
   annotation (Placement(transformation(extent={{-124,-140},{-100,-116}}),
        iconTransformation(extent={{-114,-46},{-100,-32}})));
  Buildings.Controls.OBC.CDL.Interfaces.RealInput TConLvg
  "Condenser leaving water temperature"
   annotation (Placement(transformation(extent={{-120,70},{-100,90}}),
        iconTransformation(extent={{-114,18},{-100,32}})));
  Buildings.Controls.OBC.CDL.Interfaces.RealInput TSetHea
  "Heating setpoint temperature or condenser leaving water temperature setpoint"
   annotation (Placement(transformation(extent={{-120,116},{-100,136}}),
        iconTransformation(extent={{-114,56},{-100,70}})));
  Buildings.Controls.OBC.CDL.Interfaces.RealInput TSetCoo
  "Cooling setpoint temperature or evaporator leaving water temperature setpoint"
   annotation (Placement(transformation(extent={{-124,-86},{-100,-62}}),
        iconTransformation(extent={{-114,-62},{-100,-48}})));
  Buildings.Controls.Continuous.LimPID pumConCon(
    controllerType=Modelica.Blocks.Types.SimpleController.PI,
    yMin=0.1,
    reset=Buildings.Types.Reset.Parameter,
    y_reset=0.1,
    k=0.1,
    Ti(displayUnit="s") = 300,
    reverseAction=false) "Controller for condenser pump speed"
   annotation (Placement(transformation(extent={{56,108},{76,128}})));
  Buildings.Controls.Continuous.LimPID pumEvaCon(
    controllerType=Modelica.Blocks.Types.SimpleController.PI,
    yMin=0.1,
    reset=Buildings.Types.Reset.Parameter,
    y_reset=0.1,
    k=0.1,
    Ti(displayUnit="s") = 300,
    reverseAction=true)
  "Controller for evaporator pump speed"
   annotation (Placement(transformation(extent={{60,-92},{80,-72}})));
  Buildings.Controls.OBC.CDL.Interfaces.RealOutput yPumCon
    "Condenser pump  speed outlet signal" annotation (Placement(transformation(
          extent={{160,102},{192,134}}), iconTransformation(extent={{100,68},{
            120,88}})));
  Buildings.Controls.OBC.CDL.Interfaces.RealOutput yPumEva
    "Evaporator pump speed outlet signal" annotation (Placement(transformation(
          extent={{160,-96},{192,-64}}), iconTransformation(extent={{100,-88},{
            120,-68}})));
  Buildings.Controls.OBC.CDL.Interfaces.RealInput pumConMin
  "Input signal to assure minimum flow rate to the hot buffer tank"
   annotation (Placement(transformation(extent={{-120,
            136},{-100,156}}),       iconTransformation(extent={{-114,84},{-100,
            98}})));
  Buildings.Controls.OBC.CDL.Interfaces.RealInput pumEvaMin
  "Input signal to assure minimum flow rate to the cold buffer tank"
   annotation (Placement(transformation(
          extent={{-124,-158},{-100,-134}}), iconTransformation(extent={{-114,
            -86},{-100,-72}})));
  Buildings.Controls.OBC.CDL.Interfaces.RealInput TConEnt
  "Evaporator Entering water temperature"
   annotation (Placement(
        transformation(extent={{-120,88},{-100,108}}), iconTransformation(
          extent={{-114,38},{-100,52}})));
  Buildings.Controls.OBC.CDL.Interfaces.RealInput TEvaEnt
  "Evaporator leaving water temperature"
   annotation (Placement(transformation(
          extent={{-124,-114},{-100,-90}}), iconTransformation(extent={{-114,
            -26},{-100,-12}})));
  Buildings.Controls.OBC.CDL.Continuous.AddParameter addPar(p=5, k=1)
   annotation (Placement(transformation(extent={{-80,88},{-60,108}})));
  Buildings.Controls.OBC.CDL.Continuous.Sources.Constant shuOffSig(k=0)
  "HeatPump, condenser pump  and evaporator pump shut off signal =0"
   annotation (Placement(transformation(extent={{40,-10},{20,10}})));
  Buildings.Controls.OBC.CDL.Continuous.Max max1
    annotation (Placement(transformation(extent={{100,108},{120,128}})));
  Buildings.Controls.OBC.CDL.Continuous.Max max2
    annotation (Placement(transformation(extent={{110,-90},{130,-70}})));
  Buildings.Controls.OBC.CDL.Integers.LessEqualThreshold
                                             intLesEquThr1(     threshold=-1)
    annotation (Placement(transformation(extent={{-80,-50},{-60,-30}})));
  Buildings.Controls.OBC.CDL.Integers.GreaterEqualThreshold
                                                intGreEquThr1(        threshold=1)
    annotation (Placement(transformation(extent={{-80,44},{-60,64}})));
  Modelica.Blocks.Interfaces.IntegerInput
                                       heaPumMod
    annotation (Placement(transformation(extent={{-140,-20},{-100,20}}), iconTransformation(
          extent={{-126,-12},{-100,14}})));
  Buildings.Controls.OBC.CDL.Logical.Switch swi1
    annotation (Placement(transformation(extent={{20,108},{40,128}})));
  Buildings.Controls.OBC.CDL.Logical.Switch swi3
    annotation (Placement(transformation(extent={{-20,20},{0,40}})));
  Buildings.Controls.OBC.CDL.Logical.Switch swi2
    annotation (Placement(transformation(extent={{20,-92},{40,-72}})));
  Buildings.Controls.OBC.CDL.Logical.Switch swi4
    annotation (Placement(transformation(extent={{-20,-40},{0,-20}})));
  Buildings.Controls.OBC.CDL.Continuous.AddParameter addPar1(p=5, k=1)
    annotation (Placement(transformation(extent={{-80,-112},{-60,-92}})));

equation

  connect(TConLvg, pumConCon.u_m)
    annotation (Line(points={{-110,80},{66,80},{66,106}},   color={0,0,127}));
  connect(yPumEva, yPumEva)
    annotation (Line(points={{176,-80},{176,-80}}, color={0,0,127}));
  connect(pumConCon.y, max1.u2) annotation (Line(points={{77,118},{88,118},{88,
          112},{98,112}}, color={0,0,127}));
  connect(pumConMin, max1.u1) annotation (Line(points={{-110,146},{90,146},{90,
          124},{98,124}}, color={0,0,127}));
  connect(pumEvaMin, max2.u2) annotation (Line(points={{-112,-146},{100,-146},{
          100,-86},{108,-86}}, color={0,0,127}));
  connect(TEvaLvg, pumEvaCon.u_m) annotation (Line(points={{-112,-128},{70,-128},
          {70,-94}},  color={0,0,127}));
  connect(addPar.u, TConEnt)
    annotation (Line(points={{-82,98},{-110,98}},  color={0,0,127}));
  connect(yPumCon, yPumCon)
    annotation (Line(points={{176,118},{176,118}}, color={0,0,127}));
  connect(TSetHea, swi1.u1)
    annotation (Line(points={{-110,126},{18,126}}, color={0,0,127}));
  connect(swi1.y, pumConCon.u_s)
    annotation (Line(points={{42,118},{54,118}}, color={0,0,127}));
  connect(intGreEquThr1.y, swi1.u2) annotation (Line(
      points={{-58,54},{-48,54},{-48,118},{18,118}},
      color={255,0,255},
      pattern=LinePattern.Dash));
  connect(intLesEquThr1.y, swi3.u2) annotation (Line(
      points={{-58,-40},{-52,-40},{-52,30},{-22,30}},
      color={255,0,255},
      thickness=0.5));
  connect(addPar.y, swi3.u1) annotation (Line(points={{-58,98},{-52,98},{-52,38},
          {-22,38}}, color={0,0,127}));
  connect(swi3.u3, shuOffSig.y) annotation (Line(points={{-22,22},{-44,22},{-44,
          0},{18,0}}, color={0,0,127}));
  connect(max1.y, yPumCon)
    annotation (Line(points={{122,118},{176,118}}, color={0,0,127}));
  connect(intLesEquThr1.y, pumEvaCon.trigger) annotation (Line(
      points={{-58,-40},{-52,-40},{-52,-114},{62,-114},{62,-94}},
      color={255,0,255},
      thickness=0.5));
  connect(pumEvaCon.u_s, swi2.y)
    annotation (Line(points={{58,-82},{42,-82}}, color={0,0,127}));
  connect(intLesEquThr1.y, swi2.u2) annotation (Line(
      points={{-58,-40},{-52,-40},{-52,-82},{18,-82}},
      color={255,0,255},
      thickness=0.5));
  connect(swi2.u1, TSetCoo)
    annotation (Line(points={{18,-74},{-112,-74}}, color={0,0,127}));
  connect(TEvaEnt, addPar1.u)
    annotation (Line(points={{-112,-102},{-82,-102}}, color={0,0,127}));
  connect(pumEvaCon.y, max2.u1) annotation (Line(points={{81,-82},{96,-82},{96,
          -74},{108,-74}}, color={0,0,127}));
  connect(intGreEquThr1.y, swi4.u2) annotation (Line(
      points={{-58,54},{-48,54},{-48,-30},{-22,-30}},
      color={255,0,255},
      pattern=LinePattern.Dash));
  connect(addPar1.y, swi4.u1) annotation (Line(points={{-58,-102},{-34,-102},{
          -34,-22},{-22,-22}},
                           color={0,0,127}));
  connect(swi4.y, swi2.u3) annotation (Line(points={{2,-30},{12,-30},{12,-90},{
          18,-90}},
                 color={0,0,127}));
  connect(shuOffSig.y, swi4.u3) annotation (Line(points={{18,0},{-44,0},{-44,
          -38},{-22,-38}},
                      color={0,0,127}));
  connect(max2.y, yPumEva)
    annotation (Line(points={{132,-80},{176,-80}}, color={0,0,127}));
  connect(intGreEquThr1.y, pumConCon.trigger) annotation (Line(
      points={{-58,54},{58,54},{58,106}},
      color={255,0,255},
      pattern=LinePattern.Dash));
  connect(swi3.y, swi1.u3) annotation (Line(points={{2,30},{6,30},{6,110},{18,
          110}},
        color={0,0,127}));
  connect(heaPumMod, intGreEquThr1.u) annotation (Line(points={{-120,0},{-90,0},
          {-90,54},{-82,54}}, color={255,127,0}));
  connect(heaPumMod, intLesEquThr1.u) annotation (Line(points={{-120,0},{-90,0},
          {-90,-40},{-82,-40}}, color={255,127,0}));
  annotation (defaultComponentName="pumCon",Icon(coordinateSystem(preserveAspectRatio=false, extent={{-100,
            -100},{100,100}})),                                  Diagram(
        coordinateSystem(preserveAspectRatio=false, extent={{-100,-160},{160,
            160}})),
                Documentation(info="<html>
<p>
The controller outputs the condenser and evaporator pumps status and rotating speed, taking
real inputs of heating and cooling set point temperatures <code>THeaSet</code>,<code>TCooSet</code>,
condenser and evaporator entering and leaving water temperatures <code>TConEnt</code>, <code>TConLvg</code>, <code>TEvaEnt</code>, <code>TEvaLvg</code> and the
heatpump operational mode <code>heaPumMod</code>.
</p>
<p>
There are three operational modes for the heatpump, refer to <a href=\"Buildings.DistrictHeatingCooling.EnergyTransferStations.ETSControl.HeatPumpController\">
Buildings.DistrictHeatingCooling.EnergyTransferStations.ETSControl.HeatPumpController</a> for detailed description.
</p>

<h4>Required cooling mode occurs when <code>heaPumMod=-1</code></h4>
<ol>
<li>
The controller compares between the minimum evaporator pump speed<code>pumEvaMin</code> i.e. minimum flow rate through the cold buffer tank and the computed
speed <code>pumEvaCon</code> to satisfy the cooling setpoint temperature<code>TCooSet</code>,
and it evaluates the maximum value using <code>SmoothMax2</code>.
</li>
<li>
The condenser pump speed is computed by a PI controller to maintain a temperature difference between
the condenser entering and leaving water &Delta;T<sub>Con</sub> equals to 5&#8451;.
The controller then compares the minimum condenser pump speed <code>pumConMin</code> to the
computed <code>pumConCon</code> and it evaluates the maximum value using <code>SmoothMax1</code>
</li>
</ol>
<h4>Required heating mode and istanteous required heating and cooling mode, occurs when <code>heaPumMod= 1</code></h4>
<ol>
<li>
The controller compares between the minimum condenser pump speed<code>pumConMin</code> i.e. minimum flow rate through the hot buffer tank and
the computed speed <code>pumConCon</code> to satisfy the  setpoint heating temperature<code>THeaSet</code>,
and it evaluates the maximum value using <code>SmoothMax1</code>.
</li>
<li>
The evaporator pump speed is computed by a PI controller to maintain a temperature difference between
the evaporator entering and leaving water &Delta;T<sub>Eva</sub> equals to 5&#8451;.
The controller then compares the minimum evaporator pump speed <code>pumEvaMin</code> to the
computed <code>pumEvaCon</code> and it evaluates the maximum value using <code>SmoothMax2</code>
</li>
</ol>
<h4>Shut off mode, occurs when <code>heaPumMod=0</code></h4>
<p>
It occurs if neither heating or cooling demands are required. Both the condenser and evaporator
pumps will maintain the minimum speed and flow rate through the hot and cold buffer tanks.
</p>
</html>", revisions="<html>
<ul>
<li>
 <br/>
</li>
</ul>
</html>"));
end CondenserAndEvaporatorPumpsController;
