within Buildings.Applications.DHC.EnergyTransferStations.Control;
model Source_Load_PumpsController
  "The control block of the condenser and the evaporator water pumps"
     extends Modelica.Blocks.Icons.Block;
  Buildings.Controls.OBC.CDL.Interfaces.RealInput TSouLvg
    "Source side leaving water temperature"
   annotation (Placement(transformation(extent={{-120,-120},{-100,-100}}),
        iconTransformation(extent={{-114,-48},{-100,-34}})));
  Buildings.Controls.OBC.CDL.Interfaces.RealInput TLoaLvg
    "Load side leaving water temperature"
   annotation (Placement(transformation(extent={{-120,50},{-100,70}}),
        iconTransformation(extent={{-114,36},{-100,50}})));
  Buildings.Controls.OBC.CDL.Interfaces.RealInput TSetHea
  "Heating setpoint temperature or condenser leaving water temperature setpoint"
   annotation (Placement(transformation(extent={{-120,130},{-100,150}}),
        iconTransformation(extent={{-114,52},{-100,66}})));
  Buildings.Controls.OBC.CDL.Interfaces.RealInput TSetCoo
  "Cooling setpoint temperature or evaporator leaving water temperature setpoint"
   annotation (Placement(transformation(extent={{-120,-48},{-100,-28}}),
        iconTransformation(extent={{-114,-66},{-100,-52}})));
  Buildings.Controls.Continuous.LimPID pumLoa(
    controllerType=Modelica.Blocks.Types.SimpleController.PI,
    yMin=0.1,
    reset=Buildings.Types.Reset.Parameter,
    y_reset=0.1,
    k=0.1,
    Ti(displayUnit="s") = 300) "Controller fo rload side pump speed"
    annotation (Placement(transformation(extent={{2,114},{22,134}})));
  Buildings.Controls.Continuous.LimPID pumSou(
    controllerType=Modelica.Blocks.Types.SimpleController.PI,
    yMin=0.1,
    reset=Buildings.Types.Reset.Parameter,
    y_reset=0.1,
    k=0.1,
    Ti(displayUnit="s") = 300,
    reverseAction=true) "Controller for source side pump speed"
    annotation (Placement(transformation(extent={{40,-40},{60,-20}})));
  Buildings.Controls.OBC.CDL.Interfaces.RealOutput yPumLoa
    "Load side pump  speed outlet signal" annotation (Placement(transformation(
          extent={{160,54},{192,86}}),   iconTransformation(extent={{100,70},{120,
            90}})));
  Buildings.Controls.OBC.CDL.Interfaces.RealOutput yPumSou
    "Source side pump speed outlet signal"
                                          annotation (Placement(transformation(
          extent={{160,-110},{192,-78}}),iconTransformation(extent={{100,-90},{120,
            -70}})));
  Buildings.Controls.OBC.CDL.Interfaces.RealInput pumConMin
  "Input signal to assure minimum flow rate to the hot buffer tank"
   annotation (Placement(transformation(extent={{-120,150},{-100,170}}),
                                     iconTransformation(extent={{-114,68},{-100,
            82}})));
  Buildings.Controls.OBC.CDL.Interfaces.RealInput pumEvaMin
  "Input signal to assure minimum flow rate to the cold buffer tank"
   annotation (Placement(transformation(
          extent={{-120,-160},{-100,-140}}), iconTransformation(extent={{-114,
            -82},{-100,-68}})));
  Buildings.Controls.OBC.CDL.Interfaces.RealInput TSouEnt
    "Source side entering water temperature"
   annotation (Placement(transformation(
          extent={{-120,-32},{-100,-12}}),  iconTransformation(extent={{-114,
            -32},{-100,-18}})));
  Buildings.Controls.OBC.CDL.Continuous.Sources.Constant shuOffSig(k=0)
  "HeatPump, condenser pump  and evaporator pump shut off signal =0"
   annotation (Placement(transformation(extent={{66,12},{86,32}})));
  Buildings.Controls.OBC.CDL.Continuous.Max max1
    annotation (Placement(transformation(extent={{60,120},{80,140}})));
  Buildings.Controls.OBC.CDL.Continuous.Max max2
    annotation (Placement(transformation(extent={{66,-94},{86,-74}})));
  Buildings.Controls.OBC.CDL.Logical.Switch swi1
    annotation (Placement(transformation(extent={{120,60},{140,80}})));
  Buildings.Controls.OBC.CDL.Continuous.AddParameter addPar1(p=5, k=1)
    annotation (Placement(transformation(extent={{-60,-32},{-40,-12}})));

  Buildings.Controls.OBC.CDL.Interfaces.BooleanInput ReqCoo
    "Cooling is required Boolean signal"
    annotation (Placement(transformation(extent={{-128,68},{-100,96}}),
        iconTransformation(extent={{-128,-112},{-100,-84}})));
  Buildings.Controls.OBC.CDL.Interfaces.BooleanInput ReqHea
    "Heating is required Boolean signal"
    annotation (Placement(transformation(extent={{-128,90},{-100,118}}),
        iconTransformation(extent={{-128,80},{-100,108}})));
  Buildings.Controls.OBC.CDL.Continuous.GreaterEqual
             t2Off1
    annotation (Placement(transformation(extent={{-60,-142},{-40,-122}})));
  Buildings.Controls.OBC.CDL.Interfaces.RealInput TSouEntMin
    "Minimum enetring water temperature at the source side" annotation (
      Placement(transformation(extent={{-120,-100},{-100,-80}}),
        iconTransformation(extent={{-114,-18},{-100,-4}})));
  Buildings.Controls.OBC.CDL.Interfaces.BooleanOutput disCooVal
    "Hot side valve status,true when rejection of part or full cooling load is reuired"
    annotation (Placement(transformation(extent={{160,-146},{188,-118}}),
        iconTransformation(extent={{100,-54},{128,-26}})));
  Buildings.Controls.OBC.CDL.Interfaces.RealInput TLoaEntMax
    "Maximum enetring water temperature at the load side" annotation (Placement(
        transformation(extent={{-120,172},{-100,192}}), iconTransformation(
          extent={{-114,8},{-100,22}})));
  Buildings.Controls.OBC.CDL.Continuous.GreaterEqual
             t2Off2
    annotation (Placement(transformation(extent={{-38,180},{-18,200}})));
  Buildings.Controls.OBC.CDL.Interfaces.BooleanOutput disHeaVal
    "Cold side valve status,true when rejection of part or full heating load is reuired"
    annotation (Placement(transformation(extent={{160,176},{188,204}}),
        iconTransformation(extent={{100,26},{128,54}})));
  Buildings.Controls.OBC.CDL.Logical.Or or2
    annotation (Placement(transformation(extent={{-38,94},{-18,114}})));
  Buildings.Controls.OBC.CDL.Logical.Switch swi4
    annotation (Placement(transformation(extent={{-10,-10},{10,10}},
        rotation=0,
        origin={130,-92})));
  Buildings.Controls.OBC.CDL.Logical.Switch swi2
    annotation (Placement(transformation(extent={{0,-40},{20,-20}})));
  Modelica.Blocks.Logical.And and1
    annotation (Placement(transformation(extent={{-40,10},{-20,30}})));
  Buildings.Controls.OBC.CDL.Logical.Not not1
    annotation (Placement(transformation(extent={{-74,2},{-54,22}})));
  Buildings.Controls.OBC.CDL.Interfaces.RealInput TLoaEnt
    "Enetring water temperature at the load side" annotation (Placement(
        transformation(extent={{-120,190},{-100,210}}), iconTransformation(
          extent={{-114,22},{-100,36}})));
equation

  connect(TSetCoo, swi2.u3) annotation (Line(points={{-110,-38},{-2,-38}},
                     color={0,0,127}));
  connect(yPumSou,yPumSou)
    annotation (Line(points={{176,-94},{176,-94}}, color={0,0,127}));
  connect(pumLoa.y, max1.u2) annotation (Line(points={{23,124},{58,124}},
                     color={0,0,127}));
  connect(pumConMin, max1.u1) annotation (Line(points={{-110,160},{50,160},{50,136},
          {58,136}},      color={0,0,127}));
  connect(TSouEnt, addPar1.u)
    annotation (Line(points={{-110,-22},{-62,-22}},   color={0,0,127}));
  connect(TLoaLvg, pumLoa.u_m)
    annotation (Line(points={{-110,60},{12,60},{12,112}}, color={0,0,127}));
  connect(TSouEntMin, t2Off1.u1) annotation (Line(points={{-110,-90},{-78,-90},
          {-78,-132},{-62,-132}},color={0,0,127}));
  connect(t2Off1.y, disCooVal)
    annotation (Line(points={{-38,-132},{174,-132}}, color={255,0,255}));
  connect(pumEvaMin, max2.u2) annotation (Line(points={{-110,-150},{-18,-150},{
          -18,-90},{64,-90}},
                          color={0,0,127}));
  connect(t2Off2.y, disHeaVal)
    annotation (Line(points={{-16,190},{174,190}}, color={255,0,255}));
  connect(TLoaEntMax, t2Off2.u2)
    annotation (Line(points={{-110,182},{-40,182}}, color={0,0,127}));
  connect(ReqHea, or2.u1) annotation (Line(
      points={{-114,104},{-40,104}},
      color={255,0,255},
      pattern=LinePattern.Dot));
  connect(ReqCoo, or2.u2) annotation (Line(points={{-114,82},{-84,82},{-84,96},
          {-40,96}},  color={255,0,255}));
  connect(or2.y, pumLoa.trigger) annotation (Line(
      points={{-16,104},{4,104},{4,112}},
      color={255,0,255},
      thickness=0.5));
  connect(shuOffSig.y, swi1.u3) annotation (Line(
      points={{88,22},{108,22},{108,62},{118,62}},
      color={0,0,127},
      pattern=LinePattern.Dash));
  connect(TSetHea, pumLoa.u_s) annotation (Line(points={{-110,140},{-12,140},{
          -12,124},{0,124}},
                     color={0,0,127}));
  connect(or2.y, swi1.u2) annotation (Line(points={{-16,104},{98,104},{98,70},{
          118,70}},
                color={255,0,255}));
  connect(max1.y, swi1.u1) annotation (Line(points={{82,130},{108,130},{108,78},
          {118,78}},
                color={0,0,127}));
  connect(yPumLoa, yPumLoa)
    annotation (Line(points={{176,70},{176,70}}, color={0,0,127}));
  connect(TSouLvg, pumSou.u_m) annotation (Line(points={{-110,-110},{-88,-110},
          {-88,-82},{50,-82},{50,-42}},color={0,0,127}));
  connect(pumSou.y, max2.u1) annotation (Line(points={{61,-30},{64,-30},{64,-78}},
                     color={0,0,127}));
  connect(max2.y, swi4.u1) annotation (Line(points={{88,-84},{118,-84}},
                      color={0,0,127}));
  connect(shuOffSig.y, swi4.u3) annotation (Line(
      points={{88,22},{108,22},{108,-100},{118,-100}},
      color={0,0,127},
      pattern=LinePattern.Dash));
  connect(swi4.y, yPumSou)
    annotation (Line(points={{142,-92},{160,-92},{160,-94},{176,-94}},
                                                   color={0,0,127}));
  connect(swi1.y, yPumLoa)
    annotation (Line(points={{142,70},{176,70}}, color={0,0,127}));
  connect(swi2.y, pumSou.u_s)
    annotation (Line(points={{22,-30},{38,-30}}, color={0,0,127}));
  connect(ReqCoo, pumSou.trigger) annotation (Line(points={{-114,82},{-84,82},{-84,
          -58},{42,-58},{42,-42}}, color={255,0,255}));
  connect(or2.y, swi4.u2) annotation (Line(points={{-16,104},{98,104},{98,-92},
          {118,-92}},          color={255,0,255}));
  connect(not1.y, and1.u2)
    annotation (Line(points={{-52,12},{-42,12}}, color={255,0,255}));
  connect(ReqHea, and1.u1) annotation (Line(
      points={{-114,104},{-50,104},{-50,20},{-42,20}},
      color={255,0,255},
      pattern=LinePattern.Dot));
  connect(ReqCoo, not1.u) annotation (Line(points={{-114,82},{-84,82},{-84,12},
          {-76,12}},color={255,0,255}));
  connect(addPar1.y, swi2.u1)
    annotation (Line(points={{-38,-22},{-2,-22}}, color={0,0,127}));
  connect(and1.y, swi2.u2) annotation (Line(points={{-19,20},{-14,20},{-14,-30},
          {-2,-30}}, color={255,0,255}));
  connect(TLoaEnt, t2Off2.u1) annotation (Line(points={{-110,200},{-52,200},{
          -52,190},{-40,190}}, color={0,0,127}));
  connect(TSouEnt, t2Off1.u2) annotation (Line(points={{-110,-22},{-96,-22},{
          -96,-140},{-62,-140}}, color={0,0,127}));
  annotation (defaultComponentName="pumCon",Icon(coordinateSystem(preserveAspectRatio=false, extent={{-100,
            -100},{100,100}})),                                  Diagram(
        coordinateSystem(preserveAspectRatio=false, extent={{-100,-160},{160,220}}),
        graphics={
        Rectangle(
          extent={{-90,50},{154,-160}},
          lineColor={255,0,255},
          pattern=LinePattern.Dot,
          fillColor={189,223,202},
          fillPattern=FillPattern.Solid),
        Rectangle(
          extent={{-90,218},{154,48}},
          lineColor={135,135,135},
          lineThickness=0.5,
          fillColor={223,223,223},
          fillPattern=FillPattern.Solid),
        Text(
          extent={{48,218},{152,210}},
          lineColor={0,0,0},
          lineThickness=0.5,
          fillColor={223,223,223},
          fillPattern=FillPattern.Solid,
          textString="Load side water pump",
          textStyle={TextStyle.Bold}),
        Text(
          extent={{54,-150},{158,-158}},
          lineColor={0,0,0},
          lineThickness=0.5,
          fillColor={223,223,223},
          fillPattern=FillPattern.Solid,
          textStyle={TextStyle.Bold},
          textString="Source side water pump")}),
                Documentation(info="<html>
<p>
The controller outputs the load and source side pumps rotating speed, taking
real inputs of heating and cooling set point temperatures <code>THeaSet</code>,<code>TCooSet</code>,
load and source entering and leaving water temperatures <code>TLoaLvg</code>, <code>SouEnt</code>, <code>TSouLvg</code>
and the two boolean inputs of <code>

</p>

<h4>  <code>reqCoo</code> is true</h4>
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
end Source_Load_PumpsController;
