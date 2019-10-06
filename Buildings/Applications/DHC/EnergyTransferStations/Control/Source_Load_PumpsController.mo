within Buildings.Applications.DHC.EnergyTransferStations.Control;
model Source_Load_PumpsController
  "The control block of the condenser and the evaporator water pumps"
     extends Modelica.Blocks.Icons.Block;
  Buildings.Controls.OBC.CDL.Interfaces.RealInput mLoa
    "Water mass flow rate for the load side " annotation (Placement(
        transformation(extent={{-120,108},{-100,128}}), iconTransformation(
          extent={{-114,34},{-100,48}})));
  Buildings.Controls.OBC.CDL.Interfaces.RealInput mSecHot
    "Water mass flow rate at the secondary building hot side" annotation (
      Placement(transformation(extent={{-120,86},{-100,106}}),
        iconTransformation(extent={{-114,-58},{-100,-44}})));
  Buildings.Controls.Continuous.LimPID pumLoa(
    controllerType=Modelica.Blocks.Types.SimpleController.PI,
    yMin=0.1,
    reset=Buildings.Types.Reset.Parameter,
    y_reset=0.1,
    k=0.1,
    Ti(displayUnit="s") = 300) "Controller for load side pump speed"
    annotation (Placement(transformation(extent={{-8,124},{12,144}})));
  Buildings.Controls.OBC.CDL.Interfaces.RealOutput yPumLoa
    "Load side pump  speed outlet signal" annotation (Placement(transformation(
          extent={{156,58},{188,90}}),   iconTransformation(extent={{100,70},{120,
            90}})));
  Buildings.Controls.OBC.CDL.Interfaces.RealOutput yPumSou
    "Source side pump speed outlet signal"
                                          annotation (Placement(transformation(
          extent={{160,-64},{192,-32}}), iconTransformation(extent={{100,-90},{120,
            -70}})));
  Buildings.Controls.OBC.CDL.Continuous.Sources.Constant shuOffSig(k=0)
  "HeatPump, condenser pump  and evaporator pump shut off signal =0"
   annotation (Placement(transformation(extent={{60,10},{80,30}})));
  Buildings.Controls.OBC.CDL.Continuous.Max max2
    annotation (Placement(transformation(extent={{60,144},{80,124}})));
  Buildings.Controls.OBC.CDL.Logical.Switch swi1
    annotation (Placement(transformation(extent={{110,64},{130,84}})));

  Buildings.Controls.OBC.CDL.Interfaces.BooleanInput ReqCoo
    "Cooling is required Boolean signal"
    annotation (Placement(transformation(extent={{-128,6},{-100,34}}),
        iconTransformation(extent={{-128,-112},{-100,-84}})));
  Buildings.Controls.OBC.CDL.Interfaces.BooleanInput ReqHea
    "Heating is required Boolean signal"
    annotation (Placement(transformation(extent={{-128,26},{-100,54}}),
        iconTransformation(extent={{-128,80},{-100,108}})));
  Buildings.Controls.OBC.CDL.Continuous.GreaterEqual
             t2Off1
    annotation (Placement(transformation(extent={{-32,-128},{-12,-108}})));
  Buildings.Controls.OBC.CDL.Interfaces.RealInput TSouEntMin
    "Minimum enetring water temperature at the source side" annotation (
      Placement(transformation(extent={{-120,-104},{-100,-84}}),
        iconTransformation(extent={{-114,-26},{-100,-12}})));
  Buildings.Controls.OBC.CDL.Interfaces.BooleanOutput disCooVal
    "Hot side valve status,true when rejection of part or full cooling load is reuired"
    annotation (Placement(transformation(extent={{160,-132},{188,-104}}),
        iconTransformation(extent={{100,-54},{128,-26}})));
  Buildings.Controls.OBC.CDL.Interfaces.RealInput TLoaEntMax
    "Maximum enetring water temperature at the load side" annotation (Placement(
        transformation(extent={{-120,172},{-100,192}}), iconTransformation(
          extent={{-114,-8},{-100,6}})));
  Buildings.Controls.OBC.CDL.Continuous.GreaterEqual
             t2Off2
    annotation (Placement(transformation(extent={{-38,180},{-18,200}})));
  Buildings.Controls.OBC.CDL.Interfaces.BooleanOutput disHeaVal
    "Cold side valve status,true when rejection of part or full heating load is reuired"
    annotation (Placement(transformation(extent={{160,176},{188,204}}),
        iconTransformation(extent={{100,26},{128,54}})));
  Buildings.Controls.OBC.CDL.Logical.Or or2
    annotation (Placement(transformation(extent={{-38,18},{-18,38}})));
  Buildings.Controls.OBC.CDL.Interfaces.RealInput TLoaEnt
    "Source side entering water temperature" annotation (Placement(
        transformation(extent={{-120,190},{-100,210}}), iconTransformation(
          extent={{-114,14},{-100,28}})));
  Buildings.Controls.OBC.CDL.Continuous.Product pro
    annotation (Placement(transformation(extent={{-68,80},{-48,100}})));
  Buildings.Controls.OBC.CDL.Continuous.Sources.Constant con(k=1.1)
    "10% extera to maintain the primary flow higher than the secondary flow rate"
    annotation (Placement(transformation(extent={{-96,60},{-76,80}})));
  Buildings.Controls.Continuous.LimPID pumSou(
    controllerType=Modelica.Blocks.Types.SimpleController.PI,
    yMin=0.1,
    reset=Buildings.Types.Reset.Parameter,
    y_reset=0.1,
    k=0.1,
    Ti(displayUnit="s") = 300) "Controller for source side pump speed"
    annotation (Placement(transformation(extent={{6,-54},{26,-34}})));
  Buildings.Controls.OBC.CDL.Continuous.Product pro1
    annotation (Placement(transformation(extent={{-64,-48},{-44,-28}})));
  Buildings.Controls.OBC.CDL.Continuous.Sources.Constant con1(k=1.1)
    "10% extera to maintain the primary flow higher than the secondary flow rate"
    annotation (Placement(transformation(extent={{-94,-62},{-74,-42}})));
  Buildings.Controls.OBC.CDL.Interfaces.RealInput mSecCoo
    "Water mass flow rate at the secondary building cold side" annotation (
      Placement(transformation(extent={{-120,-42},{-100,-22}}),
        iconTransformation(extent={{-114,-74},{-100,-60}})));
  Buildings.Controls.OBC.CDL.Interfaces.RealInput mSou
    "Water mass flow rate for the source side " annotation (Placement(
        transformation(extent={{-120,-82},{-100,-62}}), iconTransformation(
          extent={{-114,-90},{-100,-76}})));
  Buildings.Controls.OBC.CDL.Interfaces.RealInput TSouEnt
    "Entering water temperature at the source side" annotation (Placement(
        transformation(extent={{-120,-136},{-100,-116}}), iconTransformation(
          extent={{-114,-42},{-100,-28}})));
  Buildings.Controls.OBC.CDL.Interfaces.RealInput minLoaFloHeaPum(final
      displayUnit="kg/s")
    "minimum mass flow rate of the heatpump at the load side set by the manufacturer"
    annotation (Placement(transformation(extent={{-120,130},{-100,150}}),
        iconTransformation(extent={{-120,-70},{-100,-50}})));
  Buildings.Controls.OBC.CDL.Continuous.Max max
    annotation (Placement(transformation(extent={{-38,144},{-18,124}})));
  Buildings.Controls.OBC.CDL.Interfaces.RealInput minSouFloHeaPum(final
      displayUnit="kg/s")
    "minimum mass flow rate of the heatpump at the source side set by the manufacturer"
    annotation (Placement(transformation(extent={{-124,-20},{-100,4}}),
        iconTransformation(extent={{-120,-70},{-100,-50}})));
  Buildings.Controls.OBC.CDL.Continuous.Max max1
    annotation (Placement(transformation(extent={{-36,-4},{-16,-24}})));
equation

  connect(max1.y, pumSou.u_s) annotation (Line(points={{-14,-14},{-2,-14},{-2,
          -42},{2,-42},{2,-44},{4,-44}}, color={0,0,127}));
  connect(mLoa, pumLoa.u_m)
    annotation (Line(points={{-110,118},{2,118},{2,122}}, color={0,0,127}));
  connect(yPumSou,yPumSou)
    annotation (Line(points={{176,-48},{176,-48}}, color={0,0,127}));
  connect(TSouEntMin, t2Off1.u1) annotation (Line(points={{-110,-94},{-78,-94},
          {-78,-118},{-34,-118}},color={0,0,127}));
  connect(t2Off1.y, disCooVal)
    annotation (Line(points={{-10,-118},{174,-118}}, color={255,0,255}));
  connect(t2Off2.y, disHeaVal)
    annotation (Line(points={{-16,190},{174,190}}, color={255,0,255}));
  connect(TLoaEntMax, t2Off2.u2)
    annotation (Line(points={{-110,182},{-40,182}}, color={0,0,127}));
  connect(ReqHea, or2.u1) annotation (Line(
      points={{-114,40},{-82,40},{-82,28},{-40,28}},
      color={255,0,255},
      pattern=LinePattern.Dot));
  connect(ReqCoo, or2.u2) annotation (Line(points={{-114,20},{-40,20}},
                      color={255,0,255}));
  connect(or2.y, pumLoa.trigger) annotation (Line(
      points={{-16,28},{-6,28},{-6,122}},
      color={255,0,255},
      thickness=0.5));
  connect(shuOffSig.y, swi1.u3) annotation (Line(
      points={{82,20},{104,20},{104,66},{108,66}},
      color={0,0,127},
      pattern=LinePattern.Dash));
  connect(or2.y, swi1.u2) annotation (Line(points={{-16,28},{44,28},{44,74},{
          108,74}},
                color={255,0,255}));
  connect(max2.y, swi1.u1) annotation (Line(points={{82,134},{104,134},{104,82},
          {108,82}}, color={0,0,127}));
  connect(yPumLoa, yPumLoa)
    annotation (Line(points={{172,74},{172,74}}, color={0,0,127}));
  connect(swi1.y, yPumLoa)
    annotation (Line(points={{132,74},{172,74}}, color={0,0,127}));
  connect(TLoaEnt, t2Off2.u1) annotation (Line(points={{-110,200},{-48,200},{
          -48,190},{-40,190}}, color={0,0,127}));
  connect(mSecHot, pro.u1)
    annotation (Line(points={{-110,96},{-70,96}}, color={0,0,127}));
  connect(pro.u2, con.y)
    annotation (Line(points={{-70,84},{-74,84},{-74,70}}, color={0,0,127}));
  connect(mSecCoo, pro1.u1)
    annotation (Line(points={{-110,-32},{-66,-32}}, color={0,0,127}));
  connect(pro1.u2, con1.y)
    annotation (Line(points={{-66,-44},{-72,-44},{-72,-52}}, color={0,0,127}));
  connect(mSou, pumSou.u_m)
    annotation (Line(points={{-110,-72},{16,-72},{16,-56}}, color={0,0,127}));
  connect(swi1.y, yPumSou) annotation (Line(points={{132,74},{140,74},{140,-48},
          {176,-48}}, color={0,0,127}));
  connect(TSouEnt, t2Off1.u2)
    annotation (Line(points={{-110,-126},{-34,-126}}, color={0,0,127}));
  connect(pumLoa.u_s, max.y)
    annotation (Line(points={{-10,134},{-16,134}}, color={0,0,127}));
  connect(pro.y, max.u1) annotation (Line(points={{-46,90},{-44,90},{-44,128},{
          -40,128}}, color={0,0,127}));
  connect(minLoaFloHeaPum, max.u2)
    annotation (Line(points={{-110,140},{-40,140}}, color={0,0,127}));
  connect(or2.y, pumSou.trigger) annotation (Line(
      points={{-16,28},{-6,28},{-6,-64},{8,-64},{8,-56}},
      color={255,0,255},
      thickness=0.5));
  connect(pro1.y, max1.u1)
    annotation (Line(points={{-42,-38},{-42,-20},{-38,-20}}, color={0,0,127}));
  connect(minSouFloHeaPum, max1.u2)
    annotation (Line(points={{-112,-8},{-38,-8}}, color={0,0,127}));
  connect(pumSou.y, max2.u1) annotation (Line(points={{27,-44},{42,-44},{42,128},
          {58,128}}, color={0,0,127}));
  connect(pumLoa.y, max2.u2) annotation (Line(points={{13,134},{32,134},{32,140},
          {58,140}}, color={0,0,127}));
  annotation (defaultComponentName="pumCon",Icon(coordinateSystem(preserveAspectRatio=false, extent={{-100,
            -100},{100,100}})),                                  Diagram(
        coordinateSystem(preserveAspectRatio=false, extent={{-100,-160},{160,
            220}}),
        graphics={
        Rectangle(
          extent={{-96,-2},{152,-146}},
          lineColor={255,0,255},
          pattern=LinePattern.Dot,
          fillColor={189,223,202},
          fillPattern=FillPattern.Solid),
        Rectangle(
          extent={{-96,218},{152,44}},
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
          extent={{54,-134},{158,-142}},
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
