within Buildings.Applications.DHC.EnergyTransferStations.Control;
block HotColdSideControllerUO
  "Controller for valves on hot or cold side, and heat demand on heat pump"
  extends Modelica.Blocks.Icons.Block;
  replaceable model Inequality =
    Buildings.Controls.OBC.CDL.Continuous.GreaterEqual;

  parameter Modelica.SIunits.TemperatureDifference THys(min=0.1)
    "Temperature hysteresis";
  Modelica.Blocks.Interfaces.RealInput THeaSupSet(final unit="K", displayUnit=
        "degC") "Set point temperature of heating load" annotation (Placement(
        transformation(extent={{-180,100},{-140,140}}), iconTransformation(
          extent={{-140,60},{-100,100}})));

  Modelica.Blocks.Interfaces.RealOutput yVal(
    unit="1")
    "Control signal for valve (0: closed, or 1: open)"
    annotation (Placement(transformation(extent={{140,-110},{160,-90}}),
        iconTransformation(extent={{100,-70},{120,-50}})));
  inner Modelica.StateGraph.StateGraphRoot stateGraphRoot
    annotation (Placement(transformation(extent={{-134,130},{-114,150}})));
  Modelica.StateGraph.InitialStep noDemand(nIn=2)
    "State if no heat or heat rejection is required"
    annotation (Placement(transformation(extent={{-86,88},{-66,108}})));
  Modelica.StateGraph.TransitionWithSignal t1(enableTimer=true)
    annotation (Placement(transformation(extent={{-34,110},{-14,130}})));
  Modelica.StateGraph.StepWithSignal runHP
    "State if heat pump operation is required"
    annotation (Placement(transformation(extent={{-4,110},{16,130}})));
  Modelica.StateGraph.TransitionWithSignal t2
    annotation (Placement(transformation(extent={{34,110},{54,130}})));
  Modelica.StateGraph.TransitionWithSignal t3(enableTimer=false)
    annotation (Placement(transformation(extent={{-34,70},{-14,90}})));
  Modelica.StateGraph.StepWithSignal rejParLoa(nOut=2, nIn=2)
    "Open valves and reject heat with either borefield or hex"
    annotation (Placement(transformation(extent={{-16,70},{4,90}})));
  Buildings.Controls.OBC.CDL.Continuous.AddParameter addPar(k=1, p=THys)
    annotation (Placement(transformation(extent={{-100,-70},{-80,-50}})));
  Modelica.StateGraph.TransitionWithSignal t4
    annotation (Placement(transformation(extent={{14,84},{34,104}})));
  Buildings.Controls.OBC.CDL.Continuous.AddParameter addPar1(k=1, p=THys)
    annotation (Placement(transformation(extent={{-100,-104},{-80,-84}})));
  Buildings.Controls.OBC.CDL.Conversions.BooleanToReal booToRea
    "Boolean to real conversion"
    annotation (Placement(transformation(extent={{110,-110},{130,-90}})));
  Modelica.StateGraph.Alternative alternative
    annotation (Placement(transformation(extent={{-56,42},{126,154}})));
  Modelica.Blocks.Interfaces.BooleanOutput valSta
    "Valve status, true: open, false: close"
    annotation (Placement(transformation(extent={{140,-90},{160,-70}}),
      iconTransformation(extent={{100,-10},{120,10}})));
  Modelica.StateGraph.TransitionWithSignal t5(enableTimer=false)
    annotation (Placement(transformation(extent={{26,50},{46,70}})));
  Modelica.StateGraph.StepWithSignal rejFulLoa
    "Reject heat using both, borefield and district hex"
    annotation (Placement(transformation(extent={{46,50},{66,70}})));
  Buildings.Controls.OBC.CDL.Continuous.AddParameter addPar2(k=1, p=THys)
    annotation (Placement(transformation(extent={{-100,-158},{-80,-138}})));
  Modelica.Blocks.Interfaces.BooleanOutput rejFulHexBor
    "Reject load using borefield and heat exchanger"
    annotation (Placement(transformation(extent={{140,-58},{160,-38}}),
      iconTransformation(extent={{100,50},{120,70}})));
  Modelica.StateGraph.TransitionWithSignal t6
    annotation (Placement(transformation(extent={{66,50},{86,70}})));
  Buildings.Controls.OBC.CDL.Logical.Or or2
    annotation (Placement(transformation(extent={{64,-100},{84,-80}})));
  Inequality t1On
    annotation (Placement(transformation(extent={{-76,58},{-56,78}})));
  Inequality t3On
    annotation (Placement(transformation(extent={{-54,-62},{-34,-42}})));
  Inequality t5On
    annotation (Placement(transformation(extent={{-54,-148},{-34,-128}})));
  Inequality t2Off
    annotation (Placement(transformation(extent={{-54,-24},{-34,-4}})));
  Inequality t4Off
    annotation (Placement(transformation(extent={{-52,-100},{-32,-80}})));
  Buildings.Controls.OBC.CDL.Continuous.AddParameter           addPar3(k=1, p=THys)
    annotation (Placement(transformation(extent={{-110,58},{-90,78}})));
  Buildings.Controls.OBC.CDL.Continuous.AddParameter           addPar4(k=1, p=THys)
    annotation (Placement(transformation(extent={{-100,-32},{-80,-12}})));
equation
  connect(t1.outPort, runHP.inPort[1])
    annotation (Line(points={{-22.5,120},{-5,120}},  color={0,0,0}));
  connect(runHP.outPort[1], t2.inPort)
    annotation (Line(points={{16.5,120},{40,120}},
                                                 color={0,0,0}));
  connect(t3.outPort, rejParLoa.inPort[1])
    annotation (Line(points={{-22.5,80},{-18,80},{-18,80.5},{-17,80.5}},color={0,0,0}));
  connect(yVal, booToRea.y) annotation (Line(points={{150,-100},{132,-100}},
                     color={0,0,127}));
  connect(alternative.inPort, noDemand.outPort[1])
    annotation (Line(points={{-58.73,98},{-65.5,98}}, color={0,0,0}));
  connect(t3.inPort, alternative.split[1]) annotation (Line(points={{-28,80},{
          -32,80},{-32,98},{-36.89,98}},
                                    color={0,0,0}));
  connect(t1.inPort, alternative.split[2]) annotation (Line(points={{-28,120},{
          -34,120},{-34,98},{-36.89,98}},
                                     color={0,0,0}));
  connect(alternative.outPort, noDemand.inPort[1]) annotation (Line(points={{127.82,
          98},{120,98},{120,146},{-100,146},{-100,98.5},{-87,98.5}},
                                                               color={0,0,0}));
  connect(t5.outPort, rejFulLoa.inPort[1])
    annotation (Line(points={{37.5,60},{45,60}}, color={0,0,0}));
  connect(rejFulLoa.outPort[1], t6.inPort)
    annotation (Line(points={{66.5,60},{72,60}}, color={0,0,0}));
  connect(t2.outPort, alternative.join[1]) annotation (Line(points={{45.5,120},
          {96,120},{96,98},{106.89,98}},
                                       color={0,0,0}));
  connect(valSta, or2.y)
    annotation (Line(points={{150,-80},{100,-80},{100,-90},{86,-90}},
                                                  color={255,0,255}));
  connect(booToRea.u, or2.y) annotation (Line(points={{108,-100},{100,-100},{100,
          -90},{86,-90}}, color={255,0,255}));
  connect(t4.inPort, rejParLoa.outPort[1]) annotation (Line(points={{20,94},{14,
          94},{14,80.25},{4.5,80.25}},  color={0,0,0}));
  connect(t5.inPort, rejParLoa.outPort[2]) annotation (Line(points={{32,60},{14,
          60},{14,79.75},{4.5,79.75}},
                                    color={0,0,0}));
  connect(rejParLoa.inPort[2], t6.outPort) annotation (Line(points={{-17,79.5},
          {-20,79.5},{-20,40},{86,40},{86,60},{77.5,60}},color={0,0,0}));
  connect(t4.outPort, alternative.join[2]) annotation (Line(points={{25.5,94},{
          66,94},{66,98},{106.89,98}},
                                   color={0,0,0}));
  connect(THeaSupSet, addPar3.u) annotation (Line(points={{-160,120},{-128,120},
          {-128,68},{-112,68}}, color={0,0,127}));
  connect(t2Off.u2, addPar4.y)
    annotation (Line(points={{-56,-22},{-78,-22}}, color={0,0,127}));
  connect(THeaSupSet, addPar4.u) annotation (Line(points={{-160,120},{-128,120},
          {-128,-22},{-102,-22}}, color={0,0,127}));
  connect(addPar3.y, t1On.u1)
    annotation (Line(points={{-88,68},{-78,68}}, color={0,0,127}));
  connect(addPar1.y, t4Off.u2) annotation (Line(points={{-78,-94},{-66,-94},{-66,
          -98},{-54,-98}},     color={0,0,127}));
  connect(addPar.y, t3On.u2)
    annotation (Line(points={{-78,-60},{-56,-60}}, color={0,0,127}));
  annotation (
  defaultComponentName="conHotSid",
  Diagram(coordinateSystem(extent={{-140,-160},{140,160}})), Icon(graphics={
        Polygon(
          points={{62,-60},{42,-48},{42,-70},{62,-60}},
          lineColor={0,0,0}),
        Polygon(
          points={{62,-60},{82,-48},{82,-70},{62,-60}},
          lineColor={0,0,0})}));
end HotColdSideControllerUO;
