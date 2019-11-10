within Buildings.Applications.DHC.EnergyTransferStations.Control;
block HotColdSideControllerUO
  "Controller for valves on hot or cold side, and heat demand on heat pump"
  extends Modelica.Blocks.Icons.Block;
  replaceable model Inequality =
    Buildings.Controls.OBC.CDL.Continuous.GreaterEqual;

  parameter Modelica.SIunits.TemperatureDifference THys(min=0.1)
    "Temperature hysteresis";
  Modelica.Blocks.Interfaces.RealInput TSet(final unit="K", displayUnit="degC")
    "Set point temperature" annotation (Placement(transformation(extent={{-180,
            100},{-140,140}}), iconTransformation(extent={{-120,80},{-100,100}})));

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
  Modelica.StateGraph.TransitionWithSignal t4
    annotation (Placement(transformation(extent={{38,88},{58,108}})));
  Modelica.StateGraph.Alternative alternative
    annotation (Placement(transformation(extent={{-56,42},{126,154}})));
  Modelica.StateGraph.TransitionWithSignal t5(enableTimer=false)
    annotation (Placement(transformation(extent={{26,50},{46,70}})));
  Modelica.StateGraph.StepWithSignal rejFulLoa
    "Reject heat using both, borefield and district hex"
    annotation (Placement(transformation(extent={{46,50},{66,70}})));
  Modelica.Blocks.Interfaces.BooleanOutput rejFulHexBor
    "Reject load using borefield and heat exchanger"
    annotation (Placement(transformation(extent={{140,-58},{160,-38}}),
      iconTransformation(extent={{100,50},{120,70}})));
  Modelica.StateGraph.TransitionWithSignal t6
    annotation (Placement(transformation(extent={{66,50},{86,70}})));
  Buildings.Controls.OBC.CDL.Continuous.GreaterEqual
             greEqu
    annotation (Placement(transformation(extent={{-98,28},{-78,48}})));
  Buildings.Controls.OBC.CDL.Continuous.AddParameter addPar(k=1, p=2*THys)
    annotation (Placement(transformation(extent={{-98,-8},{-78,12}})));
  Buildings.Controls.OBC.CDL.Continuous.GreaterEqual
             greEqu1
    annotation (Placement(transformation(extent={{-58,0},{-38,20}})));
  Buildings.Controls.OBC.CDL.Continuous.AddParameter addPar2(k=1, p=THys)
    annotation (Placement(transformation(extent={{-90,-42},{-70,-22}})));
  Buildings.Controls.OBC.CDL.Continuous.GreaterEqual
             greEqu2
    annotation (Placement(transformation(extent={{-58,-34},{-38,-14}})));
  Buildings.Controls.OBC.CDL.Continuous.AddParameter addPar3(k=1, p=0.5*THys)
    annotation (Placement(transformation(extent={{-90,-72},{-70,-52}})));
  Buildings.Controls.OBC.CDL.Continuous.GreaterEqual
             greEqu3
    annotation (Placement(transformation(extent={{-60,-72},{-40,-52}})));
  Buildings.Controls.OBC.CDL.Continuous.GreaterEqual
             greEqu4
    annotation (Placement(transformation(extent={{-60,-122},{-40,-102}})));
  Buildings.Controls.OBC.CDL.Continuous.GreaterEqual
             greEqu5
    annotation (Placement(transformation(extent={{-60,-150},{-40,-130}})));
  Buildings.Controls.OBC.CDL.Continuous.AddParameter addPar1(k=1, p=THys)
    annotation (Placement(transformation(extent={{-100,-150},{-80,-130}})));
  Buildings.Controls.OBC.CDL.Logical.Or or2
    annotation (Placement(transformation(extent={{60,-90},{80,-70}})));
  Buildings.Controls.OBC.CDL.Conversions.BooleanToReal booToRea
    "Boolean to real conversion"
    annotation (Placement(transformation(extent={{100,-110},{120,-90}})));
  Modelica.Blocks.Interfaces.BooleanOutput valSta
    "Valve status, true: open, false: close"
    annotation (Placement(transformation(extent={{140,-90},{160,-70}}),
      iconTransformation(extent={{100,-10},{120,10}})));
  Modelica.Blocks.Interfaces.RealOutput yVal(unit="1")
    "Control signal for valve (0: closed, or 1: open)"
    annotation (Placement(transformation(extent={{140,-110},{160,-90}}),
        iconTransformation(extent={{100,-70},{120,-50}})));
  Modelica.Blocks.Interfaces.RealInput TTanTop(final unit="K", displayUnit="degC")
    "Temperature at top of tank"
    annotation (Placement(transformation(extent={{-180,40},{-140,80}}),
        iconTransformation(extent={{-120,40},{-100,60}})));
  Modelica.Blocks.Interfaces.RealInput TTanBot(final unit="K", displayUnit="degC")
    "Temperature at bottom of tank"
    annotation (Placement(transformation(extent={{-180,-80},{-140,-40}}),
        iconTransformation(extent={{-120,-60},{-100,-40}})));
equation
  connect(t1.outPort, runHP.inPort[1])
    annotation (Line(points={{-22.5,120},{-5,120}},  color={0,0,0}));
  connect(runHP.outPort[1], t2.inPort)
    annotation (Line(points={{16.5,120},{40,120}},
                                                 color={0,0,0}));
  connect(t3.outPort, rejParLoa.inPort[1])
    annotation (Line(points={{-22.5,80},{-18,80},{-18,80.5},{-17,80.5}},color={0,0,0}));
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
  connect(t4.inPort, rejParLoa.outPort[1]) annotation (Line(points={{44,98},{14,98},{14,80.25},
          {4.5,80.25}},                 color={0,0,0}));
  connect(t5.inPort, rejParLoa.outPort[2]) annotation (Line(points={{32,60},{14,
          60},{14,79.75},{4.5,79.75}},
                                    color={0,0,0}));
  connect(rejParLoa.inPort[2], t6.outPort) annotation (Line(points={{-17,79.5},
          {-20,79.5},{-20,40},{86,40},{86,60},{77.5,60}},color={0,0,0}));
  connect(t4.outPort, alternative.join[2]) annotation (Line(points={{49.5,98},{106.89,98}},
                                   color={0,0,0}));
  connect(greEqu.y, t1.condition) annotation (Line(points={{-76,38},{-60,38},{-60,100},{-24,
          100},{-24,108}},           color={255,0,255}));
  connect(booToRea.u, or2.y) annotation (Line(points={{98,-100},{90,-100},{90,-80},{82,-80}},
                          color={255,0,255}));
  connect(booToRea.u,or2. y) annotation (Line(points={{98,-100},{90,-100},{90,-80},{82,-80}},
                          color={255,0,255}));
  connect(valSta, or2.y)
    annotation (Line(points={{150,-80},{82,-80}}, color={255,0,255}));
  connect(yVal, booToRea.y) annotation (Line(points={{150,-100},{122,-100}},
                     color={0,0,127}));
  connect(TSet, greEqu4.u2) annotation (Line(points={{-160,120},{-128,120},{-128,
          -120},{-62,-120}}, color={0,0,127}));
  connect(TSet, greEqu.u1) annotation (Line(points={{-160,120},{-128,120},{-128,
          38},{-100,38}}, color={0,0,127}));
  connect(addPar.u, TSet) annotation (Line(points={{-100,2},{-128,2},{-128,120},
          {-160,120}}, color={0,0,127}));
  connect(addPar1.u, TSet) annotation (Line(points={{-102,-140},{-128,-140},{-128,
          120},{-160,120}}, color={0,0,127}));
  connect(addPar.y, greEqu1.u2)
    annotation (Line(points={{-76,2},{-60,2}}, color={0,0,127}));
  connect(addPar3.u, addPar.y) annotation (Line(points={{-92,-62},{-100,-62},{-100,-14},{-70,
          -14},{-70,2},{-76,2}},           color={0,0,127}));
  connect(addPar2.u, addPar.y) annotation (Line(points={{-92,-32},{-100,-32},{-100,-14},{-70,
          -14},{-70,2},{-76,2}},           color={0,0,127}));
  connect(addPar2.y, greEqu2.u2)
    annotation (Line(points={{-68,-32},{-60,-32}}, color={0,0,127}));
  connect(addPar3.y, greEqu3.u1)
    annotation (Line(points={{-68,-62},{-62,-62}}, color={0,0,127}));
  connect(addPar1.y, greEqu5.u1)
    annotation (Line(points={{-78,-140},{-62,-140}}, color={0,0,127}));
  connect(or2.u2, rejParLoa.active)
    annotation (Line(points={{58,-88},{-6,-88},{-6,69}},   color={255,0,255}));
  connect(greEqu1.y, t3.condition) annotation (Line(points={{-36,10},{-28,10},{-28,34},{-24,
          34},{-24,68}},          color={255,0,255}));
  connect(greEqu2.y, t5.condition) annotation (Line(points={{-36,-24},{-24,-24},{-24,34},{36,
          34},{36,48}},              color={255,0,255}));
  connect(greEqu3.y, t6.condition) annotation (Line(points={{-38,-62},{-16,-62},{-16,30},{76,
          30},{76,48}},              color={255,0,255}));
  connect(greEqu4.y, t2.condition) annotation (Line(points={{-38,-112},{-12,-112},{-12,26},{
          4,26},{4,100},{44,100},{44,108}},           color={255,0,255}));
  connect(greEqu5.y, t4.condition) annotation (Line(points={{-38,-140},{-8,-140},{-8,22},{8,
          22},{8,84},{48,84},{48,86}},
                                    color={255,0,255}));
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
