within Buildings.Applications.DHC.EnergyTransferStations.Control;
model TanHotUO
    extends Modelica.Blocks.Icons.Block;

      Buildings.Controls.OBC.CDL.Interfaces.BooleanOutput ReqHea
        annotation (Placement(transformation(extent={{120,142},{144,166}})));
      Buildings.Controls.OBC.CDL.Interfaces.RealInput THotTanBot
        annotation (Placement(transformation(extent={{-172,-104},{-140,-72}}),
        iconTransformation(extent={{-134,-104},{-102,-72}})));
      Buildings.Controls.OBC.CDL.Interfaces.RealInput THotTanTop
        annotation (Placement(transformation(extent={{-172,24},{-140,56}}),
        iconTransformation(extent={{-134,24},{-102,56}})));
      Buildings.Controls.OBC.CDL.Interfaces.RealInput THotTanSet
        annotation (Placement(transformation(extent={{-172,64},{-140,96}}),
        iconTransformation(extent={{-134,64},{-102,96}})));
      Modelica.StateGraph.InitialStep noDemand(nIn=2)
        "State if no heat or heat rejection is required"
        annotation (Placement(transformation(extent={{-116,96},{-100,112}})));
      Modelica.StateGraph.TransitionWithSignal t1(enableTimer=true, waitTime=60)
        annotation (Placement(transformation(extent={{-42,132},{-22,152}})));
      Modelica.StateGraph.StepWithSignal runHP
        "State if heat pump operation is required"
        annotation (Placement(transformation(extent={{-12,132},{8,152}})));
      Modelica.StateGraph.TransitionWithSignal t2
        annotation (Placement(transformation(extent={{10,132},{30,152}})));
      Modelica.StateGraph.TransitionWithSignal t3(enableTimer=false)
        annotation (Placement(transformation(extent={{-48,90},{-28,110}})));
      Modelica.StateGraph.StepWithSignal rejParLoa(nOut=2, nIn=2)
        "Open valves and reject heat with either borefield or hex"
        annotation (Placement(transformation(extent={{-22,92},{-2,112}})));
      Buildings.Controls.OBC.CDL.Continuous.AddParameter addPar(k=1, p=2*THys)
        annotation (Placement(transformation(extent={{-106,-16},{-86,4}})));
      Modelica.StateGraph.TransitionWithSignal t4
        annotation (Placement(transformation(extent={{48,102},{68,122}})));
      Buildings.Controls.OBC.CDL.Continuous.AddParameter addPar1(k=1, p=THys)
        annotation (Placement(transformation(extent={{-108,-150},{-88,-130}})));
      Modelica.StateGraph.Alternative alternative
        annotation (Placement(transformation(extent={{-88,26},{120,158}})));
      Modelica.StateGraph.TransitionWithSignal t5(enableTimer=false)
        annotation (Placement(transformation(extent={{10,72},{30,92}})));
      Modelica.StateGraph.StepWithSignal rejFulLoa
        "Reject heat using both, borefield and district hex"
        annotation (Placement(transformation(extent={{28,72},{48,92}})));
      Buildings.Controls.OBC.CDL.Continuous.AddParameter addPar2(k=1, p=THys)
        annotation (Placement(transformation(extent={{-98,-48},{-78,-28}})));
      Modelica.StateGraph.TransitionWithSignal t6
        annotation (Placement(transformation(extent={{58,72},{78,92}})));
      Buildings.Controls.OBC.CDL.Continuous.AddParameter addPar3(k=1, p=0.5*
            THys)
        annotation (Placement(transformation(extent={{-98,-78},{-78,-58}})));
      Buildings.Controls.OBC.CDL.Continuous.GreaterEqual
                 greEqu
        annotation (Placement(transformation(extent={{-96,46},{-76,66}})));
      Buildings.Controls.OBC.CDL.Continuous.GreaterEqual
                 greEqu1
        annotation (Placement(transformation(extent={{-64,-10},{-44,10}})));
      Buildings.Controls.OBC.CDL.Continuous.GreaterEqual
                 greEqu2
        annotation (Placement(transformation(extent={{-64,-40},{-44,-20}})));
      Buildings.Controls.OBC.CDL.Continuous.GreaterEqual
                 greEqu3
        annotation (Placement(transformation(extent={{-64,-78},{-44,-58}})));
      Buildings.Controls.OBC.CDL.Continuous.GreaterEqual
                 greEqu4
        annotation (Placement(transformation(extent={{-64,-118},{-44,-98}})));
      Buildings.Controls.OBC.CDL.Continuous.GreaterEqual
                 greEqu5
        annotation (Placement(transformation(extent={{-64,-150},{-44,-130}})));
      Buildings.Controls.OBC.CDL.Continuous.Min min
        annotation (Placement(transformation(extent={{-98,-108},{-78,-88}})));
equation

  connect(t1.outPort,runHP. inPort[1])
    annotation (Line(points={{-30.5,142},{-13,142}}, color={0,0,0}));
  connect(runHP.outPort[1],t2. inPort)
    annotation (Line(points={{8.5,142},{16,142}},color={0,0,0}));
  connect(t3.outPort,rejParLoa. inPort[1])
    annotation (Line(points={{-36.5,100},{-26,100},{-26,102.5},{-23,102.5}},
                                                                        color={0,0,0}));
  connect(alternative.inPort,noDemand. outPort[1])
    annotation (Line(points={{-91.12,92},{-96,92},{-96,104},{-99.6,104}},
                                                      color={0,0,0}));
  connect(t3.inPort,alternative. split[1]) annotation (Line(points={{-42,100},
          {-54,100},{-54,92},{-66.16,92}},
                                    color={0,0,0}));
  connect(t1.inPort,alternative. split[2]) annotation (Line(points={{-36,142},
          {-58,142},{-58,92},{-66.16,92}},
                                     color={0,0,0}));
  connect(t5.outPort,rejFulLoa. inPort[1])
    annotation (Line(points={{21.5,82},{27,82}}, color={0,0,0}));
  connect(rejFulLoa.outPort[1],t6. inPort)
    annotation (Line(points={{48.5,82},{64,82}}, color={0,0,0}));
  connect(t2.outPort,alternative. join[1]) annotation (Line(points={{21.5,
          142},{88,142},{88,92},{98.16,92}},
                                       color={0,0,0}));
  connect(addPar2.u,addPar. y) annotation (Line(points={{-100,-38},{-108,-38},{-108,
          -22},{-78,-22},{-78,-6},{-84,-6}},
                                           color={0,0,127}));
  connect(t4.inPort,rejParLoa. outPort[1]) annotation (Line(points={{54,112},
          {6,112},{6,102.25},{-1.5,102.25}},
                                        color={0,0,0}));
  connect(t5.inPort,rejParLoa. outPort[2]) annotation (Line(points={{16,82},
          {6,82},{6,101.75},{-1.5,101.75}},
                                    color={0,0,0}));
  connect(rejParLoa.inPort[2],t6. outPort) annotation (Line(points={{-23,
          101.5},{-26,101.5},{-26,64},{80,64},{80,82},{69.5,82}},
                                                         color={0,0,0}));
  connect(t4.outPort,alternative. join[2]) annotation (Line(points={{59.5,
          112},{88,112},{88,92},{98.16,92}},
                                   color={0,0,0}));
  connect(addPar3.u,addPar. y) annotation (Line(points={{-100,-68},{-108,-68},{-108,
          -22},{-78,-22},{-78,-6},{-84,-6}},
                                           color={0,0,127}));
  connect(greEqu.y,t1. condition) annotation (Line(points={{-74,56},{-52,56},{-52,
          122},{-32,122},{-32,130}}, color={255,0,255}));
  connect(addPar.y,greEqu1. u2)
    annotation (Line(points={{-84,-6},{-76,-6},{-76,-8},{-66,-8}},
                                               color={0,0,127}));
  connect(addPar2.y,greEqu2. u2)
    annotation (Line(points={{-76,-38},{-66,-38}}, color={0,0,127}));
  connect(addPar3.y,greEqu3. u1)
    annotation (Line(points={{-76,-68},{-66,-68}}, color={0,0,127}));
  connect(addPar1.y,greEqu5. u1)
    annotation (Line(points={{-86,-140},{-66,-140}}, color={0,0,127}));
  connect(min.y,greEqu4. u1) annotation (Line(points={{-76,-98},{-72,-98},{-72,-108},
          {-66,-108}},       color={0,0,127}));
  connect(ReqHea, ReqHea)
    annotation (Line(points={{132,154},{132,154}}, color={255,0,255}));
  connect(greEqu4.y, t2.condition) annotation (Line(points={{-42,-108},{8,-108},
          {8,122},{20,122},{20,130}},       color={255,0,255}));
  connect(THotTanSet, greEqu.u1) annotation (Line(points={{-156,80},{-110,
          80},{-110,56},{-98,56}}, color={0,0,127}));
  connect(THotTanTop, greEqu.u2) annotation (Line(points={{-156,40},{-110,
          40},{-110,48},{-98,48}}, color={0,0,127}));
  connect(THotTanSet, greEqu1.u1) annotation (Line(points={{-156,80},{-74,
          80},{-74,0},{-66,0}}, color={0,0,127}));
  connect(THotTanSet, greEqu2.u1) annotation (Line(points={{-156,80},{-74,
          80},{-74,-30},{-66,-30}}, color={0,0,127}));
  connect(THotTanSet, greEqu3.u2) annotation (Line(points={{-156,80},{-74,
          80},{-74,-76},{-66,-76}}, color={0,0,127}));
  connect(THotTanTop, min.u1) annotation (Line(points={{-156,40},{-118,40},
          {-118,-92},{-100,-92}}, color={0,0,127}));
  connect(THotTanBot, min.u2) annotation (Line(points={{-156,-88},{-124,-88},
          {-124,-104},{-100,-104}}, color={0,0,127}));
  annotation (Diagram(coordinateSystem(extent={{-140,-100},{100,100}})),
      Icon(coordinateSystem(extent={{-100,-100},{100,100}})));
end TanHotUO;
