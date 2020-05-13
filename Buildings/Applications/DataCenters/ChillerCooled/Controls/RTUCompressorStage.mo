within Buildings.Applications.DataCenters.ChillerCooled.Controls;
model RTUCompressorStage "Staging for compressor"
  extends Modelica.Blocks.Icons.Block;

  parameter Modelica.SIunits.Time tWai "Waiting time";

  parameter Modelica.SIunits.TemperatureDifference deaBan = 0.5/(1.8)
    "Dead band width 1 for switchig on/off and staging the compressor";

  Modelica.Blocks.Interfaces.RealInput TSet(
    final quantity="ThermodynamicTemperature",
    final unit="K",
    displayUnit="degC") "Indoor cooling temperature setpoint."
    annotation (Placement(transformation(extent={{-120,-50},{-100,-30}})));
  Modelica.Blocks.Interfaces.RealInput TIndMea(
    final quantity="TemperatureDifference",
    final unit="K",
    displayUnit="degC") "Indoor air measured temperature"
    annotation (Placement(transformation(extent={{-120,-88},{-100,-68}})));
  Modelica.Blocks.Interfaces.IntegerOutput y
    "Cooling mode signal, integer value of Buildings.Applications.DataCenters.Types.CoolingMode"
    annotation (Placement(transformation(extent={{100,-10},{120,10}}),
        iconTransformation(extent={{100,-10},{120,10}})));

  Modelica.StateGraph.TransitionWithSignal con1(enableTimer=true, waitTime=tWai)
    "Fire condition 1: free cooling to partially mechanical cooling"
    annotation (Placement(transformation(
        extent={{10,10},{-10,-10}},
        rotation=180,
        origin={-52,20})));
  Modelica.StateGraph.StepWithSignal staOne(nIn=2, nOut=2)
    "Partial capacity cooling" annotation (Placement(transformation(
        extent={{10,10},{-10,-10}},
        rotation=180,
        origin={-22,0})));
  Modelica.StateGraph.InitialStepWithSignal off(nIn=1) "off mode" annotation (
      Placement(transformation(
        extent={{10,10},{-10,-10}},
        rotation=180,
        origin={-80,0})));
  Modelica.StateGraph.StepWithSignal staTwo "Full cooling capacity mode"
    annotation (Placement(transformation(
        extent={{10,10},{-10,-10}},
        rotation=180,
        origin={42,0})));
  Modelica.StateGraph.TransitionWithSignal con2(enableTimer=false,
    waitTime=tWai)
    "Fire condition 2: partially mechanical cooling to fully mechanical cooling"
    annotation (Placement(transformation(
        extent={{10,10},{-10,-10}},
        rotation=180,
        origin={10,20})));
  Modelica.StateGraph.TransitionWithSignal con3(enableTimer=false,
    waitTime=tWai)
    "Fire condition 3: fully mechanical cooling to partially mechanical cooling"
    annotation (Placement(transformation(
        extent={{10,10},{-10,-10}},
        rotation=180,
        origin={68,20})));
  inner Modelica.StateGraph.StateGraphRoot stateGraphRoot
    annotation (Placement(transformation(extent={{40,72},{60,92}})));
  Modelica.Blocks.MathInteger.MultiSwitch swi(
    y_default=0,
    expr={Integer(Buildings.Applications.DataCenters.Types.CompressorModes.Off),
          Integer(Buildings.Applications.DataCenters.Types.CompressorModes.Stage1),
          Integer(Buildings.Applications.DataCenters.Types.CompressorModes.Stage2)},
    nu=3)
    "Switch boolean signals to real signal"
    annotation (Placement(transformation(extent={{72,-26},{96,-14}})));

  Buildings.Controls.OBC.CDL.Continuous.LessEqual    lesEqu
    annotation (Placement(transformation(extent={{-68,-70},{-48,-50}})));
  Buildings.Controls.OBC.CDL.Continuous.AddParameter addPar(k=1, p=deaBan)
    annotation (Placement(transformation(extent={{-96,-50},{-76,-30}})));
  Buildings.Controls.OBC.CDL.Continuous.LessEqual    lesEqu1
    annotation (Placement(transformation(extent={{-14,-70},{6,-50}})));
  Buildings.Controls.OBC.CDL.Continuous.AddParameter addPar1(k=1, p=deaBan)
    annotation (Placement(transformation(extent={{-40,-50},{-20,-30}})));
  Buildings.Controls.OBC.CDL.Continuous.AddParameter addPar2(k=1, p=-0.5*deaBan)
    annotation (Placement(transformation(extent={{10,-94},{30,-74}})));
  Buildings.Controls.OBC.CDL.Continuous.LessEqual    lesEqu2
    annotation (Placement(transformation(extent={{46,-70},{66,-50}})));
  Modelica.Blocks.Interfaces.BooleanOutput stage2Boo annotation (Placement(
        transformation(extent={{100,50},{120,70}}), iconTransformation(extent={{
            100,50},{120,70}})));
  Modelica.Blocks.Interfaces.BooleanOutput stage1Boo1 annotation (Placement(
        transformation(extent={{100,-70},{120,-50}}), iconTransformation(extent=
           {{100,-70},{120,-50}})));
equation
  connect(swi.y, y) annotation (Line(points={{96.6,-20},{98,-20},{98,0},{110,0}},
                                                color={255,127,0}));
  connect(off.outPort[1], con1.inPort) annotation (Line(points={{-69.5,0},{-62,0},
          {-62,20},{-56,20}}, color={0,0,0}));
  connect(con1.outPort, staOne.inPort[1]) annotation (Line(points={{-50.5,20},{-44,
          20},{-44,0.5},{-33,0.5}}, color={0,0,0}));
  connect(staOne.outPort[1], con2.inPort) annotation (Line(points={{-11.5,0.25},
          {-6,0.25},{-6,20},{6,20}}, color={0,0,0}));
  connect(con2.outPort, staTwo.inPort[1]) annotation (Line(points={{11.5,20},{24,
          20},{24,1.77636e-15},{31,1.77636e-15}}, color={0,0,0}));
  connect(staTwo.outPort[1], con3.inPort) annotation (Line(points={{52.5,0},{58,0},{58,20},{64,20}}, color={0,0,0}));
  connect(con3.outPort, off.inPort[1]) annotation (Line(points={{69.5,20},{86,20},
          {86,46},{-96,46},{-96,0},{-91,0}}, color={0,0,0}));
  connect(con1.condition,lesEqu. y) annotation (Line(points={{-52,8},{-52,-32},{
          -46,-32},{-46,-60}},           color={255,0,255}));
  connect(addPar1.y,lesEqu1. u1) annotation (Line(points={{-18,-40},{-18,-60},{-16,-60}}, color={0,0,127}));
  connect(lesEqu1.y, con2.condition) annotation (Line(points={{8,-60},{10,-60},{10,8}}, color={255,0,255}));
  connect(lesEqu2.y, con3.condition)
    annotation (Line(points={{68,-60},{68,8}}, color={255,0,255}));
  connect(off.active, swi.u[1]) annotation (Line(points={{-80,-11},{-80,-18.8},
          {72,-18.8}},color={255,0,255}));
  connect(staOne.active, swi.u[2]) annotation (Line(points={{-22,-11},{-22,-20},
          {72,-20}}, color={255,0,255}));
  connect(staTwo.active, swi.u[3]) annotation (Line(points={{42,-11},{42,-21.2},
          {72,-21.2}}, color={255,0,255}));

  connect(staTwo.active, stage2Boo) annotation (Line(points={{42,-11},{76,-11},{
          76,60},{110,60}}, color={255,0,255}));
  connect(staOne.active, stage1Boo1) annotation (Line(points={{-22,-11},{-22,
          -20},{60,-20},{60,-28},{84,-28},{84,-60},{110,-60}},
                                                          color={255,0,255}));
  connect(addPar.y, addPar1.u)
    annotation (Line(points={{-74,-40},{-42,-40}}, color={0,0,127}));
  connect(addPar.y, lesEqu.u1) annotation (Line(points={{-74,-40},{-72,-40},{-72,
          -60},{-70,-60}}, color={0,0,127}));
  connect(addPar.u, TSet)
    annotation (Line(points={{-98,-40},{-110,-40}}, color={0,0,127}));
  connect(TIndMea, lesEqu.u2) annotation (Line(points={{-110,-78},{-78,-78},{-78,
          -68},{-70,-68}}, color={0,0,127}));
  connect(TIndMea, lesEqu1.u2) annotation (Line(points={{-110,-78},{-30,-78},{-30,
          -68},{-16,-68}}, color={0,0,127}));
  connect(TIndMea, addPar2.u) annotation (Line(points={{-110,-78},{-78,-78},{-78,
          -84},{8,-84}}, color={0,0,127}));
  connect(addPar2.y, lesEqu2.u1) annotation (Line(points={{32,-84},{34,-84},{34,
          -60},{44,-60}}, color={0,0,127}));
  connect(TSet, lesEqu2.u2) annotation (Line(points={{-110,-40},{-98,-40},{-98,-98},
          {40,-98},{40,-68},{44,-68}}, color={0,0,127}));
annotation (    Documentation(info="<html>

</html>"));
end RTUCompressorStage;
