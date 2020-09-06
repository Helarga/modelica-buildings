within Buildings.Applications.DHC.CentralPlants.Heating.Generation4.Controls;
model BoilerStage "Stage controller for boilers"

  parameter Modelica.SIunits.Time tWai "Waiting time";

  parameter Modelica.SIunits.Power QBoi_nominal
    "Nominal heating capaciaty";

  parameter Modelica.SIunits.Power  criPoiLoa = 0.55*QBoi_nominal
    "Critical point of heating load for switching one boiler on or off";

  parameter Modelica.SIunits.Power  dQ = 0.25*QBoi_nominal
    "Deadband for critical point of heating load";
  parameter Integer numBoi
    "Number of boilers";

  Modelica.Blocks.Interfaces.RealInput QLoa(unit="W") "Total heating loads"
    annotation (Placement(transformation(extent={{-140,-60},{-100,-20}})));
  Modelica.Blocks.Interfaces.BooleanInput on "On signal of the boilers"
    annotation (Placement(transformation(extent={{-140,20},{-100,60}})));
  Modelica.Blocks.Interfaces.RealOutput y[2]
    "On/off signal for the boilers - 0: off; 1: on"
    annotation (Placement(transformation(extent={{100,-10},{120,10}})));
  Modelica.StateGraph.InitialStep off(nIn=1) "No heating is demanded"
    annotation (Placement(transformation(
        extent={{-10,10},{10,-10}},
        rotation=-90,
        origin={-50,70})));
  Modelica.StateGraph.StepWithSignal oneOn(nOut=2, nIn=2)
    "One boiler is on"
    annotation (Placement(
        transformation(
        extent={{10,-10},{-10,10}},
        rotation=90,
        origin={-50,0})));
  Modelica.StateGraph.StepWithSignal twoOn "Two boilers are on"
    annotation (Placement(
        transformation(
        extent={{10,-10},{-10,10}},
        rotation=90,
        origin={-50,-70})));
  Modelica.StateGraph.Transition offToOne(
    condition=on == true,
    enableTimer=true,
    waitTime=tWai)
    "Condition of transition from off to one boiler on"
    annotation (Placement(transformation(
        extent={{10,10},{-10,-10}},
        rotation=90,
        origin={-50,40})));
  Modelica.StateGraph.Transition oneToTwo(
    enableTimer=true,
    waitTime=tWai,
    condition= QLoa >=  (criPoiLoa + dQ))
    "Condition of transition from one boiler to two boilers"
    annotation (
      Placement(transformation(
        extent={{10,10},{-10,-10}},
        rotation=90,
        origin={-50,-40})));
  Modelica.StateGraph.Transition twoToOne(
    enableTimer=true,
    waitTime=tWai,
    condition= QLoa < (criPoiLoa - dQ))
    "Condition of transion from two boilers to one boiler"
    annotation (
      Placement(transformation(
        extent={{-10,10},{10,-10}},
        rotation=90,
        origin={0,-40})));
  Modelica.StateGraph.Transition oneToOff(
    condition=on == false,
    enableTimer=true,
    waitTime=tWai) "Transition from one boiler to off"
    annotation (Placement(
        transformation(
        extent={{-10,10},{10,-10}},
        rotation=90,
        origin={-10,40})));
  inner Modelica.StateGraph.StateGraphRoot stateGraphRoot
    annotation (Placement(transformation(extent={{-100,58},{-80,78}})));
  Modelica.Blocks.Tables.CombiTable1Ds comTab(table=[0,0,0; 1,1,0; 2,1,1])
    annotation (Placement(transformation(extent={{70,-10},{90,10}})));
  Buildings.Controls.OBC.CDL.Conversions.BooleanToInteger booToInt(
    final integerTrue=1, final integerFalse=0)
    annotation (Placement(transformation(extent={{20,-40},{40,-20}})));
  Buildings.Controls.OBC.CDL.Conversions.BooleanToInteger booToInt1(
    final integerFalse=0, final integerTrue=2)
    annotation (Placement(transformation(extent={{20,-80},{40,-60}})));
  Buildings.Controls.OBC.CDL.Integers.Add addInt
    annotation (Placement(transformation(extent={{60,-60},{80,-40}})));
  Buildings.Controls.OBC.CDL.Conversions.IntegerToReal intToRea
    annotation (Placement(transformation(extent={{38,-10},{58,10}})));
  Modelica.Blocks.Math.RealToBoolean boiOn[numBoi]
    "Real value to boolean value"
    annotation (Placement(transformation(extent={{68,30},{88,50}})));
  Modelica.Blocks.Interfaces.BooleanOutput y_On[numBoi]
    "On signal of the boilers" annotation (Placement(transformation(extent={{100,
            30},{120,50}}), iconTransformation(extent={{100,30},{120,50}})));
equation
  connect(off.outPort[1], offToOne.inPort) annotation (Line(points={{-50,59.5},{
          -50,44}},               color={0,0,0}));
  connect(oneToOff.outPort, off.inPort[1]) annotation (Line(points={{-10,41.5},{
          -10,88},{-50,88},{-50,81}},     color={0,0,0}));
  connect(oneToTwo.outPort, twoOn.inPort[1]) annotation (Line(points={{-50,-41.5},{-50,-59}}, color={0,0,0}));
  connect(twoOn.outPort[1], twoToOne.inPort) annotation (Line(points={{-50,-80.5},
          {-50,-88},{-2.22045e-16,-88},{-2.22045e-16,-44}},
                                          color={0,0,0}));
  connect(twoToOne.outPort, oneOn.inPort[2]) annotation (Line(points={{0,-38.5},
          {0,20},{-49.5,20},{-49.5,11}},   color={0,0,0}));
  connect(offToOne.outPort, oneOn.inPort[1]) annotation (Line(points={{-50,38.5},
          {-50,24},{-50,11},{-50.5,11}}, color={0,0,0}));
  connect(oneOn.outPort[2], oneToOff.inPort) annotation (Line(points={{-49.75,
          -10.5},{-49.75,-20},{-10,-20},{-10,36}},
                                            color={0,0,0}));
  connect(oneOn.outPort[1], oneToTwo.inPort) annotation (Line(points={{-50.25,-10.5},
          {-50.25,-18},{-50,-18},{-50,-36}}, color={0,0,0}));
  connect(comTab.y, y)
    annotation (Line(points={{91,0},{110,0}}, color={0,0,127}));
  connect(comTab.u, intToRea.y)
    annotation (Line(points={{68,0},{60,0}}, color={0,0,127}));
  connect(addInt.u2, booToInt1.y) annotation (Line(points={{58,-56},{48,-56},{48,
          -70},{42,-70}}, color={255,127,0}));
  connect(oneOn.active, booToInt.u) annotation (Line(points={{-39,0},{4,0},{4,-30},
          {18,-30}},      color={255,0,255}));
  connect(twoOn.active, booToInt1.u) annotation (Line(points={{-39,-70},{18,-70}}, color={255,0,255}));
  connect(booToInt.y, addInt.u1) annotation (Line(points={{42,-30},{46,-30},{46,
          -44},{58,-44}}, color={255,127,0}));
  connect(addInt.y, intToRea.u) annotation (Line(points={{82,-50},{94,-50},{94,-14},
          {34,-14},{34,0},{36,0}}, color={255,127,0}));
  connect(comTab.y, boiOn.u) annotation (Line(points={{91,0},{92,0},{92,20},{60,
          20},{60,40},{66,40}}, color={0,0,127}));
  connect(boiOn.y, y_On)
    annotation (Line(points={{89,40},{110,40}}, color={255,0,255}));
  annotation (Diagram(coordinateSystem(preserveAspectRatio=false, extent={{-100,
            -100},{100,100}})),           Icon(coordinateSystem(
          preserveAspectRatio=false, extent={{-100,-100},{100,100}}), graphics={
                                Rectangle(
        extent={{-100,-100},{100,100}},
        lineColor={0,0,127},
        fillColor={255,255,255},
        fillPattern=FillPattern.Solid), Text(
        extent={{-150,150},{150,110}},
        textString="%name",
        lineColor={0,0,255})}),
    Documentation(revisions="<html>
<ul>
<li>
March 19, 2014 by Sen Huang:<br/>
First implementation.
</li>
</ul>
</html>"));
end BoilerStage;
