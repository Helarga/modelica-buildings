within Buildings.Applications.DHC.CentralPlants.Heating.Generation4.Controls;
model BoilerLeavingWaterControl "Stage controller for boilers"

  parameter Modelica.SIunits.Time tWai "Waiting time";


  Modelica.StateGraph.InitialStep off(nIn=1) "No heating is demanded"
    annotation (Placement(transformation(
        extent={{10,10},{-10,-10}},
        rotation=180,
        origin={-70,10})));
  Modelica.StateGraph.StepWithSignal boiOn(nOut=1, nIn=1) "boiler is on"
    annotation (Placement(
        transformation(
        extent={{10,10},{-10,-10}},
        rotation=180,
        origin={0,10})));
  Modelica.StateGraph.TransitionWithSignal
                                 offToOne(
    enableTimer=true, waitTime=1200)
    "Condition of transition from off to one boiler on"
    annotation (Placement(transformation(
        extent={{-10,-10},{10,10}},
        rotation=0,
        origin={-40,10})));
  Modelica.StateGraph.TransitionWithSignal boiOff(enableTimer=true, waitTime=
        300)  "boiler to off." annotation (Placement(transformation(
        extent={{-10,-10},{10,10}},
        rotation=0,
        origin={54,10})));
  inner Modelica.StateGraph.StateGraphRoot stateGraphRoot
    annotation (Placement(transformation(extent={{-90,60},{-70,80}})));
  Buildings.Controls.OBC.CDL.Continuous.GreaterEqual
                                                   greEqu
    "Threshold for boiler control"
    annotation (Placement(transformation(extent={{-54,-72},{-34,-52}})));
  Buildings.Controls.OBC.CDL.Continuous.Hysteresis lessThreshold2( uLow=19 + 273.15,
      uHigh=20 + 273.15,
    pre_y_start=true)
    "Threshold for boiler control"
    annotation (Placement(transformation(extent={{-80,-48},{-60,-28}})));
  Modelica.Blocks.Interfaces.RealInput THWSup(unit="K")
    "Heating water supplu temperature." annotation (Placement(transformation(
          extent={{-120,-10},{-100,10}}),iconTransformation(extent={{-120,-10},
            {-100,10}})));
  Modelica.Blocks.Interfaces.BooleanOutput on
    "Heating water supply temperature control." annotation (Placement(
        transformation(extent={{100,-30},{120,-10}}), iconTransformation(extent=
           {{100,-10},{120,10}})));
  Modelica.Blocks.Sources.RealExpression           lessThreshold1(y=72 + 273.15)
    "Threshold for boiler control"
    annotation (Placement(transformation(extent={{-94,-86},{-74,-66}})));
equation
  connect(lessThreshold2.y, offToOne.condition) annotation (Line(points={{-58,-38},
          {-40,-38},{-40,-2}},      color={255,0,255}));
  connect(THWSup, lessThreshold2.u) annotation (Line(points={{-110,0},{-96,0},{
          -96,-38},{-82,-38}},       color={0,0,127}));
  connect(off.outPort[1], offToOne.inPort)
    annotation (Line(points={{-59.5,10},{-44,10}}, color={0,0,0}));
  connect(offToOne.outPort, boiOn.inPort[1])
    annotation (Line(points={{-38.5,10},{-11,10}}, color={0,0,0}));
  connect(greEqu.y, boiOff.condition)
    annotation (Line(points={{-32,-62},{54,-62},{54,-2}}, color={255,0,255}));
  connect(boiOn.outPort[1], boiOff.inPort)
    annotation (Line(points={{10.5,10},{50,10}}, color={0,0,0}));
  connect(boiOff.outPort, off.inPort[1]) annotation (Line(points={{55.5,10},{82,
          10},{82,52},{-96,52},{-96,10},{-81,10}}, color={0,0,0}));
  connect(boiOn.active, on)
    annotation (Line(points={{0,-1},{0,-20},{110,-20}}, color={255,0,255}));
  connect(THWSup, greEqu.u1) annotation (Line(points={{-110,0},{-96,0},{-96,-62},
          {-56,-62}}, color={0,0,127}));
  connect(lessThreshold1.y, greEqu.u2) annotation (Line(points={{-73,-76},{-64,
          -76},{-64,-70},{-56,-70}}, color={0,0,127}));
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
end BoilerLeavingWaterControl;
