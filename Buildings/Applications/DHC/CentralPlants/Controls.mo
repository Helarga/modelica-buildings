within Buildings.Applications.DHC.CentralPlants;
package Controls "Controllers of central plant equipments."
  model ChillersStagingController "Staging of chillers"
    extends Modelica.Blocks.Icons.Block;

    parameter Integer nChi=3 "nChi";
    parameter Modelica.SIunits.Time tWai "Waiting time";
    parameter Modelica.SIunits.Power QEva_nominal
      "Nominal cooling capaciaty(Negative means cooling)";
    parameter Modelica.SIunits.TemperatureDifference deaBan1
      "Dead band width 1 for switching chiller on ";
    parameter Modelica.SIunits.Power  dQ = 0.25*QEva_nominal
      "Deadband for critical point of cooling load";
    parameter Modelica.SIunits.Power  criPoiLoa = 0.55*QEva_nominal
      "Critical point of cooling load for switching one chiller on or off";
    parameter Modelica.SIunits.Temperature criPoiTem=279.15
      "Critical point of temperature for switching one chiller on or off";
    parameter Modelica.SIunits.TemperatureDifference dT = 1
      "Deadband width for critical point of switching temperature";

    Modelica.Blocks.Interfaces.RealInput TChWSet(
      final quantity="ThermodynamicTemperature",
      final unit="K",
      displayUnit="degC") "Supply chilled water temperature setpoint."
      annotation (Placement(transformation(extent={{-200,60},{-160,100}}),
          iconTransformation(extent={{-200,60},{-160,100}})));

    inner Modelica.StateGraph.StateGraphRoot stateGraphRoot
      annotation (Placement(transformation(extent={{-134,72},{-114,92}})));
    Modelica.Blocks.Interfaces.RealInput TChWSup(
      final quantity="ThermodynamicTemperature",
      final unit="K",
      displayUnit="degC") "Measured chilled water supply temperature."
      annotation (Placement(transformation(extent={{-200,-20},{-160,20}}),
          iconTransformation(extent={{-200,-20},{-160,20}})));

    Modelica.StateGraph.Transition con1(
      condition=TChWSup > TChWSet + deaBan1,
      enableTimer=true,
      waitTime=tWai) "Turn on the first chiller."
      annotation (Placement(transformation(extent={{-114,40},{-94,60}})));
    Modelica.StateGraph.Transition con2(
      condition= -QReq >= -(criPoiLoa * ((Integer(Buildings.Applications.DHC.CentralPlants.Types.ChillerStaging.ChillerOne)-1)-dQ)),
      enableTimer=true,
      waitTime=tWai) "Turn on chiller two in addition to chiller one."
      annotation (Placement(transformation(extent={{-64,40},{-44,60}})));
    Modelica.StateGraph.InitialStep off
      annotation (Placement(transformation(extent={{-144,20},{-124,40}})));
    Modelica.StateGraph.StepWithSignal chiOne(nIn=2, nOut=2)
      annotation (Placement(transformation(extent={{-88,20},{-68,40}})));
    Modelica.StateGraph.StepWithSignal chiTwo(nIn=2, nOut=2)
      annotation (Placement(transformation(extent={{-34,20},{-14,40}})));
    Modelica.StateGraph.StepWithSignal chiThr
      annotation (Placement(transformation(extent={{26,20},{46,40}})));

    Modelica.Blocks.Interfaces.RealInput QReq
      "Total required cooling load from the district side, negative"
      annotation (Placement(transformation(extent={{-202,-100},{-162,-60}}),
          iconTransformation(extent={{-202,-100},{-162,-60}})));
    Modelica.StateGraph.Transition con3(
      condition= -QReq >= -(criPoiLoa* ((Integer(Buildings.Applications.DHC.CentralPlants.Types.ChillerStaging.ChillerTwo)-1)-dQ)),
      enableTimer=true,
      waitTime=tWai)
      annotation (Placement(transformation(extent={{-4,40},{16,60}})));
    Modelica.StateGraph.Transition con5(
      condition=-QReq<= -(criPoiLoa* ((Integer(Buildings.Applications.DHC.CentralPlants.Types.ChillerStaging.ChillerOne)-1)-dQ)),
      enableTimer=true,
      waitTime=tWai)
      annotation (Placement(transformation(extent={{-46,-14},{-66,6}})));
    Modelica.StateGraph.Transition con4(
      condition=-QReq <= -(criPoiLoa* ((Integer(Buildings.Applications.DHC.CentralPlants.Types.ChillerStaging.ChillerTwo)-1)-dQ)),
      enableTimer=true,
      waitTime=tWai)
      annotation (Placement(transformation(extent={{12,-20},{-8,0}})));
    Modelica.StateGraph.Transition con6(
      condition=TChWSup < TChWSet -deaBan1,
      enableTimer=true,
      waitTime=tWai)
      annotation (Placement(transformation(extent={{-98,-20},{-118,0}})));
    Buildings.Controls.OBC.CDL.Conversions.BooleanToInteger booToInt(
      final integerTrue=1,
      final integerFalse=0)
      annotation (Placement(transformation(extent={{-64,-60},{-44,-40}})));
    Buildings.Controls.OBC.CDL.Conversions.BooleanToInteger booToInt1(final
        integerTrue=2,
      final integerFalse=0)
      annotation (Placement(transformation(extent={{-4,-60},{16,-40}})));
    Buildings.Controls.OBC.CDL.Conversions.BooleanToInteger booToInt2(final
        integerTrue=3,
      final integerFalse=0)
      annotation (Placement(transformation(extent={{42,-60},{62,-40}})));
    Modelica.Blocks.Tables.CombiTable1Ds combiTable1Ds(table=[
      0,0,0,0;
      1,1,0,0;
      2,1,1,0;
      3,1,1,1])
      annotation (Placement(transformation(extent={{132,-40},{152,-20}})));
    Buildings.Controls.OBC.CDL.Integers.MultiSum mulSumInt(nin=3)
      annotation (Placement(transformation(extent={{66,-80},{86,-60}})));
    Buildings.Controls.OBC.CDL.Conversions.IntegerToReal intToRea
      annotation (Placement(transformation(extent={{100,-40},{120,-20}})));
    Modelica.Blocks.Interfaces.RealOutput y[nChi]
      "On/off signal for the chillers - 0: off; 1: on"
      annotation (Placement(transformation(extent={{162,-8},{182,12}}),
          iconTransformation(extent={{162,-8},{182,12}})));
  equation
    connect(off.outPort[1], con1.inPort) annotation (Line(points={{-123.5,30},{-118,
            30},{-118,50},{-108,50}},
                                color={0,0,0}));
    connect(chiThr.outPort[1], con4.inPort) annotation (Line(
        points={{46.5,30},{76,30},{76,-10},{6,-10}},
        color={0,0,0},
        pattern=LinePattern.Dot));
    connect(con6.outPort, off.inPort[1]) annotation (Line(
        points={{-109.5,-10},{-158,-10},{-158,30},{-145,30}},
        color={0,0,0},
        pattern=LinePattern.Dot));
    connect(chiOne.active, booToInt.u)
      annotation (Line(points={{-78,19},{-78,-50},{-66,-50}},color={255,0,255}));
    connect(chiTwo.active, booToInt1.u)
      annotation (Line(points={{-24,19},{-24,-50},{-6,-50}},
                                                           color={255,0,255}));
    connect(chiThr.active, booToInt2.u) annotation (Line(points={{36,19},{36,-50},
            {40,-50}},  color={255,0,255}));
    connect(intToRea.y,combiTable1Ds. u)
      annotation (Line(points={{122,-30},{130,-30}},  color={0,0,127}));
    connect(booToInt2.y, mulSumInt.u[1]) annotation (Line(points={{64,-50},{
            64,-65.3333}},                  color={255,127,0}));
    connect(booToInt1.y, mulSumInt.u[2]) annotation (Line(points={{18,-50},{32,-50},
            {32,-70},{64,-70}},  color={255,127,0}));
    connect(booToInt.y, mulSumInt.u[3]) annotation (Line(points={{-42,-50},{
            -38,-50},{-38,-74.6667},{64,-74.6667}},
                                           color={255,127,0}));
    connect(intToRea.u, mulSumInt.y) annotation (Line(points={{98,-30},{92,-30},{92,
            -70},{88,-70}},       color={255,127,0}));
    connect(con1.outPort, chiOne.inPort[1]) annotation (Line(points={{-102.5,50},{
            -94,50},{-94,30.5},{-89,30.5}},
                                        color={0,0,0}));
    connect(chiOne.outPort[1], con2.inPort) annotation (Line(points={{-67.5,30.25},
            {-64,30.25},{-64,50},{-58,50}},
                                      color={0,0,0}));
    connect(con2.outPort, chiTwo.inPort[1]) annotation (Line(points={{-52.5,50},{-46,
            50},{-46,30.5},{-35,30.5}},
                                      color={0,0,0}));
    connect(chiTwo.outPort[1], con3.inPort) annotation (Line(points={{-13.5,30.25},
            {-4,30.25},{-4,50},{2,50}},  color={0,0,0}));
    connect(con3.outPort, chiThr.inPort[1]) annotation (Line(points={{7.5,50},{16,
            50},{16,30},{25,30}}, color={0,0,0}));
    connect(con4.outPort, chiTwo.inPort[2]) annotation (Line(
        points={{0.5,-10},{-46,-10},{-46,29.5},{-35,29.5}},
        color={0,0,0},
        pattern=LinePattern.Dot));
    connect(chiTwo.outPort[2], con5.inPort) annotation (Line(
        points={{-13.5,29.75},{-10,29.75},{-10,-4},{-52,-4}},
        color={0,0,0},
        pattern=LinePattern.Dot));
    connect(con5.outPort, chiOne.inPort[2]) annotation (Line(
        points={{-57.5,-4},{-92,-4},{-92,29.5},{-89,29.5}},
        color={0,0,0},
        pattern=LinePattern.Dot));
    connect(chiOne.outPort[2], con6.inPort) annotation (Line(
        points={{-67.5,29.75},{-66,29.75},{-66,-10},{-104,-10}},
        color={0,0,0},
        pattern=LinePattern.Dot));
    connect(combiTable1Ds.y, y) annotation (Line(points={{153,-30},{158,-30},{158,
            2},{172,2}}, color={0,0,127}));
            annotation (Placement(transformation(extent={{-150,100},{100,100}})),
            Diagram(coordinateSystem(extent={{-160,-100},{160,100}},
            preserveAspectRatio=true)),
  Documentation(info="<html>

<p>
where <i>T<sub>WSE_CHWST</sub></i> is the chilled water supply temperature for the WSE,
<i>T<sub>WetBul</sub></i> is the wet bulb temperature,
<i>T<sub>TowApp</sub></i> is the cooling tower approach, <i>T<sub>WSE_CHWRT</sub></i>
is the chilled water return temperature for the WSE, and <i>T<sub>CHWSTSet</sub></i>
is the chilled water supply temperature setpoint for the system.
<i>deaBan 1-4</i> are deadbands for each switching point.
</p>
<h4>References</h4>

</html>",          revisions="<html>
<ul>
<li>


</li>
</ul>
</html>"),
      Icon(coordinateSystem(preserveAspectRatio=false, extent={{-100,-100},{100,100}})));
  end ChillersStagingController;

  model ChillerStage "Chiller staging control logic"
    extends Modelica.Blocks.Icons.Block;

    parameter Modelica.SIunits.Time tWai "Waiting time";
    parameter Modelica.SIunits.Power QEva_nominal
      "Nominal cooling capaciaty(Negative means cooling)";
    parameter Modelica.SIunits.Power  criPoiLoa = 0.55*QEva_nominal
      "Critical point of cooling load for switching one chiller on or off";
    parameter Modelica.SIunits.Power  dQ = 0.25*QEva_nominal
      "Deadband for critical point of cooling load";
    parameter Modelica.SIunits.Temperature criPoiTem = 279.15
      "Critical point of temperature for switching one chiller on or off";
    parameter Modelica.SIunits.TemperatureDifference dT = 1
      "Deadband width for critical point of switching temperature";

    Modelica.Blocks.Interfaces.IntegerInput cooMod
      "Cooling mode signal, integer value of
    Buildings.Applications.DataCenters.Types.CoolingMode"
      annotation (Placement(transformation(extent={{-140,68},{-100,108}})));
    Modelica.Blocks.Interfaces.RealInput QReq
      "Total required cooling load from the district side, negative"
      annotation (Placement(transformation(extent={{-140,8},{-100,48}})));
    Modelica.Blocks.Interfaces.RealOutput y[2]
      "On/off signal for the chillers - 0: off; 1: on"
      annotation (Placement(transformation(extent={{224,-2},{244,18}})));
    Modelica.Blocks.Interfaces.RealInput TCHWSup(
      final quantity="ThermodynamicTemperature",
      final unit="K",
      displayUnit="degC") "Temperature of leaving chilled water "
      annotation (Placement(transformation(extent={{-140,-52},{-100,-12}})));

    Modelica.StateGraph.TransitionWithSignal con1(
      enableTimer=true,
      waitTime=tWai)
      "Fire condition 1: free cooling to partially mechanical cooling"
      annotation (Placement(transformation(
          extent={{10,10},{-10,-10}},
          rotation=180,
          origin={-60,20})));
    Modelica.StateGraph.StepWithSignal oneOn(nIn=2, nOut=2)
      "One chiller is commanded on"
      annotation (Placement(transformation(
          extent={{10,10},{-10,-10}},
          rotation=180,
          origin={-34,20})));
    Modelica.StateGraph.InitialStep off(nIn=1) "Free cooling mode"
      annotation (Placement(transformation(
          extent={{10,10},{-10,-10}},
          rotation=180,
          origin={-84,20})));
    Modelica.StateGraph.StepWithSignal twoOn "Two chillers are commanded on"
      annotation (Placement(transformation(
          extent={{10,10},{-10,-10}},
          rotation=180,
          origin={26,20})));
    Modelica.StateGraph.TransitionWithSignal con2(
      enableTimer=true,
      waitTime=tWai)
      "Fire condition 2: partially mechanical cooling to fully mechanical cooling"
      annotation (Placement(transformation(
          extent={{10,10},{-10,-10}},
          rotation=180,
          origin={-4,20})));
    Modelica.StateGraph.TransitionWithSignal con3(
      enableTimer=true,
      waitTime=tWai)
      "Fire condition 3: fully mechanical cooling to partially mechanical cooling"
      annotation (Placement(transformation(
          extent={{-10,-10},{10,10}},
          rotation=0,
          origin={50,20})));
    Modelica.StateGraph.Transition con4(
      enableTimer=true,
      waitTime=tWai,
      condition=cooMod == Integer(Buildings.Applications.DataCenters.Types.CoolingModes.FreeCooling))
      "Fire condition 4: partially mechanical cooling to free cooling"
      annotation (Placement(transformation(
          extent={{-10,-10},{10,10}},
          rotation=180,
          origin={-48,68})));
    inner Modelica.StateGraph.StateGraphRoot stateGraphRoot
      annotation (Placement(transformation(extent={{160,90},{180,110}})));

    Modelica.Blocks.Tables.CombiTable1Ds combiTable1Ds(table=[0,0,0; 1,1,0; 2,1,1])
      annotation (Placement(transformation(extent={{196,-2},{216,18}})));

    Buildings.Controls.OBC.CDL.Conversions.BooleanToInteger booToInt(
      final integerTrue=1,
      final integerFalse=0)
      annotation (Placement(transformation(extent={{60,78},{80,98}})));
    Buildings.Controls.OBC.CDL.Conversions.BooleanToInteger booToInt1(
      final integerFalse=0, final integerTrue=2)
      annotation (Placement(transformation(extent={{60,-80},{80,-60}})));
    Buildings.Controls.OBC.CDL.Integers.Add addInt
      annotation (Placement(transformation(extent={{122,0},{142,20}})));
    Buildings.Controls.OBC.CDL.Conversions.IntegerToReal intToRea
      annotation (Placement(transformation(extent={{158,-2},{178,18}})));
  equation
    connect(off.outPort[1], con1.inPort)
      annotation (Line(
        points={{-73.5,20},{-64,20}},
        color={0,0,0},
        pattern=LinePattern.Dash));
    connect(con1.outPort, oneOn.inPort[1])
      annotation (Line(
        points={{-58.5,20},{-58.5,20.5},{-45,20.5}},
        color={0,0,0},
        pattern=LinePattern.Dash));
    connect(con2.inPort, oneOn.outPort[1])
      annotation (Line(
        points={{-8,20},{-23.5,20},{-23.5,20.25}},
        color={0,0,0},
        pattern=LinePattern.Dash));
    connect(con2.outPort, twoOn.inPort[1])
      annotation (Line(
        points={{-2.5,20},{15,20}},
        color={0,0,0},
        pattern=LinePattern.Dash));
    connect(twoOn.outPort[1], con3.inPort)
      annotation (Line(
        points={{36.5,20},{46,20}},
        color={0,0,0},
        pattern=LinePattern.Dash));
    connect(con4.outPort, off.inPort[1])
      annotation (Line(
        points={{-49.5,68},{-49.5,70.5},{-95,70.5},{-95,20}},
        color={0,0,0},
        pattern=LinePattern.Dash));
    connect(combiTable1Ds.y, y)
      annotation (Line(points={{217,8},{234,8}},color={0,0,127}));
    connect(booToInt.y, addInt.u1) annotation (Line(points={{82,88},{90,88},{90,16},
            {120,16}},      color={255,127,0}));
    connect(booToInt1.y, addInt.u2) annotation (Line(points={{82,-70},{94,-70},{94,
            4},{120,4}},    color={255,127,0}));
    connect(intToRea.y, combiTable1Ds.u)
      annotation (Line(points={{180,8},{194,8}},      color={0,0,127}));
    connect(addInt.y, intToRea.u)
      annotation (Line(points={{144,10},{156,10},{156,8}}, color={255,127,0}));
    connect(oneOn.outPort[1], con4.inPort) annotation (Line(points={{-23.5,
            20.25},{-14,20.25},{-14,68},{-44,68}},
                                            color={0,0,0}));
    connect(con3.outPort, oneOn.inPort[2]) annotation (Line(points={{51.5,20},
            {80,20},{80,-20},{-52,-20},{-52,19.5},{-45,19.5}},
                                                           color={0,0,0}));
    annotation (Documentation(info="<html>
<p>
This is a chiller staging control that works as follows:
</p>
<ul>
<li>
The chillers are all off when cooling mode is Free Cooling.
</li>
<li>
One chiller is commanded on when cooling mode is not Free Cooling.
</li>
<li>
Two chillers are commanded on when cooling mode is not Free Cooling
and the cooling load addressed by each chiller is larger than
a critical value.
</li>
</ul>
</html>",   revisions="<html>
<ul>
<li>
September 11, 2017, by Michael Wetter:<br/>
Revised switch that selects the operation mode for
<a href=\"https://github.com/lbl-srg/modelica-buildings/issues/921\">issue 921</a>
</li>
<li>
July 30, 2017, by Yangyang Fu:<br/>
First implementation.
</li>
</ul>
</html>"));
  end ChillerStage;

  package Validation "Package which validates differnt controllers."
      extends Modelica.Icons.ExamplesPackage;
    model StagingChillers
      extends Modelica.Icons.Example;
      package Medium = Buildings.Media.Water
        "Source side medium";
      parameter Modelica.SIunits.MassFlowRate m_flow_nominal = 1
        "Mass flow rate at nominal conditions";
      Buildings.Controls.OBC.CDL.Continuous.Sources.Constant TSetCoo(k=5 +
            273.15,
          y(final unit="K", displayUnit="degC"))
        "Chilled water setpoint temperature"
        annotation (Placement(transformation(extent={{-60,80},{-40,100}})));
      Modelica.Blocks.Sources.Trapezoid QReq(
        amplitude=-10000,
        rising=100,
        width=100,
        falling=100,
        period=400,
        y(final unit="W", displayUnit="W"),
        offset=-1000,
        startTime=0) "Required cooling loads"
        annotation (Placement(transformation(extent={{-60,0},{-40,20}})));
      Buildings.Controls.OBC.CDL.Continuous.Sources.Pulse TSupChWat(
        amplitude=3,
        period=300,
        offset=3.5 + 273.15) "Measured chilled water supply temperature."
        annotation (Placement(transformation(extent={{-60,40},{-40,60}})));
      CentralPlants.Controls.ChillersStagingController chillersStagingController(
        nChi=3,
        tWai=10,
        QEva_nominal=-10000,
        deaBan1=1)
        annotation (Placement(transformation(extent={{0,40},{20,60}})));
    equation
      connect(chillersStagingController.TChWSet, TSetCoo.y) annotation (Line(
            points={{-8,58},{-18,58},{-18,90},{-38,90}}, color={0,0,127}));
      connect(TSupChWat.y, chillersStagingController.TChWSup)
        annotation (Line(points={{-38,50},{-8,50}}, color={0,0,127}));
      connect(QReq.y, chillersStagingController.QReq) annotation (Line(points=
             {{-39,10},{-20,10},{-20,42},{-8.2,42}}, color={0,0,127}));
      annotation (
    Documentation(
    info="<html>
<p>
This model validates
<a href=\"modelica://Buildings.Applications.DHC.Controls.MixingValveControl\">
Buildings.Applications.DHC.Controls.MixingValveControl</a>
(as part of
<a href=\"modelica://Buildings.Applications.DHC.Loads.BaseClasses.FlowDistribution\">
Buildings.Applications.DHC.Loads.BaseClasses.FlowDistribution</a>)
in change-over mode.
</p>
</html>",
    revisions=
    "<html>
<ul>
<li>
February 21, 2020, by Antoine Gautier:<br/>
First implementation.
</li>
</ul>
</html>"),
      Diagram(
        coordinateSystem(preserveAspectRatio=false, extent={{-180,-200},{180,200}})),
      experiment(
          StopTime=1000,
          Tolerance=1e-06),
      __Dymola_Commands(file="Resources/Scripts/Dymola/Applications/DHC/Controls/Validation/MixingValveControl.mos"
        "Simulate and plot"));
    end StagingChillers;
  end Validation;
end Controls;
