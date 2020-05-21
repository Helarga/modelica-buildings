within Buildings.Applications.DHC;
package Controls "Package of control sequences for district heating and cooling systems"
  extends Modelica.Icons.VariantsPackage;

  block MixingValveControl "Mixing valve controller"
    extends Modelica.Blocks.Icons.Block;

    import Type_dis = Buildings.Applications.DHC.Loads.Types.DistributionType
      "Types of distribution system";

    parameter Type_dis typDis = Type_dis.HeatingWater
      "Type of distribution system"
      annotation(Evaluate=true);
    parameter Real k(
      final min=0,
      final unit="1") = 0.1 "Gain of controller";
    parameter Modelica.SIunits.Time Ti(final min=Modelica.Constants.small) = 10
      "Time constant of integrator block";

    // IO CONNECTORS
    Modelica.Blocks.Interfaces.RealInput TSupSet(
      final quantity="ThermodynamicTemperature", final unit="K", final displayUnit="degC")
      "Supply temperature set point"
      annotation (Placement(transformation(
        extent={{-20,-20},{20,20}},
        rotation=0,
        origin={-120,-40}),
        iconTransformation(
        extent={{-10,-10},{10,10}},
        rotation=0,
        origin={-110,40})));
    Modelica.Blocks.Interfaces.IntegerInput modChaOve if typDis == Type_dis.ChangeOver
      "Operating mode in change-over (1 for heating, -1 for cooling)"
      annotation (Placement(
        transformation(
        extent={{-20,-20},{20,20}},
        rotation=0,
        origin={-120,80}),
        iconTransformation(
        extent={{-10,-10},{10,10}},
        rotation=0,
        origin={-110,80})));
    Modelica.Blocks.Interfaces.RealInput TSupMes(
      final quantity="ThermodynamicTemperature",  final unit="K", final displayUnit="degC")
      "Supply temperature (measured)"
      annotation (Placement(
          transformation(
          extent={{-20,-20},{20,20}},
          rotation=0,
          origin={-120,-80}), iconTransformation(
          extent={{-10,-10},{10,10}},
          rotation=0,
          origin={-110,-40})));
    Modelica.Blocks.Interfaces.RealOutput yVal(final unit="1")
      "Valve control signal"
      annotation (
        Placement(transformation(
          extent={{-20,-20},{20,20}},
          rotation=0,
          origin={120,0}), iconTransformation(
          extent={{-10,-10},{10,10}},
          rotation=0,
          origin={110,0})));
    // COMPONENTS
    Buildings.Controls.OBC.CDL.Continuous.Sources.Constant zer(k=0)
      "Zero constant"
      annotation (Placement(transformation(extent={{-70,-30},{-50,-10}})));
    Modelica.Blocks.Math.IntegerToBoolean toBoo(threshold=2) if
      typDis == Type_dis.ChangeOver
      "Conversion to boolean (true if heating mode)"
      annotation (Placement(transformation(extent={{-10,30},{10,50}})));
    Buildings.Controls.OBC.CDL.Continuous.LimPID conTSup(
      controllerType=Buildings.Controls.OBC.CDL.Types.SimpleController.PI,
      final k=k,
      final Ti=Ti,
      final yMax=1,
      final yMin=-1,
      final reverseAction=false,
      final reset=if typDis == Type_dis.ChangeOver then
        Buildings.Controls.OBC.CDL.Types.Reset.Parameter else
        Buildings.Controls.OBC.CDL.Types.Reset.Disabled,
      final y_reset=0)
      "PI controller tracking supply temperature"
      annotation (Placement(transformation(extent={{-70,-70},{-50,-50}})));
    Buildings.Controls.OBC.CDL.Continuous.Min negPar
      "Negative part of control signal"
      annotation (Placement(transformation(extent={{-10,-50},{10,-30}})));
    Buildings.Controls.OBC.CDL.Continuous.Max posPar
      "Positive part of control signal"
      annotation (Placement(transformation(extent={{-10,-90},{10,-70}})));
    Buildings.Controls.OBC.CDL.Continuous.Gain opp(k=-1)
      "Opposite value"
      annotation (Placement(transformation(extent={{20,-50},{40,-30}})));
    Buildings.Controls.OBC.CDL.Logical.Switch swi
      "Logical switch"
      annotation (Placement(transformation(extent={{70,-10},{90,10}})));
    Modelica.Blocks.Sources.BooleanExpression fixMod(final y=typDis == Type_dis.ChilledWater) if
      typDis <> Type_dis.ChangeOver
      "Fixed operating mode"
      annotation (Placement(transformation(extent={{-10,-10},{10,10}})));
    Buildings.Controls.OBC.CDL.Integers.Change cha if
      typDis == Type_dis.ChangeOver
      "Evaluate the integer input u to check if its value changes"
      annotation (Placement(transformation(extent={{-80,70},{-60,90}})));
  equation
    connect(modChaOve, toBoo.u)
      annotation (Line(points={{-120,80},{-90,80},{-90,40},{-12,40}},
        color={255,127,0}));
    connect(toBoo.y, swi.u2)
      annotation (Line(points={{11,40},{30,40},{30,0},{68,0}},
        color={255,0,255}));
    connect(fixMod.y, swi.u2)
      annotation (Line(points={{11,0},{68,0}}, color={255,0,255}));
    connect(conTSup.y, posPar.u2)
      annotation (Line(points={{-48,-60},{-20,-60},{-20,-86},{-12,-86}}, color={0,0,127}));
    connect(zer.y, posPar.u1)
      annotation (Line(points={{-48,-20},{-40,-20},{-40,-74},{-12,-74}}, color={0,0,127}));
    connect(zer.y, negPar.u1)
      annotation (Line(points={{-48,-20},{-40,-20},{-40,-34},{-12,-34}}, color={0,0,127}));
    connect(conTSup.y, negPar.u2)
      annotation (Line(points={{-48,-60},{-20,-60},{-20,-46},{-12,-46}}, color={0,0,127}));
    connect(negPar.y, opp.u)
      annotation (Line(points={{12,-40},{18,-40}}, color={0,0,127}));
    connect(conTSup.u_s, TSupSet)
      annotation (Line(points={{-72,-60},{-90,-60},{-90,-40},{-120,-40}}, color={0,0,127}));
    connect(TSupMes, conTSup.u_m)
      annotation (Line(points={{-120,-80},{-60,-80},{-60,-72}}, color={0,0,127}));
    connect(swi.y, yVal)
      annotation (Line(points={{92,0},{120,0}}, color={0,0,127}));
    connect(modChaOve, cha.u)
      annotation (Line(points={{-120,80},{-82,80}}, color={255,127,0}));
    connect(cha.y, conTSup.trigger)
      annotation (Line(points={{-58,80},{-40,80},{-40,60},{-80,60},{-80,-76},{
            -66,-76},{-66,-72}},
        color={255,0,255}));
    connect(opp.y, swi.u1)
      annotation (Line(points={{42,-40},{50,-40},{50,8},{68, 8}}, color={0,0,127}));
    connect(posPar.y, swi.u3)
      annotation (Line(points={{12,-80},{60,-80},{60,-8}, {68,-8}}, color={0,0,127}));
    annotation (
    defaultComponentName="conVal",
    Documentation(info="
<html>
<p>
This model implements a generic controller for a three-way mixing valve.
Three operating modes are supported:
</p>
<ul>
<li>
Heating: the controller tracks a minimum supply temperature.
</li>
<li>
Cooling: the controller tracks a maximum supply temperature.
</li>
<li>
Change-over: the controller tracks either a minimum or a maximum
supplied temperature depending on the actual value of the integer input
<code>modChaOve</code> (1 for heating, 2 for cooling).
The model instantiates only one PI block to limit the number of state
variables in large models. Therefore the PI gain
is independent from the change-over mode: the reverse action is modeled
by taking the opposite value of the PI block output. Eventually the
integral part is reset whenever the change-over mode is switched.
</li>
</ul>
<p>
See
<a href=\"modelica://Buildings.Applications.DHC.Controls.Validation.MixingValveControl\">
Buildings.Applications.DHC.Controls.Validation.MixingValveControl</a>
for a simulation with change-over.
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
    Icon(coordinateSystem(preserveAspectRatio=false), graphics={
          Text(
            extent={{-90,96},{-10,66}},
            lineColor={244,125,35},
            fillColor={0,0,0},
            fillPattern=FillPattern.Solid,
            horizontalAlignment=TextAlignment.Left,
            visible=typDis == Type_dis.ChangeOver,
            textString="modChaOve"),
          Text(
            extent={{-90,54},{-22,26}},
            lineColor={0,0,127},
            fillColor={0,0,0},
            fillPattern=FillPattern.Solid,
            horizontalAlignment=TextAlignment.Left,
            textString="TSupSet"),
          Text(
            extent={{-90,-26},{-16,-52}},
            lineColor={0,0,127},
            fillColor={0,0,0},
            fillPattern=FillPattern.Solid,
            horizontalAlignment=TextAlignment.Left,
            textString="TSupMes"),
          Text(
            extent={{50,12},{88,-14}},
            lineColor={0,0,127},
            fillColor={0,0,0},
            fillPattern=FillPattern.Solid,
            horizontalAlignment=TextAlignment.Right,
            textString="yVal")}),
    Diagram(
          coordinateSystem(preserveAspectRatio=false)));
  end MixingValveControl;

  package Validation "Collection of validation models"
    extends Modelica.Icons.ExamplesPackage;

    model MixingValveControl
      "Validation of mixing valve control in change-over mode"
      extends Modelica.Icons.Example;
      package Medium = Buildings.Media.Water
        "Source side medium";
      parameter Modelica.SIunits.MassFlowRate m_flow_nominal = 1
        "Mass flow rate at nominal conditions";
      Buildings.Applications.DHC.Loads.BaseClasses.FlowDistribution disFlo(
        redeclare package Medium = Medium,
        m_flow_nominal=m_flow_nominal,
        typDis=Buildings.Applications.DHC.Loads.Types.DistributionType.ChangeOver,
        have_pum=true,
        have_val=true,
        dp_nominal=100000,
        nPorts_a1=1,
        nPorts_b1=1) "Secondary distribution system"
        annotation (Placement(transformation(extent={{40,10},{60,30}})));
      Buildings.Fluid.Sources.Boundary_pT souPri(
        redeclare package Medium = Medium,
        use_T_in=true,
        nPorts=1)
        "Primary supply stream"
        annotation (Placement(transformation(
            extent={{-10,-10},{10,10}},
            rotation=0,
            origin={-10,20})));
      Buildings.Controls.OBC.CDL.Continuous.Sources.Constant TPriHea(k=313.15,
          y(final unit="K", displayUnit="degC"))
        "Heating water primary supply temperature"
        annotation (Placement(transformation(extent={{-140,110},{-120,130}})));
      Buildings.Controls.OBC.CDL.Continuous.Sources.Constant TPriChi(k=280.15,
          y(final unit="K", displayUnit="degC"))
        "Chilled water primary supply temperature"
        annotation (Placement(transformation(extent={{-140,70},{-120,90}})));
      Buildings.Controls.OBC.CDL.Logical.Switch TPri(
        y(final unit="K", displayUnit="degC"))
        "Actual primary supply temperature"
        annotation (Placement(transformation(extent={{-60,70},{-40,90}})));
      Buildings.Controls.OBC.CDL.Continuous.Sources.Pulse mod(
        amplitude=-1,
        period=1000,
        offset=2) "Operating mode (1 for heating, 2 for cooling)"
        annotation (Placement(transformation(extent={{-140,30},{-120,50}})));
      Buildings.Controls.OBC.CDL.Continuous.LessEqualThreshold
                                                         lesEquThr(threshold=1)
        "Return true if heating mode"
        annotation (Placement(transformation(extent={{-100,30},{-80,50}})));
      Buildings.Controls.OBC.CDL.Continuous.Sources.Constant TSetSecHea(k=303.15,
                    y(final unit="K", displayUnit="degC"))
        "Heating water secondary supply temperature set point"
        annotation (Placement(transformation(extent={{-140,-70},{-120,-50}})));
      Buildings.Controls.OBC.CDL.Continuous.Sources.Constant TSetSecChi(k=291.15)
                    "Chilled water secondary supply temperature set point"
        annotation (Placement(transformation(extent={{-140,-110},{-120,-90}})));
      Buildings.Controls.OBC.CDL.Logical.Switch TSetSecAct(
        y(final unit="K", displayUnit="degC"))
        "Actual secondary supply temperature set point"
        annotation (Placement(transformation(extent={{-60,-90},{-40,-70}})));
      Buildings.Fluid.Sources.MassFlowSource_T souSec(
        use_m_flow_in=true,
        redeclare package Medium = Medium,
        use_T_in=true,
        nPorts=1)
        "Secondary return stream"
        annotation (Placement(
            transformation(
            extent={{10,-10},{-10,10}},
            rotation=0,
            origin={110,60})));
      Buildings.Fluid.Sources.Boundary_pT sinSec(
        redeclare package Medium = Medium, nPorts=1) "Sink for secondary stream"
        annotation (Placement(transformation(
            extent={{10,-10},{-10,10}},
            rotation=0,
            origin={110,100})));
      Buildings.Controls.OBC.CDL.Continuous.Sources.Constant dTSecHea(k=-5)
        "Secondary temperature difference between supply and return"
        annotation (Placement(transformation(extent={{-20,-110},{0,-90}})));
      Buildings.Fluid.Sensors.TemperatureTwoPort senTSecSup(
        redeclare package Medium = Medium,
        m_flow_nominal=m_flow_nominal) "Secondary supply temperature (measured)"
        annotation (Placement(transformation(extent={{70,90},{90,110}})));
      Buildings.Fluid.Sensors.TemperatureTwoPort senTSecRet(redeclare package
          Medium =
            Medium, m_flow_nominal=m_flow_nominal)
        "Secondary return temperature (measured)"
        annotation (Placement(transformation(extent={{90,50},{70,70}})));
      Buildings.Fluid.Sources.Boundary_pT sinPri(redeclare package Medium = Medium, nPorts=1)
        "Sink for primary stream" annotation (Placement(transformation(
            extent={{10,-10},{-10,10}},
            rotation=0,
            origin={110,20})));
      Buildings.Controls.OBC.CDL.Continuous.Sources.Pulse mSec_flow(
        amplitude=m_flow_nominal,
        period=200,
        offset=0) "Secondary mass flow rate"
        annotation (Placement(transformation(extent={{-140,150},{-120,170}})));
      Buildings.Controls.OBC.CDL.Continuous.Add add
        "Computation of secondary return temperature"
        annotation (Placement(transformation(extent={{100,-90},{120,-70}})));
      Buildings.Fluid.Sensors.TemperatureTwoPort senTPriSup(redeclare package
          Medium =
            Medium, m_flow_nominal=m_flow_nominal)
        "Primary supply temperature (measured)"
        annotation (Placement(transformation(extent={{8,10},{28,30}})));
      Buildings.Controls.OBC.CDL.Conversions.RealToInteger reaToInt
        "Convert to integer"
        annotation (Placement(transformation(extent={{-60,-10},{-40,10}})));
      Buildings.Controls.OBC.CDL.Logical.Switch dTSec(y(final unit="K", displayUnit=
             "degC")) "Actual secondary delta T"
        annotation (Placement(transformation(extent={{20,-130},{40,-110}})));
      Buildings.Controls.OBC.CDL.Continuous.Sources.Constant dTSecCoo(k=5)
        "Secondary temperature difference between supply and return"
        annotation (Placement(transformation(extent={{-20,-150},{0,-130}})));
    equation
      connect(lesEquThr.y, TPri.u2) annotation (Line(points={{-78,40},{-74,40},{-74,
              80},{-62,80}}, color={255,0,255}));
      connect(TPri.y, souPri.T_in) annotation (Line(points={{-38,80},{-32,80},{-32,
              24},{-22,24}},
                         color={0,0,127}));
      connect(TSetSecAct.y, disFlo.TSupSet) annotation (Line(points={{-38,-80},{32,-80},
              {32,12},{39,12}}, color={0,0,127}));
      connect(lesEquThr.y, TSetSecAct.u2) annotation (Line(points={{-78,40},{-74,40},
              {-74,-80},{-62,-80}}, color={255,0,255}));
      connect(disFlo.ports_b1[1], senTSecSup.port_a) annotation (Line(points={{40,
              26},{36,26},{36,100},{70,100}}, color={0,127,255}));
      connect(souSec.ports[1], senTSecRet.port_a)
        annotation (Line(points={{100,60},{90,60}}, color={0,127,255}));
      connect(senTSecRet.port_b, disFlo.ports_a1[1]) annotation (Line(points={{70,
              60},{64,60},{64,26},{60,26}}, color={0,127,255}));
      connect(disFlo.port_b, sinPri.ports[1])
        annotation (Line(points={{60,20},{100,20}}, color={0,127,255}));
      connect(senTSecSup.port_b, sinSec.ports[1])
        annotation (Line(points={{90,100},{100,100}}, color={0,127,255}));
      connect(mSec_flow.y, disFlo.mReq_flow[1]) annotation (Line(points={{-118,160},
              {32,160},{32,16},{39,16}}, color={0,0,127}));
      connect(mSec_flow.y, souSec.m_flow_in) annotation (Line(points={{-118,160},{
              140,160},{140,68},{122,68}},
                                  color={0,0,127}));
      connect(add.y, souSec.T_in) annotation (Line(points={{122,-80},{140,-80},{140,
              64},{122,64}},                     color={0,0,127}));
      connect(souPri.ports[1], senTPriSup.port_a)
        annotation (Line(points={{0,20},{8,20}}, color={0,127,255}));
      connect(senTPriSup.port_b, disFlo.port_a)
        annotation (Line(points={{28,20},{40,20}}, color={0,127,255}));
      connect(TSetSecChi.y, TSetSecAct.u3) annotation (Line(points={{-118,-100},{
              -100,-100},{-100,-88},{-62,-88}}, color={0,0,127}));
      connect(TSetSecHea.y, TSetSecAct.u1) annotation (Line(points={{-118,-60},{
              -100,-60},{-100,-72},{-62,-72}}, color={0,0,127}));
      connect(TPriChi.y, TPri.u3) annotation (Line(points={{-118,80},{-80,80},{-80,
              72},{-62,72}},      color={0,0,127}));
      connect(TPriHea.y, TPri.u1) annotation (Line(points={{-118,120},{-80,120},{
              -80,88},{-62,88}},  color={0,0,127}));
      connect(mod.y, lesEquThr.u)
        annotation (Line(points={{-118,40},{-102,40}}, color={0,0,127}));
      connect(mod.y, reaToInt.u) annotation (Line(points={{-118,40},{-110,40},{-110,
              0},{-62,0}}, color={0,0,127}));
      connect(reaToInt.y, disFlo.modChaOve) annotation (Line(points={{-38,0},{30,0},
              {30,14},{39,14}}, color={255,127,0}));
      connect(TSetSecAct.y, add.u1) annotation (Line(points={{-38,-80},{32,-80},{32,
              -74},{98,-74}}, color={0,0,127}));
      connect(dTSec.y, add.u2) annotation (Line(points={{42,-120},{80,-120},{80,-86},
              {98,-86}}, color={0,0,127}));
      connect(dTSecHea.y, dTSec.u1) annotation (Line(points={{2,-100},{10,-100},{10,
              -112},{18,-112}}, color={0,0,127}));
      connect(dTSecCoo.y, dTSec.u3) annotation (Line(points={{2,-140},{10,-140},{10,
              -128},{18,-128}}, color={0,0,127}));
      connect(lesEquThr.y, dTSec.u2) annotation (Line(points={{-78,40},{-74,40},{
              -74,-120},{18,-120}}, color={255,0,255}));
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
    end MixingValveControl;
  annotation (preferredView="info", Documentation(info="<html>
<p>
This package contains validation models for the classes in
<a href=\"modelica://Buildings.Applications.DHC.Controls\">
Buildings.Applications.DHC.Controls</a>.
</p>
</html>"));
  end Validation;

  block ChillerStageController "Chiller staging up and down controller."
    extends Modelica.Blocks.Icons.Block;

    import Type_dis = Buildings.Applications.DHC.Loads.Types.DistributionType
      "Types of distribution system";

    parameter Type_dis typDis = Type_dis.HeatingWater
      "Type of distribution system"
      annotation(Evaluate=true);
    parameter Real k(
      final min=0,
      final unit="1") = 0.1 "Gain of controller";
    parameter Modelica.SIunits.Time Ti(final min=Modelica.Constants.small) = 10
      "Time constant of integrator block";

    // IO CONNECTORS
    Modelica.Blocks.Interfaces.RealInput TSupSet(
      final quantity="ThermodynamicTemperature", final unit="K", final displayUnit="degC")
      "Supply temperature set point"
      annotation (Placement(transformation(
        extent={{-10,-10},{10,10}},
        rotation=0,
        origin={-110,-30}),
        iconTransformation(
        extent={{-10,-10},{10,10}},
        rotation=0,
        origin={-110,40})));
    Modelica.Blocks.Interfaces.IntegerInput modChaOve if typDis == Type_dis.ChangeOver
      "Operating mode in change-over (1 for heating, -1 for cooling)"
      annotation (Placement(
        transformation(
        extent={{-10,-10},{10,10}},
        rotation=0,
        origin={-110,90}),
        iconTransformation(
        extent={{-10,-10},{10,10}},
        rotation=0,
        origin={-110,80})));
    Modelica.Blocks.Interfaces.RealInput TSupMes(
      final quantity="ThermodynamicTemperature",  final unit="K", final displayUnit="degC")
      "Supply temperature (measured)"
      annotation (Placement(
          transformation(
          extent={{-10,-10},{10,10}},
          rotation=0,
          origin={-110,-70}), iconTransformation(
          extent={{-10,-10},{10,10}},
          rotation=0,
          origin={-110,-40})));
    // COMPONENTS
    annotation (
    defaultComponentName="conVal",
    Documentation(info="
<html>
<p>
This model implements a generic controller for a three-way mixing valve.
Three operating modes are supported:
</p>
<ul>
<li>
Heating: the controller tracks a minimum supply temperature.
</li>
<li>
Cooling: the controller tracks a maximum supply temperature.
</li>
<li>
Change-over: the controller tracks either a minimum or a maximum
supplied temperature depending on the actual value of the integer input
<code>modChaOve</code> (1 for heating, 2 for cooling).
The model instantiates only one PI block to limit the number of state
variables in large models. Therefore the PI gain
is independent from the change-over mode: the reverse action is modeled
by taking the opposite value of the PI block output. Eventually the
integral part is reset whenever the change-over mode is switched.
</li>
</ul>
<p>
See
<a href=\"modelica://Buildings.Applications.DHC.Controls.Validation.MixingValveControl\">
Buildings.Applications.DHC.Controls.Validation.MixingValveControl</a>
for a simulation with change-over.
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
    Icon(coordinateSystem(preserveAspectRatio=false), graphics={
          Text(
            extent={{-90,96},{-10,66}},
            lineColor={244,125,35},
            fillColor={0,0,0},
            fillPattern=FillPattern.Solid,
            horizontalAlignment=TextAlignment.Left,
            visible=typDis == Type_dis.ChangeOver,
            textString="modChaOve"),
          Text(
            extent={{-90,54},{-22,26}},
            lineColor={0,0,127},
            fillColor={0,0,0},
            fillPattern=FillPattern.Solid,
            horizontalAlignment=TextAlignment.Left,
            textString="TSupSet"),
          Text(
            extent={{-90,-26},{-16,-52}},
            lineColor={0,0,127},
            fillColor={0,0,0},
            fillPattern=FillPattern.Solid,
            horizontalAlignment=TextAlignment.Left,
            textString="TSupMes"),
          Text(
            extent={{50,12},{88,-14}},
            lineColor={0,0,127},
            fillColor={0,0,0},
            fillPattern=FillPattern.Solid,
            horizontalAlignment=TextAlignment.Right,
            textString="yVal")}),
    Diagram(
          coordinateSystem(preserveAspectRatio=false)));
  end ChillerStageController;
annotation (preferredView="info", Documentation(info="<html>
<p>
This package contains models of control sequences used for district heating and
cooling systems.
</p>
</html>"));
end Controls;
