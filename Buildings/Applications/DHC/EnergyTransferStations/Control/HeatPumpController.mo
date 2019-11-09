within Buildings.Applications.DHC.EnergyTransferStations.Control;
model HeatPumpController "The control block of the heatpump on heating mode"
     extends Modelica.Blocks.Icons.Block;

  Buildings.Controls.OBC.CDL.Interfaces.BooleanInput ReqHea
    "Heating is required Boolean signal"
    annotation (Placement(transformation(extent={{-128,62},{-100,90}}),
        iconTransformation(extent={{-128,76},{-100,104}})));
  Buildings.Controls.OBC.CDL.Interfaces.BooleanInput ReqCoo
    "Cooling is required Boolean signal"
    annotation (Placement(transformation(extent={{-128,32},{-100,60}}),
        iconTransformation(extent={{-128,46},{-100,74}})));

  Buildings.Controls.OBC.CDL.Interfaces.RealInput           TSetCoo(final unit="K",
      displayUnit="degC") "Setpoint for cooling supply water to space loads"
                                                       annotation (Placement(transformation(extent={{-128,
            -160},{-100,-132}}),
                     iconTransformation(extent={{-120,-58},{-100,-38}})));
  Modelica.Blocks.Logical.Or or1
    annotation (Placement(transformation(extent={{-46,44},{-26,64}})));
  Buildings.Controls.OBC.CDL.Logical.Switch swi1
    annotation (Placement(transformation(extent={{2,44},{22,64}})));
  Buildings.Controls.OBC.CDL.Continuous.Sources.Constant heaModHeaPum(k=1)
    "Heating mode signal for the heatpump =1"
    annotation (Placement(transformation(extent={{-46,74},{-26,94}})));
  Buildings.Controls.OBC.CDL.Continuous.Sources.Constant shuOffSig(k=0)
    "HeatPump shut off signal =0"
    annotation (Placement(transformation(extent={{-46,18},{-26,38}})));
  Modelica.Blocks.Math.RealToInteger realToInteger
    annotation (Placement(transformation(extent={{58,44},{78,64}})));
  Buildings.Controls.OBC.CDL.Interfaces.IntegerOutput yHeaPumMod
    "Heatpump operational mode" annotation (Placement(transformation(extent={{102,44},
            {122,64}}),     iconTransformation(extent={{100,-14},{128,14}})));
  Buildings.Controls.OBC.CDL.Interfaces.RealOutput TSetHeaPum(final unit="K",
      displayUnit="degC") "Setpint temperture for the heatpump" annotation (
      Placement(transformation(extent={{100,-18},{120,2}}),
        iconTransformation(extent={{100,28},{128,56}})));
  Buildings.Controls.OBC.CDL.Logical.Switch swi2
    annotation (Placement(transformation(extent={{60,-18},{80,2}})));
  Buildings.Controls.OBC.CDL.Interfaces.RealInput TEvaLvg(final unit="K", displayUnit=
        "degC") "Evaporator leaving water temperature" annotation (Placement(
        transformation(extent={{-128,-204},{-100,-176}}), iconTransformation(
          extent={{-120,-104},{-100,-84}})));
  Modelica.Blocks.Logical.And simHeaCoo "Simultaneous heating and cooling mode"
    annotation (Placement(transformation(extent={{-60,-88},{-40,-68}})));
  Buildings.Controls.Continuous.LimPID PI(
    controllerType=Modelica.Blocks.Types.SimpleController.PI,
    yMax=1,
    yMin=0,
    reset=Buildings.Types.Reset.Parameter,
    y_reset=0,
    k=0.1,
    Ti(displayUnit="s") = 300,
    reverseAction=true)
    "Resetting of heating set point tempearture in case reqCoo or (reqCoo and reqHea) are true."
    annotation (Placement(transformation(extent={{-58,-172},{-38,-152}})));
  Buildings.Controls.OBC.CDL.Continuous.Line mapFun
    "Mapping control function to reset the TsetHea"
    annotation (Placement(transformation(extent={{6,-148},{26,-128}})));
  Buildings.Controls.OBC.CDL.Continuous.Sources.Constant X1(k=0)
    "PI minimum error"
    annotation (Placement(transformation(extent={{-34,-118},{-14,-98}})));
  Buildings.Controls.OBC.CDL.Continuous.Sources.Constant X2(k=1)
    "PI maximum error"
    annotation (Placement(transformation(extent={{-28,-202},{-8,-182}})));
  Buildings.Controls.OBC.CDL.Interfaces.RealInput TSetHeaMax(final unit="K",
      displayUnit="degC") "Maximum setpoint for heating water " annotation (
      Placement(transformation(extent={{-128,-228},{-100,-200}}),
        iconTransformation(extent={{-120,-82},{-100,-62}})));
  Buildings.Controls.OBC.CDL.Interfaces.RealInput TSetHea(final unit="K",
      displayUnit="degC") "Setpoint for heating supply water to space loads"
    annotation (Placement(transformation(extent={{-128,-14},{-100,14}}),
        iconTransformation(extent={{-120,-38},{-100,-18}})));
  Buildings.Controls.OBC.CDL.Logical.Switch swi4
    annotation (Placement(transformation(extent={{40,-102},{60,-82}})));
  Modelica.Blocks.Logical.Or cooOnl "Cooling only mode"
    annotation (Placement(transformation(extent={{-20,-80},{0,-60}})));
  Buildings.Controls.OBC.CDL.Continuous.Sources.Constant X3(k=25 + 273.15)
    "Minimum heating setpoint temperature"
    annotation (Placement(transformation(extent={{-2,-216},{18,-196}})));
  Buildings.Controls.OBC.CDL.Logical.Not not1
    annotation (Placement(transformation(extent={{-48,-30},{-28,-10}})));
  Modelica.Blocks.Logical.And heaMod "Heating only mode"
    annotation (Placement(transformation(extent={{-2,-28},{18,-8}})));
  Buildings.Controls.Continuous.LimPID valEva(
    controllerType=Modelica.Blocks.Types.SimpleController.PI,
    yMax=1,
    yMin=0,
    reset=Buildings.Types.Reset.Parameter,
    y_reset=0,
    k=0.05,
    Ti(displayUnit="s") = 300,
    reverseAction=true) "Evaporator three way valve PI control signal "
    annotation (Placement(transformation(extent={{32,-270},{52,-250}})));
  Buildings.Controls.OBC.CDL.Interfaces.RealInput TEvaEnt(final unit="K",
      displayUnit="degC") "Evaporator entering water temperature" annotation (
      Placement(transformation(extent={{-128,-302},{-100,-274}}),
        iconTransformation(extent={{-120,-104},{-100,-84}})));
  Buildings.Controls.OBC.CDL.Interfaces.RealInput TMaxEvaEnt(final unit="K",
      displayUnit="degC") "Maximum evaporator entering water temperature"
    annotation (Placement(transformation(extent={{-128,-274},{-100,-246}}),
        iconTransformation(extent={{-120,-104},{-100,-84}})));
  Buildings.Controls.OBC.CDL.Interfaces.RealOutput yValEva(final unit="K",
      displayUnit="degC")
    "Control signal of the modulating three way valve to maintain the evaporator entering temperature below the maximum value."
    annotation (Placement(transformation(extent={{100,-270},{120,-250}}),
        iconTransformation(extent={{100,28},{128,56}})));
  Buildings.Controls.Continuous.LimPID valEva1(
    controllerType=Modelica.Blocks.Types.SimpleController.PI,
    yMax=1,
    yMin=0,
    reset=Buildings.Types.Reset.Parameter,
    y_reset=0,
    k=0.05,
    Ti(displayUnit="s") = 300,
    reverseAction=true) "Evaporator three way valve PI control signal "
    annotation (Placement(transformation(extent={{30,-322},{50,-302}})));
  Buildings.Controls.OBC.CDL.Interfaces.RealInput TConEnt(final unit="K",
      displayUnit="degC") "Condenser entering water temperature" annotation (
      Placement(transformation(extent={{-126,-354},{-98,-326}}),
        iconTransformation(extent={{-120,-104},{-100,-84}})));
  Buildings.Controls.OBC.CDL.Interfaces.RealInput TMinConEnt(final unit="K",
      displayUnit="degC") "Minimum condenser entering water temperature"
    annotation (Placement(transformation(extent={{-126,-326},{-98,-298}}),
        iconTransformation(extent={{-120,-104},{-100,-84}})));
  Buildings.Controls.OBC.CDL.Interfaces.RealOutput yValEva1(final unit="K",
      displayUnit="degC")
    "Control signal of the modulating three way valve to maintain the evaporator entering temperature below the maximum value."
    annotation (Placement(transformation(extent={{102,-322},{122,-302}}),
        iconTransformation(extent={{100,28},{128,56}})));
equation

  connect(ReqCoo, or1.u2)
    annotation (Line(points={{-114,46},{-48,46}}, color={255,0,255}));
  connect(ReqHea, or1.u1)
    annotation (Line(points={{-114,76},{-56,76},{-56,54},{-48,54}},
                     color={255,0,255}));
  connect(or1.y, swi1.u2)
    annotation (Line(points={{-25,54},{0,54}},  color={255,0,255}));
  connect(swi1.u1, heaModHeaPum.y)
    annotation (Line(points={{0,62},{-16,62},{-16,84},{-24,84}},
                             color={0,0,127}));
  connect(swi1.u3, shuOffSig.y)
    annotation (Line(points={{0,46},{-16,46},{-16,28},{-24,28}},
                         color={0,0,127}));
  connect(realToInteger.y, yHeaPumMod)
    annotation (Line(points={{79,54},{112,54}}, color={255,127,0}));
  connect(swi1.y, realToInteger.u)
    annotation (Line(points={{24,54},{56,54}}, color={0,0,127}));
  connect(swi2.y, TSetHeaPum)
    annotation (Line(points={{82,-8},{110,-8}},   color={0,0,127}));
  connect(ReqCoo, simHeaCoo.u2) annotation (Line(points={{-114,46},{-84,46},{
          -84,-86},{-62,-86}}, color={255,0,255}));
  connect(ReqHea, simHeaCoo.u1) annotation (Line(points={{-114,76},{-70,76},{
          -70,-78},{-62,-78}}, color={255,0,255}));
  connect(PI.u_s, TSetCoo)
    annotation (Line(points={{-60,-162},{-78,-162},{-78,-146},{-114,-146}},
                                                    color={0,0,127}));
  connect(X1.y, mapFun.x1) annotation (Line(points={{-12,-108},{-12,-130},{4,
          -130}},     color={0,0,127}));
  connect(PI.y, mapFun.u)
    annotation (Line(points={{-37,-162},{-12,-162},{-12,-138},{4,-138}},
                                                   color={0,0,127}));
  connect(TSetHeaMax, mapFun.f2) annotation (Line(points={{-114,-214},{-4,-214},
          {-4,-146},{4,-146}},  color={0,0,127}));
  connect(X2.y, mapFun.x2) annotation (Line(points={{-6,-192},{-6,-142},{4,-142}},
                      color={0,0,127}));
  connect(TEvaLvg, PI.u_m) annotation (Line(points={{-114,-190},{-48,-190},{-48,
          -174}}, color={0,0,127}));
  connect(TSetHea, mapFun.f1) annotation (Line(points={{-114,1.77636e-15},{-96,
          1.77636e-15},{-96,-134},{4,-134}}, color={0,0,127}));
  connect(cooOnl.y, swi4.u2) annotation (Line(points={{1,-70},{24,-70},{24,-92},
          {38,-92}}, color={255,0,255}));
  connect(simHeaCoo.y, cooOnl.u2)
    annotation (Line(points={{-39,-78},{-22,-78}}, color={255,0,255}));
  connect(ReqCoo, cooOnl.u1) annotation (Line(points={{-114,46},{-84,46},{-84,
          -60},{-30,-60},{-30,-70},{-22,-70}}, color={255,0,255}));
  connect(ReqCoo, PI.trigger) annotation (Line(points={{-114,46},{-84,46},{-84,
          -184},{-56,-184},{-56,-174}}, color={255,0,255}));
  connect(mapFun.y, swi4.u1) annotation (Line(points={{28,-138},{32,-138},{32,
          -84},{38,-84}}, color={0,0,127}));
  connect(X3.y, swi4.u3) annotation (Line(points={{20,-206},{36,-206},{36,-100},
          {38,-100}}, color={0,0,127}));
  connect(swi4.y, swi2.u3) annotation (Line(points={{62,-92},{64,-92},{64,-40},
          {42,-40},{42,-16},{58,-16}}, color={0,0,127}));
  connect(TSetHea, swi2.u1) annotation (Line(points={{-114,1.77636e-15},{-34,
          1.77636e-15},{-34,0},{58,0}}, color={0,0,127}));
  connect(ReqCoo, not1.u) annotation (Line(points={{-114,46},{-84,46},{-84,-20},
          {-50,-20}}, color={255,0,255}));
  connect(not1.y, heaMod.u2) annotation (Line(points={{-26,-20},{-20,-20},{-20,
          -26},{-4,-26}}, color={255,0,255}));
  connect(ReqHea, heaMod.u1) annotation (Line(points={{-114,76},{-70,76},{-70,
          -4},{-12,-4},{-12,-18},{-4,-18}}, color={255,0,255}));
  connect(heaMod.y, swi2.u2) annotation (Line(points={{19,-18},{26,-18},{26,-8},
          {58,-8}}, color={255,0,255}));
  connect(TMaxEvaEnt, valEva.u_s)
    annotation (Line(points={{-114,-260},{30,-260}}, color={0,0,127}));
  connect(TEvaEnt, valEva.u_m) annotation (Line(points={{-114,-288},{42,-288},{
          42,-272}}, color={0,0,127}));
  connect(valEva.y, yValEva)
    annotation (Line(points={{53,-260},{110,-260}}, color={0,0,127}));
  connect(TMinConEnt, valEva1.u_s)
    annotation (Line(points={{-112,-312},{28,-312}}, color={0,0,127}));
  connect(TConEnt, valEva1.u_m) annotation (Line(points={{-112,-340},{40,-340},
          {40,-324}}, color={0,0,127}));
  connect(valEva1.y, yValEva1)
    annotation (Line(points={{51,-312},{112,-312}}, color={0,0,127}));
  connect(or1.y, valEva.trigger) annotation (Line(
      points={{-25,54},{-8,54},{-8,26},{28,26},{28,-280},{34,-280},{34,-272}},
      color={255,0,255},
      pattern=LinePattern.Dash));
  connect(or1.y, valEva1.trigger) annotation (Line(
      points={{-25,54},{-8,54},{-8,26},{28,26},{28,-336},{32,-336},{32,-324}},
      color={255,0,255},
      pattern=LinePattern.Dash));
  annotation (defaultComponentName="heaPumCon",Icon(coordinateSystem(preserveAspectRatio=false, extent={{-100,
            -100},{100,100}})),                                  Diagram(
        coordinateSystem(preserveAspectRatio=false, extent={{-100,-360},{100,
            100}}),
        graphics={
        Rectangle(
          extent={{-100,8},{100,-234}},
          lineColor={28,108,200},
          lineThickness=0.5,
          fillColor={213,255,170},
          fillPattern=FillPattern.Solid),
        Rectangle(
          extent={{-100,102},{100,20}},
          lineColor={28,108,200},
          fillColor={215,215,215},
          fillPattern=FillPattern.Solid),
        Text(
          extent={{-14,106},{96,82}},
          lineColor={0,0,255},
          fillColor={215,215,215},
          fillPattern=FillPattern.None,
          textString="Heatpump operational mode"),
        Text(
          extent={{-30,-224},{114,-232}},
          lineColor={0,0,255},
          fillColor={215,215,215},
          fillPattern=FillPattern.None,
          textString="Reset of water setpoint temperature"),
        Text(
          extent={{-50,-352},{94,-360}},
          lineColor={0,0,255},
          fillColor={215,215,215},
          fillPattern=FillPattern.None,
          textString="Evaporator and Condenser three way valves control")}),
                Documentation(info="<html>
<p>
The block is applied for the reversible heat pump control. It outputs the heat pump status
and resets the water temperature setpoint input signal to the heat pump <code>TReSetHea</code> based on the operational mode.
</p>
<h4>Heat pump status</h4>
<p>
If either <code>reqHea</code> or <code>reqCoo</code> is true, the controller outputs the integer output
<code>yHeaPumMod</code> to switch on the heat pump, other wise it switches off. 
</p>
<h4>Modes of operation</h4> 
<ul>
<li> 
Heating-only mode, the leaving water form the heat pump condenser side tracks the heating set point<code>TSetHea</code>
and the leaving chilled water from the evaporator floats depending on the entering water temperature and flow rate.
</li>
<li> 
Simultaneous cooling and heating and cooling only modes, the control sequence resets the heating set point<code>TReSetHea</code> till the leaving chilled water temperature
from the evaporator side meets the cooling set point<code>TSetCoo</code> as shown below in the figure
</li>
</ul>
<p align=\"center\">
<img alt=\"Image PI controller to reset the TSetHea\"
src=\"modelica://Buildings/Resources/Images/Applications/DHC/EnergyTransferStations/resetTsetHea.png\"/>
</p> 
<p>
The required leverage in <code>TSetHea</code> is estimated by a reverse acting PI loop , with a reference set point of <code>TSetCoo</code>
and measured temperature value of <code>TSouLvg</code>. Hence, when the evaporator leaving water temperature is higher than <code>TSetCoo</code>, 
TSetHea is increased.
</p>
<p>
During the simultaneous cooling and heating mode, the minimum re-set value of <code>TReSetHea</code> is considered equal to
<code>TsetHea</code> to assure that heating loads are covered. However, in case of cooling only mode, the minimum re-set value is considered <code>TSetHeaMin</code>
i.e. minimum leaving water temperature from the condenser, in order to reduce the heat pump compressor lift and improve the COP. The control mapping function
is illustrated below
</p>
<p align=\"center\">
<img alt=\"Image Control Mapping function of resetting TsetHea\"
src=\"modelica://Buildings/Resources/Images/Applications/DHC/EnergyTransferStations/controlMappingFunction.png\"/>
</p>  
<p>
See <a href=\"Buildings.Fluid.HeatPumps.EquationFitReversible\">
Buildings.Fluid.HeatPumps.EquationFitReversible</a> for detailed description of the heat pump theory of operation.
</p>
</html>", revisions="<html>

<ul>
<li>
 <br/>
</li>
</ul>
</html>"));
end HeatPumpController;
