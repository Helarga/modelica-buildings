within Buildings.Fluid.HeatPumps.Examples;
model DOE2Reversible
  "Test model for reverse heat pump based on performance curves"
 package Medium = Buildings.Media.Water "Medium model";

  parameter Buildings.Fluid.Chillers.Data.ElectricEIR.ElectricEIRChiller_McQuay_WSC_471kW_5_89COP_Vanes per
   "Reverse heat pump performance data"
   annotation (Placement(transformation(extent={{-98,80},{-78,100}})));
  /*
  parameter Modelica.SIunits.MassFlowRate mSou_flow_nominal=per.mCon_flow_nominal
   "Source heat exchanger nominal mass flow rate";
  parameter Modelica.SIunits.MassFlowRate mLoa_flow_nominal=per.mEva_flow_nominal
   "Load heat exchanger nominal mass flow rate";
  */

  parameter Real scaling_factor=1
   "Scaling factor for heat pump capacity";

  Buildings.Fluid.HeatPumps.DOE2Reversible heaPum(
    redeclare package Medium1 = Medium,
    redeclare package Medium2 = Medium,
    dp1_nominal=6000,
    dp2_nominal=6000,
    energyDynamics=Modelica.Fluid.Types.Dynamics.SteadyState,
    massDynamics=Modelica.Fluid.Types.Dynamics.SteadyState,
    per=per,
    scaling_factor=scaling_factor)
   "Water to Water heat pump"
   annotation (Placement(transformation(extent={{40,0},{60,20}})));
  Modelica.Blocks.Math.RealToInteger reaToInt
   "Real to integer conversion"
   annotation (Placement(transformation(extent={{-86,0},{-66,20}})));
  Sources.MassFlowSource_T loaPum(
    redeclare package Medium = Medium,
    use_m_flow_in=false,
    m_flow=scaling_factor*per.mEva_flow_nominal,
    use_T_in=true,
    nPorts=1)
   "Load Side water pump"
   annotation (Placement(transformation(
      extent={{10,-10},{-10,10}},
      rotation=180,
      origin={70,70})));
  Sources.MassFlowSource_T souPum(
    redeclare package Medium = Medium,
    m_flow=scaling_factor*per.mEva_flow_nominal,
    nPorts=1,
    use_T_in=true)
   "Source side water pump"
   annotation (Placement(transformation(
      extent={{-10,-10},{10,10}},
      rotation=180,
      origin={92,-8})));
  Modelica.Fluid.Sources.FixedBoundary loaVol(
     redeclare package Medium = Medium,
     nPorts=1)
   "Volume for the load side"
   annotation (Placement(transformation(extent={{100,20},{80,40}})));
  Modelica.Fluid.Sources.FixedBoundary souVol(
     redeclare package Medium = Medium,
     nPorts=1)
   "Volume for source side"
   annotation (Placement(transformation(extent={{-58,-80},{-38,-60}})));
  Controls.OBC.CDL.Continuous.Sources.Ramp THeaLoaSet(
    height=5,
    duration(displayUnit="s") = 2600,
    offset=35 + 273.15)
   "Heating load side setpoint water temperature"
   annotation (Placement(transformation(extent={{-60,40},{-40,60}})));
  Controls.OBC.CDL.Continuous.Sources.Ramp TCooLoaSet(
    height=2,
    duration(displayUnit="s") = 2600,
    offset=8 + 273.15) "Cooling load setpoint water temperature"
    annotation (Placement(transformation(extent={{-60,-40},{-40,-20}})));
  Controls.OBC.CDL.Integers.GreaterThreshold intGreThr(threshold=-1)
   "Integer threshold"
    annotation (Placement(transformation(extent={{-60,0},{-40,20}})));
  Controls.OBC.CDL.Logical.Switch swi "Switch for set point temperature"
    annotation (Placement(transformation(extent={{-20,0},{0,20}})));
  Controls.OBC.CDL.Continuous.Sources.Ramp TSouEntCoo(
    height=1,
    duration(displayUnit="s") = 2600,
    offset=12 + 273.15)
  "Source side entering water temperature in cooling mode"
    annotation (Placement(transformation(extent={{40,-80},{60,-60}})));
  Modelica.Blocks.Sources.Ramp     uMod1(
    height=2,
    duration=2600,
    offset=-1)
   "Heat pump operates in heating mode"
    annotation (Placement(transformation(extent={{-114,0},{-94,20}})));
  Controls.OBC.CDL.Logical.Switch swi1
   "Switch for set point temperature"
    annotation (Placement(transformation(extent={{20,60},{40,80}})));
  Controls.OBC.CDL.Logical.Switch swi2
   "Switch for set point temperature"
    annotation (Placement(transformation(extent={{80,-60},{100,-40}})));
  Controls.OBC.CDL.Continuous.Sources.Ramp TSouEntHea(
    height=5,
    duration(displayUnit="s") = 2600,
    offset=8 + 273.15)
   "Source side entering water temperature in heating mode"
    annotation (Placement(transformation(extent={{40,-40},{60,-20}})));
  Controls.OBC.CDL.Continuous.Sources.Ramp TLoaEntHea(
    height=1,
    duration(displayUnit="s") = 2600,
    offset=26 + 273.15)
   "Load side entering water temperature  in heating mode"
    annotation (Placement(transformation(extent={{-20,80},{0,100}})));
  Controls.OBC.CDL.Continuous.Sources.Ramp TLoaEntCoo(
    height=2,
    duration(displayUnit="s") = 2000,
    offset=12 + 273.15)
   "Load side entering water temperature in cooling mode"
    annotation (Placement(transformation(extent={{-20,40},{0,60}})));
equation
  connect(souPum.ports[1], heaPum.port_a2)
    annotation (Line(points={{82,-8},{72,-8},{72,4},{60,4}},
                                             color={0,127,255}));
  connect(heaPum.port_b1, loaVol.ports[1]) annotation (Line(points={{60,16},{74,
          16},{74,30},{80,30}},      color={0,127,255}));
  connect(heaPum.port_b2, souVol.ports[1])
    annotation (Line(points={{40,4},{26,4},{26,-70},{-38,-70}},color={0,127,255}));
  connect(swi.u2,intGreThr. y)
    annotation (Line(points={{-22,10},{-38,10}}, color={255,0,255}));
  connect(intGreThr.u, reaToInt.y)
    annotation (Line(points={{-62,10},{-65,10}}, color={255,127,0}));
  connect(reaToInt.u, uMod1.y)
    annotation (Line(points={{-88,10},{-93,10}}, color={0,0,127}));
  connect(swi2.u3, TSouEntCoo.y)
    annotation (Line(points={{78,-58},{68,-58},{68,-70},{62,-70}}, color={0,0,127}));
  connect(TSouEntHea.y, swi2.u1)
    annotation (Line(points={{62,-30},{66,-30},{66,-42},{78,-42}}, color={0,0,127}));
  connect(intGreThr.y, swi2.u2)
    annotation (Line(points={{-38,10},{-28,10},{-28,-50},{78,-50}}, color={255,0,255}));
  connect(swi1.y, loaPum.T_in)
    annotation (Line(points={{42,70},{52,70},{52,66},{58,66}}, color={0,0,127}));
  connect(intGreThr.y, swi1.u2) annotation (Line(points={{-38,10},{-28,10},{-28,
          70},{18,70}}, color={255,0,255}));
  connect(TLoaEntCoo.y, swi1.u3)
    annotation (Line(points={{2,50},{10,50},{10,62},{18,62}},
                                                            color={0,0,127}));
  connect(TLoaEntHea.y, swi1.u1)
    annotation (Line(points={{2,90},{14,90},{14,78},{18,78}}, color={0,0,127}));
  connect(swi.y, heaPum.TSet)
    annotation (Line(points={{2,10},{18,10},{18,19},{38.6,19}}, color={0,0,127}));
  connect(swi2.y, souPum.T_in)
    annotation (Line(points={{102,-50},{110,-50},{110,-12},{104,-12}}, color={0,0,127}));
  connect(loaPum.ports[1], heaPum.port_a1)
    annotation (Line(
      points={{80,70},{80,40},{26,40},{26,16},{40,16}},
      color={0,127,255},pattern=LinePattern.Dash));
  connect(THeaLoaSet.y, swi.u1)
    annotation (Line(
      points={{-38,50},{-32,50},{-32,18},{-22,18}},
      color={0,0,127},
      pattern=LinePattern.Dot));
  connect(TCooLoaSet.y, swi.u3)
    annotation (Line(
      points={{-38,-30},{-32,-30},{-32,2},{-22,2}},
      color={0,0,127},
      pattern=LinePattern.Dot));
  connect(reaToInt.y, heaPum.uMod)
    annotation (Line(
      points={{-65,10},{-62,10},{-62,-8},{22,-8},{22,10},{39,10}},
      color={255,127,0}));
     annotation (Icon(coordinateSystem(preserveAspectRatio=false),
                               graphics={
        Ellipse(lineColor = {75,138,73},
                fillColor={255,255,255},
                fillPattern = FillPattern.Solid,
                extent={{-98,-100},{98,98}}),
        Polygon(lineColor = {0,0,255},
                fillColor = {75,138,73},
                pattern = LinePattern.None,
                fillPattern = FillPattern.Solid,
                points={{-30,64},{70,4},{-30,-56},{-30,64}})}),
              Diagram(
        coordinateSystem(preserveAspectRatio=false, extent={{-120,-100},{120,
            120}}), graphics={Line(points={{-14,-8}}, color={28,108,200})}),
             __Dymola_Commands(
  file="modelica://Buildings/Resources/Scripts/Dymola/Fluid/HeatPumps/Examples/DOE2Reversible.mos"
        "Simulate and plot"),
         experiment(Tolerance=1e-6, StopTime=14400),
Documentation(info="<html>
<p>
Example that simulates the performance of
<a href=\"modelica://Buildings.Fluid.HeatPumps.DOE2Reversible\">
Buildings.Fluid.HeatPumps.DOE2Reversible</a> based on the equation fit method.
The heat pump takes as an input the set point for the heating or the chilled leaving
water temperature and an integer input to
specify the heat pump operational mode.
</p>
</html>", revisions="<html>
<ul>
<li>
September 17, 2019, by Michael Wetter:<br/>
Revised implementation.
</li>
<li>
June 18, 2019, by Hagar Elarga:<br/>
First implementation.
 </li>
 </ul>
 </html>"));
end DOE2Reversible;
