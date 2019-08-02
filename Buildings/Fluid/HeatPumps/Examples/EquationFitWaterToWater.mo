within Buildings.Fluid.HeatPumps.Examples;
model EquationFitWaterToWater "example"
 package Medium = Buildings.Media.Water "Medium model";

  Buildings.Fluid.HeatPumps.EquationFitWaterToWater heaPum(
    per=Data.EquationFitWaterToWater.Trane_Axiom_EXW240(),
    redeclare package Medium1 = Medium,
    redeclare package Medium2 = Medium,
    show_T=true,
    dp1_nominal=200,
    dp2_nominal=200,
    energyDynamics=Modelica.Fluid.Types.Dynamics.FixedInitial,
    massDynamics=Modelica.Fluid.Types.Dynamics.FixedInitial)
      "Water to Water heatpump"
       annotation (Placement(transformation(extent={{30,-12},{50,8}})));

    parameter Data.EquationFitWaterToWater.Trane_Axiom_EXW240 per
    "HeatPump performance"
        annotation (Placement(transformation(extent={{100,20},{120,40}})));
    parameter Modelica.SIunits.MassFlowRate mEva_flow_nominal=per.mEva_flow_nominal
       "Evaporator nominal mass flow rate";
    parameter Modelica.SIunits.MassFlowRate mCon_flow_nominal=per.mCon_flow_nominal
       "Condenser nominal mass flow rate";

    Modelica.Blocks.Math.RealToInteger reaToInt
       annotation (Placement(transformation(extent={{-60,-10},{-40,10}})));
    Sources.MassFlowSource_T conPum(
      use_m_flow_in=false,
      m_flow=mCon_flow_nominal,
      nPorts=1,
      use_T_in=true,
      redeclare package Medium = Medium)
      "Condenser water Pump"
       annotation (Placement(transformation(
        extent={{10,-10},{-10,10}},
        rotation=180,
        origin={-30,70})));
    Sources.MassFlowSource_T evaPum(
      m_flow=mEva_flow_nominal,
      nPorts=1,
      use_T_in=true,
      redeclare package Medium = Medium)
      "Evaporator water Pump"
       annotation (Placement(transformation(
        extent={{-10,-10},{10,10}},
        rotation=180,
        origin={90,-8})));
    Controls.OBC.CDL.Continuous.Sources.Ramp TConEnt(
      height=20,
      duration(displayUnit="h") = 14400,
      offset=20 + 273.15,
      startTime=0)
      "Condesner entering water temperature"
       annotation (Placement(transformation(extent={{-100,60},{-80,80}})));
    Controls.OBC.CDL.Continuous.Sources.Ramp TEvaEnt(
      height=4,
      duration(displayUnit="h") = 14400,
      offset=12 + 273.15,
      startTime=0)
      "Evaporator entering water temperature"
       annotation (Placement(transformation(extent={{100,-60},{120,-40}})));
    FixedResistances.PressureDrop res1(
      redeclare package Medium = Medium,
      m_flow_nominal=mCon_flow_nominal,
      dp_nominal=6000)
      "Flow resistance"
       annotation (Placement(transformation(extent={{62,56},{82,76}})));
    FixedResistances.PressureDrop res2(
      redeclare package Medium = Medium,
       m_flow_nominal=mEva_flow_nominal,
       dp_nominal=6000)
      "Flow resistance"
       annotation (Placement(transformation(extent={{-20,-80},{0,-60}})));
    Modelica.Fluid.Sources.FixedBoundary heaVol(nPorts=1, redeclare package
      Medium =                                                                       Medium)
      "Volume for heating load"
       annotation (Placement(transformation(extent={{120,60},{100,80}})));
    Modelica.Fluid.Sources.FixedBoundary cooVol(nPorts=1, redeclare package
      Medium =                                                                       Medium)
      "Volume for cooling load"
       annotation (Placement(transformation(extent={{-60,-80},{-40,-60}})));
    Controls.OBC.CDL.Continuous.Sources.Ramp TEvaSet(
      height=4,
      duration(displayUnit="h") = 14400,
      offset=6 + 273.15,
      startTime=0)
      "Evaporator setpoint water temperature"
       annotation (Placement(transformation(extent={{-20,-40},{0,-20}})));
    Controls.OBC.CDL.Continuous.Sources.Ramp TConSet(
      height=20,
      duration(displayUnit="h") = 14400,
      offset=40 + 273.15,
      startTime=0)
      "Condenser setpoint water temperature"
        annotation (Placement(transformation(extent={{-20,20},{0,40}})));
    Controls.OBC.CDL.Continuous.Sources.Ramp uMod(
      height=2,
      duration(displayUnit="h") = 14400,
      offset=-1,
      startTime=0)
      "HeatPump operational mode input signal"
       annotation (Placement(transformation(extent={{-100,-10},{-80,10}})));

equation
  connect(heaPum.port_a1, conPum.ports[1])
  annotation (Line(points={{33.3333,4},{24,4},{24,70},{-20,70}},
                                                            color={0,127,255}));
  connect(TEvaEnt.y, evaPum.T_in)
  annotation (Line(points={{121,-50},{134,-50},{134,-12},{102,-12}},   color={0,0,127}));
  connect(evaPum.ports[1], heaPum.port_a2)
  annotation (Line(points={{80,-8},{50,-8}},                   color={0,127,255}));
  connect(cooVol.ports[1], res2.port_a)
  annotation (Line(points={{-40,-70},{-20,-70}}, color={0,127,255}));
  connect(res1.port_a, heaPum.port_b1)
  annotation (Line(points={{62,66},{62,4},{50,4}},   color={0,127,255}));
  connect(res2.port_b, heaPum.port_b2)
  annotation (Line(points={{0,-70},{24,-70},{24,-8},{33.3333,-8}},
                                                              color={0,127,255}));
  connect(TConSet.y, heaPum.TConSet)
  annotation (Line(points={{1,30},{16,30},{16,7},{32.1667,7}},
                                                            color={0,0,127}));
  connect(TEvaSet.y, heaPum.TEvaSet)
  annotation (Line(points={{1,-30},{16,-30},{16,-11},{32.1667,-11}},
                                                               color={0,0,127}));
  connect(res1.port_b, heaVol.ports[1])
  annotation (Line(points={{82,66},{100,66},{100,70}},
                                                     color={0,127,255}));
  connect(uMod.y, reaToInt.u)
  annotation (Line(points={{-79,0},{-62,0}}, color={0,0,127}));
  connect(reaToInt.y, heaPum.uMod)
  annotation (Line(points={{-39,0},{-2,0},{-2,-2},{32.1667,-2}},
                                              color={255,127,0}));
  connect(conPum.T_in, TConEnt.y) annotation (Line(points={{-42,66},{-70,66},{
          -70,70},{-79,70}}, color={0,0,127}));
  annotation (Icon(coordinateSystem(preserveAspectRatio=false, extent={{-100,-100},
            {100,100}}), graphics={
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
        coordinateSystem(preserveAspectRatio=false, extent={{-120,-100},{140,100}})),
             __Dymola_Commands(
  file="modelica://Buildings/Resources/Scripts/Dymola/Fluid/HeatPumps/Examples/EquationFitWaterToWater.mos"
        "Simulate and plot"),
         experiment(Tolerance=1e-6, StopTime=14400),
Documentation(info="<html>
  <p>
  Example that simulates the performance of <a href=\"modelica://Buildings.Fluid.HeatPumps.EquationFitWaterToWater\">
  Buildings.Fluid.HeatPumps.EquationFitWaterToWater </a> based on the equation fit method.
  The heat pump takes as an input the condenser or the evaporator leaving water temperature and an integer input to
  specify the heat pump operational mode.
  </p>
  </html>", revisions="<html>
  <ul>
  <li>
June 18, 2019, by Hagar Elarga:<br/>
First implementation.
 </li>
 </ul>
 </html>"));
end EquationFitWaterToWater;
