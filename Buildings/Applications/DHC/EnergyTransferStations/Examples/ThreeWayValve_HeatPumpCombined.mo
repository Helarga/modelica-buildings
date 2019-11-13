within Buildings.Applications.DHC.EnergyTransferStations.Examples;
model ThreeWayValve_HeatPumpCombined
  "Validation of the installed three way valave at the heat pump condenser entering"
  package Medium = Buildings.Media.Water "Medium model";

  parameter Modelica.SIunits.MassFlowRate mEva_flow_nominal=heaPumDat.hea.mSou_flow
   "Evaporator heat exchanger nominal mass flow rate";
  parameter Modelica.SIunits.MassFlowRate mCon_flow_nominal=heaPumDat.hea.mLoa_flow
   "Condenser heat exchanger nominal mass flow rate";
  parameter Modelica.SIunits.MassFlowRate mSecHea_flow_nominal=1.5
   "Secondary circuit heating nominal mass flow rate";
  parameter Boolean show_T=true
      "= true, if actual temperature at port is computed"
      annotation (Dialog(group="Advanced"));
  parameter Modelica.SIunits.PressureDifference dpCon_nominal= heaPumDat.dpHeaLoa_nominal
      "Pressure difference accross the condenser"
      annotation (Dialog(tab="WSHP system"));


  Fluid.HeatPumps.EquationFitReversible heaPum(
    allowFlowReversal1=false,
    allowFlowReversal2=false,
    energyDynamics=Modelica.Fluid.Types.Dynamics.FixedInitial,
    T1_start=308.15,
    T2_start=285.15,
    per=heaPumDat,
    redeclare package Medium1 = Medium,
    redeclare package Medium2 = Medium,
    scaling_factor=1)
    "Reversible water to water heat pump"
      annotation (Placement(transformation(extent={{-16,-8},{4,12}})));
  Fluid.Movers.SpeedControlled_y pumCon(
    redeclare package Medium = Medium,
    energyDynamics=Modelica.Fluid.Types.Dynamics.SteadyState,
    T_start=308.15,
    addPowerToMedium=false,
    show_T=show_T,
    per(pressure(dp={3*dpCon_nominal,0}, V_flow={0,3*mCon_flow_nominal/1000})),
    allowFlowReversal=false,
    use_inputFilter=false,
    riseTime=10)
    "Condenser variable speed pump-primary circuit"
    annotation (Placement(transformation(extent={{18,-2},{38,18}})));
    Fluid.FixedResistances.Junction splVal3(
    final dp_nominal={0,0,0},
    from_dp=false,
    tau=1,
    m_flow_nominal={mCon_flow_nominal,-mSecHea_flow_nominal,
        mSecHea_flow_nominal - mCon_flow_nominal},
    redeclare package Medium = Medium,
    energyDynamics=Modelica.Fluid.Types.Dynamics.SteadyState)
      "Flow splitter" annotation (Placement(transformation(
          extent={{-10,10},{10,-10}},
          rotation=0,
          origin={62,8})));
    Fluid.Actuators.Valves.ThreeWayLinear                valCon(
    redeclare package Medium = Medium,
    energyDynamics=Modelica.Fluid.Types.Dynamics.FixedInitial,
    m_flow_nominal=mCon_flow_nominal,
    l={0.01,0.01},
    dpValve_nominal=6000)
    "Three way valve modulated to control the entering water temperature to the condenser."
    annotation (Placement(transformation(
        extent={{10,-10},{-10,10}},
        rotation=90,
        origin={-46,52})));
  Modelica.Fluid.Sources.FixedBoundary sou(redeclare package Medium = Medium,
    T=293.15,
      nPorts=1) "Source volume"
    annotation (Placement(transformation(extent={{-120,82},{-100,62}})));
  Fluid.Sources.MassFlowSource_T souPum(
    m_flow=mEva_flow_nominal,
    use_T_in=true,
    redeclare package Medium = Medium,
    nPorts=1)
   "Source side water pump"
   annotation (Placement(transformation(
      extent={{-10,-10},{10,10}},
      rotation=180,
      origin={50,-50})));
  Buildings.Controls.OBC.CDL.Continuous.Sources.Constant
                                                     TEvaEnt(k=12 + 273.15)
    "Entering water temperature at the evaporator side"
    annotation (Placement(transformation(extent={{40,-100},{60,-80}})));
  Modelica.Fluid.Sources.FixedBoundary sin2(redeclare package Medium = Medium,
      nPorts=1) "Source volume for cooling"
    annotation (Placement(transformation(extent={{-60,-40},{-40,-60}})));
    Fluid.Sensors.TemperatureTwoPort           TConEnt(
    redeclare final package Medium = Medium,
    allowFlowReversal=false,
    m_flow_nominal=mCon_flow_nominal,
    tau=0) "Condenser Entering water temperature"
      annotation (Placement(
          transformation(
          extent={{10,-10},{-10,10}},
          rotation=90,
          origin={-46,20})));
  Modelica.Blocks.Continuous.LimPID    valCon1(
    controllerType=Modelica.Blocks.Types.SimpleController.PI,
    yMax=1,
    yMin=0,
    k=0.1,
    Ti(displayUnit="s") = 300)
                        "Condenser three way valve PI control signal "
    annotation (Placement(transformation(extent={{-150,36},{-130,56}})));
  Buildings.Controls.OBC.CDL.Continuous.Sources.Constant TConEntMin(k=25 + 273.15)
    "Minimum condenser entering water temperature"
    annotation (Placement(transformation(extent={{-186,58},{-166,78}})));
  Modelica.Blocks.Sources.IntegerConstant                integerConstant(k=1)
    "Primary pump control signal to maintain the condenser minimum flow rate recommended by the manufacturer. "
    annotation (Placement(transformation(extent={{-104,-20},{-84,0}})));
  Buildings.Controls.OBC.CDL.Continuous.Sources.Constant THeaSet(k=35 + 273.15)
    "Heating Setpoint temperature"
    annotation (Placement(transformation(extent={{-58,82},{-38,102}})));
  Modelica.Fluid.Sources.FixedBoundary sin(redeclare package Medium = Medium,
      nPorts=1) "Source volume for heating"
    annotation (Placement(transformation(extent={{142,12},{122,-8}})));
   Fluid.HeatPumps.Data.EquationFitReversible.Trane_Axiom_EXW240 heaPumDat
   annotation (Placement(transformation(extent={{-160,140},{-140,160}})));
  Buildings.Controls.Continuous.LimPID valCon2(
    controllerType=Modelica.Blocks.Types.SimpleController.PI,
    yMax=1,
    yMin=0.1,
    reset=Buildings.Types.Reset.Parameter,
    y_reset=0,
    k=1,
    Ti(displayUnit="s") = 300,
    reverseAction=false)
                        "Condenser three way valve PI control signal "
    annotation (Placement(transformation(extent={{82,100},{62,120}})));
  Buildings.Controls.OBC.CDL.Continuous.Sources.Constant mConFlo(k=1.4)
    "Condenser mass flow rate."
    annotation (Placement(transformation(extent={{120,100},{100,120}})));
  Fluid.Sensors.MassFlowRate priHeaFlo(redeclare package Medium = Media.Water)
    "Primary circuit condenser side heating water flow rate" annotation (
      Placement(transformation(
        extent={{10,10},{-10,-10}},
        rotation=180,
        origin={102,4})));
  Modelica.Blocks.Sources.BooleanConstant                booleanConstant1(k=true)
    "Primary pump control signal to maintain the condenser minimum flow rate recommended by the manufacturer. "
    annotation (Placement(transformation(extent={{134,58},{114,78}})));
equation
  connect(valCon.port_3,splVal3. port_3)
    annotation (Line(points={{-36,52},{62,52},{62,18}},  color={0,127,255},
      thickness=0.5));
  connect(heaPum.port_b1, pumCon.port_a)
    annotation (Line(points={{4,8},{18,8}}, color={0,127,255}));
  connect(pumCon.port_b, splVal3.port_1)
    annotation (Line(points={{38,8},{52,8}}, color={0,127,255}));
  connect(valCon.port_1, sou.ports[1])
    annotation (Line(points={{-46,62},{-46,72},{-100,72}},color={0,127,255}));
  connect(TEvaEnt.y,souPum. T_in) annotation (Line(points={{62,-90},{72,-90},{72,
          -54},{62,-54}},    color={0,0,127}));
  connect(heaPum.port_a2,souPum. ports[1])
    annotation (Line(points={{4,-4},{12,-4},{12,-50},{40,-50}},
                                               color={0,127,255}));
  connect(heaPum.port_b2, sin2.ports[1]) annotation (Line(points={{-16,-4},{-28,
          -4},{-28,-50},{-40,-50}}, color={0,127,255}));
  connect(valCon.port_2, TConEnt.port_a)
    annotation (Line(points={{-46,42},{-46,30}}, color={0,127,255}));
  connect(heaPum.port_a1, TConEnt.port_b) annotation (Line(points={{-16,8},{-32,
          8},{-32,10},{-46,10}}, color={0,127,255}));
  connect(TConEnt.T, valCon1.u_m)
    annotation (Line(points={{-57,20},{-140,20},{-140,34}}, color={0,0,127}));
  connect(valCon1.u_s, TConEntMin.y) annotation (Line(points={{-152,46},{-162,
          46},{-162,68},{-164,68}},
                                color={0,0,127}));
  connect(integerConstant.y, heaPum.uMod) annotation (Line(points={{-83,-10},{-62,
          -10},{-62,2},{-17,2}}, color={255,127,0}));
  connect(THeaSet.y, heaPum.TSet) annotation (Line(points={{-36,92},{-24,92},{-24,
          11},{-17.4,11}}, color={0,0,127}));
  connect(valCon1.y, valCon.y)
    annotation (Line(points={{-129,46},{-94,46},{-94,52},{-58,52}},
                                                           color={0,0,127}));
  connect(valCon2.u_s, mConFlo.y)
    annotation (Line(points={{84,110},{98,110}}, color={0,0,127}));
  connect(splVal3.port_2, priHeaFlo.port_a)
    annotation (Line(points={{72,8},{82,8},{82,4},{92,4}}, color={0,127,255}));
  connect(sin.ports[1], priHeaFlo.port_b) annotation (Line(points={{122,2},{118,
          2},{118,4},{112,4}}, color={0,127,255}));
  connect(priHeaFlo.m_flow, valCon2.u_m) annotation (Line(points={{102,15},{102,
          80},{72,80},{72,98}}, color={0,0,127}));
  connect(booleanConstant1.y, valCon2.trigger) annotation (Line(points={{113,68},
          {104,68},{104,86},{80,86},{80,98}}, color={255,0,255}));
  connect(valCon2.y, pumCon.y)
    annotation (Line(points={{61,110},{28,110},{28,20}}, color={0,0,127}));
  annotation (Icon(coordinateSystem(preserveAspectRatio=false), graphics={
        Ellipse(lineColor = {75,138,73},
                fillColor={255,255,255},
                fillPattern = FillPattern.Solid,
                extent={{-98,-100},{98,98}}),
        Polygon(lineColor = {0,0,255},
                fillColor = {75,138,73},
                pattern = LinePattern.None,
                fillPattern = FillPattern.Solid,
                points={{-30,64},{70,4},{-30,-56},{-30,64}})}),  Diagram(coordinateSystem(preserveAspectRatio=false, extent={{-200,
            -160},{140,200}}),
        graphics={Line(points={{-22,22}}, color={28,108,200})}),
    experiment(StopTime=3600),
    __Dymola_Commands(
  file="modelica://Buildings/Resources/Scripts/Dymola/Applications/DHC/EnergyTransferStations/Control/AmbientCircuitControllerBlock.mos"
        "Simulate and plot"),
         experiment(Tolerance=1e-6, StopTime=14400),
         Documentation(info="<html>
<p>
This model validates the controller block
<a href=\"Buildings.Applications.DHC.EnergyTransferStations.Control.AmbientCircuitController\"> 
Buildings.Applications.DHC.EnergyTransferStations.Control.AmbientCircuitController</a>.
<p>

</html>", revisions="<html>
<ul>
<li>
 <br/>
</li>
</ul>
</html>"));
end ThreeWayValve_HeatPumpCombined;
