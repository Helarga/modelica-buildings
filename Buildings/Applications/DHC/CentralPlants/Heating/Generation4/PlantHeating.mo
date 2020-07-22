within Buildings.Applications.DHC.CentralPlants.Heating.Generation4;
model PlantHeating "Central heating plant."

  package Medium = Buildings.Media.Water "Medium model";

  parameter Integer numChi(min=1, max=2)=2 "Number of chillers, maximum is 2";

  parameter Boolean show_T = true
    "= true, if actual temperature at port is computed"
    annotation(Dialog(tab="Advanced",group="Diagnostics"));

  // chiller parameters
  parameter Modelica.SIunits.MassFlowRate mCHW_flow_nominal
    "Nominal chilled water mass flow rate"
    annotation (Dialog(group="Chiller"));
  parameter Modelica.SIunits.Pressure dpCHW_nominal
    "Pressure difference at the chilled water side"
    annotation (Dialog(group="Chiller"));
  parameter Modelica.SIunits.Power QEva_nominal
    "Nominal cooling capacity of single chiller (negative means cooling)"
    annotation (Dialog(group="Chiller"));
  parameter Modelica.SIunits.MassFlowRate mMin_flow
    "Minimum mass flow rate of single chiller"
    annotation (Dialog(group="Chiller"));

  // cooling tower parameters
  parameter Modelica.SIunits.MassFlowRate mCW_flow_nominal
    "Nominal condenser water mass flow rate"
    annotation (Dialog(group="Cooling Tower"));
  parameter Modelica.SIunits.Pressure dpCW_nominal
    "Pressure difference at the condenser water side"
    annotation (Dialog(group="Cooling Tower"));
  parameter Modelica.SIunits.Temperature TAirInWB_nominal
    "Nominal air wetbulb temperature"
    annotation (Dialog(group="Cooling Tower"));
  parameter Modelica.SIunits.Temperature TCW_nominal
    "Nominal condenser water temperature at tower inlet"
    annotation (Dialog(group="Cooling Tower"));
  parameter Modelica.SIunits.TemperatureDifference dT_nominal
    "Temperature difference between inlet and outlet of the tower"
     annotation (Dialog(group="Cooling Tower"));
  parameter Modelica.SIunits.Temperature TMin
    "Minimum allowed water temperature entering chiller"
    annotation (Dialog(group="Cooling Tower"));
  parameter Modelica.SIunits.Power PFan_nominal
    "Fan power"
    annotation (Dialog(group="Cooling Tower"));

  // pump parameters
  replaceable parameter Buildings.Fluid.Movers.Data.Generic perCHWPum
    constrainedby Buildings.Fluid.Movers.Data.Generic
    "Performance data of chilled water pump"
    annotation (Dialog(group="Pump"),choicesAllMatching=true,
      Placement(transformation(extent={{120,82},{134,96}})));
  replaceable parameter Buildings.Fluid.Movers.Data.Generic perCWPum
    constrainedby Buildings.Fluid.Movers.Data.Generic
    "Performance data of condenser water pump"
    annotation (Dialog(group="Pump"),choicesAllMatching=true,
      Placement(transformation(extent={{142,82},{156,96}})));
  parameter Modelica.SIunits.Pressure dpCHWPum_nominal
    "Nominal pressure drop of chilled water pumps"
    annotation (Dialog(group="Pump"));
  parameter Modelica.SIunits.Pressure dpCWPum_nominal
    "Nominal pressure drop of condenser water pumps"
    annotation (Dialog(group="Pump"));

  // control settings
  parameter Modelica.SIunits.Time tWai "Waiting time"
    annotation (Dialog(group="Control Settings"));
  parameter Modelica.SIunits.PressureDifference dpSetPoi(displayUnit="Pa")
   "Demand side pressure difference setpoint"
    annotation (Dialog(group="Control Settings"));

  // dynamics
  parameter Modelica.Fluid.Types.Dynamics energyDynamics=Modelica.Fluid.Types.Dynamics.DynamicFreeInitial
    "Type of energy balance: dynamic (3 initialization options) or steady state"
    annotation(Evaluate=true, Dialog(tab = "Dynamics", group="Equations"));
  parameter Modelica.Fluid.Types.Dynamics massDynamics=energyDynamics
    "Type of mass balance: dynamic (3 initialization options) or steady state"
    annotation(Evaluate=true, Dialog(tab = "Dynamics", group="Equations"));

  Medium.ThermodynamicState sta_a=
      Medium.setState_phX(port_a.p,
                          noEvent(actualStream(port_a.h_outflow)),
                          noEvent(actualStream(port_a.Xi_outflow))) if
         show_T "Medium properties in port_a";

  Medium.ThermodynamicState sta_b=
      Medium.setState_phX(port_b.p,
                          noEvent(actualStream(port_b.h_outflow)),
                          noEvent(actualStream(port_b.Xi_outflow))) if
          show_T "Medium properties in port_b";

  Modelica.Fluid.Interfaces.FluidPort_a port_a(redeclare package Medium = Medium)
    "Fluid connector a (positive design flow direction is from port_a to port_b)"
    annotation (Placement(transformation(extent={{150,40},{170,60}}),
        iconTransformation(extent={{90,40},{110,60}})));

  Modelica.Fluid.Interfaces.FluidPort_b port_b(redeclare package Medium = Medium)
    "Fluid connector b (positive design flow direction is from port_a to port_b)"
    annotation (Placement(transformation(extent={{150,-60},{170,-40}}),
        iconTransformation(extent={{90,-60},{110,-40}})));

  Modelica.Blocks.Interfaces.BooleanInput on "On signal of the plant"
    annotation (Placement(transformation(extent={{-180,40},{-140,80}}),
        iconTransformation(extent={{-140,60},{-100,100}})));

  Modelica.Blocks.Interfaces.RealInput TCHWSupSet(
    final unit="K",
    displayUnit="degC")
    "Set point for chilled water supply temperature"
    annotation (Placement(transformation(extent={{-180,0},{-140,40}}),
        iconTransformation(extent={{-140,10},{-100,50}})));

  Modelica.Blocks.Interfaces.RealInput TWetBul(
    final unit="K",
    displayUnit="degC")
    "Entering air wetbulb temperature"
    annotation (Placement(transformation(extent={{-180,-80},{-140,-40}}),
        iconTransformation(extent={{-140,-100},{-100,-60}})));

  Modelica.Blocks.Interfaces.RealInput dpMea(final unit="Pa")
    "Measured pressure difference" annotation (Placement(transformation(extent=
            {{-180,-40},{-140,0}}), iconTransformation(extent={{-140,-50},{-100,
            -10}})));

  Buildings.Applications.DataCenters.ChillerCooled.Equipment.FlowMachine_y pumCHW(
    redeclare package Medium = Medium,
    per=fill(perCHWPum, numChi),
    riseTimePump=100,
    energyDynamics=energyDynamics,
    m_flow_nominal=mCHW_flow_nominal,
    dpValve_nominal=dpCHWPum_nominal,
    num=numChi) "Chilled water pumps"
    annotation (Placement(transformation(extent={{10,40},{-10,60}})));

  Buildings.Fluid.Sensors.TemperatureTwoPort senTCHWSup(
    redeclare package Medium = Medium,
    m_flow_nominal=mCHW_flow_nominal) "Chilled water supply temperature"
    annotation (Placement(transformation(
        extent={{-10,-10},{10,10}},
        rotation=0,
        origin={130,-50})));

  Buildings.Applications.DHC.CentralPlants.Cooling.Controls.ChilledWaterPumpSpeed
    CHWPumCon(
    tWai=0,
    m_flow_nominal=mCHW_flow_nominal,
    dpSetPoi=dpSetPoi,
    controllerType=Modelica.Blocks.Types.SimpleController.PI)
    "Chilled water pump controller"
    annotation (Placement(transformation(extent={{-120,-26},{-100,-6}})));

  Buildings.Applications.DHC.CentralPlants.Cooling.Controls.ChillerStage chiStaCon(
    tWai=tWai,
    QEva_nominal=QEva_nominal) "Chiller staging controller"
    annotation (Placement(transformation(extent={{-120,46},{-100,66}})));

  Modelica.Blocks.Sources.RealExpression mPum_flow(y=pumCHW.port_a.m_flow)
    "Total chilled water pump mass flow rate"
    annotation (Placement(transformation(extent={{-100,-2},{-120,18}})));

  Modelica.Blocks.Sources.RealExpression mValByp_flow(y=valByp.port_a.m_flow/(
        if chiOn[numChi].y then numChi*mMin_flow else mMin_flow))
    "Chilled water bypass valve mass flow rate"
    annotation (Placement(transformation(extent={{160,-40},{140,-20}})));

  Buildings.Controls.Continuous.LimPID bypValCon(
    controllerType=Modelica.Blocks.Types.SimpleController.PI,
    k=1,
    Ti=60,
    reset=Buildings.Types.Reset.Parameter,
    y_reset=0)
           "Chilled water bypass valve controller"
    annotation (Placement(transformation(extent={{140,-10},{120,10}})));
  Buildings.Fluid.Sensors.TemperatureTwoPort senTCHWRet(redeclare package
      Medium =                                                                     Medium,
      m_flow_nominal=mCHW_flow_nominal) "Chilled water return temperature"
    annotation (Placement(transformation(
        extent={{10,-10},{-10,10}},
        rotation=0,
        origin={130,50})));

  Buildings.Fluid.Sources.Boundary_pT expTanCW(redeclare package Medium = Medium)
              "Condenser water expansion tank"
    annotation (Placement(transformation(extent={{-60,-74},{-40,-54}})));

  DataCenters.ChillerCooled.Equipment.ElectricChillerParallel                        mulChiSys(
    show_T=true,
    per=fill(perChi, numChi),
    m1_flow_nominal=mCHW_flow_nominal,
    m2_flow_nominal=mCW_flow_nominal,
    dp1_nominal=dpCHW_nominal,
    dp2_nominal=dpCW_nominal,
    num=numChi,
    redeclare package Medium1 = Medium,
    redeclare package Medium2 = Medium)
                                      "Chillers connected in parallel"
    annotation (Placement(transformation(extent={{18,4},{-2,-16}})));
  BoilerParallel boilerParallel
    annotation (Placement(transformation(extent={{-40,-2},{-20,18}})));
protected
  final parameter Medium.ThermodynamicState sta_default = Medium.setState_pTX(
    T=Medium.T_default,
    p=Medium.p_default,
    X=Medium.X_default) "Medium state at default properties";
  final parameter Modelica.SIunits.SpecificHeatCapacity cp_default=
    Medium.specificHeatCapacityCp(sta_default)
    "Specific heat capacity of the fluid";

equation

  connect(senTCHWSup.port_b, port_b) annotation (Line(
      points={{140,-50},{160,-50}},
      color={0,127,255}));
  connect(on, chiStaCon.on) annotation (Line(points={{-160,60},{-122,60}},
                      color={255,0,255}));
  connect(mPum_flow.y, CHWPumCon.masFloPum) annotation (Line(points={{-121,8},
          {-132,8},{-132,-12},{-122,-12}}, color={0,0,127}));
  connect(mValByp_flow.y, bypValCon.u_m) annotation (Line(points={{139,-30},{
          130,-30},{130,-12}},                                                                    color={0,0,127}));
  connect(port_a, senTCHWRet.port_a) annotation (Line(points={{160,50},{140,50}}, color={0,127,255}));
  connect(dpMea, CHWPumCon.dpMea)
    annotation (Line(points={{-160,-20},{-122,-20}}, color={0,0,127}));
  annotation (__Dymola_Commands,
  Diagram(coordinateSystem(preserveAspectRatio=false, extent={{-140,-80},{160,100}})),
    experiment(
      StartTime=1.728e+007,
      StopTime=1.73664e+007,
      __Dymola_NumberOfIntervals=1440,
      __Dymola_Algorithm="Dassl"),
    __Dymola_experimentSetupOutput,
    Documentation(info="<html>
<p>The schematic drawing of the Lejeune plant is shown as folowing.</p>
<p><img src=\"Resources/Images/lejeunePlant/lejeune_schematic_drawing.jpg\" alt=\"image\"/> </p>
<p>In addition, the parameters are listed as below.</p>
<p>The parameters for the chiller plant.</p>
<p><img src=\"Resources/Images/lejeunePlant/Chiller.png\" alt=\"image\"/> </p>
<p>The parameters for the primary chilled water pump.</p>
<p><img src=\"Resources/Images/lejeunePlant/PriCHWPum.png\" alt=\"image\"/> </p>
<p>The parameters for the secondary chilled water pump.</p>
<p><img src=\"Resources/Images/lejeunePlant/SecCHWPum1.png\" alt=\"image\"/> </p>
<p><img src=\"Resources/Images/lejeunePlant/SecCHWPum2.png\" alt=\"image\"/> </p>
<p>The parameters for the condenser water pump.</p>
<p><img src=\"Resources/Images/lejeunePlant/CWPum.png\" alt=\"image\"/> </p>
</html>"),
    Icon(coordinateSystem(extent={{-100,-100},{100,100}}),    graphics={
                                Rectangle(
        extent={{-100,-100},{100,100}},
        lineColor={0,0,127},
        fillColor={255,255,255},
        fillPattern=FillPattern.Solid),
        Polygon(
          points={{-62,-14},{-62,-14}},
          lineColor={238,46,47},
          fillColor={255,255,255},
          fillPattern=FillPattern.Solid),
        Polygon(
          points={{80,-60},{-80,-60},{-80,60},{-60,60},{-60,0},{-40,0},{-40,20},
              {0,0},{0,20},{40,0},{40,20},{80,0},{80,-60}},
          lineColor={95,95,95},
          fillColor={28,108,200},
          fillPattern=FillPattern.Solid),
      Rectangle(
        extent={{46,-38},{58,-26}},
        lineColor={255,255,255},
        fillColor={255,255,255},
        fillPattern=FillPattern.Solid),
      Rectangle(
        extent={{62,-38},{74,-26}},
        lineColor={255,255,255},
        fillColor={255,255,255},
        fillPattern=FillPattern.Solid),
      Rectangle(
        extent={{62,-54},{74,-42}},
        lineColor={255,255,255},
        fillColor={255,255,255},
        fillPattern=FillPattern.Solid),
      Rectangle(
        extent={{46,-54},{58,-42}},
        lineColor={255,255,255},
        fillColor={255,255,255},
        fillPattern=FillPattern.Solid),
      Rectangle(
        extent={{22,-54},{34,-42}},
        lineColor={255,255,255},
        fillColor={255,255,255},
        fillPattern=FillPattern.Solid),
      Rectangle(
        extent={{6,-54},{18,-42}},
        lineColor={255,255,255},
        fillColor={255,255,255},
        fillPattern=FillPattern.Solid),
      Rectangle(
        extent={{6,-38},{18,-26}},
        lineColor={255,255,255},
        fillColor={255,255,255},
        fillPattern=FillPattern.Solid),
      Rectangle(
        extent={{22,-38},{34,-26}},
        lineColor={255,255,255},
        fillColor={255,255,255},
        fillPattern=FillPattern.Solid),
      Rectangle(
        extent={{-18,-54},{-6,-42}},
        lineColor={255,255,255},
        fillColor={255,255,255},
        fillPattern=FillPattern.Solid),
      Rectangle(
        extent={{-34,-54},{-22,-42}},
        lineColor={255,255,255},
        fillColor={255,255,255},
        fillPattern=FillPattern.Solid),
      Rectangle(
        extent={{-34,-38},{-22,-26}},
        lineColor={255,255,255},
        fillColor={255,255,255},
        fillPattern=FillPattern.Solid),
      Rectangle(
        extent={{-18,-38},{-6,-26}},
        lineColor={255,255,255},
        fillColor={255,255,255},
        fillPattern=FillPattern.Solid),
        Text(
          extent={{-149,-114},{151,-154}},
          lineColor={0,0,255},
          textString="%name")}));
end PlantHeating;
