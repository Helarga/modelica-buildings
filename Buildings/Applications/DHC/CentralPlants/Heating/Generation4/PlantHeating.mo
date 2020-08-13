within Buildings.Applications.DHC.CentralPlants.Heating.Generation4;
model PlantHeating "Central heating plant."

  package  Medium = Buildings.Media.Water "MediumW model";

  parameter Integer numBoi(min=1, max=2)=2 "Number of chillers, maximum is 2";

  parameter Boolean show_T = true
    "= true, if actual temperature at port is computed"
    annotation(Dialog(tab="Advanced",group="Diagnostics"));

  // boiler parameters
  parameter Modelica.SIunits.MassFlowRate mHW_flow_nominal=8
    "Nominal heating water mass flow rate"
    annotation (Dialog(group="Chiller"));
  parameter Modelica.SIunits.Pressure dpHW_nominal=10000
    "Pressure difference at the heating water side"
    annotation (Dialog(group="Chiller"));
  parameter Modelica.SIunits.Power QBoi_flow_nominal=60000
    "Nominal heating capacity of single boiler"
    annotation (Dialog(group="Chiller"));
  parameter Modelica.SIunits.MassFlowRate mMin_flow=3
    "Minimum mass flow rate of single boiler"
    annotation (Dialog(group="Boiler"));
  parameter Modelica.SIunits.MassFlowRate mBoi_flow_nominal=20
    "Nominal mass flow rate of single boiler"
    annotation (Dialog(group="Boiler"));
  parameter Modelica.SIunits.Pressure dp_nominalBoi=70000
    "Pressure difference at the boiler water side"
    annotation (Dialog(group="Boiler"));

  // pump parameters
  replaceable parameter Buildings.Fluid.Movers.Data.Generic perHWPum
    constrainedby Buildings.Fluid.Movers.Data.Generic
    "Performance data of heating water pump" annotation (
    Dialog(group="Pump"),
    choicesAllMatching=true,
    Placement(transformation(extent={{138,82},{152,96}})));
  parameter Modelica.SIunits.Pressure dpHWPum_nominal
    "Nominal pressure drop of heating water pumps"
    annotation (Dialog(group="Pump"));

  // control settings
  parameter Modelica.SIunits.Time tWai "Waiting time"
    annotation (Dialog(group="Control Settings"));
  parameter Modelica.SIunits.PressureDifference dpSetPoi(displayUnit="Pa")
   "Demand side pressure difference setpoint"
    annotation (Dialog(group="Control Settings"));

  // dynamics
  parameter Modelica.Fluid.Types.Dynamics energyDynamics=Modelica.Fluid.Types.Dynamics.FixedInitial
    "Type of energy balance: dynamic (3 initialization options) or steady state"
    annotation(Evaluate=true, Dialog(tab = "Dynamics", group="Equations"));
  parameter Modelica.Fluid.Types.Dynamics massDynamics=energyDynamics
    "Type of mass balance: dynamic (3 initialization options) or steady state"
    annotation(Evaluate=true, Dialog(tab = "Dynamics", group="Equations"));

  Medium.ThermodynamicState sta_a=
      Medium.setState_phX(port_a.p,
                          noEvent(actualStream(port_a.h_outflow)),
                          noEvent(actualStream(port_a.Xi_outflow))) if
         show_T "MediumW properties in port_a";

  Medium.ThermodynamicState sta_b=
      Medium.setState_phX(port_b.p,
                          noEvent(actualStream(port_b.h_outflow)),
                          noEvent(actualStream(port_b.Xi_outflow))) if
          show_T "MediumW properties in port_b";

  Modelica.Fluid.Interfaces.FluidPort_a port_a(redeclare package Medium = Medium)
    "Fluid connector a (positive design flow direction is from port_a to port_b)"
    annotation (Placement(transformation(extent={{150,40},{170,60}}),
        iconTransformation(extent={{90,40},{110,60}})));

  Modelica.Fluid.Interfaces.FluidPort_b port_b(redeclare package Medium = Medium)
    "Fluid connector b (positive design flow direction is from port_a to port_b)"
    annotation (Placement(transformation(extent={{150,-60},{170,-40}}),
        iconTransformation(extent={{90,-60},{110,-40}})));

  Modelica.Blocks.Interfaces.BooleanInput on "On signal of the plant"
    annotation (Placement(transformation(extent={{-160,30},{-140,50}}),
        iconTransformation(extent={{-140,60},{-100,100}})));

  Modelica.Blocks.Interfaces.RealInput dpMea(final unit="Pa")
    "Measured pressure difference" annotation (Placement(transformation(extent={{-160,
            -44},{-140,-24}}),      iconTransformation(extent={{-140,-50},{-100,
            -10}})));

  BoilerParallel boiHotWat(
    redeclare package Medium = Medium,
    num=numBoi,
    m_flow_nominal=mBoi_flow_nominal,
    Q_flow_nominal=QBoi_flow_nominal,
    dp_nominal=dp_nominalBoi)
    "Parallel boilers "
    annotation (Placement(transformation(extent={{-14,-30},{6,-10}})));

  Buildings.Fluid.Sensors.TemperatureTwoPort senTHWSup(
    redeclare package Medium = Medium,
    m_flow_nominal=mHW_flow_nominal)
    "Heating water supply temperature" annotation (Placement(transformation(
        extent={{-10,-10},{10,10}},
        rotation=0,
        origin={106,-50})));

  Controls.HeatingWaterPumpSpeed heaWatPumCon(
    tWai=0,
    m_flow_nominal=mHW_flow_nominal,
    dpSetPoi=dpSetPoi,
    controllerType=Modelica.Blocks.Types.SimpleController.PI)
    "Heating water pump controller."
    annotation (Placement(transformation(extent={{-120,-40},{-100,-20}})));

  Controls.BoilerStage boiStaCon(
    tWai=tWai,
    QBoi_nominal=QBoi_flow_nominal) "Boiler staging controller"
    annotation (Placement(transformation(extent={{-120,50},{-100,70}})));

  Modelica.Blocks.Sources.RealExpression mPum_flow(y=pumHW.port_a.m_flow)
    "Total heating water pump mass flow rate"
    annotation (Placement(transformation(extent={{-100,-2},{-120,18}})));

  Modelica.Blocks.Sources.RealExpression norMeaDecMasFlo(y=valByp.port_a.m_flow/
        (if on then numBoi*mMin_flow else mMin_flow))
    "Normalised decoupler line measured mass flow rate."
    annotation (Placement(transformation(extent={{160,-40},{140,-20}})));

  Buildings.Controls.Continuous.LimPID bypValCon(
    controllerType=Modelica.Blocks.Types.SimpleController.PI,
    k=1,
    Ti=60,
    reset=Buildings.Types.Reset.Parameter,
    y_reset=0)
     "Heating water bypass valve controller"
    annotation (Placement(transformation(extent={{132,-10},{112,10}})));
  Buildings.Fluid.Sensors.TemperatureTwoPort senTHWRet(
    redeclare package Medium =Medium,
    m_flow_nominal=mHW_flow_nominal)
    "Chilled water return temperature" annotation (Placement(transformation(
        extent={{10,-10},{-10,10}},
        rotation=0,
        origin={130,50})));

  Buildings.Fluid.Sources.Boundary_pT expTanCW(
    redeclare package Medium = Medium,
    nPorts=1)
     "Heating water expansion tank"
    annotation (Placement(transformation(extent={{40,12},{20,32}})));
  Subsystems.FlowMachine_y pumHW(
     redeclare package Medium = Medium,
     m_flow_nominal=mBoi_flow_nominal,
     dpValve_nominal=7000)
    annotation (Placement(transformation(extent={{0,60},{-20,40}})));
  Buildings.Fluid.Actuators.Valves.TwoWayEqualPercentage valByp(
    redeclare package Medium = Medium,
    allowFlowReversal=false,
    m_flow_nominal=mHW_flow_nominal*0.05,
    dpValve_nominal=dpHWPum_nominal,
    l=0.001)
    "Heating water bypass valve"
    annotation (Placement(transformation(
        extent={{-10,10},{10,-10}},
        rotation=90,
        origin={50,0})));

  Modelica.Blocks.Math.Add dT(final k1=-1, final k2=+1)
    "Temperature difference"
    annotation (Placement(transformation(extent={{102,76},{82,96}})));
  Modelica.Blocks.Math.Product pro "Product"
    annotation (Placement(transformation(extent={{54,74},{34,94}})));
  Modelica.Blocks.Math.Gain cp(final k=4200)
    "Specific heat multiplier to calculate heat flow rate"
    annotation (Placement(transformation(extent={{10,74},{-10,94}})));
  Fluid.Sensors.MassFlowRate senMasFlo(
    redeclare package Medium =Medium)
    "Heating water return mass flow"
    annotation (Placement(transformation(extent={{84,40},{64,60}})));
  Fluid.Sensors.MassFlowRate senMasFloByp(
    redeclare package Medium = Medium)
    "Heating water bypass valve mass flow meter"
    annotation (Placement(transformation(
        extent={{10,-10},{-10,10}},
        rotation=-90,
        origin={50,-30})));
  Modelica.Blocks.Math.Product pro1[numBoi] "Product" annotation (Placement(
        transformation(
        extent={{-10,10},{10,-10}},
        rotation=-90,
        origin={-72,4})));
  Modelica.Blocks.Sources.RealExpression norDecMasFlo(y=1)
    "Normalized decoupler line mass flow rate."
    annotation (Placement(transformation(extent={{160,-10},{140,10}})));
  final parameter Medium.ThermodynamicState sta_default=
    Medium.setState_pTX(
      T=Medium.T_default,
      p=Medium.p_default,
      X=Medium.X_default)
    "Medium state at default properties";
  final parameter Modelica.SIunits.SpecificHeatCapacity cp_default=
    Medium.specificHeatCapacityCp(sta_default)
    "Specific heat capacity of the fluid";

equation
  for i in 1:numBoi loop
     connect(heaWatPumCon.PLR, pro1[i].u1) annotation (Line(points={{-99,-24},{
            -92,-24},{-92,24},{-78,24},{-78,16}},
                                              color={0,0,127}));
  end for;
  connect(senTHWSup.port_b, port_b)
    annotation (Line(points={{116,-50},{160,-50}}, color={0,127,255}));
  connect(on,boiStaCon. on)
    annotation (Line(points={{-150,40},{-136,40},{-136,64},{-122,64}},
                      color={255,0,255}));
  connect(mPum_flow.y, heaWatPumCon.masFloPum) annotation (Line(points={{-121,8},
          {-132,8},{-132,-23.4},{-122,-23.4}},
                                           color={0,0,127}));
  connect(norMeaDecMasFlo.y, bypValCon.u_m)
    annotation (Line(points={{139,-30},{122,-30},{122,-12}}, color={0,0,127}));
  connect(port_a, senTHWRet.port_a)
    annotation (Line(points={{160,50},{140,50}}, color={0,127,255}));
  connect(dpMea, heaWatPumCon.dpMea)
    annotation (Line(points={{-150,-34},{-122,-34}}, color={0,0,127}));
  connect(pumHW.port_b, boiHotWat.port_a)
    annotation (Line(points={{-20,50},{-48,50},{-48,-20},{-14,-20}},
                                    color={0,127,255}));
  connect(valByp.port_b, pumHW.port_a)
    annotation (Line(points={{50,10},{50,50},{0,50}},  color={0,127,255}));
  connect(senTHWRet.port_b, senMasFlo.port_a)
    annotation (Line(points={{120,50},{84,50}}, color={0,127,255}));
  connect(senMasFlo.port_b, pumHW.port_a)
    annotation (Line(points={{64,50},{0,50}},  color={0,127,255}));
  connect(senTHWSup.T, dT.u2)
    annotation (Line(points={{106,-39},{106,80},{104,80}},
                         color={0,0,127}));
  connect(senTHWRet.T, dT.u1)
    annotation (Line(points={{130,61},{130,92},{104,92}}, color={0,0,127}));
  connect(dT.y, pro.u1)
    annotation (Line(points={{81,86},{74,86},{74,90},{56,90}},
        color={0,0,127}));
  connect(pro.u2, senMasFlo.m_flow)
    annotation (Line(points={{56,78},{74,78},{74,61}}, color={0,0,127}));
  connect(pro.y, cp.u)
    annotation (Line(points={{33,84},{12,84}},
        color={0,0,127}));
  connect(cp.y, boiStaCon.QLoa)
    annotation (Line(points={{-11,84},{-132,84},{-132,56},{-122,56}},
                               color={0,0,127}));
  connect(valByp.port_a, senMasFloByp.port_b)
    annotation (Line(points={{50,-10},{50,-20}}, color={0,127,255}));
  connect(boiHotWat.port_b, senTHWSup.port_a)
    annotation (Line(points={{6,-20},{
          20,-20},{20,-50},{96,-50}},  color={0,127,255}));
  connect(senMasFloByp.port_a, senTHWSup.port_a)
    annotation (Line(points={{50,-40},{50,-50},{96,-50}}, color={0,127,255}));
  connect(valByp.y, bypValCon.y)
    annotation (Line(points={{62,-6.66134e-16},{94,-6.66134e-16},{94,0},{111,0}},
                                         color={0,0,127}));
  connect(pumHW.port_a, expTanCW.ports[1])
    annotation (Line(points={{0,50},{18,50},{18,22},{20,22}},
                                color={0,127,255}));
  connect(heaWatPumCon.y, pumHW.u) annotation (Line(points={{-99,-30},{-52,-30},
          {-52,62},{28,62},{28,54},{2.2,54}},  color={0,0,127}));
  connect(bypValCon.u_s, norDecMasFlo.y)
    annotation (Line(points={{134,0},{139,0}}, color={0,0,127}));
  connect(on, bypValCon.trigger)
    annotation (Line(points={{-150,40},{-136,40},{-136,-76},{130,-76},{130,-12}},
                                     color={255,0,255}));
  connect(pro1.y, boiHotWat.PLR)
    annotation (Line(points={{-72,-7},{-72,-16},{-15,-16}}, color={0,0,127}));
  connect(boiStaCon.y, pro1.u2)
    annotation (Line(points={{-99,60},{-66,60},{-66,16}}, color={0,0,127}));
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
          fillColor={238,46,47},
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
