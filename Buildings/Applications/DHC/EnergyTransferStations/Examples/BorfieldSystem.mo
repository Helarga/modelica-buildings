within Buildings.Applications.DHC.EnergyTransferStations.Examples;
model BorfieldSystem
  "The hydraluic connection of the borfield system integertaed with the ambient circuit controller."
  package Medium = Buildings.Media.Water "Medium model";

  parameter Modelica.SIunits.MassFlowRate mSou_flow_nominal=1.89
   "Source heat exchanger nominal mass flow rate";
  parameter Modelica.SIunits.MassFlowRate mLoa_flow_nominal=1.89
   "Load heat exchanger nominal mass flow rate";
  parameter Real scaling_factor=1
   "Scaling factor for heatpump capacity";

  parameter Modelica.SIunits.MassFlowRate mSecHea_flow_nominal=1.0
   "Secondary(building side) heatig water nominal mass flow rate";
  parameter Modelica.SIunits.MassFlowRate mSecCoo_flow_nominal=1.0
   "Secondary(building side) cooling water mass flow rate";

    parameter Modelica.SIunits.MassFlowRate valEva_flow_nominal= mSou_flow_nominal
   "Evaporator three way valve nominal mass flow rate";
  parameter Modelica.SIunits.PressureDifference dpSou_nominal= per.dpHeaSou_nominal
      "Pressure difference accross the condenser"
      annotation (Dialog(tab="WSHP system"));
  parameter Boolean show_T=true
      "= true, if actual temperature at port is computed"
      annotation (Dialog(group="Advanced"));

    Control.AmbientCircuitSid ambCon(dTGeo=dTGeo, dTHex=dTHex)
    "control of the ambient hydraulic circuit"
        annotation (Placement(transformation(extent={{86,32},{106,52}})));
    Fluid.Actuators.Valves.ThreeWayEqualPercentageLinear valBor(
    redeclare package Medium = Medium,
    energyDynamics=Modelica.Fluid.Types.Dynamics.FixedInitial,
    m_flow_nominal=mGeo_flow_nominal,
    l={0.01,0.01},
    dpValve_nominal=6000)
    "Three way valve modulated to control the entering water temperature to the borefield system."
     annotation (Placement(transformation(extent={{10,-10},{-10,10}},
                                          rotation=90,
                                          origin={160,2})));
    Fluid.Movers.FlowControlled_m_flow pumBor(
    redeclare package Medium = Medium,
    energyDynamics=Modelica.Fluid.Types.Dynamics.SteadyState,
    m_flow_nominal=mGeo_flow_nominal,
    addPowerToMedium=false,
    show_T=show_T,
    per(pressure(dp={2*dpBorFie_nominal,0}, V_flow={0,2*mGeo_flow_nominal/1000})),
    use_inputFilter=false,
    riseTime=10)
      "Pump (or valve) that forces the flow rate to be set to the control signal"
       annotation (Placement(transformation(extent={{10,-10},{-10,10}},
        rotation=90,  origin={160,-38})));

   Fluid.Geothermal.Borefields.OneUTube borFie(
    redeclare package Medium = Medium,
    allowFlowReversal=false,
    borFieDat=borFieDat,
    energyDynamics=Modelica.Fluid.Types.Dynamics.FixedInitial,
    show_T=show_T,
    dT_dz=0,
    TExt0_start=285.95)
      "Geothermal borefield"
      annotation (Placement(transformation(extent={{-10,-10},{10,10}},
          rotation=270,
          origin={160,-78})));
   Fluid.Sensors.TemperatureTwoPort TBorLvg(
    tau=0,
    redeclare final package Medium = Medium,
    allowFlowReversal=false,
    m_flow_nominal=mGeo_flow_nominal)
    "Borefield system leaving water temperature"
    annotation (Placement(transformation(
        extent={{-10,-10},{10,10}},
        rotation=90,
        origin={198,-98})));
    Fluid.FixedResistances.Junction splVal1(
    final dp_nominal=200*{1,-1,-1},
    from_dp=false,
    tau=1,
    m_flow_nominal={mGeo_flow_nominal,-mGeo_flow_nominal,-mGeo_flow_nominal},
    redeclare package Medium = Medium,
    energyDynamics=Modelica.Fluid.Types.Dynamics.SteadyState)
      "Flow splitter" annotation (Placement(transformation(
          extent={{-10,10},{10,-10}},
          rotation=90,
          origin={198,-58})));
    Buildings.Controls.OBC.CDL.Continuous.Gain gaiBor(k=mGeo_flow_nominal)
      "Gain for mass flow rate of borefield"
        annotation (Placement(transformation(extent={{120,-48},{140,-28}})));
    Fluid.Actuators.Valves.TwoWayLinear valSupCoo1(
    redeclare final package Medium = Medium,
    use_inputFilter=false,
    dpFixed_nominal=0,
    show_T=true,
    dpValve_nominal=dp_nominal,
    riseTime=10,
    l=1e-8,
    m_flow_nominal=mGeo_flow_nominal + mHex_flow_nominal)
      "Two way modulating valve"
      annotation (Placement(transformation(extent={{194,-28},{214,-8}})));
     Fluid.Sensors.TemperatureTwoPort TBorEnt(
    tau=0,
    redeclare final package Medium = Medium,
    allowFlowReversal=false,
    m_flow_nominal=mGeo_flow_nominal)
      "Entering water temperature to the borefield system"
      annotation (Placement(
        transformation(
        extent={{-10,10},{10,-10}},
        rotation=270,
        origin={160,-18})));
equation

  connect(valBor.port_3,splVal1. port_3)
    annotation (Line(points={{170,2},{180,2},{180,-58},{188,-58}},         color={0,127,
          255},
      thickness=0.5));
  connect(ambRetHed.ports_b[2],valBor. port_1) annotation (Line(
      points={{179.6,64.85},{160,64.85},{160,12}},
      color={0,127,255},
      thickness=0.5));
  connect(ambCon.yBorPum,gaiBor. u) annotation (Line(points={{107,36.8},{114,
          36.8},{114,-38},{118,-38}},      color={0,0,127}));
  connect(ambCon.yDisHexPum, gaiMDisHex.u) annotation (Line(points={{107,33.2},
          {110,33.2},{110,-140},{268,-140}},   color={0,0,127}));
  connect(splVal1.port_2,valSupCoo1. port_a) annotation (Line(points={{198,-48},
          {198,-42},{186,-42},{186,-18},{194,-18}},     color={0,127,255}));
  connect(gaiBor.y, pumBor.m_flow_in)
    annotation (Line(points={{142,-38},{148,-38}},
                                                color={0,0,127}));
  connect(valBor.port_2, TBorEnt.port_a)
    annotation (Line(points={{160,-8},{160,-8}},
                                             color={0,127,255}));
  connect(pumBor.port_a, TBorEnt.port_b)
    annotation (Line(points={{160,-28},{160,-28}},
                                                 color={0,127,255}));
  connect(pumBor.port_b, borFie.port_a)
    annotation (Line(points={{160,-48},{160,-68}},        color={0,127,255}));
  connect(borFie.port_b, TBorLvg.port_a) annotation (Line(points={{160,-88},{
          160,-120},{198,-120},{198,-108}},
                               color={0,127,255}));
  connect(TBorLvg.port_b, splVal1.port_1)
    annotation (Line(points={{198,-88},{198,-68}},
                                                 color={0,127,255}));
  connect(ambCon.yBorThrVal, valBor.y) annotation (Line(points={{107,47.8},{132,
          47.8},{132,2},{148,2}},
                            color={0,0,127}));
  connect(ETSCon.reqHea, ambCon.reqHea) annotation (Line(points={{63,-16.9},{70,
          -16.9},{70,51.8},{85,51.8}}, color={255,0,255}));
  connect(ETSCon.ValHea, ambCon.valHea) annotation (Line(points={{63,-18.7},{72,
          -18.7},{72,49.6},{85,49.6}}, color={255,0,255}));
  connect(ETSCon.ValCoo, ambCon.valCoo) annotation (Line(points={{63,-20.5},{74,
          -20.5},{74,47.2},{85,47.2}}, color={255,0,255}));
  annotation (Icon(coordinateSystem(preserveAspectRatio=false), graphics={
        Ellipse(lineColor = {75,138,73},
                fillColor={255,255,255},
                fillPattern = FillPattern.Solid,
                extent={{-98,-100},{98,98}}),
        Polygon(lineColor = {0,0,255},
                fillColor = {75,138,73},
                pattern = LinePattern.None,
                fillPattern = FillPattern.Solid,
                points={{-30,64},{70,4},{-30,-56},{-30,64}})}),  Diagram(coordinateSystem(preserveAspectRatio=false, extent={{-100,
            -140},{220,80}}),
        graphics={Line(points={{-22,22}}, color={28,108,200})}),
    experiment(StopTime=5000, Tolerance=1e-06),
    __Dymola_Commands(
  file="modelica://Buildings/Resources/Scripts/Dymola/Applications/DHC/EnergyTransferStations/Control/HeatpumpControllerHeaOnly.mos"
        "Simulate and plot"),
         experiment(Tolerance=1e-6, StopTime=14400));
end BorfieldSystem;
