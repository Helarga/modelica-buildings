within Buildings.Applications.DHC.EnergyTransferStations.Examples;
model ETSExample "ETS example first try"
  package Medium = Buildings.Media.Water "Medium model";

  parameter Modelica.SIunits.MassFlowRate mSou_flow_nominal=per.mSou_flow_nominal
   "Source heat exchanger nominal mass flow rate";
  parameter Modelica.SIunits.MassFlowRate mLoa_flow_nominal=per.mLoa_flow_nominal
   "Load heat exchanger nominal mass flow rate";
  parameter Real scaling_factor=1
   "Scaling factor for heatpump capacity";

  Buildings.Controls.OBC.CDL.Continuous.Sources.Sine TSetHeaMax(
    amplitude=5,
    freqHz=1/3000,
    offset=30 + 273.15)
    annotation (Placement(transformation(extent={{-120,120},{-100,140}})));
  Fluid.Sources.MassFlowSource_T cooPum(
    use_m_flow_in=false,
    m_flow=mLoa_flow_nominal,
    use_T_in=true,
    redeclare package Medium = Medium) "Cooling load Side water pump"
   annotation (Placement(transformation(
      extent={{10,-10},{-10,10}},
      rotation=180,
      origin={-70,-8})));
  Modelica.Blocks.Sources.BooleanConstant heaMod(k=false) "Step control"
    annotation (Placement(transformation(extent={{-176,-40},{-156,-20}})));
  Modelica.Blocks.Sources.BooleanConstant CooMod "Step control"
    annotation (Placement(transformation(extent={{-194,-74},{-174,-54}})));
  Fluid.FixedResistances.PressureDrop resDis(
    redeclare package Medium = Medium,
    m_flow_nominal=mSou_flow_nominal,
    dp_nominal=6000) "Flow resistance"
    annotation (Placement(transformation(extent={{12,-120},{32,-100}})));
  Fluid.Sensors.TemperatureTwoPort TSouLvg(
    redeclare final package Medium = Medium,
    allowFlowReversal=false,
    m_flow_nominal=mSou_flow_nominal,
    tau=30) "Source heat exchanger side leaving water temperature"
    annotation (Placement(transformation(extent={{10,-10},{-10,10}},
      rotation=90,
      origin={-166,12})));
  Modelica.Blocks.Sources.Constant TBorMaxEnt(k=7 + 273.15)
    "Cooling setpoint temperature"
    annotation (Placement(transformation(extent={{-120,0},{-100,20}})));
  Modelica.Fluid.Sources.FixedBoundary heaLoa(redeclare package Medium = Medium,
      nPorts=1) "Volume for the heating load"
   annotation (Placement(transformation(extent={{80,-58},{60,-38}})));
  Fluid.Sources.MassFlowSource_T heaPum(
    m_flow=mSec_flow_nominal,
    use_T_in=true,
    redeclare package Medium = Medium,
    nPorts=1) "heating load water pump"
   annotation (Placement(transformation(
      extent={{-10,-10},{10,10}},
      rotation=180,
      origin={70,-8})));
  Modelica.Blocks.Sources.Constant TMaxEvaEnt(k=55 + 273.15)
    "Maximum heating set point temperature"
    annotation (Placement(transformation(extent={{-120,40},{-100,60}})));
  Modelica.Blocks.Sources.Constant TMinConEnt(k=25)
    "Minimum heating set point temperature"
    annotation (Placement(transformation(extent={{-120,78},{-100,98}})));
  Substation ETS(heaPumDat=
        Fluid.HeatPumps.Data.EquationFitReversible.Trane_Axiom_EXW240())
    "Energy transfer station for the 5th generation of district heating and cooling"
    annotation (Placement(transformation(extent={{-10,-24},{10,-4}})));
  Fluid.FixedResistances.PressureDrop resHea(
    redeclare package Medium = Medium,
    m_flow_nominal=mCon_flow_nominal,
    dp_nominal=6000) "Flow resistance"
    annotation (Placement(transformation(extent={{30,-58},{50,-38}})));
  Modelica.Fluid.Sources.FixedBoundary cooLoa(redeclare package Medium = Medium,
      nPorts=1) "Volume for the cooling load"
    annotation (Placement(transformation(extent={{-80,-56},{-60,-36}})));
  Fluid.FixedResistances.PressureDrop resCoo(
    redeclare package Medium = Medium,
    m_flow_nominal=mEva_flow_nominal,
    dp_nominal=6000) "Flow resistance"
    annotation (Placement(transformation(extent={{-30,-58},{-50,-38}})));
  Modelica.Fluid.Sources.FixedBoundary disLoa(redeclare package Medium = Medium,
      nPorts=2) "Volume for the district system"
    annotation (Placement(transformation(extent={{60,-120},{40,-100}})));
  Fluid.Sources.MassFlowSource_T disPum(
    m_flow=mDis_flow_nominal,
    use_T_in=true,
    redeclare package Medium = Medium,
    nPorts=1) "District system water pump" annotation (Placement(transformation(
        extent={{10,-10},{-10,10}},
        rotation=180,
        origin={-30,-88})));
    final parameter Fluid.Geothermal.Borefields.Data.Filling.Bentonite filDat(kFil=2.1)
      annotation (Placement(transformation(extent={{110,132},{130,152}})));
    final parameter Fluid.Geothermal.Borefields.Data.Soil.SandStone soiDat(
    kSoi=2.42,
    dSoi=1920,
    cSoi=1210)
      "Soil data"
       annotation (Placement(transformation(extent={{110,108},{130,128}})));
    final parameter Fluid.Geothermal.Borefields.Data.Configuration.Template conDat(
    borCon=Buildings.Fluid.Geothermal.Borefields.Types.BoreholeConfiguration.SingleUTube,

    use_Rb=false,
    mBor_flow_nominal=mGeo_flow_nominal/(nXBorHol*nYBorHol),
    mBorFie_flow_nominal=mGeo_flow_nominal,
    hBor=244,
    dBor=1,
    rBor=0.2,
    rTub=rTub,
    kTub=0.5,
    eTub=0.002,
    cooBor={{dBorHol*mod((i - 1), nXBorHol),dBorHol*floor((i - 1)/nXBorHol)}
        for i in 1:nBorHol},
    xC=0.075,
    dp_nominal=dpBorFie_nominal)
      "Borefield configuration"
      annotation (Placement(transformation(extent={{110,84},{130,104}})));
    final parameter Fluid.Geothermal.Borefields.Data.Borefield.Template borFieDat(
    filDat=filDat,
    soiDat=soiDat,
    conDat=conDat)
      "Borefield parameters"
      annotation (Placement(transformation(extent={{110,60},{130,80}})));
    parameter Fluid.HeatPumps.Data.EquationFitReversible.Generic heaPumDat
      "Performance data of the water to water heat pump"
      annotation (choicesAllMatching=true, Placement(
          transformation(extent={{110,36},{130,56}})));
equation
  connect(ETS.heaSup, heaPum.ports[1]) annotation (Line(points={{11,-18.8},{22,
          -18.8},{22,-8},{60,-8}}, color={0,127,255}));
  connect(ETS.heaRet, resHea.port_a) annotation (Line(points={{11,-21.2},{22,
          -21.2},{22,-48},{30,-48}}, color={0,127,255}));
  connect(resHea.port_b, heaLoa.ports[1])
    annotation (Line(points={{50,-48},{60,-48}}, color={0,127,255}));
  connect(cooPum.ports[1], ETS.cooSup) annotation (Line(points={{-60,-8},{-22,
          -8},{-22,-18.8},{-11,-18.8}}, color={0,127,255}));
  connect(ETS.cooRet, resCoo.port_a) annotation (Line(points={{-11,-21.2},{-22,
          -21.2},{-22,-48},{-30,-48}}, color={0,127,255}));
  connect(resCoo.port_b, cooLoa.ports[1]) annotation (Line(points={{-50,-48},{
          -56,-48},{-56,-46},{-60,-46}}, color={0,127,255}));
  connect(disPum.ports[1], ETS.disWatIn) annotation (Line(points={{-20,-88},{
          -1.2,-88},{-1.2,-25}}, color={0,127,255}));
  connect(disLoa.ports[1], resDis.port_b) annotation (Line(points={{40,-108},{
          40,-110},{32,-110}}, color={0,127,255}));
  connect(resDis.port_a, ETS.disWatOut) annotation (Line(points={{12,-110},{1.2,
          -110},{1.2,-25}}, color={0,127,255}));
  connect(TSetHeaMax.y, ETS.TSetHeaMax) annotation (Line(points={{-98,130},{-14,
          130},{-14,-7.8},{-10.8,-7.8}}, color={0,0,127}));
  connect(TMinConEnt.y, ETS.TMinConEnt) annotation (Line(points={{-99,88},{-16,
          88},{-16,-10.8},{-10.8,-10.8}}, color={0,0,127}));
  connect(TMaxEvaEnt.y, ETS.TMaxEvaEnt) annotation (Line(points={{-99,50},{-20,
          50},{-20,-13.6},{-10.8,-13.6}}, color={0,0,127}));
  connect(TBorMaxEnt.y, ETS.TMaxBorEnt) annotation (Line(points={{-99,10},{-22,
          10},{-22,-16},{-10.8,-16}}, color={0,0,127}));
  annotation (Icon(coordinateSystem(preserveAspectRatio=false, extent={{-100,
            -100},{100,100}}),                                  graphics={
        Ellipse(lineColor = {75,138,73},
                fillColor={255,255,255},
                fillPattern = FillPattern.Solid,
                extent={{-98,-100},{98,98}}),
        Polygon(lineColor = {0,0,255},
                fillColor = {75,138,73},
                pattern = LinePattern.None,
                fillPattern = FillPattern.Solid,
                points={{-30,64},{70,4},{-30,-56},{-30,64}})}),  Diagram(coordinateSystem(preserveAspectRatio=false, extent={{-140,
            -140},{140,160}}),
        graphics={Line(points={{-22,22}}, color={28,108,200})}),
    experiment(StopTime=14400),
    __Dymola_Commands(
  file="modelica://Buildings/Resources/Scripts/Dymola/Applications/DHC/EnergyTransferStations/Control/HeatpumpController.mos"
        "Simulate and plot"),
         experiment(Tolerance=1e-6, StopTime=14400));
end ETSExample;
