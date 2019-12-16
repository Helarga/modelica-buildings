within Buildings.Applications.DHC.EnergyTransferStations;
model Substation
  "5th generation of district heating and cooling plant"
   package Medium = Buildings.Media.Water "Medium model";

  //--------------------------WSHP system------------------------

    parameter Modelica.SIunits.MassFlowRate mCon_flow_nominal=heaPumDat.hea.mLoa_flow
     "Condenser nominal water flow rate"
      annotation (Dialog(tab="WSHP system"));
    parameter Modelica.SIunits.MassFlowRate mEva_flow_nominal=heaPumDat.hea.mSou_flow
     "Evaporator nominal water flow rate"
      annotation (Dialog(tab="WSHP system"));
    parameter Modelica.SIunits.MassFlowRate mSecHea_flow_nominal
   "Condenser nominal water flow rate"
    annotation (Dialog(tab="WSHP system"));
    parameter Modelica.SIunits.MassFlowRate mSecCoo_flow_nominal
     "Evaporator nominal water flow rate"
      annotation (Dialog(tab="WSHP system"));
    parameter Modelica.SIunits.PressureDifference dpCon_nominal= heaPumDat.dpHeaLoa_nominal
      "Pressure difference accross the condenser"
      annotation (Dialog(tab="WSHP system"));
    parameter Modelica.SIunits.PressureDifference dpEva_nominal= heaPumDat.dpHeaSou_nominal
      "Pressure difference accross the evaporator"
      annotation (Dialog(tab="WSHP system"));
    parameter Modelica.SIunits.Temperature TConEnt_nominal=35+273.15
    "Nominal heating supply water temperature";
    parameter Modelica.SIunits.Temperature TConLvg_nominal=40+273.15
    "Nominal heating supply water temperature";
    parameter Modelica.SIunits.Temperature TEvaEnt_nominal=12+273.15
    "Nominal heating supply water temperature";
    parameter Modelica.SIunits.Temperature TEvaLvg_nominal=7+273.15
    "Nominal heating supply water temperature";
    parameter Real scaling_factor
   "Scaling factor for heat pump capacity";


  Fluid.HeatPumps.EquationFitReversible heaPum(
      allowFlowReversal1=false,
      allowFlowReversal2=false,
    energyDynamics=Modelica.Fluid.Types.Dynamics.FixedInitial,
      per = heaPumDat,
      redeclare package Medium1 = Medium,
      redeclare package Medium2 = Medium,
      scaling_factor=scaling_factor)
    "Reversible water to water heat pump"
      annotation (Placement(transformation(extent={{-30,116},{-10,136}})));

  Buildings.Fluid.Movers.SpeedControlled_y pumCon(
    redeclare package Medium = Medium,
    energyDynamics=Modelica.Fluid.Types.Dynamics.SteadyState,
    addPowerToMedium=false,
    show_T=show_T,
    per(pressure(dp={2*dpCon_nominal,0}, V_flow={0,2*mCon_flow_nominal/1000})),
    allowFlowReversal=false,
    use_inputFilter=false,
    riseTime=10)
    "Condenser variable speed pump-primary circuit"
    annotation (Placement(transformation(extent={{0,122},{20,142}})));
   Buildings.Fluid.Movers.SpeedControlled_y pumEva(
      redeclare package Medium = Medium,
      energyDynamics=Modelica.Fluid.Types.Dynamics.SteadyState,
      addPowerToMedium=false,
      show_T=show_T,
    per(pressure(dp={2*dpEva_nominal,0}, V_flow={0,2*mEva_flow_nominal/1000})),
      allowFlowReversal=false,
      use_inputFilter=false,
      riseTime=10)
    "Evaporator variable speed pump-primary circuit"
    annotation (Placement(transformation(
          extent={{-10,-10},{10,10}},
          rotation=0,
          origin={-76,130})));

 //-----------------------------------------------------------------
 final parameter Modelica.SIunits.MassFlowRate m_flow_nominal=
    max(mSecHea_flow_nominal,mSecCoo_flow_nominal)
    "Nominal mass flow rate";
  //---------------------------------Buffer tanks-------------------
   final parameter Modelica.SIunits.Volume VTan = 5*60*mCon_flow_nominal/1000
      "Tank volume, ensure at least 5 minutes buffer flow"
      annotation (Dialog(tab="Water Buffer Tank"));
   final parameter Modelica.SIunits.Length hTan = 5
      "Height of tank (without insulation)"
      annotation (Dialog(tab="Water Buffer Tank"));
   final parameter Modelica.SIunits.Length dIns = 0.3
      "Thickness of insulation"
      annotation (Dialog(tab="Water Buffer Tank"));
   final parameter Integer nSegTan=10   "Number of volume segments"
      annotation (Dialog(tab="Water Buffer Tank"));

    BaseClasses.StratifiedTank hotBufTan(
      redeclare package Medium = Medium,
      VTan=VTan,
      hTan=hTan,
      dIns=dIns,
      nSeg=nSegTan,
      show_T=show_T,
      energyDynamics=fixedEnergyDynamics,
      m_flow_nominal=mCon_flow_nominal,
    T_start=313.15)
      "Hot buffer tank"
      annotation (Placement(transformation(extent={{156,30},{180,54}})));
    BaseClasses.StratifiedTank colBufTan(
      redeclare package Medium = Medium,
      VTan=VTan,
      hTan=hTan,
      dIns=dIns,
      nSeg=nSegTan,
      show_T=show_T,
      m_flow_nominal=mEva_flow_nominal)
      "Cold Buffer tank"
      annotation (Placement(transformation(extent={{-234,48},{-210,72}})));
  //-------------------------------Design Parameters----------------
    parameter Modelica.SIunits.Temperature THeaWatSup_nominal=40+273.15
      "Nominal heating supply water temperature"
      annotation (Dialog(group="Design parameter"));
    parameter Modelica.SIunits.Temperature THeaWatRet_nominal=35+273.15
      "Nominal heating Return water temperature"
      annotation (Dialog(group="Design parameter"));
    parameter Modelica.SIunits.Temperature TCooWatSup_max=15+273.15
      "Maximum cooling water supply temperature"
      annotation (Dialog(group="Design parameter"));
    parameter Modelica.SIunits.Temperature TCooWatSup_min=4+273.15
      "Minimum cooling water supply temperature"
      annotation (Dialog(group="Design parameter"));
    parameter Modelica.SIunits.TemperatureDifference dTCooWat=4
      "Cooling water supply and return temperature difference"
      annotation (Dialog(group="Design parameter"));
    parameter Modelica.SIunits.TemperatureDifference dTHeaPum = 2
      "Temperature difference in and out of heat pump";

 //----------------------------Borefield system----------------------------------
   parameter Modelica.SIunits.TemperatureDifference dTGeo
    "Temperature difference in and out of borefield";
   parameter Modelica.SIunits.MassFlowRate mGeo_flow_nominal= m_flow_nominal*dTHeaPum/dTGeo
    "Borefiled nominal water flow rate"
      annotation (Dialog(tab="Borefield"));
   parameter Modelica.SIunits.Length xBorFie
    "Borefield length"
      annotation (Dialog(tab="Borefield"));
    parameter Modelica.SIunits.Length yBorFie
     "Borefield width"
      annotation (Dialog(tab="Borefield"));
    parameter Modelica.SIunits.Length dBorHol = 5
     "Distance between two boreholes"
      annotation (Dialog(tab="Borefield"));
    parameter Modelica.SIunits.Pressure dpBorFie_nominal
      "Pressure losses for the entire borefield"
      annotation (Dialog(tab="Borefield"));
    parameter Integer nXBorHol = integer((xBorFie+dBorHol)/dBorHol)
      "Number of boreholes in x-direction"
      annotation(Dialog(tab="Borefield"));
    parameter Integer nYBorHol = integer((yBorFie+dBorHol)/dBorHol)
      "Number of boreholes in y-direction"
      annotation(Dialog(tab="Borefield"));
    parameter Integer nBorHol = nXBorHol*nYBorHol
     "Number of boreholes"
      annotation(Dialog(tab="Borefield"));
    parameter Modelica.SIunits.Radius rTub =  0.05
     "Outer radius of the tubes"
      annotation(Dialog(tab="Borefield"));
   /*parameter Boolean allowFlowReversal = false
    "= true to allow flow reversal, false restricts to design direction (port_a -> port_b)"
    annotation(Dialog(tab="Assumptions"), Evaluate=true);*/
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
          origin={-70,-190})));
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
        rotation=90,  origin={-70,-150})));
  //---------------------------DistrictHeatExchanger----------
    parameter Modelica.SIunits.MassFlowRate mHex_flow_nominal= m_flow_nominal*dTHeaPum/dTHex
      "District heat exhanger nominal water flow rate"
      annotation (Dialog(tab="DistrictHeatExchanger"));
    parameter Real eps_nominal=0.71
      "Heat exchanger effectiveness"
      annotation (Dialog(tab="DistrictHeatExchanger"));
    parameter Modelica.SIunits.PressureDifference dpHex_nominal(displayUnit="Pa")=50000
      "Pressure difference across heat exchanger"
      annotation (Dialog(tab="DistrictHeatExchanger"));
    parameter Modelica.SIunits.TemperatureDifference dTHex
      "Temperature difference in and out of substation heat exchanger";

    BaseClasses.WaterWaterHeatExchanger hex(
      redeclare package Medium1 = Medium,
      redeclare package Medium2 = Medium,
      dp1_nominal=dpHex_nominal,
      eps_nominal=eps_nominal,
      dp2_nominal=dpHex_nominal,
      m1_flow_nominal=mHex_flow_nominal,
      m2_flow_nominal=mHex_flow_nominal)
      "Heat exchanger"
       annotation (Placement(
          transformation(
          extent={{10,-10},{-10,10}},
          rotation=90,
          origin={110,-170})));
    Fluid.Movers.FlowControlled_m_flow pumHexDis(
      redeclare package Medium = Medium,
      energyDynamics=Modelica.Fluid.Types.Dynamics.SteadyState,
      m_flow_nominal=mHex_flow_nominal,
      addPowerToMedium=false,
      show_T=show_T,
    per(pressure(dp={2*dpHex_nominal,0}, V_flow={0,2*mHex_flow_nominal/1000})),
      use_inputFilter=false,
      riseTime=10)
      "Pump (or valve) that forces the flow rate to be set to the control signal"
       annotation (
        Placement(transformation(extent={{-10,10},{10,-10}},rotation=270,
                                            origin={110,-110})));
//----------------------------------Generanl---------------------------------------
    parameter Modelica.Fluid.Types.Dynamics fixedEnergyDynamics=Modelica.Fluid.Types.Dynamics.FixedInitial
      "Formulation of energy balance for mixing volume at inlet and outlet"
      annotation (Dialog(group="Dynamics"));
    parameter Modelica.SIunits.PressureDifference dp_nominal(displayUnit="Pa")=1000
    "Pressure difference at nominal flow rate"
      annotation (Dialog(tab="Design Parameter"));
    parameter Boolean show_T=true
      "= true, if actual temperature at port is computed"
      annotation (Dialog(group="Advanced"));

 //-----------------------------Controllers------------------------------------

    Control.ETSController ETSCon(THys=2)
      "Overall control of the ETS cold and hot sides."
        annotation (Placement(transformation(extent={{-198,194},{-178,214}})));
    Control.HeatPumpController heaPumCon
      "Control of the heatpump model and associated three way valves"
        annotation (Placement(transformation(extent={{-120,200},{-100,220}})));
    Control.EvaporatorCondenserPumpsController pumPrimCon(
        mCon_flow_nominal=mCon_flow_nominal,
        mEva_flow_nominal=mEva_flow_nominal)
      "Control of the primary circuit pumps"
        annotation (Placement(transformation(extent={{-120,142},{-100,162}})));
    Control.AmbientCircuitSid  ambCon(
        dTGeo=dTGeo,
        dTHex=dTHex)
      "control of the ambient hydraulic circuit"
        annotation (Placement(transformation(extent={{-144,-80},{-124,-60}})));
    Buildings.Controls.OBC.CDL.Continuous.Gain gaiBor(k=mGeo_flow_nominal)
      "Gain for mass flow rate of borefield"
        annotation (Placement(transformation(extent={{-110,-160},{-90,-140}})));
    Buildings.Controls.OBC.CDL.Continuous.Gain gaiMDisHex(k=mHex_flow_nominal)
      "Gain for mass flow of heat exchanger"
        annotation (Placement(transformation(extent={{40,-262},{60,-242}})));

 //-----------------------------Sensors------------------------------------
    Buildings.Fluid.Sensors.TemperatureTwoPort TConLvg(
      redeclare final package Medium = Medium,
      allowFlowReversal=false,
      m_flow_nominal=mCon_flow_nominal,
      tau=0)
      "Condenser leaving water temperature"
      annotation (Placement(
          transformation(
          extent={{10,10},{-10,-10}},
          rotation=180,
          origin={68,132})));

    Buildings.Fluid.Sensors.TemperatureTwoPort TConEnt(
      redeclare final package Medium = Medium,
           allowFlowReversal=false,
      m_flow_nominal=mCon_flow_nominal,
      tau=0)
      "Condenser entering water temperature"
      annotation (Placement(transformation(extent={{0,10},{-20,30}})));

    Buildings.Fluid.Sensors.TemperatureTwoPort TEvaEnt(
      redeclare final package Medium = Medium,
       allowFlowReversal=false,
      m_flow_nominal=mEva_flow_nominal,
      tau=0)
      "Evaporator entering water temperature"
      annotation (Placement(
        transformation(
        extent={{10,10},{-10,-10}},
        rotation=180,
        origin={-20,96})));

    Buildings.Fluid.Sensors.TemperatureTwoPort TEvaLvg(
      redeclare final package Medium = Medium,
      allowFlowReversal=false,
      m_flow_nominal=mEva_flow_nominal,
      tau=30)
      "Evaporator leaving water temperature"
      annotation (Placement(transformation(extent={{-68,10},{-88,30}})));

    Modelica.Thermal.HeatTransfer.Sensors.TemperatureSensor topCooTan
      "Cold tank top temperature"
      annotation (Placement(transformation(extent={{-224,84},{-244,104}})));
    Modelica.Thermal.HeatTransfer.Sensors.TemperatureSensor botCooTan
      "Cold tank bottom temperature"
      annotation (Placement(transformation(extent={{-230,-10},{-250,10}})));
    Modelica.Thermal.HeatTransfer.Sensors.TemperatureSensor topHotTan
      "Hot tank top temperature"
      annotation (Placement(transformation(extent={{214,198},{194,218}})));
    Modelica.Thermal.HeatTransfer.Sensors.TemperatureSensor botHotTan
      "Hot tank bottom temperature"
      annotation (Placement(transformation(extent={{176,-42},{196,-22}})));
   Fluid.Sensors.TemperatureTwoPort TBorLvg(
    tau=0,
    redeclare final package Medium = Medium,
          allowFlowReversal=false,
    m_flow_nominal=mGeo_flow_nominal)
    "Borefield system leaving water temperature"
    annotation (Placement(transformation(
        extent={{-10,-10},{10,10}},
        rotation=90,
        origin={-30,-210})));

  Fluid.Sensors.TemperatureTwoPort TDisHexLvg(
    redeclare final package Medium = Medium,
          allowFlowReversal=false,
    tau=10,
    m_flow_nominal=mHex_flow_nominal)
     "District heat exchanger leaving water temperature"
     annotation (Placement(
        transformation(
        extent={{-10,-10},{10,10}},
        rotation=90,
        origin={22,-200})));


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
        origin={-70,-170})));

    Fluid.Sensors.TemperatureTwoPort disRetTem(
      tau=0,
      redeclare final package Medium = Medium,
              allowFlowReversal=false,
      m_flow_nominal=mHex_flow_nominal)
      "District system return water temperature"
       annotation (Placement(
          transformation(
          extent={{-10,-10},{10,10}},
          rotation=0,
          origin={230,-150})));

    Fluid.Sensors.TemperatureTwoPort disSupTem(
      tau=0,
      redeclare final package Medium = Medium,
              allowFlowReversal=false,
      m_flow_nominal=mHex_flow_nominal)
      "District system supply water temperature"
       annotation (Placement(
          transformation(
          extent={{10,-10},{-10,10}},
          rotation=0,
          origin={230,-192})));

   Fluid.Sensors.TemperatureTwoPort TDisHex(
      redeclare final package Medium = Medium,
              allowFlowReversal=false,
      tau=10,
      m_flow_nominal=mHex_flow_nominal)
      "Entering water tmperature to the district heat exchanger"
       annotation (Placement(transformation(extent={{-10,10},{10,-10}},
                                            rotation=270,
                                            origin={110,-70})));
  Fluid.Sensors.MassFlowRate secCooFlo(redeclare package Medium = Medium)
    "Secondary circuit chilled water flow rate"
    annotation (Placement(transformation(extent={{-244,30},{-264,50}})));
  Fluid.Sensors.MassFlowRate secHeaFlo(redeclare package Medium = Medium)
    "Secondary circuit heating water flow rate"
    annotation (Placement(transformation(extent={{264,42},{284,62}})));
  Fluid.Sensors.MassFlowRate priCooFlo(redeclare package Medium =Medium)
    "Primary circuit evaporator side chilled water flow rate"
    annotation (Placement(transformation(extent={{-180,10},{-200,30}})));
  Fluid.Sensors.MassFlowRate priLoaFlo(redeclare package Medium = Medium)
    "Primary circuit load side heating water flow rate"
    annotation (Placement(transformation(extent={{126,50},{146,70}})));


 //-----------------------------Fluid Interfaces and headers------------------------------------
    Modelica.Fluid.Interfaces.FluidPort_a chiWatSup(
    h_outflow(start=Medium.h_default, nominal=Medium.h_default),
    redeclare final package Medium = Medium,
    p(start=Medium.p_default)) "Chilled water supply to the building."
    annotation (Placement(transformation(extent={{-314,30},{-294,50}}),
        iconTransformation(extent={{-120,-58},{-100,-38}})));
    Modelica.Fluid.Interfaces.FluidPort_b chiWatRet(
    h_outflow(start=Medium.h_default, nominal=Medium.h_default),
    redeclare final package Medium = Medium,
    p(start=Medium.p_default)) "Chilled water return from the building."
    annotation (Placement(transformation(extent={{-294,50},{-314,70}}),
        iconTransformation(extent={{-100,-82},{-120,-62}})));
  BaseClasses.HydraulicHeader heaSupHed(
    redeclare package Medium = Medium,
    m_flow_nominal=mCon_flow_nominal,
    nPorts_b=2,
    nPorts_a=1) "Heating supply water header" annotation (Placement(transformation(extent={{92,50},{112,70}})));
  BaseClasses.HydraulicHeader heaRetHed(
    redeclare package Medium = Medium,
    m_flow_nominal=mCon_flow_nominal,
    nPorts_a=2,
    nPorts_b=1) "Heating return water header" annotation (Placement(transformation(extent={{122,30},{102,10}})));
    Modelica.Fluid.Interfaces.FluidPort_a hotWatSup(
    h_outflow(start=Medium.h_default, nominal=Medium.h_default),
    redeclare final package Medium = Medium,
    p(start=Medium.p_default)) "Hot water supply to the building."
      annotation (Placement(transformation(extent={{292,42},{312,62}}), iconTransformation(
          extent={{100,-58},{120,-38}})));
    Modelica.Fluid.Interfaces.FluidPort_b hotWatRet(
    h_outflow(start=Medium.h_default, nominal=Medium.h_default),
    redeclare final package Medium = Medium,
    p(start=Medium.p_default)) "Hot water return from the building."
    annotation (Placement(transformation(extent={{312,22},{292,42}}),
        iconTransformation(extent={{120,-82},{100,-62}})));
     BaseClasses.HydraulicHeader cooRetHed(
      redeclare package Medium = Medium,
      m_flow_nominal=mEva_flow_nominal,
      nPorts_a=2,
      nPorts_b=1) "Return chilled water header.  " annotation (Placement(transformation(extent={{-148,70},
            {-128,50}})));
    BaseClasses.HydraulicHeader cooSupHed(
      redeclare package Medium = Medium,
      m_flow_nominal=mEva_flow_nominal,
      nPorts_a=1,
      nPorts_b=2) "Supply chilled water header. " annotation (Placement(transformation(extent={{-108,10},{-128,30}})));

     BaseClasses.HydraulicHeader ambRetHed(
      redeclare package Medium = Medium,
      m_flow_nominal=mHex_flow_nominal + mGeo_flow_nominal,
      nPorts_a=2,
      nPorts_b=2) "ambient circuit return header" annotation (Placement(transformation(extent={{-30,-58},{-50,-38}})));

     BaseClasses.HydraulicHeader ambHedSup(
      redeclare package Medium = Medium,
      m_flow_nominal=mHex_flow_nominal + mGeo_flow_nominal,
      nPorts_a=2,
      nPorts_b=2) "Ambient circuit supply header" annotation (Placement(transformation(extent={{0,-120},{20,-142}})));
     Modelica.Fluid.Interfaces.FluidPort_a disWatSup(
    p(start=Medium.p_default),
    redeclare final package Medium = Medium,
    h_outflow(start=Medium.h_default, nominal=Medium.h_default))
    "District water supply port." annotation (Placement(transformation(extent={
            {290,-202},{310,-182}}), iconTransformation(extent={{-22,-120},{-2,
            -100}})));
     Modelica.Fluid.Interfaces.FluidPort_b disWatRet(
      p(start=Medium.p_default),
      redeclare final package Medium = Medium,
      h_outflow(start=Medium.h_default, nominal=Medium.h_default))
    "District water return port."
      annotation (Placement(transformation(extent={{310,-160},{290,-140}}),
          iconTransformation(extent={{22,-120},{2,-100}})));

    Fluid.FixedResistances.Junction splVal1(
      final dp_nominal = {0, 0, 0},
      from_dp=false,
      tau=1,
    m_flow_nominal={mGeo_flow_nominal,-mGeo_flow_nominal,-mGeo_flow_nominal},
      redeclare package Medium = Medium,
      energyDynamics=Modelica.Fluid.Types.Dynamics.SteadyState)
      "Flow splitter" annotation (Placement(transformation(
          extent={{-10,10},{10,-10}},
          rotation=90,
          origin={-30,-170})));
    Fluid.FixedResistances.Junction splVal2(
    final dp_nominal=200*{1,-1,-1},
      from_dp=false,
      tau=1,
    m_flow_nominal={mEva_flow_nominal,-mEva_flow_nominal,-mEva_flow_nominal},
      redeclare package Medium = Medium,
      energyDynamics=Modelica.Fluid.Types.Dynamics.SteadyState)
      "Flow splitter" annotation (Placement(transformation(
          extent={{-10,-10},{10,10}},
          rotation=270,
          origin={-60,78})));
    Fluid.FixedResistances.Junction splVal3(
      final dp_nominal = {0, 0, 0},
      from_dp=false,
      tau=1,
    m_flow_nominal={mCon_flow_nominal,-mCon_flow_nominal,-mCon_flow_nominal},
      redeclare package Medium = Medium,
      energyDynamics=Modelica.Fluid.Types.Dynamics.SteadyState)
      "Flow splitter" annotation (Placement(transformation(
          extent={{-10,-10},{10,10}},
          rotation=0,
          origin={42,132})));

 //-----------------------------Valves------------------------------------
    Fluid.Actuators.Valves.TwoWayLinear valSupHea(
      redeclare final package Medium = Medium,
      use_inputFilter=false,
      dpFixed_nominal=0,
      show_T=true,
      dpValve_nominal=dp_nominal,
      riseTime=10,
      l=1e-8,
      m_flow_nominal=mGeo_flow_nominal + mHex_flow_nominal)
    "Two way modulating valve."
      annotation (Placement(transformation(extent={{18,-30},{-2,-10}})));
    Fluid.Actuators.Valves.TwoWayLinear valSupCoo(
      redeclare final package Medium = Medium,
      use_inputFilter=false,
      dpFixed_nominal=0,
      show_T=true,
      dpValve_nominal=dp_nominal,
      riseTime=10,
      l=1e-8,
      m_flow_nominal=mGeo_flow_nominal + mHex_flow_nominal)
      "Two way modulating valve"
      annotation (Placement(transformation(extent={{-94,-30},{-74,-10}})));
    Fluid.Actuators.Valves.ThreeWayEqualPercentageLinear valBor(
        redeclare package Medium = Medium,
        energyDynamics=Modelica.Fluid.Types.Dynamics.FixedInitial,
        m_flow_nominal=mGeo_flow_nominal,
        l={0.01,0.01},
        dpValve_nominal=6000)
    "Three way valve modulated to control the entering water temperature to the borefield system."
     annotation (Placement(transformation(extent={{10,-10},{-10,10}},
                                          rotation=90,
                                          origin={-70,-110})));

    parameter Modelica.SIunits.MassFlowRate valEva_flow_nominal= 1.2
    "Evaporator three way valve nominal mass flow rate";
      //heaPumDat.coo.Q_flow/4200/(TEvaEnt_nominal-TEvaLvg_nominal)

    Fluid.Actuators.Valves.ThreeWayEqualPercentageLinear valEva(
      redeclare package Medium = Medium,
      energyDynamics=Modelica.Fluid.Types.Dynamics.FixedInitial,
      m_flow_nominal=valEva_flow_nominal,
      l={0.01,0.01},
    dpValve_nominal=6000)
    "Three way valve modulated to control the entering water temperature to the evaporator."
     annotation (Placement(transformation(extent={{10,10},{-10,-10}},
                                          rotation=270,
                                          origin={-94,78})));

    parameter Modelica.SIunits.MassFlowRate valCon_flow_nominal=1.2
    "Condenser three way valve nominal mass flow rate";
      //heaPumDat.hea.Q_flow/4200/(TConLvg_nominal-TConEnt_nominal)

    Fluid.Actuators.Valves.ThreeWayEqualPercentageLinear valCon(
    redeclare package Medium = Medium,
    energyDynamics=Modelica.Fluid.Types.Dynamics.FixedInitial,
    m_flow_nominal=mCon_flow_nominal,
    l={0.01,0.01},
    dpValve_nominal=6000)
    "Three way valve modulated to control the entering water temperature to the condenser."
    annotation (Placement(transformation(
        extent={{-10,-10},{10,10}},
        rotation=90,
        origin={-34,52})));

  //----------------------------Performance data------------------------------------
    final parameter Fluid.Geothermal.Borefields.Data.Filling.Bentonite filDat(kFil=2.1)
      annotation (Placement(transformation(extent={{-292,-184},{-272,-164}})));
    final parameter Fluid.Geothermal.Borefields.Data.Soil.SandStone soiDat(
      kSoi=2.42,
      dSoi=1920,
      cSoi=1210)
      "Soil data"
      annotation (Placement(transformation(extent={{-292,-208},{-272,-188}})));
    final parameter Buildings.Fluid.Geothermal.Borefields.Data.Configuration.Template conDat(
      final borCon=Buildings.Fluid.Geothermal.Borefields.Types.BoreholeConfiguration.SingleUTube,
      final use_Rb=false,
      final mBor_flow_nominal=mGeo_flow_nominal/(nXBorHol*nYBorHol),
      final mBorFie_flow_nominal=mGeo_flow_nominal,
      final hBor=244,
      final dBor=1,
      final rBor=0.2,
      final rTub=rTub,
      final kTub=0.5,
      final eTub=0.002,
      final cooBor={{dBorHol*mod((i - 1), nXBorHol),dBorHol*floor((i - 1)/
          nXBorHol)} for i in 1:nBorHol},
      final xC=0.075,
      final dp_nominal=dpBorFie_nominal)
    "Borefield configuration"
      annotation (Placement(transformation(extent={{-292,-232},{-272,-212}})));
    final parameter Fluid.Geothermal.Borefields.Data.Borefield.Template borFieDat(
       final filDat=filDat,
       final soiDat=soiDat,
       final conDat=conDat)
      "Borefield parameters"
      annotation (Placement(transformation(extent={{-292,-256},{-272,-236}})));
    parameter Fluid.HeatPumps.Data.EquationFitReversible.Generic heaPumDat
      "Performance data of the water to water heat pump"
      annotation (choicesAllMatching=true, Placement(
          transformation(extent={{-292,-282},{-272,-262}})));

  //---------------------------I/O interfaces-----------------------------------

    Modelica.Blocks.Interfaces.RealInput TSetHeaMax(final unit="K",displayUnit="degC")
    "Maximum heating setpoint temperature"
    annotation (Placement(transformation(extent={{-320,218},{-300,238}}),
                    iconTransformation(extent={{-116,66},{-100,82}})));


    Modelica.Blocks.Interfaces.RealInput TMaxBorEnt(final unit="K",displayUnit="degC")
      "Maximum allowed enetring water temperature to the borefiled holes."
         annotation (Placement(transformation(extent={{-320,-84},{-300,-64}}),
                               iconTransformation(extent={{-116,-28},{-100,-12}})));

    Modelica.Blocks.Interfaces.RealInput TMinConEnt(final unit="K",displayUnit="degC")
      "Minimum condenser entering water temperature"
         annotation (Placement(transformation(extent={{-320,184},{-300,204}}),
                     iconTransformation(extent={{-116,50},{-100,66}})));
    Modelica.Blocks.Interfaces.RealInput TMaxEvaEnt(final unit="K",displayUnit="degC")
    "Maximum evaporator entering water temperature"
        annotation (
      Placement(transformation(extent={{-320,170},{-300,190}}), iconTransformation(extent={{-116,-4},
            {-100,12}})));

  /*
  //-------------------------SolarCollector------------------------
    parameter Integer nSeg(min=3)
     "Number of segments used to discretize the collector model"
      annotation (Dialog(tab="SolarCollector"));
    parameter Modelica.SIunits.Angle lat
     "Latitude"
      annotation (Dialog(tab="SolarCollector"));
    parameter Modelica.SIunits.Angle azi
     "Surface azimuth (0 for south-facing; -90 degree for east-facing; +90 degree for west facing"
      annotation (Dialog(tab="SolarCollector"));
    parameter Modelica.SIunits.Angle til
     "Surface tilt (0 for horizontally mounted collector)"
      annotation (Dialog(tab="SolarCollector"));
    parameter Real rho
     "Ground reflectance"
      annotation (Dialog(tab="SolarCollector"));
    parameter Fluid.SolarCollectors.Types.NumberSelection nColType=Buildings.Fluid.SolarCollectors.Types.NumberSelection.Number
     "Selection of area specification format"
      annotation (Dialog(tab="SolarCollector"));
    parameter Fluid.SolarCollectors.Types.SystemConfiguration sysConfig=Buildings.Fluid.SolarCollectors.Types.SystemConfiguration.Series
     "Selection of system configuration"
      annotation (Dialog(tab="SolarCollector"));
    parameter Real shaCoe
      "Shading coefficient. 0.0: no shading, 1.0: full shading"
      annotation (Dialog(tab="SolarCollector"));
    parameter Integer nPanels
     "Desired number of panels in the simulation"
      annotation (Dialog(tab="SolarCollector"));
    parameter Modelica.SIunits.HeatCapacity C=385*solColDat.mDry
     "Heat capacity of solar collector without fluid (default: cp_copper*mDry*nPanels)"
      annotation (Dialog(tab="SolarCollector"));
    parameter Modelica.SIunits.MassFlowRate mSol_flow_nominal
     "Solar thermal collecDialogtor nominal water flow rate"
      annotation (Dialog(tab="SolarCollector"));
    parameter Modelica.SIunits.PressureDifference dpSol_nominal
      "Pressure difference accross the sollar colletor"
      annotation (Dialog(tab="SolarCollector"));
 */

    Modelica.Blocks.Interfaces.RealInput conFloMin(final unit="kg/s",displayUnit="kg/s")
    "Minimum condenser flow rate as recommended by the manufacturer."
    annotation (Placement(transformation(extent={{-320,154},{-300,174}}),
        iconTransformation(extent={{-116,16},{-100,32}})));
    Modelica.Blocks.Interfaces.RealInput evaFloMin(final unit="kg/s",displayUnit="kg/s")
    "Minimum evaporator flow rate as recommended by the manufacturer."
    annotation (Placement(transformation(extent={{-320,138},{-300,158}}),
        iconTransformation(extent={{-116,34},{-100,50}})));
  parameter Modelica.SIunits.TemperatureDifference THys=2
    "Temperature hysteresis";
    Modelica.Blocks.Interfaces.RealInput TSetHea( final unit="K",displayUnit="degC")
     "Heating setpoint temperature"
    annotation (Placement(transformation(extent={{-320,270},{-300,290}}),
        iconTransformation(extent={{-116,80},{-100,96}})));
    Modelica.Blocks.Interfaces.RealInput TSetCoo( final unit="K",displayUnit="degC")
     "Cooling setpoint temperature"
    annotation (Placement(transformation(extent={{-320,256},{-300,276}}),
        iconTransformation(extent={{-116,94},{-100,110}})));
equation
  connect(heaPum.port_b1,pumCon. port_a)
    annotation (Line(points={{-10,132},{0,132}}, color={238,46,47},
      pattern=LinePattern.Dash,
      thickness=0.5));
  connect(heaPumCon.yHeaPumMod, heaPum.uMod) annotation (Line(points={{-98.6,211.2},{-40,211.2},{-40,126},{-31,126}},
                                                            color={255,127,0},
      pattern=LinePattern.Dash));
  connect(ETSCon.reqHea,heaPumCon.reqHea)
    annotation (Line(
      points={{-177,213},{-140,213},{-140,219},{-121.4,219}},
      color={255,0,255},
      pattern=LinePattern.Dot));
  connect(ETSCon.reqCoo,heaPumCon.reqCoo)
    annotation (Line(
      points={{-177,195},{-142,195},{-142,216},{-121.4,216}},
      color={255,0,255},
      pattern=LinePattern.Dot));
  connect(pumEva.port_b,TEvaEnt. port_a) annotation (Line(
      points={{-66,130},{-50,130},{-50,96},{-30,96}},
      color={238,46,47},
      pattern=LinePattern.Dash,
      thickness=0.5));
  connect(TEvaEnt.port_b, heaPum.port_a2) annotation (Line(
      points={{-10,96},{-6,96},{-6,120},{-10,120}},
      color={238,46,47},
      pattern=LinePattern.Dash,
      thickness=0.5));
  connect(hotWatRet, hotBufTan.port_a1) annotation (Line(
      points={{302,32},{266,32},{266,32.4},{180,32.4}},
      color={0,127,255},
      thickness=0.5));
  connect(chiWatRet, colBufTan.port_a) annotation (Line(
      points={{-304,60},{-234,60}},
      color={238,46,47},
      thickness=0.5));
  connect(colBufTan.heaPorVol[1], topCooTan.port)
    annotation (Line(points={{-222,60},{-222,94},{-224,94}},color={191,0,0},
      pattern=LinePattern.Dash));
  connect(colBufTan.heaPorVol[nSegTan], botCooTan.port)
    annotation (Line(points={{-222,60},{-222,0},{-230,0}},
                                     color={191,0,0},
      pattern=LinePattern.Dash));
  connect(hotBufTan.heaPorVol[nSegTan], botHotTan.port)
    annotation (Line(points={{168,42},{168,-32},{176,-32}},color={191,0,0},
      pattern=LinePattern.Dash));
  connect(hotBufTan.heaPorVol[1], topHotTan.port)
    annotation (Line(points={{168,42},{168,58},{214,58},{214,208}},
                                                      color={191,0,0},
      pattern=LinePattern.Dash));
  connect(topHotTan.T,ETSCon. TTanHeaTop)
    annotation (Line(
      points={{194,208},{84,208},{84,246},{-204,246},{-204,213},{-199,213}},
      color={0,0,127},
      pattern=LinePattern.Dot));
  connect(botHotTan.T,ETSCon. TTanHeaBot)
    annotation (Line(
      points={{196,-32},{224,-32},{224,250},{-206,250},{-206,211},{-199,211}},
      color={0,0,127},
      pattern=LinePattern.Dot));
  connect(topCooTan.T,ETSCon. TTanCooTop)
    annotation (Line(
      points={{-244,94},{-266,94},{-266,195},{-199,195}},
      color={0,0,127},
      pattern=LinePattern.Dot));
  connect(botCooTan.T,ETSCon. TTanCooBot)
    annotation (Line(
      points={{-250,0},{-274,0},{-274,197},{-199,197}},
      color={0,0,127},
      pattern=LinePattern.Dot));
  connect(hotBufTan.mNor_flow,ETSCon. mTanHotNor)
    annotation (Line(points={{181.2,46.8},{236,46.8},{236,260},{-210,260},{-210,
          205},{-199,205}},
        color={0,0,127},
      pattern=LinePattern.Dash));
  connect(hex.port_a1, pumHexDis.port_b)
    annotation (Line(points={{104,-160},{104,-120},{110,-120}},                         color={0,127,
          255},
      thickness=0.5));
  connect(disWatSup, disSupTem.port_a) annotation (Line(
      points={{300,-192},{240,-192}},
      color={0,127,255},
      thickness=0.5));
  connect(disSupTem.port_b, hex.port_a2)
    annotation (Line(points={{220,-192},{116,-192},{116,-180}},
                               color={0,127,255},
      thickness=0.5));
  connect(hex.port_b2, disRetTem.port_a)
    annotation (Line(points={{116,-160},{116,-150},{220,-150}},
                                       color={238,46,47},
      thickness=0.5));
  connect(disRetTem.port_b,disWatRet)
    annotation (Line(points={{240,-150},{300,-150}}, color={238,46,47},
      thickness=0.5));
  connect(TBorLvg.port_a, borFie.port_b) annotation (Line(
      points={{-30,-220},{-70,-220},{-70,-200}},
      color={28,108,200},
      thickness=0.5,
      pattern=LinePattern.DashDotDot));
  connect(hex.port_b1, TDisHexLvg.port_a) annotation (Line(
      points={{104,-180},{104,-224},{22,-224},{22,-210}},
      color={28,108,200},
      pattern=LinePattern.DashDotDot,
      thickness=0.5));
  connect(heaRetHed.ports_a[1], hotBufTan.port_b1)
    annotation (Line(points={{122.2,19.55},{150,19.55},{150,32.4},{156,32.4}},
                                                        color={0,127,255},
      thickness=0.5));

  connect(TMinConEnt, heaPumCon.TMinConEnt) annotation (Line(
      points={{-310,194},{-136,194},{-136,207.2},{-121,207.2}},
      color={0,0,127},
      pattern=LinePattern.Dash));
  connect(TMaxEvaEnt, heaPumCon.TMaxEvaEnt) annotation (Line(
      points={{-310,180},{-296,180},{-296,190},{-134,190},{-134,205.4},{-121,
          205.4}},
      color={0,0,127},
      pattern=LinePattern.Dash));
  connect(TEvaLvg.T,heaPumCon.TEvaLvg)  annotation (Line(
      points={{-78,31},{-78,36},{-124,36},{-124,201.6},{-121,201.6}},
      color={0,0,127},
      pattern=LinePattern.Dot));
  connect(heaPumCon.TSetHeaPum, heaPum.TSet) annotation (Line(points={{-98.6,214.6},{-38,214.6},{-38,135},{-31.4,135}},
                                                     color={0,0,127},
      pattern=LinePattern.Dash));
  connect(ETSCon.reqHea,pumPrimCon.reqHea)  annotation (Line(points={{-177,213},
          {-140,213},{-140,162},{-121.4,162}},              color={255,0,255},
      pattern=LinePattern.Dot));
  connect(colBufTan.port_b1, secCooFlo.port_a) annotation (Line(points={{-234,50.4},{-234,40},{-244,40}},
                                      color={0,127,255}));
  connect(chiWatSup, secCooFlo.port_b) annotation (Line(
      points={{-304,40},{-264,40}},
      color={0,127,255},
      thickness=0.5));
  connect(hotWatSup, secHeaFlo.port_b)
    annotation (Line(points={{302,52},{284,52}}, color={238,46,47}));
  connect(hotBufTan.port_b, secHeaFlo.port_a) annotation (Line(points={{180,42},
          {222,42},{222,52},{264,52}},   color={238,46,47},
      thickness=0.5));
  connect(hotBufTan.port_a, priLoaFlo.port_b)
    annotation (Line(points={{156,42},{150,42},{150,60},{146,60}},
                                                 color={238,46,47}));
  connect(colBufTan.port_a1,priCooFlo. port_b) annotation (Line(points={{-210,
          50.4},{-210,20},{-200,20}},
                                    color={0,127,255},
      thickness=0.5));
  connect(secCooFlo.m_flow, pumPrimCon.mSecCoo) annotation (Line(points={{-254,51},
          {-254,146.4},{-120.8,146.4}},     color={0,140,72},
      pattern=LinePattern.DashDot));
  connect(TSetHeaMax, heaPumCon.TSetHeaMax) annotation (Line(
      points={{-310,228},{-136,228},{-136,208.8},{-121,208.8}},
      color={0,0,127},
      pattern=LinePattern.DashDot));

  connect(pumHexDis.port_a, TDisHex.port_b)
    annotation (Line(points={{110,-100},{110,-80}},color={0,127,255},
      thickness=0.5));
  connect(ambCon.yBorThrVal, valBor.y)
    annotation (Line(points={{-123,-64.2},{-123,-64},{-100,-64},{-100,-110},{
          -82,-110}},                                                                    color={0,0,127},
      pattern=LinePattern.Dash));

  connect(pumBor.port_b, TBorEnt.port_a) annotation (Line(points={{-70,-160},{
          -70,-160}},                                                                     color={0,127,255}));
  connect(borFie.port_a, TBorEnt.port_b) annotation (Line(points={{-70,-180},{
          -70,-180}},                                                                     color={0,127,255}));
  connect(TMaxBorEnt, ambCon.TBorMaxEnt) annotation (Line(
      points={{-310,-74},{-172,-74},{-172,-73},{-145,-73}},
      color={0,0,127},
      pattern=LinePattern.Dot));
  connect(TBorEnt.T, ambCon.TBorEnt) annotation (Line(
      points={{-81,-170},{-154,-170},{-154,-79.8},{-145,-79.8}},
      color={0,0,127},
      pattern=LinePattern.Dot));
  connect(TDisHexLvg.T, ambCon.TDisHexLvg) annotation (Line(
      points={{11,-200},{0,-200},{0,-282},{-160,-282},{-160,-77.2},{-145,-77.2}},
      color={0,0,127},
      pattern=LinePattern.Dot));
  connect(ambCon.TDisHexEnt, TDisHex.T) annotation (Line(
      points={{-145,-75.2},{-162,-75.2},{-162,-288},{90,-288},{90,-70},{99,-70}},
      color={0,0,127},
      pattern=LinePattern.Dot));
  connect(ETSCon.mTanColNor, colBufTan.mNor_flow)
    annotation (Line(points={{-199,203},{-204,203},{-204,64.8},{-208.8,64.8}}, color={0,0,127},
      pattern=LinePattern.Dash));

  connect(TEvaLvg.port_b, cooSupHed.ports_a[1]) annotation (Line(points={{-88,20},{-88,19.7},{-107.8,19.7}},
                                                                                                color={0,127,
          255},
      thickness=0.5));
  connect(priCooFlo.port_a, cooSupHed.ports_b[2]) annotation (Line(points={{-180,20},
          {-154,20},{-154,20.85},{-128.4,20.85}},                                                color={0,127,
          255},
      thickness=0.5));
  connect(cooSupHed.ports_b[1], valSupCoo.port_a)
    annotation (Line(points={{-128.4,19.35},{-128.4,-20},{-94,-20}},    color={0,127,
          255},
      thickness=0.5));
  connect(colBufTan.port_b, cooRetHed.ports_a[2])
    annotation (Line(points={{-210,60},{-170,60},{-170,61.05},{-148.2,61.05}},
                                                                         color={238,46,
          47},
      thickness=0.5,
      pattern=LinePattern.Dash));
  connect(pumEva.port_a, valEva.port_2) annotation (Line(points={{-86,130},{-94,
          130},{-94,88}},                                                                       color={238,46,
          47},
      thickness=0.5,
      pattern=LinePattern.Dash));
  connect(cooRetHed.ports_b[1], valEva.port_1)
    annotation (Line(points={{-127.6,59.9},{-94,59.9},{-94,68}}, color={238,46,
          47},
      thickness=0.5,
      pattern=LinePattern.Dash));
  connect(splVal2.port_3, valEva.port_3) annotation (Line(points={{-70,78},{-84,78}}, color={0,127,255}));
  connect(heaPum.port_b2, splVal2.port_1) annotation (Line(points={{-30,120},{-60,120},{-60,88}}, color={0,127,
          255},
      thickness=0.5));
  connect(splVal2.port_2, TEvaLvg.port_a) annotation (Line(points={{-60,68},{-60,20},{-68,20}}, color={0,127,
          255},
      thickness=0.5));
  connect(TDisHexLvg.port_b, ambHedSup.ports_a[1]) annotation (Line(points={{22,-190},
          {22,-168},{-8,-168},{-8,-131.495},{-0.2,-131.495}},
                            color={0,127,255},
      thickness=0.5));
  connect(heaRetHed.ports_b[1], TConEnt.port_a)
    annotation (Line(points={{101.6,19.9},{50,19.9},{50,20},{0,20}},
                                                               color={0,127,255},
      thickness=0.5));

  connect(heaSupHed.ports_b[1], valSupHea.port_a)
    annotation (Line(points={{112.4,59.35},{112.4,38},{60,38},{60,-20},{18,-20}},
                                                                           color={0,127,
          255},
      thickness=0.5));
  connect(TConEnt.port_b, valCon.port_1)
    annotation (Line(points={{-20,20},{-34,20},{-34,42}}, color={0,127,255},
      thickness=0.5));
  connect(heaPum.port_a1, valCon.port_2) annotation (Line(points={{-30,132},{-34,
          132},{-34,62}}, color={0,127,255},
      thickness=0.5));
  connect(valBor.port_3, splVal1.port_3)
    annotation (Line(points={{-60,-110},{-50,-110},{-50,-170},{-40,-170}}, color={0,127,
          255},
      thickness=0.5));
  connect(TBorLvg.port_b, splVal1.port_1) annotation (Line(points={{-30,-200},{
          -30,-180}},                                                                      color={0,127,255}));
  connect(splVal1.port_2, ambHedSup.ports_a[2])
    annotation (Line(points={{-30,-160},{-30,-129.845},{-0.2,-129.845}}, color={0,127,
          255},
      thickness=0.5));
  connect(cooRetHed.ports_a[1], ambHedSup.ports_b[1]) annotation (Line(points={{-148.2,
          59.55},{-148.2,2},{28,2},{28,-130.285},{20.4,-130.285}},
                            color={238,46,47},
      thickness=0.5,
      pattern=LinePattern.Dash));
  connect(ambHedSup.ports_b[2], heaRetHed.ports_a[2]) annotation (Line(points={{20.4,-131.935},{72,-131.935},{72,2},{122.2,
          2},{122.2,21.05}}, color={0,127,255},
      thickness=0.5));
  connect(pumCon.port_b, splVal3.port_1) annotation (Line(points={{20,132},{32,132}}, color={0,127,255}));
  connect(TConLvg.port_a, splVal3.port_2) annotation (Line(points={{58,132},{52,132}}, color={0,127,255}));
  connect(TConLvg.port_b, heaSupHed.ports_a[1])
    annotation (Line(points={{78,132},{84,132},{84,59.7},{91.8,59.7}},
                                                                   color={238,46,
          47},
      thickness=0.5));
  connect(valCon.port_3, splVal3.port_3)
    annotation (Line(points={{-24,52},{42,52},{42,122}}, color={0,127,255},
      thickness=0.5));
  connect(heaSupHed.ports_b[2], priLoaFlo.port_a)
    annotation (Line(points={{112.4,60.85},{120,60.85},{120,60},{126,60}},
                                                                   color={0,127,255}));
  connect(valSupCoo.port_b, ambRetHed.ports_a[1])
    annotation (Line(points={{-74,-20},{-24,-20},{-24,-47.55},{-29.8,-47.55}},
                                                                         color={0,127,
          255},
      thickness=0.5));
  connect(ambRetHed.ports_a[2], valSupHea.port_b)
    annotation (Line(points={{-29.8,-49.05},{-12,-49.05},{-12,-20},{-2,-20}},
                                                                        color={0,127,
          255},
      thickness=0.5));
  connect(ambRetHed.ports_b[1], TDisHex.port_a)
    annotation (Line(points={{-50.4,-48.65},{-56,-48.65},{-56,-60},{110,-60}},
                                                                       color={0,127,
          255},
      thickness=0.5));
  connect(pumBor.m_flow_in, gaiBor.y) annotation (Line(points={{-82,-150},{-88,-150}}, color={0,0,127}));
  connect(ambCon.yBorPum, gaiBor.u) annotation (Line(
      points={{-123,-75.2},{-116,-75.2},{-116,-150},{-112,-150}},
      color={0,0,127},
      pattern=LinePattern.Dash));
  connect(ambCon.yDisHexPum, gaiMDisHex.u) annotation (Line(
      points={{-123,-78.8},{-120,-78.8},{-120,-252},{38,-252}},
      color={0,0,127},
      pattern=LinePattern.Dash));
  connect(gaiMDisHex.y, pumHexDis.m_flow_in) annotation (Line(
      points={{62,-252},{80,-252},{80,-110},{98,-110}},
      color={0,0,127},
      pattern=LinePattern.Dash));
  connect(TConEnt.T, heaPumCon.TConEnt) annotation (Line(
      points={{-10,31},{-10,40},{-122,40},{-122,200.2},{-121,200.2}},
      color={0,0,127},
      pattern=LinePattern.Dot));
  connect(TEvaEnt.T, heaPumCon.TEvaEnt) annotation (Line(
      points={{-20,107},{-20,114},{-62,114},{-62,190},{-126,190},{-126,203.4},{
          -121,203.4}},
      color={0,0,127},
      pattern=LinePattern.Dot));
  connect(heaPumCon.yValCon, valEva.y) annotation (Line(
      points={{-98.6,204.4},{-96,204.4},{-96,114},{-116,114},{-116,78},{-106,78}},
      color={0,0,127},
      pattern=LinePattern.Dash));

  connect(heaPumCon.yValEva, valCon.y) annotation (Line(
      points={{-98.6,207.6},{-54,207.6},{-54,52},{-46,52}},
      color={0,0,127},
      pattern=LinePattern.Dash));
  connect(pumPrimCon.mSecHea, secHeaFlo.m_flow)
    annotation (Line(points={{-120.8,158},{-130,158},{-130,190},{274,190},{274,
          63}},                                                                          color={0,140,72},
      pattern=LinePattern.DashDot));

  connect(priLoaFlo.m_flow, pumPrimCon.mPriHea)
    annotation (Line(points={{136,71},{136,186},{-128,186},{-128,160.4},{-120.8,
          160.4}},                                                                       color={0,140,72},
      pattern=LinePattern.DashDot));

  connect(priCooFlo.m_flow,pumPrimCon.mPriCoo)
    annotation (Line(points={{-190,31},{-190,144},{-120.8,144}}, color={0,140,72},
      pattern=LinePattern.DashDot));

  connect(ETSCon.reqCoo,pumPrimCon.reqCoo)  annotation (Line(
      points={{-177,195},{-166,195},{-166,142.2},{-121.4,142.2}},
      color={255,0,255},
      pattern=LinePattern.Dot));
  connect(ETSCon.ValHeaPos, valSupHea.y)
    annotation (Line(
      points={{-177,203},{-152,203},{-152,-2},{8,-2},{8,-8}},
      color={28,108,200},
      pattern=LinePattern.DashDot));
  connect(ETSCon.ValCooPos, valSupCoo.y)
    annotation (Line(
      points={{-177,201},{-154,201},{-154,-8},{-84,-8}},
      color={28,108,200},
      pattern=LinePattern.DashDot));
  connect(ETSCon.reqCoo, ambCon.reqCoo) annotation (Line(
      points={{-177,195},{-166,195},{-166,-71},{-145,-71}},
      color={255,0,255},
      pattern=LinePattern.Dot));
  connect(ambCon.reqHea, ETSCon.reqHea) annotation (Line(
      points={{-145,-60.2},{-156,-60.2},{-156,213},{-177,213}},
      color={255,0,255},
      pattern=LinePattern.Dot));
  connect(ETSCon.ValHea, ambCon.valHea) annotation (Line(
      points={{-177,211},{-158,211},{-158,-62.4},{-145,-62.4}},
      color={255,0,255},
      pattern=LinePattern.Dot));
  connect(ETSCon.ValCoo, ambCon.valCoo) annotation (Line(
      points={{-177,209},{-160,209},{-160,-64.8},{-145,-64.8}},
      color={255,0,255},
      pattern=LinePattern.Dot));
  connect(ambCon.rejCooFulLoa,ETSCon. rejColFulLoa) annotation (Line(
      points={{-145,-69},{-164,-69},{-164,196.8},{-177,196.8}},
      color={255,0,255},
      pattern=LinePattern.Dot));
  connect(ETSCon.rejHeaFulLoa, ambCon.rejHeaFulLoa) annotation (Line(
      points={{-177,198.8},{-162,198.8},{-162,-67},{-145,-67}},
      color={255,0,255},
      pattern=LinePattern.Dot));
  connect(pumPrimCon.yPumCon, pumCon.y) annotation (Line(points={{-99,160},{10,
          160},{10,144}},                                                                      color={0,0,127},
      pattern=LinePattern.Dot));

  connect(pumPrimCon.yPumEva, pumEva.y) annotation (Line(points={{-99,144},{-76,
          144},{-76,142}},                                                                       color={0,0,127},
      pattern=LinePattern.Dot));

  connect(valBor.port_2, pumBor.port_a) annotation (Line(points={{-70,-120},{
          -70,-140}},            color={0,127,255},
      thickness=0.5));
  connect(ambRetHed.ports_b[2], valBor.port_1) annotation (Line(
      points={{-50.4,-47.15},{-70,-47.15},{-70,-100}},
      color={0,127,255},
      thickness=0.5));
  connect(ETSCon.pumCooTanMin, pumPrimCon.cooTanMin) annotation (Line(
      points={{-177,205},{-148,205},{-148,148.4},{-120.8,148.4}},
      color={0,0,127},
      pattern=LinePattern.Dash));
  connect(ETSCon.pumHeaTanMin, pumPrimCon.heaTanMin) annotation (Line(
      points={{-177,207},{-146,207},{-146,150.6},{-120.8,150.6}},
      color={0,0,127},
      pattern=LinePattern.Dash));
  connect(evaFloMin, pumPrimCon.evaFloMin) annotation (Line(
      points={{-310,148},{-286,148},{-286,153.6},{-120.8,153.6}},
      color={0,0,127},
      pattern=LinePattern.Dash));
  connect(conFloMin, pumPrimCon.conFloMin) annotation (Line(
      points={{-310,164},{-216,164},{-216,156},{-120.8,156}},
      color={0,0,127},
      pattern=LinePattern.Dash));
  connect(TSetHea, ETSCon.TSetHea) annotation (Line(
      points={{-310,280},{-214,280},{-214,209},{-199,209}},
      color={238,46,47},
      pattern=LinePattern.Dash,
      thickness=0.5));
  connect(TSetCoo, ETSCon.TSetCoo) annotation (Line(
      points={{-310,266},{-228,266},{-228,199},{-199,199}},
      color={0,128,255},
      pattern=LinePattern.Dash,
      thickness=0.5));
  connect(TSetHea, heaPumCon.TSetHea) annotation (Line(
      points={{-310,280},{-126,280},{-126,213},{-121,213}},
      color={238,46,47},
      pattern=LinePattern.Dash,
      thickness=0.5));
  connect(TSetCoo, heaPumCon.TSetCoo) annotation (Line(
      points={{-310,266},{-130,266},{-130,211},{-121,211}},
      color={0,128,255},
      pattern=LinePattern.Dash,
      thickness=0.5));
   annotation (Icon(coordinateSystem(preserveAspectRatio=false), graphics={
                                Rectangle(
        extent={{-100,-100},{100,100}},
        lineColor={0,0,127},
        fillColor={255,255,255},
        fillPattern=FillPattern.Solid),
        Rectangle(
          extent={{-62,40},{64,-80}},
          lineColor={27,0,55},
          fillColor={170,213,255},
          fillPattern=FillPattern.Solid),
        Line(points={{2,78},{-68,40}}, color={27,0,55}),
        Line(points={{2,78},{68,40},{-70,40}}, color={27,0,55}),
        Polygon(
          points={{-78,40},{2,100},{82,40},{-78,40}},
          lineColor={27,0,55},
          fillColor={255,85,85},
          fillPattern=FillPattern.Solid),
                                        Text(
        extent={{-150,146},{150,106}},
        textString="%name",
        lineColor={0,0,255}),
        Rectangle(
          extent={{-54,30},{-18,2}},
          lineColor={0,140,72},
          pattern=LinePattern.Dot,
          lineThickness=0.5),
        Rectangle(
          extent={{14,30},{54,4}},
          lineColor={0,140,72},
          pattern=LinePattern.Dot,
          lineThickness=0.5),
        Rectangle(
          extent={{-14,-18},{16,-76}},
          lineColor={0,140,72},
          pattern=LinePattern.Dot,
          lineThickness=0.5)}),                                  Diagram(
        coordinateSystem(preserveAspectRatio=false, extent={{-300,-300},{300,
            300}}),     graphics={Line(
          points={{86,92}},
          color={28,108,200},
          pattern=LinePattern.Dash)}),
          defaultComponentName="ETS",
 Documentation(info="<html>
 <h4> Energy Transfer Station </h4>
<p>
This models represents a 5<sup>th</sup> generation district heating and cooling energy transfer station 5GDHC-ETS, where the main purposes are; to exploit the available renewable energy i.e. solar thermal energy modules,
to implement the energy storage concept within the hot and cold buffer tanks and to successfully integrate the low temperature or ambient district circuit with the building thermal requirements.
</p>
<p align=\"center\">
<img alt=\"Image the 5th generation of district heating and cooling substation\"
src=\"modelica://Buildings/Resources/Images/Applications/DHC/EnergyTransferStations/SubstationLayout.png\"/>
</p>          
<p>
The layout consists of three main water circuits categorized by the water temperature and accordingly three interfaces with both the building and district system
</p>
<h4> Hot water circuit, interface with ETS and the building heating requirements</h4>
<p>
The hot water circuit operates to satisfy the building heating requirements. It consists of four main systems:
</p>
<ol>
<li>
The heatpump condenser loop, where the heatpump operates to satisfy the setpoint of the condenser leaving water temperature <code>TSetHea</code>.
</li>
<li>
The condenser water pump <code>pumCon</code>, where pump the speed is modulated using a PI controller.
</li>
<li>
The solar thermal system, where the solar pump  <code>pumSol</code> turns on when both the incident solar radiation is large enough to produce useful energy gain to heat the water and the top level water temperature is below the
solar setpoint temperature<code>TSetsolHotTan</code>.
</li>
<li>
The hot buffer tank, where it is implemented to provide hydraulic separations between the primary and secondary water circulators i.e. pumps,store heat/cool supplied using lower cost time-use of electrical rates, reduce the size of the heat source relative to the peak load.
</ol>
<h4> Cold water circuit, interface with ETS and the building cooling requirements</h4>
The cold water circuit operates to satisfy the building cooling requirements and it consists of:
</p>
<ol>
<li>
The heatpump evaporator loop, where the heatpump operates to satisfy the setpoint of the evaporator leaving water temperature, <code>TSetCoo</code>.
</li>
<li>
The evaporator water pump <code>pumCon</code>, where pump the speed is modulated using a PI controller.
</li>
<li>The cold buffer tank, is implemented obviously for the same mentioned reasons of the hot buffer tank.
</li>
</ol>
<h4>Notices for the cold and hot water circuits </h4>
<ol>
<li>
There are two finite state machine that transitions the mode of operation of
the heatpump i.e. required heating or cooling and energy rejection to the borefield and district heat exchanger systems between the modes
<i>part load rejection i.e. only to the borefield</i>, <i>full load rejection to both the borefield and district heat exchanger system</i>, <i>rejection to only the district heat exchanger system </i>.
See more detailed description in <a href=\"Buildings.DistrictHeatingCooling.EnergyTransferStations.ETSControl.SubstationUO\">
Buildings.DistrictHeatingCooling.EnergyTransferStations.ETSControl.SubstationUO</a>
</li>
<li>
See more detailed description of the heat pump, <code>pumEva</code> and <code>pumCon</code> systems control in
<a href=\"Buildings.DistrictHeatingCooling.EnergyTransferStations.ETSControl.HeatPumpUO\"> Buildings.DistrictHeatingCooling.EnergyTransferStations.ETSControl.HeatPumpUO </a>.
</li>
<li>
See more detailed description of the solar thermal module control in
<a href=\"Buildings.DistrictHeatingCooling.EnergyTransferStations.EnergyTransferStation.SolarThermalModule\"> Buildings.DistrictHeatingCooling.EnergyTransferStations.EnergyTransferStation.SolarThermalModule </a>.
</li>
</ol>
<h4> Ambient water circuit, interface with ETS and the low temperature district network</h4>
<p>
The ambient water circuit operates to maximize the system exergy by rejecting surplus i.e. heating or cooling energy first to the borefiled system and second to the district heat exchanger system.
It consists of two main systems
</p>
<ol>
<li>
The borefield pump <code>pumBor</code>, where its mass flow rate is modulated using a PI controller and it turns on and off based on:
</li>
<p>
The difference between the measured and setpoint temperature of the buffer tank i.e. cold or hot buffer tank is identified using the finite state machine in
<a href=\"Buildings.DistrictHeatingCooling.EnergyTransferStations.ETSControl.ColdSideControlleUO\">
Buildings.DistrictHeatingCooling.EnergyTransferStations.ETSControl.HotSideControlleUO</a> and <a href=\"Buildings.DistrictHeatingCooling.EnergyTransferStations.ETSControl.ColdSideControlleUO\">
Buildings.DistrictHeatingCooling.EnergyTransferStations.ETSControl.HotSideControlleUO</a>.
</p>
<li>
The two way valves <code>valHea</code> and <code>valcoo</code> status.
</li>
<li>
The heat exchanger district pump <code>pumHexDis</code>, where its mass flow rate is modulated using a PI controller and it turns on and off based on the status of the boolean input signals of
<code>rejHeaFulLoa</code> and <code>rejCooFulLoa</code>.
</li>
</ol>
<p>
For more detailed description of the energy rejection control see
<a href=\"Buildings.DistrictHeatingCooling.EnergyTransferStations.ETSControl.AmbientCircuitController\"> Buildings.DistrictHeatingCooling.EnergyTransferStations.ETSControl.AmbientCircuitController </a>
</p>

</html>", revisions="<html>
<ul>
<li>
 <br/>
</li>
</ul>
</html>"));
end Substation;
