within Buildings.Applications.DHC.EnergyTransferStations;
model Substation
  "5th generation of district heating and cooling plant"
   package Medium = Buildings.Media.Water "Medium model";

  //--------------------------WSHP------------------------
    parameter Modelica.SIunits.MassFlowRate mCon_flow_nominal
     "Condenser nominal water flow rate"
      annotation (Dialog(tab="WSHP"));
    parameter Modelica.SIunits.MassFlowRate mEva_flow_nominal
     "Evaporator nominal water flow rate"
      annotation (Dialog(tab="WSHP"));
    parameter Modelica.SIunits.PressureDifference dpCon_nominal
      "Pressure difference accross the condenser"
      annotation (Dialog(tab="WSHP"));
    parameter Modelica.SIunits.PressureDifference dpEva_nominal
      "Pressure difference accross the evaporator"
      annotation (Dialog(tab="WSHP"));
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
 //-----------------------------------------------------------------
    parameter Modelica.SIunits.MassFlowRate m_flow_nominal
    "Nominal mass flow rate";
  //---------------------------------Buffer tanks-------------------
    parameter Modelica.SIunits.Volume VTan
      "Tank volume, ensure at least 5 minutes buffer flow"
      annotation (Dialog(tab="Water Buffer Tank"));
    parameter Modelica.SIunits.Length hTan
      "Height of tank (without insulation)"
      annotation (Dialog(tab="Water Buffer Tank"));
    parameter Modelica.SIunits.Length dIns "Thickness of insulation"
      annotation (Dialog(tab="Water Buffer Tank"));
    parameter Integer nSegTan(min=2)   "Number of volume segments"
      annotation (Dialog(tab="Water Buffer Tank"));
  //-------------------------------Design Parameters----------------
    parameter Modelica.SIunits.Temperature THeaWatSup_nominal=314.15
      "Nominal heating supply water temperature"
      annotation (Dialog(group="Design parameter"));
    parameter Modelica.SIunits.Temperature THeaWatRet_nominal=303.15
      "Nominal heating Return water temperature"
      annotation (Dialog(group="Design parameter"));
    parameter Modelica.SIunits.Temperature TCooWatSup_max=288.15
      "Maximum cooling water supply temperature"
      annotation (Dialog(group="Design parameter"));
    parameter Modelica.SIunits.Temperature TCooWatSup_min=277.15
      "Minimum cooling water supply temperature"
      annotation (Dialog(group="Design parameter"));
    parameter Modelica.SIunits.TemperatureDifference dTCooWat=4
      "Cooling water supply and return temperature difference"
      annotation (Dialog(group="Design parameter"));
 //----------------------------Borefield system----------------------------------
   parameter Modelica.SIunits.MassFlowRate mGeo_flow_nominal
    "Borefiled nominal water flow rate"
      annotation (Dialog(tab="Borefield"));
   parameter Modelica.SIunits.Length xBorFie
    "Borefield length"
      annotation (Dialog(tab="Borefield"));
    parameter Modelica.SIunits.Length yBorFie
     "Borefield width"
      annotation (Dialog(tab="Borefield"));
    parameter Modelica.SIunits.Length dBorHol
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
    parameter Modelica.SIunits.Radius rTub
     "Outer radius of the tubes"
      annotation(Dialog(tab="Borefield"));
  //---------------------------HeatExchanger----------
    parameter Modelica.SIunits.MassFlowRate mHex_flow_nominal
      "District heat exhanger nominal water flow rate"
      annotation (Dialog(tab="DistrictHeatExchanger"));
    parameter Real eps_nominal=0.71
      "Heat exchanger effectiveness"
      annotation (Dialog(tab="DistrictHeatExchanger"));
    parameter Modelica.SIunits.PressureDifference dpHex_nominal(displayUnit="Pa")
      "Pressure difference across heat exchanger"
      annotation (Dialog(tab="DistrictHeatExchanger"));
//----------------------------------Generanl---------------------------------------
    parameter Modelica.Fluid.Types.Dynamics fixedEnergyDynamics=Modelica.Fluid.Types.Dynamics.FixedInitial
      "Formulation of energy balance for mixing volume at inlet and outlet"
      annotation (Dialog(group="Dynamics"));
    parameter Modelica.SIunits.PressureDifference dp_nominal(displayUnit="Pa")
      "Pressure difference at nominal flow rate"
      annotation (Dialog(tab="Design Parameter"));
    parameter Boolean show_T=true
      "= true, if actual temperature at port is computed"
      annotation (Dialog(group="Advanced"));
 //------------------------------------------------------------------------
    Fluid.HeatPumps.EquationFitReversible heaPum(
      energyDynamics=Modelica.Fluid.Types.Dynamics.FixedInitial,
      massDynamics=Modelica.Fluid.Types.Dynamics.FixedInitial,
      per = heaPumDat,
      redeclare package Medium1 = Medium,
      redeclare package Medium2 = Medium)
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
      "Pump (or valve) that forces the flow rate to be set to the control signal"
      annotation (Placement(transformation(extent={{0,122},{20,142}})));
  Control.HeatPumpController heaPumCon "Control of the heatpump model"
    annotation (Placement(transformation(extent={{-120,200},{-100,220}})));
    Buildings.Fluid.Movers.SpeedControlled_y pumEva(
      redeclare package Medium = Medium,
      energyDynamics=Modelica.Fluid.Types.Dynamics.SteadyState,
      addPowerToMedium=false,
      show_T=show_T,
      per(pressure(dp={2*dpEva_nominal,0}, V_flow={0,2*mEva_flow_nominal/1000})),
      allowFlowReversal=false,
      use_inputFilter=false,
      riseTime=10)
      "Pump (or valve) that forces the flow rate to be set to the control signal"
      annotation (Placement(transformation(
          extent={{-10,-10},{10,10}},
          rotation=0,
          origin={-78,112})));
   Control.SubstationUO subCon
    "Substation control "
      annotation (Placement(transformation(extent={{-200,200},{-180,220}})));
    Buildings.Fluid.Sensors.TemperatureTwoPort TConLvg(
      redeclare final package Medium = Medium,
      allowFlowReversal=false,
      m_flow_nominal=mCon_flow_nominal,
      tau=0) "Condenser leaving water temperature"
                                         annotation (Placement(
          transformation(
          extent={{10,10},{-10,-10}},
          rotation=180,
          origin={68,132})));
    Buildings.Fluid.Sensors.TemperatureTwoPort TConEnt(
      redeclare final package Medium = Medium,
      allowFlowReversal=false,
      m_flow_nominal=mCon_flow_nominal,
      tau=0) "Condenser entering water temperature"
      annotation (Placement(transformation(extent={{0,10},{-20,30}})));
    Buildings.Fluid.Sensors.TemperatureTwoPort TEvaEnt(
    redeclare final package Medium = Medium,
    allowFlowReversal=false,
    m_flow_nominal=mEva_flow_nominal,
    tau=0) "Evaporator entering water temperature"
                                              annotation (Placement(
        transformation(
        extent={{10,10},{-10,-10}},
        rotation=180,
        origin={-20,96})));
    Buildings.Fluid.Sensors.TemperatureTwoPort TEvaLvg(
      redeclare final package Medium = Medium,
      allowFlowReversal=false,
    m_flow_nominal=mEva_flow_nominal,
      tau=30) "Evaporator leaving water temperature"
      annotation (Placement(transformation(extent={{-68,10},{-88,30}})));
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
      m_flow_nominal=mEva_flow_nominal) "Cold Buffer tank"
      annotation (Placement(transformation(extent={{-234,48},{-210,72}})));
    Modelica.Fluid.Interfaces.FluidPort_a cooSup(
      h_outflow(start=Medium.h_default, nominal=Medium.h_default),
      redeclare final package Medium = Medium,
      p(start=Medium.p_default))
      "Fluid connector a (positive design flow direction is from port_a to port_b), cold water supply to the building"
      annotation (Placement(transformation(extent={{-314,30},{-294,50}}), iconTransformation(extent={{-120,
            -58},{-100,-38}})));
    Modelica.Fluid.Interfaces.FluidPort_b cooRet(
      h_outflow(start=Medium.h_default, nominal=Medium.h_default),
      redeclare final package Medium = Medium,
      p(start=Medium.p_default))
      "Fluid connector b (positive design flow direction is from port_a to port_b), cold water return from the building"
      annotation (Placement(transformation(extent={{-294,50},{-314,70}}), iconTransformation(extent={{-100,-82},{-120,
              -62}})));
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
    Modelica.Fluid.Interfaces.FluidPort_a heaSup(
      h_outflow(start=Medium.h_default, nominal=Medium.h_default),
      redeclare final package Medium = Medium,
      p(start=Medium.p_default))
      "Fluid connector a (positive design flow direction is from port_a to port_b), hot water supply to the building"
      annotation (Placement(transformation(extent={{292,42},{312,62}}),   iconTransformation(extent={{100,-58},{120,
              -38}})));
    Modelica.Fluid.Interfaces.FluidPort_b heaRet(
      h_outflow(start=Medium.h_default, nominal=Medium.h_default),
      redeclare final package Medium = Medium,
      p(start=Medium.p_default))
      "Fluid connector b (positive design flow direction is from port_a to port_b), hot water return from the building"
      annotation (Placement(transformation(extent={{312,22},{292,42}}),  iconTransformation(extent={{120,-82},
            {100,-62}})));
  BaseClasses.HydraulicHeader cooRetHed(
    redeclare package Medium = Medium,
    m_flow_nominal=mEva_flow_nominal,
    nPorts_a=2,
    nPorts_b=1) "Return chilled water header.  " annotation (Placement(transformation(extent={{-148,70},{-128,50}})));
  BaseClasses.HydraulicHeader cooSupHed(
    redeclare package Medium = Medium,
    m_flow_nominal=mEva_flow_nominal,
    nPorts_a=1,
    nPorts_b=2) "Supply chilled water header. " annotation (Placement(transformation(extent={{-108,10},{-128,30}})));
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
    Modelica.Blocks.Interfaces.RealInput TSolSetHotTan
    "Setpoint temperature of the water inside the hot tank incase the solar system is active"
      annotation (Placement(transformation(extent={{-320,138},{-300,158}}),
          iconTransformation(extent={{-118,34},{-102,50}})));
    BaseClasses.SupplyTemperatureSet supTemSet(
      THeaWatSup_nominal=THeaWatSup_nominal,
      THeaWatRet_nominal=THeaWatRet_nominal,
      TCooWatSup_max=TCooWatSup_max,
      TCooWatSup_min=TCooWatSup_min,
      dTCooWat=dTCooWat)
      "Calculation of heating and cooling setpoint temperature"
      annotation (Placement(transformation(extent={{-248,240},{-228,260}})));
    Buildings.BoundaryConditions.WeatherData.Bus weaBus
      "Data bus that stores weather data"
      annotation (Placement(transformation(extent={{-330,256},{-290,296}}),
        iconTransformation(extent={{-120,86},{-100,106}})));
    Control.AmbientCircuitSid  ambCon
      "controller of the ambient hydraulic circuit"
      annotation (Placement(transformation(extent={{-144,-80},{-124,-60}})));
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
    Fluid.Actuators.Valves.TwoWayLinear valSupHea(
      redeclare final package Medium = Medium,
      use_inputFilter=false,
      dpFixed_nominal=0,
      show_T=true,
      dpValve_nominal=dp_nominal,
      riseTime=10,
      l=1e-8,
    m_flow_nominal=mGeo_flow_nominal + mHex_flow_nominal)
      "Two way modulating valve"
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
          origin={-68,-204})));
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
        rotation=90,  origin={-68,-140})));

    Fluid.Sensors.TemperatureTwoPort TBorLvg(
    allowFlowReversal=false,
    tau=0,
    redeclare final package Medium = Medium,
    m_flow_nominal=mGeo_flow_nominal) "Borefield system leaving water temperature"
    annotation (Placement(transformation(
        extent={{-10,-10},{10,10}},
        rotation=90,
        origin={-34,-214})));
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
          origin={112,-152})));
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
        Placement(transformation(
          extent={{-10,10},{10,-10}},
          rotation=270,
          origin={106,-104})));

    Fluid.Sensors.TemperatureTwoPort TDisHexLvg(
    allowFlowReversal=false,
    redeclare final package Medium = Medium,
    tau=10,
    m_flow_nominal=mHex_flow_nominal) "District heat exchanger leaving water temperature" annotation (Placement(
        transformation(
        extent={{-10,-10},{10,10}},
        rotation=90,
        origin={22,-200})));
    Modelica.Fluid.Interfaces.FluidPort_a disWatIn(
      p(start=Medium.p_default),
      redeclare final package Medium = Medium,
      h_outflow(start=Medium.h_default, nominal=Medium.h_default))
      "Fluid connector a (positive design flow direction is from port_a to port_b), district water inport"
      annotation (Placement(transformation(extent={{290,-202},{310,-182}}),
          iconTransformation(extent={{-22,-120},{-2,-100}})));
    Modelica.Fluid.Interfaces.FluidPort_b disWatOut(
      p(start=Medium.p_default),
      redeclare final package Medium = Medium,
      h_outflow(start=Medium.h_default, nominal=Medium.h_default))
      "Fluid connector b (positive design flow direction is from port_a to port_b), district water outport"
      annotation (Placement(transformation(extent={{310,-152},{290,-132}}),
          iconTransformation(extent={{22,-120},{2,-100}})));
    Fluid.Sensors.TemperatureTwoPort disRetTem(
      allowFlowReversal=false,
      tau=0,
      redeclare final package Medium = Medium,
      m_flow_nominal=mHex_flow_nominal)
      "District system return water temperature"
       annotation (Placement(
          transformation(
          extent={{-10,-10},{10,10}},
          rotation=0,
          origin={226,-142})));
    Fluid.Sensors.TemperatureTwoPort disSupTem(
      allowFlowReversal=false,
      tau=0,
      redeclare final package Medium = Medium,
      m_flow_nominal=mHex_flow_nominal)
      "District system supply water temperature"
       annotation (Placement(
          transformation(
          extent={{10,-10},{-10,10}},
          rotation=0,
          origin={224,-192})));
    Buildings.Controls.OBC.CDL.Continuous.Gain gaiBor(k=mGeo_flow_nominal)
     "Gain for mass flow rate of borefield"
      annotation (Placement(transformation(extent={{-110,-150},{-90,-130}})));
    Buildings.Controls.OBC.CDL.Continuous.Gain gaiMDisHex(k=mHex_flow_nominal) "Gain for mass flow of heat exchanger"
    annotation (Placement(transformation(extent={{40,-262},{60,-242}})));
    final parameter Fluid.Geothermal.Borefields.Data.Filling.Bentonite filDat(kFil=2.1)
      annotation (Placement(transformation(extent={{-292,-156},{-272,-136}})));
    final parameter Fluid.Geothermal.Borefields.Data.Soil.SandStone soiDat(
      kSoi=2.42,
      dSoi=1920,
      cSoi=1210)
      "Soil data"
       annotation (Placement(transformation(extent={{-292,-180},{-272,-160}})));
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
       cooBor={{dBorHol*mod((i - 1), nXBorHol),dBorHol*floor((i - 1)/
          nXBorHol)} for i in 1:nBorHol},
       xC=0.075,
       dp_nominal=dpBorFie_nominal)
      "Borefield configuration"
      annotation (Placement(transformation(extent={{-292,-204},{-272,-184}})));
    final parameter Fluid.Geothermal.Borefields.Data.Borefield.Template borFieDat(
       filDat=filDat,
       soiDat=soiDat,
       conDat=conDat)
      "Borefield parameters"
      annotation (Placement(transformation(extent={{-292,-228},{-272,-208}})));
    Fluid.Sensors.TemperatureTwoPort TAmbSup(
    allowFlowReversal=false,
    tau=0,
    redeclare final package Medium = Medium,
    m_flow_nominal=mGeo_flow_nominal) "ambient system supply water temperature to the borefield" annotation (Placement(
        transformation(
        extent={{-10,10},{10,-10}},
        rotation=270,
        origin={-68,-74})));
    parameter Fluid.HeatPumps.Data.EquationFitReversible.Generic heaPumDat
      "Performance data of the water to water heat pump"
      annotation (choicesAllMatching=true, Placement(
          transformation(extent={{-292,-252},{-272,-232}})));
    parameter Fluid.SolarCollectors.Data.GlazedFlatPlate.FP_GuangdongFSPTY95 solColDat
      "Performance data of glazed flat plate solar collector"
      annotation (Placement(transformation(extent={{-292,-280},{-272,-260}})));

    Control.EvaporatorCondenserPumpsController pumPrimCon
    "Primary circuit pumps control block"
    annotation (Placement(transformation(extent={{-120,158},{-100,178}})));

  Fluid.Sensors.MassFlowRate secCooFlo(redeclare package Medium = Media.Water)
    "Secondary circuit chilled water flow rate"
    annotation (Placement(transformation(extent={{-244,30},{-264,50}})));
  Fluid.Sensors.MassFlowRate secHeaFlo(redeclare package Medium = Media.Water)
    "Secondary circuit heating water flow rate"
    annotation (Placement(transformation(extent={{264,42},{284,62}})));
  Fluid.Sensors.MassFlowRate priCooFlo(redeclare package Medium = Media.Water)
    "Primary circuit evaporator side chilled water flow rate"
    annotation (Placement(transformation(extent={{-180,12},{-200,32}})));
  Fluid.Sensors.MassFlowRate priLoaFlo(redeclare package Medium = Media.Water)
    "Primary circuit load side heating water flow rate"
    annotation (Placement(transformation(extent={{126,50},{146,70}})));

    Modelica.Blocks.Interfaces.RealInput TSetHeaMax "Maximum heating setpoint temperature" annotation (Placement(
        transformation(extent={{-320,218},{-300,238}}), iconTransformation(extent={{-118,34},{-102,50}})));
    Fluid.Sensors.TemperatureTwoPort TBorEnt(
    allowFlowReversal=false,
    tau=0,
    redeclare final package Medium = Medium,
    m_flow_nominal=mGeo_flow_nominal) "Entering water temperature to the borefield system" annotation (Placement(
        transformation(
        extent={{-10,10},{10,-10}},
        rotation=270,
        origin={-68,-168})));
  Fluid.Actuators.Valves.ThreeWayEqualPercentageLinear valBor(
    redeclare package Medium = Medium,
    energyDynamics=Modelica.Fluid.Types.Dynamics.FixedInitial,
    m_flow_nominal=mGeo_flow_nominal,
    l={0.01,0.01},
    dpValve_nominal=6000)
    "Three way valve modulated to control the entering water temperature to the borefield system."
     annotation (Placement(transformation(
        extent={{10,-10},{-10,10}},
        rotation=90,
        origin={-68,-106})));
    Fluid.Sensors.TemperatureTwoPort TDisHex(
    allowFlowReversal=false,
    redeclare final package Medium = Medium,
    tau=10,
    m_flow_nominal=mHex_flow_nominal)
    "Entering water tmperature to the district heat exchanger" annotation (
      Placement(transformation(
        extent={{-10,10},{10,-10}},
        rotation=270,
        origin={106,-70})));
    Modelica.Blocks.Interfaces.RealInput TBorMaxEnt
    "Maximum allowed enetring water temperature to the borefiled holes." annotation (Placement(transformation(extent={{-320,
            -82},{-300,-62}}), iconTransformation(extent={{-118,34},{-102,50}})));
  Fluid.Actuators.Valves.ThreeWayEqualPercentageLinear valEva(
    redeclare package Medium = Medium,
    energyDynamics=Modelica.Fluid.Types.Dynamics.FixedInitial,
    m_flow_nominal=mEva_flow_nominal,
    l={0.01,0.01},
    dpValve_nominal=6000)
    "Three way valve modulated to control the entering water temperature to the borefield system." annotation (
      Placement(transformation(
        extent={{10,10},{-10,-10}},
        rotation=270,
        origin={-94,78})));
  Fluid.FixedResistances.Junction splVal2(
    dp_nominal={200,-200,-200},
    m_flow_nominal={mEva_flow_nominal,-mGeo_flow_nominal,mGeo_flow_nominal - mEva_flow_nominal},
    redeclare package Medium = Medium,
    energyDynamics=Modelica.Fluid.Types.Dynamics.SteadyState)
    "Flow splitter" annotation (Placement(transformation(
        extent={{-10,-10},{10,10}},
        rotation=270,
        origin={-60,78})));
  Fluid.Actuators.Valves.ThreeWayEqualPercentageLinear valBor2(
    redeclare package Medium = Medium,
    energyDynamics=Modelica.Fluid.Types.Dynamics.FixedInitial,
    m_flow_nominal=mCon_flow_nominal,
    l={0.01,0.01},
    dpValve_nominal=6000)
    "Three way valve modulated to control the entering water temperature to the borefield system."
     annotation (Placement(transformation(
        extent={{-10,-10},{10,10}},
        rotation=90,
        origin={-34,52})));
  Fluid.FixedResistances.Junction splVal1(
    dp_nominal=200*{1,-1,-1},
    m_flow_nominal={mGeo_flow_nominal,-mCon_flow_nominal,-mGeo_flow_nominal + mCon_flow_nominal},
    redeclare package Medium = Medium,
    energyDynamics=Modelica.Fluid.Types.Dynamics.SteadyState)
    "Flow splitter" annotation (Placement(transformation(
        extent={{-10,10},{10,-10}},
        rotation=90,
        origin={-34,-178})));
  Fluid.FixedResistances.Junction splVal3(
    dp_nominal={200,-200,200},
    m_flow_nominal={mCon_flow_nominal,-mGeo_flow_nominal,mGeo_flow_nominal - mCon_flow_nominal},
    redeclare package Medium = Medium,
    energyDynamics=Modelica.Fluid.Types.Dynamics.SteadyState)
    "Flow splitter" annotation (Placement(transformation(
        extent={{-10,-10},{10,10}},
        rotation=0,
        origin={42,132})));
    Modelica.Blocks.Interfaces.RealInput TMinConEnt "Minimum condenser entering water temperature" annotation (
      Placement(transformation(extent={{-320,184},{-300,204}}), iconTransformation(extent={{-118,34},{-102,50}})));
    Modelica.Blocks.Interfaces.RealInput TMaxEvaEnt "Maximum evaporator entering water temperature" annotation (
      Placement(transformation(extent={{-320,170},{-300,190}}), iconTransformation(extent={{-118,34},{-102,50}})));

equation
  connect(heaPum.port_b1,pumCon. port_a)
    annotation (Line(points={{-10,132},{0,132}}, color={238,46,47},
      pattern=LinePattern.Dash,
      thickness=0.5));
  connect(heaPumCon.yHeaPumMod, heaPum.uMod) annotation (Line(points={{-98.6,211.2},{-40,211.2},{-40,126},{-31,126}},
                                                            color={255,127,0}));
  connect(subCon.reqHea, heaPumCon.ReqHea)
    annotation (Line(
      points={{-179,219},{-140,219},{-140,219},{-121.4,219}},
      color={255,0,255},
      pattern=LinePattern.Dot));
  connect(subCon.reqCoo, heaPumCon.ReqCoo)
    annotation (Line(
      points={{-179,201},{-142,201},{-142,216},{-121.4,216}},
      color={255,0,255},
      pattern=LinePattern.Dot));
  connect(pumEva.port_b,TEvaEnt. port_a) annotation (Line(
      points={{-68,112},{-50,112},{-50,96},{-30,96}},
      color={238,46,47},
      pattern=LinePattern.Dash));
  connect(TEvaEnt.port_b, heaPum.port_a2) annotation (Line(
      points={{-10,96},{-6,96},{-6,120},{-10,120}},
      color={238,46,47},
      pattern=LinePattern.Dash));
  connect(heaRet, hotBufTan.port_a1)
    annotation (Line(points={{302,32},{266,32},{266,32.4},{180,32.4}},   color={0,127,255}));
  connect(cooRet, colBufTan.port_a)
    annotation (Line(points={{-304,60},{-234,60}},                     color={238,46,47}));
  connect(colBufTan.heaPorVol[1], topCooTan.port)
    annotation (Line(points={{-222,60},{-222,94},{-224,94}},color={191,0,0}));
  connect(colBufTan.heaPorVol[nSeg], botCooTan.port)
    annotation (Line(points={{-222,60},{-222,0},{-230,0}},
                                     color={191,0,0}));
  connect(hotBufTan.heaPorVol[nSeg], botHotTan.port)
    annotation (Line(points={{168,42},{168,-32},{176,-32}},color={191,0,0}));
  connect(hotBufTan.heaPorVol[1], topHotTan.port)
    annotation (Line(points={{168,42},{168,58},{214,58},{214,208}},
                                                      color={191,0,0}));
  connect(topHotTan.T, subCon.TTanHeaTop)
    annotation (Line(
      points={{194,208},{78,208},{78,232},{-202,232},{-202,219},{-201,219}},
      color={0,0,127},
      pattern=LinePattern.Dot));
  connect(botHotTan.T, subCon.TTanHeaBot)
    annotation (Line(
      points={{196,-32},{224,-32},{224,236},{-206,236},{-206,217},{-201,217}},
      color={0,0,127},
      pattern=LinePattern.Dot));
  connect(topCooTan.T, subCon.TTanCooTop)
    annotation (Line(
      points={{-244,94},{-266,94},{-266,201},{-201,201}},
      color={0,0,127},
      pattern=LinePattern.Dot));
  connect(botCooTan.T, subCon.TTanCooBot)
    annotation (Line(
      points={{-250,0},{-274,0},{-274,203},{-201,203}},
      color={0,0,127},
      pattern=LinePattern.Dot));
  connect(hotBufTan.mNor_flow, subCon.mTanHotNor)
    annotation (Line(points={{181.2,46.8},{236,46.8},{236,246},{-210,246},{-210,211},{-201,211}},
        color={0,0,127}));
  connect(weaBus, supTemSet.weaBus)
    annotation (Line(
      points={{-310,276},{-278,276},{-278,250},{-248,250}},
      color={255,204,51},
      thickness=0.5), Text(
      string="%first",
      index=-1,
      extent={{-6,3},{-6,3}},
      horizontalAlignment=TextAlignment.Right));
  connect(supTemSet.THeaSupSet, subCon.TSetHea)
    annotation (Line(
      points={{-227,254},{-208,254},{-208,215},{-201,215}},
      color={238,46,47},
      pattern=LinePattern.DashDot));
  connect(supTemSet.TCooSupSet, subCon.TSetCoo)
    annotation (Line(
      points={{-227,246},{-212,246},{-212,205},{-201,205}},
      color={85,170,255},
      pattern=LinePattern.DashDot));
  connect(hex.port_a1, pumHexDis.port_b)
    annotation (Line(points={{106,-142},{106,-114}},                                    color={0,127,255}));
  connect(disWatIn, disSupTem.port_a)
    annotation (Line(points={{300,-192},{234,-192}}, color={0,127,255}));
  connect(disSupTem.port_b, hex.port_a2)
    annotation (Line(points={{214,-192},{118,-192},{118,-162}},
                               color={0,127,255}));
  connect(hex.port_b2, disRetTem.port_a)
    annotation (Line(points={{118,-142},{216,-142}},
                                       color={0,127,255}));
  connect(disRetTem.port_b, disWatOut)
    annotation (Line(points={{236,-142},{300,-142}}, color={0,127,255}));
  connect(TBorLvg.port_a, borFie.port_b) annotation (Line(
      points={{-34,-224},{-34,-226},{-68,-226},{-68,-214}},
      color={28,108,200},
      thickness=0.5,
      pattern=LinePattern.DashDotDot));
  connect(hex.port_b1, TDisHexLvg.port_a) annotation (Line(
      points={{106,-162},{106,-226},{22,-226},{22,-210}},
      color={28,108,200},
      pattern=LinePattern.DashDotDot,
      thickness=0.5));
  connect(heaRetHed.ports_a[1], hotBufTan.port_b1)
    annotation (Line(points={{122.2,19.55},{150,19.55},{150,32.4},{156,32.4}},
                                                        color={0,127,255}));

  connect(subCon.pumEvamin, pumPrimCon.minEvaMasFlo)
    annotation (Line(points={{-179,211},{-148,211},{-148,166.2},{-120.8,166.2}},   color={0,0,127}));
  connect(subCon.pumConMin, pumPrimCon.minConMasFlo)
    annotation (Line(points={{-179,213},{-144,213},{-144,169.4},{-120.8,169.4}},       color={0,0,127}));
  connect(TMinConEnt, heaPumCon.TMinConEnt) annotation (Line(
      points={{-310,194},{-136,194},{-136,207.2},{-121,207.2}},
      color={0,0,127},
      pattern=LinePattern.Dash));
  connect(TMaxEvaEnt, heaPumCon.TMaxEvaEnt) annotation (Line(
      points={{-310,180},{-296,180},{-296,190},{-134,190},{-134,205.4},{-121,205.4}},
      color={0,0,127},
      pattern=LinePattern.Dash));
  connect(TEvaLvg.T,heaPumCon.TEvaLvg)  annotation (Line(
      points={{-78,31},{-78,36},{-124,36},{-124,201.6},{-121,201.6}},
      color={0,0,127},
      pattern=LinePattern.Dot));
  connect(heaPumCon.TSetHeaPum, heaPum.TSet) annotation (Line(points={{-98.6,214.6},{-38,214.6},{-38,135},{-31.4,135}},
                                                     color={0,0,127}));
  connect(subCon.reqHea, pumPrimCon.ReqHea) annotation (Line(points={{-179,219},{-140,219},{-140,178},{-121.4,178}},
                                                            color={255,0,255},
      pattern=LinePattern.Dot));
  connect(supTemSet.THeaSupSet, heaPumCon.TSetHea) annotation (Line(
      points={{-227,254},{-132,254},{-132,213},{-121,213}},
      color={238,46,47},
      pattern=LinePattern.DashDot));
  connect(supTemSet.TCooSupSet, heaPumCon.TSetCoo) annotation (Line(
      points={{-227,246},{-134,246},{-134,211},{-121,211}},
      color={85,170,255},
      pattern=LinePattern.DashDot));
  connect(colBufTan.port_b1, secCooFlo.port_a) annotation (Line(points={{-234,50.4},{-234,40},{-244,40}},
                                      color={0,127,255}));
  connect(cooSup, secCooFlo.port_b)
    annotation (Line(points={{-304,40},{-264,40}}, color={0,127,255}));
  connect(heaSup, secHeaFlo.port_b)
    annotation (Line(points={{302,52},{284,52}},   color={238,46,47}));
  connect(hotBufTan.port_b, secHeaFlo.port_a) annotation (Line(points={{180,42},{222,42},{222,52},{264,52}},
                                         color={238,46,47}));
  connect(hotBufTan.port_a, priLoaFlo.port_b)
    annotation (Line(points={{156,42},{150,42},{150,60},{146,60}},
                                                 color={238,46,47}));
  connect(colBufTan.port_a1,priCooFlo. port_b) annotation (Line(points={{-210,50.4},{-210,22},{-200,22}},
                                    color={0,127,255}));
  connect(secCooFlo.m_flow, pumPrimCon.mSecCoo) annotation (Line(points={{-254,51},{-254,162.6},{-120.8,162.6}},
                                            color={0,0,127}));
  connect(TSetHeaMax, heaPumCon.TSetHeaMax) annotation (Line(
      points={{-310,228},{-136,228},{-136,208.8},{-121,208.8}},
      color={0,0,127},
      pattern=LinePattern.DashDot));

  connect(pumHexDis.port_a, TDisHex.port_b)
    annotation (Line(points={{106,-94},{106,-80}}, color={0,127,255}));
  connect(ambCon.yBorThrVal, valBor.y)
    annotation (Line(points={{-123,-64.2},{-123,-64},{-100,-64},{-100,-106},{-80,-106}}, color={0,0,127}));
  connect(pumBor.port_b, TBorEnt.port_a) annotation (Line(points={{-68,-150},{-68,-158}}, color={0,127,255}));
  connect(borFie.port_a, TBorEnt.port_b) annotation (Line(points={{-68,-194},{-68,-178}}, color={0,127,255}));
  connect(TBorMaxEnt, ambCon.TBorMaxEnt) annotation (Line(
      points={{-310,-72},{-172,-72},{-172,-73},{-145,-73}},
      color={0,0,127},
      pattern=LinePattern.Dot));
  connect(TBorEnt.T, ambCon.TBorEnt) annotation (Line(
      points={{-79,-168},{-154,-168},{-154,-79.8},{-145,-79.8}},
      color={0,0,127},
      pattern=LinePattern.Dot));
  connect(TDisHexLvg.T, ambCon.TDisHexLvg) annotation (Line(
      points={{11,-200},{0,-200},{0,-282},{-160,-282},{-160,-77.2},{-145,-77.2}},
      color={0,0,127},
      pattern=LinePattern.Dot));
  connect(ambCon.TDisHexEnt, TDisHex.T) annotation (Line(
      points={{-145,-75.2},{-162,-75.2},{-162,-290},{90,-290},{90,-70},{95,-70}},
      color={0,0,127},
      pattern=LinePattern.Dot));
  connect(subCon.mTanColNor, colBufTan.mNor_flow)
    annotation (Line(points={{-201,209},{-204,209},{-204,64.8},{-208.8,64.8}}, color={0,0,127}));
  connect(TEvaLvg.port_b, cooSupHed.ports_a[1]) annotation (Line(points={{-88,20},{-88,19.7},{-107.8,19.7}},
                                                                                                color={0,127,255}));
  connect(priCooFlo.port_a, cooSupHed.ports_b[2]) annotation (Line(points={{-180,22},{-154,22},{-154,20.85},{-128.4,20.85}},
                                                                                                 color={0,127,255}));
  connect(cooSupHed.ports_b[1], valSupCoo.port_a)
    annotation (Line(points={{-128.4,19.35},{-128.4,-20},{-94,-20}},    color={0,127,255}));
  connect(colBufTan.port_b, cooRetHed.ports_a[2])
    annotation (Line(points={{-210,60},{-170,60},{-170,61.05},{-148.2,61.05}},
                                                                         color={0,127,255}));
  connect(pumEva.port_a, valEva.port_2) annotation (Line(points={{-88,112},{-94,112},{-94,88}}, color={0,127,255}));
  connect(cooRetHed.ports_b[1], valEva.port_1)
    annotation (Line(points={{-127.6,59.9},{-94,59.9},{-94,68}}, color={0,127,255}));
  connect(splVal2.port_3, valEva.port_3) annotation (Line(points={{-70,78},{-84,78}}, color={0,127,255}));
  connect(heaPum.port_b2, splVal2.port_1) annotation (Line(points={{-30,120},{-60,120},{-60,88}}, color={0,127,255}));
  connect(splVal2.port_2, TEvaLvg.port_a) annotation (Line(points={{-60,68},{-60,20},{-68,20}}, color={0,127,255}));
  connect(TDisHexLvg.port_b, ambHedSup.ports_a[1]) annotation (Line(points={{22,-190},{22,-166},{-22,-166},{-22,-131.495},
          {-0.2,-131.495}}, color={0,127,255}));
  connect(heaRetHed.ports_b[1], TConEnt.port_a)
    annotation (Line(points={{101.6,19.9},{50,19.9},{50,20},{0,20}},
                                                               color={0,127,255}));
  connect(heaSupHed.ports_b[1], valSupHea.port_a)
    annotation (Line(points={{112.4,59.35},{112.4,40},{60,40},{60,-20},{18,-20}},
                                                                           color={0,127,255}));
  connect(TConEnt.port_b, valBor2.port_1) annotation (Line(points={{-20,20},{-34,20},{-34,42}}, color={0,127,255}));
  connect(heaPum.port_a1, valBor2.port_2) annotation (Line(points={{-30,132},{-34,132},{-34,62}}, color={0,127,255}));
  connect(valBor.port_3, splVal1.port_3)
    annotation (Line(points={{-58,-106},{-50,-106},{-50,-178},{-44,-178}}, color={0,127,255}));
  connect(TBorLvg.port_b, splVal1.port_1) annotation (Line(points={{-34,-204},{-34,-188}}, color={0,127,255}));
  connect(splVal1.port_2, ambHedSup.ports_a[2])
    annotation (Line(points={{-34,-168},{-34,-129.845},{-0.2,-129.845}}, color={0,127,255}));
  connect(cooRetHed.ports_a[1], ambHedSup.ports_b[1]) annotation (Line(points={{-148.2,59.55},{-148.2,2},{28,2},{28,-130.285},
          {20.4,-130.285}}, color={0,127,255}));
  connect(ambHedSup.ports_b[2], heaRetHed.ports_a[2]) annotation (Line(points={{20.4,-131.935},{72,-131.935},{72,2},{122.2,
          2},{122.2,21.05}}, color={0,127,255}));
  connect(pumCon.port_b, splVal3.port_1) annotation (Line(points={{20,132},{32,132}}, color={0,127,255}));
  connect(TConLvg.port_a, splVal3.port_2) annotation (Line(points={{58,132},{52,132}}, color={0,127,255}));
  connect(TConLvg.port_b, heaSupHed.ports_a[1])
    annotation (Line(points={{78,132},{84,132},{84,59.7},{91.8,59.7}},
                                                                   color={0,127,255}));
  connect(valBor2.port_3, splVal3.port_3) annotation (Line(points={{-24,52},{42,52},{42,122}}, color={0,127,255}));
  connect(heaSupHed.ports_b[2], priLoaFlo.port_a)
    annotation (Line(points={{112.4,60.85},{120,60.85},{120,60},{126,60}},
                                                                   color={0,127,255}));
  connect(valSupCoo.port_b, ambRetHed.ports_a[1])
    annotation (Line(points={{-74,-20},{-24,-20},{-24,-47.55},{-29.8,-47.55}},
                                                                         color={0,127,255}));
  connect(ambRetHed.ports_a[2], valSupHea.port_b)
    annotation (Line(points={{-29.8,-49.05},{-12,-49.05},{-12,-20},{-2,-20}},
                                                                        color={0,127,255}));
  connect(ambRetHed.ports_b[2], TAmbSup.port_a)
    annotation (Line(points={{-50.4,-47.15},{-68,-47.15},{-68,-64}},
                                                             color={0,127,255}));
  connect(ambRetHed.ports_b[1], TDisHex.port_a)
    annotation (Line(points={{-50.4,-48.65},{-56,-48.65},{-56,-60},{106,-60}},
                                                                       color={0,127,255}));
  connect(pumBor.m_flow_in, gaiBor.y) annotation (Line(points={{-80,-140},{-88,-140}}, color={0,0,127}));
  connect(ambCon.yBorPum, gaiBor.u) annotation (Line(
      points={{-123,-75.2},{-116,-75.2},{-116,-140},{-112,-140}},
      color={0,0,127},
      pattern=LinePattern.Dash));
  connect(ambCon.yDisHexPum, gaiMDisHex.u) annotation (Line(
      points={{-123,-78.8},{-120,-78.8},{-120,-252},{38,-252}},
      color={0,0,127},
      pattern=LinePattern.Dash));
  connect(gaiMDisHex.y, pumHexDis.m_flow_in) annotation (Line(
      points={{62,-252},{80,-252},{80,-104},{94,-104}},
      color={0,0,127},
      pattern=LinePattern.Dash));
  connect(TConEnt.T, heaPumCon.TConEnt) annotation (Line(
      points={{-10,31},{-10,40},{-122,40},{-122,200.2},{-121,200.2}},
      color={0,0,127},
      pattern=LinePattern.Dot));
  connect(TEvaEnt.T, heaPumCon.TEvaEnt) annotation (Line(
      points={{-20,107},{-20,114},{-62,114},{-62,190},{-126,190},{-126,203.4},{-121,203.4}},
      color={0,0,127},
      pattern=LinePattern.Dot));
  connect(heaPumCon.yValEva1, valEva.y) annotation (Line(
      points={{-98.6,204.4},{-96,204.4},{-96,114},{-116,114},{-116,78},{-106,78}},
      color={0,0,127},
      pattern=LinePattern.Dash));
  connect(heaPumCon.yValEva, valBor2.y) annotation (Line(
      points={{-98.6,207.6},{-54,207.6},{-54,52},{-46,52}},
      color={0,0,127},
      pattern=LinePattern.Dash));
  connect(pumPrimCon.mSecHea, secHeaFlo.m_flow)
    annotation (Line(points={{-120.8,173.8},{-130,173.8},{-130,190},{274,190},{274,63}}, color={0,0,127}));
  connect(priLoaFlo.m_flow, pumPrimCon.mPriHea)
    annotation (Line(points={{136,71},{136,186},{-128,186},{-128,176.4},{-120.8,176.4}}, color={0,0,127}));
  connect(priCooFlo.m_flow, pumPrimCon.mPriEva)
    annotation (Line(points={{-190,33},{-190,160},{-120.8,160}}, color={0,0,127}));
  connect(subCon.reqCoo, pumPrimCon.ReqCoo) annotation (Line(
      points={{-179,201},{-166,201},{-166,158.2},{-121.4,158.2}},
      color={255,0,255},
      pattern=LinePattern.Dot));
  connect(subCon.ValHeaPos, valSupHea.y)
    annotation (Line(
      points={{-179,209},{-152,209},{-152,-2},{8,-2},{8,-8}},
      color={28,108,200},
      pattern=LinePattern.DashDot));
  connect(subCon.ValCooPos, valSupCoo.y)
    annotation (Line(
      points={{-179,207},{-154,207},{-154,-8},{-84,-8}},
      color={28,108,200},
      pattern=LinePattern.DashDot));
  connect(subCon.reqCoo, ambCon.requireCold) annotation (Line(
      points={{-179,201},{-166,201},{-166,-71},{-145,-71}},
      color={255,0,255},
      pattern=LinePattern.Dot));
  connect(ambCon.requireHeat, subCon.reqHea) annotation (Line(
      points={{-145,-60.2},{-156,-60.2},{-156,219},{-179,219}},
      color={255,0,255},
      pattern=LinePattern.Dot));
  connect(subCon.ValHea, ambCon.valHea) annotation (Line(
      points={{-179,217},{-158,217},{-158,-62.4},{-145,-62.4}},
      color={255,0,255},
      pattern=LinePattern.Dot));
  connect(subCon.ValCoo, ambCon.valCoo) annotation (Line(
      points={{-179,215},{-160,215},{-160,-64.8},{-145,-64.8}},
      color={255,0,255},
      pattern=LinePattern.Dot));
  connect(ambCon.rejColFulLoa, subCon.rejColFulLoa) annotation (Line(
      points={{-145,-69},{-164,-69},{-164,202.8},{-179,202.8}},
      color={255,0,255},
      pattern=LinePattern.Dot));
  connect(subCon.rejHeaFulLoa, ambCon.rejHeaFulLoa) annotation (Line(
      points={{-179,204.8},{-162,204.8},{-162,-67},{-145,-67}},
      color={255,0,255},
      pattern=LinePattern.Dot));
  connect(pumPrimCon.yPumCon, pumCon.y) annotation (Line(points={{-99,176},{10,176},{10,144}}, color={0,0,127}));
  connect(pumPrimCon.yPumEva, pumEva.y) annotation (Line(points={{-99,160},{-78,160},{-78,124}}, color={0,0,127}));
  connect(TAmbSup.port_b, valBor.port_1) annotation (Line(points={{-68,-84},{-68,-96}}, color={0,127,255}));
  connect(valBor.port_2, pumBor.port_a) annotation (Line(points={{-68,-116},{
          -68,-130},{-68,-130}}, color={0,127,255}));
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
          defaultComponentName="SubSta",
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
