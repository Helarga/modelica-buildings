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
    SolarThermalModule solTheMod(
      redeclare package Medium = Medium,
      rho=rho,
      nColType=nColType,
      sysConfig=sysConfig,
      per=solColDat,
      nPanels=nPanels,
      nSeg=nSeg,
      lat= lat,
      azi=azi,
      til=til)
      "Module of the solar thermal system"
      annotation (Placement(transformation(extent={{106,256},{126,276}})));
    Buildings.Fluid.HeatPumps.EquationFitWaterToWater heaPum(
      energyDynamics=Modelica.Fluid.Types.Dynamics.FixedInitial,
      massDynamics=Modelica.Fluid.Types.Dynamics.FixedInitial,
      per =     heaPumDat,
      redeclare package Medium1 = Medium,
      redeclare package Medium2 = Medium)
      annotation (Placement(transformation(extent={{-36,116},{-16,136}})));
    Buildings.Fluid.Movers.SpeedControlled_y     pumCon(
      redeclare package Medium = Medium,
      energyDynamics=Modelica.Fluid.Types.Dynamics.SteadyState,
      addPowerToMedium=false,
      show_T=show_T,
      per(pressure(dp={2*dpCon_nominal,0}, V_flow={0,2*mCon_flow_nominal/1000})),
      allowFlowReversal=false,
      use_inputFilter=false,
      riseTime=10)
      "Pump (or valve) that forces the flow rate to be set to the control signal"
      annotation (Placement(transformation(extent={{6,120},{26,140}})));
    Buildings.Fluid.Movers.SpeedControlled_y pumSol(
      redeclare package Medium = Medium,
      energyDynamics=Modelica.Fluid.Types.Dynamics.SteadyState,
      addPowerToMedium=false,
      show_T=show_T,
      per(pressure(dp={2*dpSol_nominal,0}, V_flow={0,2*mSol_flow_nominal/1000})),
      allowFlowReversal=false,
      use_inputFilter=false,
      riseTime=10)
      "Pump (or valve) that forces the flow rate to be set to the control signal"
      annotation (Placement(transformation(extent={{162,116},{182,136}})));
    Control.HeatPumpController heaPumCon "Control of the heatpump model"
      annotation (Placement(transformation(extent={{-120,198},{-100,218}})));
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
          origin={-90,98})));
   Control.SubstationUO subCon
    "Substation control "
      annotation (Placement(transformation(extent={{-200,200},{-180,220}})));
    Buildings.Fluid.Sensors.TemperatureTwoPort TConLvg(
      redeclare final package Medium = Medium,
      allowFlowReversal=false,
      m_flow_nominal=mCon_flow_nominal,
      tau=0)
      "Heating water supply temperature" annotation (Placement(
          transformation(
          extent={{10,10},{-10,-10}},
          rotation=180,
          origin={48,130})));
    Buildings.Fluid.Sensors.TemperatureTwoPort TConEnt(
      redeclare final package Medium = Medium,
      allowFlowReversal=false,
      m_flow_nominal=mCon_flow_nominal,
      tau=0)
      "Condenser entering water temperature"
      annotation (Placement(transformation(extent={{-2,16},{-22,36}})));
    Buildings.Fluid.Sensors.TemperatureTwoPort TEvaEnt(
    redeclare final package Medium = Medium,
    allowFlowReversal=false,
    m_flow_nominal=mEva_flow_nominal,
    tau=0) "Cooling return water temperature" annotation (Placement(
        transformation(
        extent={{10,10},{-10,-10}},
        rotation=180,
        origin={-20,98})));
    Buildings.Fluid.Sensors.TemperatureTwoPort TEvaLvg(
      redeclare final package Medium = Medium,
      allowFlowReversal=false,
      m_flow_nominal=mEva_flow_nominal,
      tau=30)
      "Evaporator leaving water temperature"
      annotation (Placement(transformation(extent={{-56,10},{-76,30}})));
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
      annotation (Placement(transformation(extent={{-228,32},{-204,56}})));
    Modelica.Fluid.Interfaces.FluidPort_a cooSup(
      h_outflow(start=Medium.h_default, nominal=Medium.h_default),
      redeclare final package Medium = Medium,
      p(start=Medium.p_default))
      "Fluid connector a (positive design flow direction is from port_a to port_b), cold water supply to the building"
      annotation (Placement(transformation(extent={{-314,14},{-294,34}}), iconTransformation(extent={{-120,
            -58},{-100,-38}})));
    Modelica.Fluid.Interfaces.FluidPort_b cooRet(
      h_outflow(start=Medium.h_default, nominal=Medium.h_default),
      redeclare final package Medium = Medium,
      p(start=Medium.p_default))
      "Fluid connector b (positive design flow direction is from port_a to port_b), cold water return from the building"
      annotation (Placement(transformation(extent={{-294,34},{-314,54}}), iconTransformation(extent={{-100,-82},{-120,
              -62}})));
    BaseClasses.HeatingSupplyHeader heaSupHed(
      m_flow_nominal=m_flow_nominal,
      nPorts_a=2,nPorts_b=2)
      "Heating supply water header"
      annotation (Placement(transformation(extent={{90,54},{116,74}})));
    BaseClasses.CoolingReturnHeader heaRetHed(
      m_flow_nominal=m_flow_nominal,
      nPorts_a=2, nPorts_b=2)
      "Heating return water header"
      annotation (Placement(transformation(extent={{74,10},{46,-12}})));
    Modelica.Fluid.Interfaces.FluidPort_a heaSup(
      h_outflow(start=Medium.h_default, nominal=Medium.h_default),
      redeclare final package Medium = Medium,
      p(start=Medium.p_default))
      "Fluid connector a (positive design flow direction is from port_a to port_b), hot water supply to the building"
      annotation (Placement(transformation(extent={{292,154},{312,174}}), iconTransformation(extent={{100,-58},{120,
              -38}})));
    Modelica.Fluid.Interfaces.FluidPort_b heaRet(
      h_outflow(start=Medium.h_default, nominal=Medium.h_default),
      redeclare final package Medium = Medium,
      p(start=Medium.p_default))
      "Fluid connector b (positive design flow direction is from port_a to port_b), hot water return from the building"
      annotation (Placement(transformation(extent={{312,134},{292,154}}),iconTransformation(extent={{120,-82},
            {100,-62}})));
    BaseClasses.CoolingReturnHeader cooRetHed(
      m_flow_nominal=m_flow_nominal,
      nPorts_a=2, nPorts_b=1)
      annotation (Placement(transformation(extent={{-116,80},{-90,60}})));
    BaseClasses.CoolingSupplyHeader cooSupHed(
      m_flow_nominal=m_flow_nominal,
      nPorts_a=2,
    nPorts_b=1)
      annotation (Placement(transformation(extent={{-126,8},{-100,30}})));
    Modelica.Thermal.HeatTransfer.Sensors.TemperatureSensor topCooTan
      "Cold tank top temperature"
      annotation (Placement(transformation(extent={{-232,84},{-252,104}})));
    Modelica.Thermal.HeatTransfer.Sensors.TemperatureSensor botCooTan
      "Cold tank bottom temperature"
      annotation (Placement(transformation(extent={{-230,-26},{-250,-6}})));
    Modelica.Thermal.HeatTransfer.Sensors.TemperatureSensor topHotTan
      "Hot tank top temperature"
      annotation (Placement(transformation(extent={{200,186},{180,206}})));
    Modelica.Thermal.HeatTransfer.Sensors.TemperatureSensor botHotTan
      "Hot tank bottom temperature"
      annotation (Placement(transformation(extent={{176,-42},{196,-22}})));
    Modelica.Blocks.Interfaces.RealInput TSolSetHotTan
      "Setpoint temperature of the water inside the hot tank incase the solar system is active"
      annotation (Placement(transformation(extent={{-340,128},{-300,168}}),
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
    Control.AmbientCircuitController ambCon
      "controller of the ambient hydraulic circuit"
      annotation (Placement(transformation(extent={{-214,-162},{-234,-142}})));
    BaseClasses.AmbientReturnHeader ambRetHed(
      m_flow_nominal=m_flow_nominal,
      nPorts_b=2, nPorts_a=2)
      "ambient circuit return header"
      annotation (Placement(transformation(extent={{-54,-62},{-20,-36}})));
    BaseClasses.AmbientSupplyHeader ambSupHed(
      m_flow_nominal=m_flow_nominal,
      nPorts_a=2, nPorts_b=2)
      "Ambient circuit supply header"
      annotation (Placement(transformation(extent={{-14,-112},{20,-142}})));
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
      annotation (Placement(transformation(extent={{18,-32},{-2,-12}})));
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
      annotation (Placement(transformation(extent={{-94,-40},{-74,-20}})));
    Fluid.Geothermal.Borefields.OneUTube           borFie(
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
          origin={-90,-194})));
    Fluid.Movers.FlowControlled_m_flow           pumBor(
      redeclare package Medium = Medium,
      energyDynamics=Modelica.Fluid.Types.Dynamics.SteadyState,
      addPowerToMedium=false,
      nominalValuesDefineDefaultPressureCurve=true,
      show_T=show_T,
      m_flow_nominal=mGeo_flow_nominal,
      per(pressure(dp={2*dpBorFie_nominal,0}, V_flow={0,2*mGeo_flow_nominal/1000})),
      use_inputFilter=false,
      riseTime=10)
      "Pump (or valve) that forces the flow rate to be set to the control signal"
       annotation (Placement(transformation(extent={{10,10},{-10,-10}},
        rotation=90,  origin={-90,-136})));

    Fluid.Sensors.TemperatureTwoPort           geoTem(
      allowFlowReversal=false,
      tau=0,
      redeclare final package Medium = Medium,
      m_flow_nominal=mGeo_flow_nominal)
      "Borefield system supply water temperature"
       annotation (Placement(transformation(
          extent={{-10,-10},{10,10}},
          rotation=90,
          origin={-32,-200})));
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
      addPowerToMedium=false,
      nominalValuesDefineDefaultPressureCurve=true,
      show_T=show_T,
      m_flow_nominal=mHex_flow_nominal,
      per(pressure(dp={2*dpHex_nominal,0}, V_flow={0,2*mHex_flow_nominal/1000})),
      use_inputFilter=false,
      riseTime=10)
      "Pump (or valve) that forces the flow rate to be set to the control signal"
       annotation (
        Placement(transformation(
          extent={{-10,10},{10,-10}},
          rotation=270,
          origin={106,-102})));

    Fluid.Sensors.TemperatureTwoPort           cirSupTem(
      allowFlowReversal=false,
      redeclare final package Medium = Medium,
      tau=10,
      m_flow_nominal=mHex_flow_nominal)
      "Supply temperature after district HEX"
      annotation (Placement(transformation(extent={{-10,-10},{10,10}},
          rotation=90,  origin={22,-200})));
    Modelica.Fluid.Interfaces.FluidPort_a disWatIn(
      p(start=Medium.p_default),
      redeclare final package Medium = Medium,
      h_outflow(start=Medium.h_default, nominal=Medium.h_default))
      "Fluid connector a (positive design flow direction is from port_a to port_b), district water inport"
      annotation (Placement(transformation(extent={{296,-202},{316,-182}}),
          iconTransformation(extent={{-22,-120},{-2,-100}})));
    Modelica.Fluid.Interfaces.FluidPort_b disWatOut(
      p(start=Medium.p_default),
      redeclare final package Medium = Medium,
      h_outflow(start=Medium.h_default, nominal=Medium.h_default))
      "Fluid connector b (positive design flow direction is from port_a to port_b), district water outport"
      annotation (Placement(transformation(extent={{314,-152},{294,-132}}),
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
    Fluid.Actuators.Valves.TwoWayLinear valRetCoo(
      redeclare final package Medium = Medium,
      use_inputFilter=false,
      dpFixed_nominal=0,
      show_T=true,
      dpValve_nominal=dp_nominal,
      riseTime=10,
      l=1e-8,
      m_flow_nominal=mGeo_flow_nominal + mHex_flow_nominal)
      "Two way modulating valve"
      annotation (Placement(transformation(extent={{-106,-78},{-126,-58}})));
    Fluid.Actuators.Valves.TwoWayLinear valRetHea(
      redeclare final package Medium = Medium,
      use_inputFilter=false,
      dpFixed_nominal=0,
      show_T=true,
      dpValve_nominal=dp_nominal,
      riseTime=10,
      l=1e-8,
      m_flow_nominal=mGeo_flow_nominal + mHex_flow_nominal)
      "Two way modulating valve"
      annotation (Placement(transformation(extent={{-10,-10},{10,10}},
        rotation=90,
        origin={46,-50})));
    Buildings.Controls.OBC.CDL.Continuous.Gain        gaiBor(k=mGeo_flow_nominal)
     "Gain for mass flow rate of borefield"
      annotation (Placement(transformation(extent={{-80,-272},{-60,-252}})));
    Buildings.Controls.OBC.CDL.Continuous.Gain           gaiMHex(k=mHex_flow_nominal)
     "Gain for mass flow of heat exchanger"
      annotation (Placement(transformation(extent={{24,-270},{44,-250}})));
    final parameter Fluid.Geothermal.Borefields.Data.Filling.Bentonite filDat(kFil=2.1)
      annotation (Placement(transformation(extent={{-292,-156},{-272,-136}})));
    final parameter Fluid.Geothermal.Borefields.Data.Soil.SandStone           soiDat(
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
    Fluid.Sensors.TemperatureTwoPort ambSup(
      allowFlowReversal=false,
      tau=0,
      redeclare final package Medium = Medium,
      m_flow_nominal=mGeo_flow_nominal)
      "ambient system supply water temperature to the borefield"
      annotation (
        Placement(transformation(
          extent={{-10,10},{10,-10}},
          rotation=270,
          origin={-68,-90})));
    parameter Fluid.HeatPumps.Data.EquationFitWaterToWater.Generic_EquationFit heaPumDat
      "Performance data of the water to water heat pump"
      annotation (choicesAllMatching=true, Placement(
          transformation(extent={{-292,-252},{-272,-232}})));
    parameter Fluid.SolarCollectors.Data.GlazedFlatPlate.FP_GuangdongFSPTY95 solColDat
      "Performance data of glazed flat plate solar collector"
      annotation (Placement(transformation(extent={{-292,-280},{-272,-260}})));

    Control.CondenserAndEvaporatorPumpsController pumEvaConCon
      "Condenser and evaporator pumps controller"
      annotation (Placement(transformation(extent={{-120,160},{-100,180}})));
    Fluid.Movers.FlowControlled_dp               pumBor1(
    redeclare package Medium = Medium,
    energyDynamics=Modelica.Fluid.Types.Dynamics.SteadyState,
    addPowerToMedium=false,
    nominalValuesDefineDefaultPressureCurve=true,
    show_T=show_T,
    m_flow_nominal=mGeo_flow_nominal,
    per(pressure(dp={2*dpBorFie_nominal,0}, V_flow={0,2*mGeo_flow_nominal/1000})),
    use_inputFilter=false,
    riseTime=10)
      "Pump (or valve) that forces the flow rate to be set to the control signal"
       annotation (Placement(transformation(extent={{10,10},{-10,-10}},
        rotation=180, origin={-252,-86})));

  Modelica.Thermal.FluidHeatFlow.Sensors.RelPressureSensor relPressureSensor
    annotation (Placement(transformation(extent={{-234,-66},{-214,-46}})));
equation
  connect(heaPum.port_b1, pumCon.port_a)
    annotation (Line(points={{-16,132},{-6,132},{-6,130},{6,130}},
                                                 color={238,46,47},
      pattern=LinePattern.Dash,
      thickness=0.5));
  connect(solTheMod.pumSol, pumSol.y)
    annotation (Line(points={{127,274.4},{172,274.4},{172,138}},
                            color={0,0,127}));
  connect(heaPumCon.heaPumMod, heaPum.uMod)
    annotation (Line(points={{-98.6,209.111},{-56,209.111},{-56,126},{-35.4545,
          126}},                                        color={255,127,0}));
  connect(subCon.reqHea, heaPumCon.ReqHea)
    annotation (Line(
      points={{-179.3,219.1},{-138,219.1},{-138,219.111},{-121.4,219.111}},
      color={255,0,255},
      pattern=LinePattern.Dot));
  connect(subCon.reqCoo, heaPumCon.ReqCoo)
    annotation (Line(
      points={{-179.2,200.6},{-138,200.6},{-138,215.778},{-121.4,215.778}},
      color={255,0,255},
      pattern=LinePattern.Dot));
  connect(pumCon.port_b, TConLvg.port_a)
    annotation (Line(points={{26,130},{38,130}},
                                               color={238,46,47},
      pattern=LinePattern.Dash,
      thickness=0.5));
  connect(TConEnt.port_b, heaPum.port_a1)
    annotation (Line(points={{-22,26},{-40,26},{-40,132},{-34.1818,132}},
                                        color={0,127,255}));
  connect(pumEva.port_b, TEvaEnt.port_a) annotation (Line(
      points={{-80,98},{-30,98}},
      color={238,46,47},
      pattern=LinePattern.Dash));
  connect(TEvaEnt.port_b, heaPum.port_a2) annotation (Line(
      points={{-10,98},{-4,98},{-4,120},{-16,120}},
      color={238,46,47},
      pattern=LinePattern.Dash));
  connect(TEvaLvg.port_a, heaPum.port_b2)
    annotation (Line(
      points={{-56,20},{-48,20},{-48,120},{-34.1818,120}},
      color={0,0,255},
      thickness=0.5));
  connect(hotBufTan.port_b, heaSup)
    annotation (Line(points={{180,42},{258,42},{258,164},{302,164}}, color={238,46,47}));
  connect(heaRet, hotBufTan.port_a1)
    annotation (Line(points={{302,144},{266,144},{266,32.4},{180,32.4}}, color={0,127,255}));
  connect(colBufTan.port_b1, cooSup)
    annotation (Line(points={{-228,34.4},{-304,34.4},{-304,24}},           color={0,127,255}));
  connect(cooRet, colBufTan.port_a)
    annotation (Line(points={{-304,44},{-228,44}},                     color={238,46,47}));
  connect(solTheMod.port_b, pumSol.port_a)
    annotation (Line(points={{127,266},{146,266},{146,126},{162,126}},
                                     color={238,46,47}));
  connect(colBufTan.heaPorVol[1], topCooTan.port)
    annotation (Line(points={{-216,44},{-216,94},{-232,94}},color={191,0,0}));
  connect(colBufTan.heaPorVol[nSeg], botCooTan.port)
    annotation (Line(points={{-216,44},{-216,-16},{-230,-16}},
                                     color={191,0,0}));
  connect(hotBufTan.heaPorVol[nSeg], botHotTan.port)
    annotation (Line(points={{168,42},{168,-32},{176,-32}},color={191,0,0}));
  connect(hotBufTan.heaPorVol[1], topHotTan.port)
    annotation (Line(points={{168,42},{168,58},{200,58},{200,196}},
                                                      color={191,0,0}));
  connect(topHotTan.T, subCon.TTanHeaTop)
    annotation (Line(
      points={{180,196},{62,196},{62,232},{-204,232},{-204,220},{-201,220}},
      color={0,0,127},
      pattern=LinePattern.Dot));
  connect(botHotTan.T, subCon.TTanHeaBot)
    annotation (Line(
      points={{196,-32},{224,-32},{224,236},{-206,236},{-206,217.2},{-201,217.2}},
      color={0,0,127},
      pattern=LinePattern.Dot));
  connect(topCooTan.T, subCon.TTanCooTop)
    annotation (Line(
      points={{-252,94},{-266,94},{-266,200.4},{-201,200.4}},
      color={0,0,127},
      pattern=LinePattern.Dot));
  connect(botCooTan.T, subCon.TTanCooBot)
    annotation (Line(
      points={{-250,-16},{-274,-16},{-274,203},{-201,203}},
      color={0,0,127},
      pattern=LinePattern.Dot));
  connect(colBufTan.mNor_flow, subCon.mTanColNor)
    annotation (Line(points={{-202.8,48.8},{-178,48.8},{-178,144},{-222,144},{
          -222,208.6},{-201,208.6}},
        color={0,0,127}));
  connect(hotBufTan.mNor_flow, subCon.mTanHotNor)
    annotation (Line(points={{181.2,46.8},{236,46.8},{236,246},{-210,246},{-210,
          212},{-201,212}},
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
      points={{-227,254},{-208,254},{-208,214.6},{-201,214.6}},
      color={238,46,47},
      pattern=LinePattern.DashDot));
  connect(supTemSet.TCooSupSet, subCon.TSetCoo)
    annotation (Line(
      points={{-227,246},{-212,246},{-212,206},{-201,206}},
      color={85,170,255},
      pattern=LinePattern.DashDot));
  connect(topHotTan.T, solTheMod.TTanHeaTop)
    annotation (Line(
      points={{180,196},{78,196},{78,272},{105.2,272}},
      color={0,0,127},
      pattern=LinePattern.Dot));
  connect(TSolSetHotTan, solTheMod.TSetSol)
    annotation (Line(
      points={{-320,148},{62,148},{62,260},{105.2,260}},
      color={238,46,47},
      pattern=LinePattern.DashDotDot));
  connect(supTemSet.THeaSupSet, heaPum.TConSet)
    annotation (Line(
      points={{-227,254},{-52,254},{-52,138},{-35.4545,138},{-35.4545,135}},
      color={238,46,47},
      pattern=LinePattern.DashDotDot));
  connect(subCon.ValHea, ambCon.valHea)
    annotation (Line(
      points={{-179.2,217.2},{-164,217.2},{-164,-147.4},{-213,-147.4}},
      color={255,0,255},
      pattern=LinePattern.Dot));
  connect(subCon.ValCoo, ambCon.valCoo)
    annotation (Line(
      points={{-179.2,202.4},{-170,202.4},{-170,-155},{-213,-155}},
      color={255,0,255},
      pattern=LinePattern.Dot));
  connect(subCon.reqHea, ambCon.reqHea)
    annotation (Line(
      points={{-179.3,219.1},{-162,219.1},{-162,-144.8},{-213,-144.8}},
      color={255,0,255},
      pattern=LinePattern.Dot));
  connect(ambCon.reqCoo, subCon.reqCoo)
    annotation (Line(
      points={{-213,-157.4},{-172,-157.4},{-172,200.6},{-179.2,200.6}},
      color={255,0,255},
      pattern=LinePattern.Dot));
  connect(ambCon.rejHeaFulLoa, subCon.rejHeaFulLoa)
    annotation (Line(
      points={{-213,-150},{-166,-150},{-166,215.2},{-179.2,215.2}},
      color={255,0,255},
      pattern=LinePattern.Dot));
  connect(subCon.rejColFulLoa, ambCon.rejCooFulLoa)
    annotation (Line(
      points={{-179.2,204.4},{-168,204.4},{-168,-152.4},{-213,-152.4}},
      color={255,0,255},
      pattern=LinePattern.Dot));
  connect(hex.port_a1, pumHexDis.port_b)
    annotation (Line(points={{106,-142},{106,-112}},                                    color={0,127,255}));
  connect(disWatIn, disSupTem.port_a)
    annotation (Line(points={{306,-192},{234,-192}}, color={0,127,255}));
  connect(disSupTem.port_b, hex.port_a2)
    annotation (Line(points={{214,-192},{118,-192},{118,-162}},
                               color={0,127,255}));
  connect(hex.port_b2, disRetTem.port_a)
    annotation (Line(points={{118,-142},{216,-142}},
                                       color={0,127,255}));
  connect(disRetTem.port_b, disWatOut)
    annotation (Line(points={{236,-142},{304,-142}}, color={0,127,255}));
  connect(geoTem.port_a, borFie.port_b)
    annotation (Line(
      points={{-32,-210},{-32,-226},{-90,-226},{-90,-204}},
      color={28,108,200},
      thickness=0.5,
      pattern=LinePattern.DashDotDot));
  connect(borFie.port_a,pumBor. port_b)
    annotation (Line(
      points={{-90,-184},{-90,-146}},
      color={0,140,72},
      pattern=LinePattern.DashDotDot,
      thickness=0.5));
  connect(gaiBor.y,pumBor. m_flow_in)
    annotation (Line(points={{-58,-262},{-50,-262},{-50,-136},{-78,-136}},
                                        color={0,0,127},
      pattern=LinePattern.Dash));
  connect(gaiMHex.y, pumHexDis.m_flow_in)
    annotation (Line(points={{46,-260},{70,-260},{70,-102},{94,-102}}, color={0,0,127},
      pattern=LinePattern.Dash));
  connect(subCon.ValHeaPos, valSupHea.y)
    annotation (Line(
      points={{-179.1,208.5},{-152,208.5},{-152,-2},{8,-2},{8,-10}},
      color={28,108,200},
      pattern=LinePattern.DashDot));
  connect(subCon.ValCooPos, valSupCoo.y)
    annotation (Line(
      points={{-179.1,206.3},{-154,206.3},{-154,-8},{-84,-8},{-84,-18}},
      color={28,108,200},
      pattern=LinePattern.DashDot));
  connect(valRetCoo.y, subCon.ValCooPos)
    annotation (Line(
      points={{-116,-56},{-116,-8},{-154,-8},{-154,206.3},{-179.1,206.3}},
      color={28,108,200},
      pattern=LinePattern.DashDot));
  connect(geoTem.T, ambCon.TBorOut)
    annotation (Line(
      points={{-43,-200},{-48,-200},{-48,-236},{-126,-236},{-126,-162},{-213,
          -162}},
      color={0,0,127},
      pattern=LinePattern.Dot));
  connect(hex.port_b1, cirSupTem.port_a)
    annotation (Line(
      points={{106,-162},{106,-226},{22,-226},{22,-210}},
      color={28,108,200},
      pattern=LinePattern.DashDotDot,
      thickness=0.5));
  connect(subCon.ValHeaPos, valRetHea.y)
    annotation (Line(
      points={{-179.1,208.5},{-152,208.5},{-152,-2},{34,-2},{34,-50}},
      color={28,108,200},
      pattern=LinePattern.DashDot));
  connect(pumEva.port_a, cooRetHed.ports_a[1])
    annotation (Line(points={{-100,
          98},{-117.04,98},{-117.04,68}}, color={0,127,255}));
  connect(colBufTan.port_b, cooRetHed.ports_a[2])
    annotation (Line(points={{-204,44},{-156,44},{-156,72},{-117.04,72}},
                                                      color={0,127,255}));
  connect(valRetCoo.port_b, cooRetHed.ports_b[1])
    annotation (Line(points={{-126,-68},{-136,-68},{-136,38},{-78,38},{-78,70},
          {-88.7,70}},                                                  color={
          0,127,255}));
  connect(cooSupHed.ports_a[1], valSupCoo.port_a)
    annotation (Line(points={{-127.04,21.2},{-127.04,-30},{-94,-30}},
                                                  color={0,127,255}));
  connect(ambRetHed.ports_b[1], valSupHea.port_b)
    annotation (Line(points={{
          -18.3,-51.6},{-18.3,-22},{-2,-22}}, color={0,127,255}));
  connect(pumHexDis.port_a, ambRetHed.ports_b[2])
    annotation (Line(
      points={{106,-92},{106,-84},{-2,-84},{-2,-46.4},{-18.3,-46.4}},
      color={0,127,255},
      pattern=LinePattern.DashDotDot,
      thickness=0.5));
  connect(ambRetHed.ports_a[1], valSupCoo.port_b)
    annotation (Line(points={{-55.36,-46.4},{-55.36,-30},{-74,-30}},
                                                 color={0,127,255}));
  connect(ambRetHed.ports_a[2], ambSup.port_a)
    annotation (Line(
      points={{-55.36,-51.6},{-68,-51.6},{-68,-80}},
      color={0,127,255},
      pattern=LinePattern.DashDotDot,
      thickness=0.5));
  connect(pumBor.port_a, ambSup.port_b)
    annotation (Line(points={{-90,-126},{-90,-102},{-68,-102},{-68,-100}},
                                                    color={0,127,255}));
  connect(ambSup.T, ambCon.TAmbRetHed)
    annotation (Line(
      points={{-79,-90},{-128,-90},{-128,-142.2},{-213,-142.2}},
      color={0,0,127},
      pattern=LinePattern.Dot));
  connect(heaRetHed.ports_a[1], hotBufTan.port_b1)
    annotation (Line(points={{75.12,-3.2},{142,-3.2},{142,32.4},{156,32.4}},
                                                        color={0,127,255}));
  connect(heaRetHed.ports_b[1], valRetHea.port_b)
    annotation (Line(points={{44.6,1.2},{44.6,-40},{46,-40}},
                                          color={0,127,255}));
  connect(geoTem.port_b, ambSupHed.ports_a[1])
    annotation (Line(points={{-32,-190},{-32,-130},{-15.36,-130}},
                                                      color={0,127,255}));
  connect(ambSupHed.ports_a[2], valRetCoo.port_a)
    annotation (Line(points={{-15.36,-124},{-32,-124},{-32,-68},{-106,-68}},
                                                      color={0,127,255}));
  connect(ambSupHed.ports_b[1], cirSupTem.port_b)
    annotation (Line(points={{21.7,-124},{21.7,-190},{22,-190}},
                                            color={0,127,255}));
  connect(heaRetHed.ports_b[2], TConEnt.port_a)
    annotation (Line(points={{44.6,
          -3.2},{44.6,26},{-2,26}}, color={0,127,255}));
  connect(heaRetHed.ports_a[2], solTheMod.port_a)
    annotation (Line(points={{75.12,1.2},{75.12,266},{105,266}},
                                          color={0,127,255}));
  connect(TConLvg.port_b, heaSupHed.ports_a[1])
    annotation (Line(points={{58,130},{70,130},{70,66},{88.96,66}},
                                        color={0,127,255}));
  connect(heaSupHed.ports_a[2], valSupHea.port_a)
    annotation (Line(points={{88.96,
          62},{88,62},{88,-22},{18,-22}}, color={0,127,255}));
  connect(weaBus, solTheMod.weaBus)
    annotation (Line(
      points={{-310,276},{82,276},{82,275.6},{105,275.6}},
      color={255,204,51},
      thickness=0.5), Text(
      string="%first",
      index=-1,
      extent={{-6,3},{-6,3}},
      horizontalAlignment=TextAlignment.Right));
  connect(heaSupHed.ports_b[1], pumSol.port_b)
    annotation (Line(points={{117.3,66},{194,66},{194,126},{182,126}},
                                         color={238,46,47}));
  connect(heaSupHed.ports_b[2], hotBufTan.port_a)
    annotation (Line(points={{117.3,
          62},{148,62},{148,42},{156,42}}, color={238,46,47}));

  connect(cooSupHed.ports_a[2], colBufTan.port_a1) annotation (Line(points={{
          -127.04,16.8},{-182,16.8},{-182,34.4},{-204,34.4}}, color={0,127,255}));
  connect(cooSupHed.ports_b[1], TEvaLvg.port_b) annotation (Line(points={{-98.7,
          19},{-90,19},{-90,20},{-76,20}}, color={0,127,255}));
  connect(cirSupTem.T, ambCon.TAmbSupHed) annotation (Line(
      points={{11,-200},{-6,-200},{-6,-240},{-122,-240},{-122,-159.6},{-213,
          -159.6}},
      color={0,0,127},
      pattern=LinePattern.Dot));
  connect(gaiBor.u, ambCon.pumBor) annotation (Line(
      points={{-82,-262},{-242,-262},{-242,-160},{-235,-160}},
      color={0,0,127},
      pattern=LinePattern.Dot));
  connect(gaiMHex.u, ambCon.pumHexDis) annotation (Line(
      points={{22,-260},{-40,-260},{-40,-280},{-260,-280},{-260,-155},{-235,
          -155}},
      color={0,0,127},
      pattern=LinePattern.Dot));
  connect(ambSupHed.ports_b[2], valRetHea.port_a) annotation (Line(points={{
          21.7,-130},{21.7,-70},{44,-70},{44,-60},{46,-60}}, color={0,127,255}));
  connect(supTemSet.TCooSupSet, heaPum.TEvaSet) annotation (Line(
      points={{-227,246},{-212,246},{-212,118},{-120,118},{-120,117},{-35.4545,
          117}},
      color={85,170,255},
      pattern=LinePattern.DashDot));

  connect(pumEvaConCon.yPumCon, pumCon.y) annotation (Line(points={{-99,177.8},
          {16,177.8},{16,142}}, color={0,0,127}));
  connect(pumEvaConCon.yPumEva, pumEva.y) annotation (Line(points={{-99,162.2},
          {-90,162.2},{-90,110}}, color={0,0,127}));
  connect(subCon.pumConMin, pumEvaConCon.pumConMin) annotation (Line(points={{
          -179.1,213.1},{-132,213.1},{-132,179.1},{-120.7,179.1}}, color={0,0,
          127}));
  connect(subCon.pumEvamin, pumEvaConCon.pumEvaMin) annotation (Line(points={{
          -179.1,210.7},{-150,210.7},{-150,162.1},{-120.7,162.1}}, color={0,0,
          127}));
  connect(supTemSet.THeaSupSet, pumEvaConCon.TSetHea) annotation (Line(
      points={{-227,254},{-134,254},{-134,176.3},{-120.7,176.3}},
      color={238,46,47},
      pattern=LinePattern.DashDotDot));
  connect(supTemSet.TCooSupSet, pumEvaConCon.TSetCoo) annotation (Line(
      points={{-227,246},{-212,246},{-212,164.5},{-120.7,164.5}},
      color={85,170,255},
      pattern=LinePattern.DashDotDot));
  connect(TEvaEnt.T, pumEvaConCon.TEvaEnt) annotation (Line(points={{-20,109},{
          -20,114},{-130,114},{-130,168.1},{-120.7,168.1}}, color={0,0,127}));
  connect(TEvaLvg.T, pumEvaConCon.TEvaLvg) annotation (Line(
      points={{-66,31},{-66,80},{-128,80},{-128,166.1},{-120.7,166.1}},
      color={0,0,127},
      pattern=LinePattern.Dot));
  connect(TConEnt.T, pumEvaConCon.TConEnt) annotation (Line(
      points={{-12,37},{-12,78},{-134,78},{-134,174.5},{-120.7,174.5}},
      color={0,0,127},
      pattern=LinePattern.Dot));
  connect(TConLvg.T, pumEvaConCon.TConLvg) annotation (Line(points={{48,141},{
          48,154},{-132,154},{-132,172.5},{-120.7,172.5}}, color={0,0,127}));
  connect(heaPumCon.heaPumModRea, pumEvaConCon.heaPumMod) annotation (Line(
      points={{-99,204.667},{-94,204.667},{-94,188},{-126,188},{-126,170.1},{
          -121.3,170.1}},
      color={0,0,127},
      pattern=LinePattern.Dot));
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
