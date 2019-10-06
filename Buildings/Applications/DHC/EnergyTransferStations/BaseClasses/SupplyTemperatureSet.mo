within Buildings.Applications.DHC.EnergyTransferStations.BaseClasses;
model SupplyTemperatureSet "Calculation of supply water temperature setpoint"
  extends Modelica.Blocks.Icons.Block;

  /* ------------- Parameters used in residential heating loop ------------- */
  parameter Modelica.SIunits.Temperature THeaWatSup_nominal=314.15
    "Nominal heating supply water temperature"
    annotation (Dialog(tab="ResidentialHeating", group="HeatingSupplyReturn"));
  parameter Modelica.SIunits.Temperature THeaWatRet_nominal=303.15
    "Nominal heating Return water temperature"
    annotation (Dialog(tab="ResidentialHeating", group="HeatingSupplyReturn"));
  parameter Modelica.SIunits.Temperature THeaWatOut_nominal=263.15
    "Outside temperature"
    annotation (Dialog(tab="ResidentialHeating", group="HeatingSupplyReturn"));
  parameter Modelica.SIunits.Temperature THeaWatRoo_nominal=293.15
    "Nominal room temperature when supply heat"
    annotation (Dialog(tab="ResidentialHeating", group="HeatingSupplyReturn"));
  parameter Modelica.SIunits.TemperatureDifference dTOutHeaBal=5
    "Offset for heating curve"
    annotation (Dialog(tab="ResidentialHeating", group="HeatingSupplyReturn"));
  parameter Modelica.SIunits.TemperatureDifference dTHeaWat_min=4
    "Minimum supply and return temperature difference"
    annotation (Dialog(tab="ResidentialHeating", group="HeatingSupplyReturn"));
  /* ------------- Parameters used in residential cooling loop ------------- */
  parameter Modelica.SIunits.Temperature TCooWatSup_max=288.15
    "Maximum cooling water supply temperature"
    annotation (Dialog(tab="ResidentialCooling",group="CoolingWaterSupplyReturnSetting"));
  parameter Modelica.SIunits.Temperature TCooWatSup_min=277.15
    "Minimum cooling water supply temperature"
    annotation (Dialog(tab="ResidentialCooling",group="CoolingWaterSupplyReturnSetting"));
  parameter Modelica.SIunits.Temperature TCooWatSup_ini=280.15
    "Cooling water supply temperature when it starts to do dehumidification"
    annotation (Dialog(tab="ResidentialCooling", group="CoolingWaterSupplyReturnSetting"));
  parameter Modelica.SIunits.MassFraction Xi_ini=7.5e-3
    "Outdoor humidity ratio when it starts to do dehumidification"
    annotation (Dialog(tab="ResidentialCooling", group="CoolingWaterSupplyReturnSetting"));
  parameter Modelica.SIunits.MassFraction dXi=0.5e-3
    annotation (Dialog(tab="ResidentialCooling", group="CoolingWaterSupplyReturnSetting"));
  parameter Modelica.SIunits.TemperatureDifference dTCooWat=4
    "Cooling water supply and return temperature difference"
    annotation (Dialog(tab="ResidentialCooling", group="CoolingWaterSupplyReturnSetting"));
  /* ----------------------------------------------------------------------- */

  Buildings.Controls.OBC.CDL.Interfaces.RealOutput TCooSupSet(final unit="K",
      displayUnit="degC") "Cooling supply temperature setpoint"
    annotation (Placement(transformation(extent={{120,-50},{140,-30}}),
        iconTransformation(extent={{100,-50},{120,-30}})));
  Buildings.Controls.OBC.CDL.Interfaces.RealOutput TCooRetSet(final unit="K",
      displayUnit="degC") "Cooling return temperature setpoint"
    annotation (Placement(transformation(extent={{120,-10},{140,10}}),
      iconTransformation(extent={{100,-100},{120,-80}})));

 Buildings.Applications.DHC.EnergyTransferStations.BaseClasses.HeatingWaterSupplyReturn heaWatSupRet(
    THeaWatSup_nominal=THeaWatSup_nominal,
    THeaWatRet_nominal=THeaWatRet_nominal,
    THeaWatOut_nominal=THeaWatOut_nominal,
    THeaWatRoo_nominal=THeaWatRoo_nominal,
    dTOutHeaBal=dTOutHeaBal,
    dT_min=dTHeaWat_min)
    annotation (Placement(transformation(extent={{-60,6},{-40,26}})));
  Buildings.Utilities.Psychrometrics.X_pTphi x_pTphi
    annotation (Placement(transformation(extent={{-60,-50},{-40,-30}})));
  Buildings.Utilities.Psychrometrics.ToDryAir toDryAir
    annotation (Placement(transformation(extent={{0,-50},{20,-30}})));
  Buildings.Applications.DHC.EnergyTransferStations.BaseClasses.CoolingWaterSupplyTemperature cooSupTem_set(
    TSup_max=TCooWatSup_max,
    TSup_min=TCooWatSup_min,
    TSup_ini=TCooWatSup_ini,
    Xi_ini=Xi_ini,
    dXi=dXi) "Cooling supply water temperature setpoint"
    annotation (Placement(transformation(extent={{40,-50},{60,-30}})));
  Modelica.Blocks.Sources.Constant const1(final k=dTCooWat)
    "Temperature difference between cooling supply and return water"
    annotation (Placement(transformation(extent={{40,-10},{60,10}})));
  Modelica.Blocks.Math.Add cooRetTem_set
    "Cooling return water temperature setpoint"
    annotation (Placement(transformation(extent={{80,-10},{100,10}})));
  Buildings.BoundaryConditions.WeatherData.Bus weaBus
    annotation (Placement(transformation(extent={{-120,-20},{-80,20}}),
      iconTransformation(extent={{-110,-10},{-90,10}})));

  Buildings.Controls.OBC.CDL.Interfaces.RealOutput THeaSupSet(final unit="K",
      displayUnit="degC") "Heating supply temperature setpoint for space loads"
    annotation (Placement(transformation(extent={{120,50},{140,70}}),
        iconTransformation(extent={{100,30},{120,50}})));
equation
  connect(weaBus.TDryBul, heaWatSupRet.TOut) annotation (Line(
      points={{-100,0},{-80,0},{-80,16},{-62,16}},
      color={255,204,51},
      thickness=0.5), Text(
      string="%first",
      index=-1,
      extent={{-6,3},{-6,3}},
      horizontalAlignment=TextAlignment.Right));
  connect(weaBus.pAtm, x_pTphi.p_in) annotation (Line(
      points={{-100,0},{-80,0},{-80,-34},{-62,-34}},
      color={255,204,51},
      thickness=0.5), Text(
      string="%first",
      index=-1,
      extent={{-6,3},{-6,3}},
      horizontalAlignment=TextAlignment.Right));
  connect(weaBus.TDryBul, x_pTphi.T) annotation (Line(
      points={{-100,0},{-80,0},{-80,-40},{-62,-40}},
      color={255,204,51},
      thickness=0.5), Text(
      string="%first",
      index=-1,
      extent={{-6,3},{-6,3}},
      horizontalAlignment=TextAlignment.Right));
  connect(weaBus.relHum, x_pTphi.phi) annotation (Line(
      points={{-100,0},{-80,0},{-80,-46},{-62,-46}},
      color={255,204,51},
      thickness=0.5), Text(
      string="%first",
      index=-1,
      extent={{-6,3},{-6,3}},
      horizontalAlignment=TextAlignment.Right));
  connect(x_pTphi.X[1], toDryAir.XiTotalAir)
    annotation (Line(points={{-39,-40},{-1,-40}}, color={0,0,127}));
  connect(toDryAir.XiDry, cooSupTem_set.uXi)
    annotation (Line(points={{21,-40},{38,-40}}, color={0,0,127}));
  connect(cooSupTem_set.TSup, cooRetTem_set.u2) annotation (Line(points={{61,-40},
          {70,-40},{70,-6},{78,-6}},   color={0,0,127}));
  connect(const1.y, cooRetTem_set.u1) annotation (Line(points={{61,0},{70,0},{70,
          6},{78,6}},       color={0,0,127}));
  connect(cooRetTem_set.y, TCooRetSet) annotation (Line(points={{101,0},{130,0}},
                                color={0,0,127}));
  connect(cooSupTem_set.TSup, TCooSupSet) annotation (Line(points={{61,-40},{130,
          -40}},                    color={0,0,127}));
  connect(heaWatSupRet.TSup, THeaSupSet) annotation (Line(points={{-39,21},{40,21},
          {40,60},{130,60}},     color={0,0,127}));
  annotation (Diagram(
        coordinateSystem(preserveAspectRatio=false, extent={{-120,-60},{120,140}})));
end SupplyTemperatureSet;
