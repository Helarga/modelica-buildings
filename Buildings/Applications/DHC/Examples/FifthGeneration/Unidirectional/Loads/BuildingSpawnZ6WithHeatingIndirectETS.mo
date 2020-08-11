within Buildings.Applications.DHC.Examples.FifthGeneration.Unidirectional.Loads;
model BuildingSpawnZ6WithHeatingIndirectETS
  "Model of a building (Spawn 6 zones) with an energy transfer station"
  package MediumW = Buildings.Media.Water;
  extends BaseClasses.PartialBuildingWithCoolingIndirectETS(
      m1_flow_nominal=mBuiHea_flow_nominal,
      m2_flow_nominal=mDis_flow_nominal,
      redeclare package Medium1 =MediumW,
      redeclare package Medium2 =MediumW,
      show_T = true,
    redeclare DHC.Loads.Examples.BaseClasses.BuildingSpawnZ6 bui(
      final idfName=idfName,
      final weaName=weaName,
      nPorts_aChiWat=1,
      nPorts_bChiWat=1,
      nPorts_bHeaWat=1,
      nPorts_aHeaWat=1));

  parameter String idfName=
    "modelica://Buildings/Resources/Data/ThermalZones/EnergyPlus/Validation/RefBldgSmallOffice/RefBldgSmallOfficeNew2004_Chicago.idf"
    "Name of the IDF file"
    annotation(Dialog(group="Building model parameters"));
  parameter String weaName=
    "modelica://Buildings/Resources/weatherdata/USA_IL_Chicago-OHare.Intl.AP.725300_TMY3.mos"
    "Name of the weather file"
    annotation(Dialog(group="Building model parameters"));
  parameter Modelica.SIunits.MassFlowRate mDis_flow_nominal= bui.disFloCoo.m_flow_nominal*(bui.delTBuiCoo/bui.delTDisCoo)
   "Nominal mass flow rate of primary (district) district cooling side";

  //bui.disFloCoo.m_flow_nominal
  parameter Modelica.SIunits.MassFlowRate mBuiHea_flow_nominal= bui.disFloHea.m_flow_nominal
    "Nominal mass flow rate of secondary (building) district cooling side";

  parameter Modelica.SIunits.MassFlowRate mBui_flow_nominal= bui.disFloCoo.m_flow_nominal
    "Nominal mass flow rate of secondary (building) district cooling side";

  Buildings.Applications.DHC.EnergyTransferStations.Heating.HeatingIndirect
                                                                    ets(
    redeclare package Medium =MediumW,
    final mDis_flow_nominal=mDis_flow_nominal,
    final mBui_flow_nominal=mBui_flow_nominal,
    dp1_nominal=500,
    dp2_nominal=500,
    use_Q_flow_nominal=true,
    Q_flow_nominal=(sum(bui.terUni.QHea_flow_nominal)),
    T_a1_nominal=273.15 + 45,
    T_a2_nominal=273.15 + 55)
    "Energy transfer station model"
    annotation (Placement(transformation(extent={{-28,-84},{32,-24}})));

  Modelica.Fluid.Sources.FixedBoundary preSou(redeclare package Medium = MediumW,
      nPorts=1)
    annotation (Placement(transformation(extent={{-80,-100},{-60,-80}})));
equation
  connect(TSetWat, ets.TSetBuiSup) annotation (Line(points={{-120,20},{-88,20},{
          -88,-54},{-34,-54}}, color={0,0,127}));
  connect(port_a2, bui.ports_aChiWat[1]) annotation (Line(points={{100,-60},{60,
          -60},{60,80},{-60,80},{-60,20},{-30,20}}, color={0,127,255}));
  connect(bui.ports_bChiWat[1], port_b2) annotation (Line(points={{30,20},{50,20},
          {50,-6},{-80,-6},{-80,-60},{-100,-60}}, color={0,127,255}));
  connect(port_a1, ets.port_a1) annotation (Line(points={{-100,60},{-72,60},{-72,
          -36},{-28,-36}}, color={0,127,255}));
  connect(ets.port_b1, port_b1) annotation (Line(points={{32,-36},{82,-36},{82,60},
          {100,60}}, color={0,127,255}));
  connect(bui.ports_bHeaWat[1], ets.port_a2) annotation (Line(points={{30,32},{54,
          32},{54,-72},{32,-72}}, color={0,127,255}));
  connect(ets.port_b2, bui.ports_aHeaWat[1]) annotation (Line(points={{-28,-72},
          {-46,-72},{-46,32},{-30,32}}, color={0,127,255}));
  connect(ets.port_b2, preSou.ports[1]) annotation (Line(points={{-28,-72},{-46,
          -72},{-46,-90},{-60,-90}}, color={0,127,255}));
  annotation (Icon(graphics={
          Bitmap(extent={{-72,-62},{62,74}},
          fileName="modelica://Buildings/Resources/Images/ThermalZones/EnergyPlus/EnergyPlusLogo.png")}),
      Diagram(coordinateSystem(extent={{-100,-140},{100,100}})));
end BuildingSpawnZ6WithHeatingIndirectETS;
