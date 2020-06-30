within Buildings.Applications.DHC.Examples.FifthGeneration.Unidirectional.Loads;
model BuildingSpawnZ6WithCoolingIndirectETS
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
      T_aChiWat_nominal=280.15,
      T_bChiWat_nominal=285.15,
      nPorts_aHeaWat=1,
      nPorts_bHeaWat=1,
      nPorts_aChiWat=1,
      nPorts_bChiWat=1));

  parameter String idfName=
    "modelica://Buildings/Resources/Data/ThermalZones/EnergyPlus/Validation/RefBldgSmallOffice/RefBldgSmallOfficeNew2004_Chicago.idf"
    "Name of the IDF file"
    annotation(Dialog(group="Building model parameters"));
  parameter String weaName=
    "modelica://Buildings/Resources/weatherdata/USA_IL_Chicago-OHare.Intl.AP.725300_TMY3.mos"
    "Name of the weather file"
    annotation(Dialog(group="Building model parameters"));
  parameter Modelica.SIunits.MassFlowRate mDis_flow_nominal=bui.disFloCoo.m_flow_nominal*(bui.delTBuiCoo/bui.delTDisCoo)
   "Nominal mass flow rate of primary (district) district cooling side";
  //bui.disFloCoo.m_flow_nominal
  parameter Modelica.SIunits.MassFlowRate mBuiHea_flow_nominal= bui.disFloHea.m_flow_nominal
    "Nominal mass flow rate of secondary (building) district cooling side";

  parameter Modelica.SIunits.MassFlowRate mBui_flow_nominal= bui.disFloCoo.m_flow_nominal
    "Nominal mass flow rate of secondary (building) district cooling side";

  Buildings.Applications.DHC.EnergyTransferStations.CoolingIndirect ets(
    redeclare package Medium =MediumW,
    final mDis_flow_nominal=mDis_flow_nominal,
    final mBui_flow_nominal=mBui_flow_nominal,
    dp1_nominal=500,
    dp2_nominal=500,
    use_Q_flow_nominal=true,
    Q_flow_nominal=-1*(sum(bui.terUni.QCoo_flow_nominal)),
    T_a1_nominal=273.15 + 5,
    T_a2_nominal=273.15 + 12)
    "Energy transfer station model"
    annotation (Placement(transformation(extent={{-28,-88},{32,-28}})));

  Modelica.Fluid.Sources.FixedBoundary preSou(redeclare package Medium = MediumW,
      nPorts=1)
    annotation (Placement(transformation(extent={{-98,-86},{-78,-66}})));
  Buildings.Fluid.Actuators.Valves.TwoWayLinear val(
    redeclare final package Medium = MediumW,
    final dpValve_nominal=7000,
    final use_inputFilter=false,
    final m_flow_nominal=mDis_flow_nominal,
    final linearized=false)
    "Mixing valve"
    annotation (Placement(transformation(extent={{10,10},{-10,-10}}, origin={50,-96})));
  Buildings.Controls.Continuous.LimPID con(
    final controllerType=Modelica.Blocks.Types.SimpleController.PI,
    final k=1,
    final yMax=1,
    final yMin=0.1,
    final Ti=300,
    final initType=Modelica.Blocks.Types.InitPID.InitialOutput,
    final y_start=1,
    final reverseActing=true)
    "Controller"
    annotation (Placement(transformation(extent={{-12,-124},{8,-104}})));
  Modelica.Blocks.Sources.RealExpression mDis_flowSet(y=0.6)
    annotation (Placement(transformation(extent={{-44,-124},{-24,-104}})));
  Modelica.Blocks.Sources.RealExpression mDis_mea(y=port_a2.m_flow)
    annotation (Placement(transformation(extent={{-44,-144},{-24,-124}})));
equation
  connect(port_a1, bui.ports_aHeaWat[1]) annotation (Line(points={{-100,60},{-80,
          60},{-80,32},{-30,32}}, color={0,127,255}));
  connect(bui.ports_bHeaWat[1], port_b1) annotation (Line(points={{30,32},{80,32},
          {80,60},{100,60}}, color={0,127,255}));
  connect(TSetChiWat,ets. TSetBuiSup) annotation (Line(points={{-120,20},{-88,20},
          {-88,-58},{-34,-58}}, color={0,0,127}));
  connect(preSou.ports[1],ets. port_b2) annotation (Line(points={{-78,-76},{-28,
          -76}},                color={0,127,255}));
  connect(ets.port_b2, bui.ports_aChiWat[1]) annotation (Line(points={{-28,-76},
          {-70,-76},{-70,20},{-30,20}}, color={0,127,255}));
  connect(ets.port_a2, bui.ports_bChiWat[1]) annotation (Line(points={{32,-76},{
          74,-76},{74,20},{30,20}}, color={0,127,255}));
  connect(con.y, val.y) annotation (Line(points={{9,-114},{50,-114},{50,-108}},  color={0,0,127}));
  connect(con.u_s, mDis_flowSet.y) annotation (Line(points={{-14,-114},{-23,-114}}, color={0,0,127}));
  connect(val.port_b, ets.port_a1) annotation (Line(points={{40,-96},{-50,-96},
          {-50,-40},{-28,-40}}, color={0,127,255}));
  connect(mDis_mea.y, con.u_m) annotation (Line(points={{-23,-134},{-2,-134},{
          -2,-126}}, color={0,0,127}));
  connect(ets.port_b1, port_b2) annotation (Line(points={{32,-40},{52,-40},{52,0},
          {-60,0},{-60,-60},{-100,-60}}, color={0,127,255}));
  connect(port_a2, val.port_a) annotation (Line(points={{100,-60},{80,-60},{80,-96},
          {60,-96}}, color={0,127,255}));
  annotation (Icon(graphics={
          Bitmap(extent={{-72,-62},{62,74}},
          fileName="modelica://Buildings/Resources/Images/ThermalZones/EnergyPlus/EnergyPlusLogo.png")}),
      Diagram(coordinateSystem(extent={{-100,-140},{100,100}})));
end BuildingSpawnZ6WithCoolingIndirectETS;
