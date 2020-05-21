within Buildings.Applications.DHC.Loads.Examples.BaseClasses;
model BuildingSpawnZ6_modified_scalable
  "Six-zone EnergyPlus building model based on URBANopt GeoJSON export, with distribution pumps"
  extends Buildings.Applications.DHC.Loads.BaseClasses.PartialBuilding(
    redeclare package Medium = MediumW,
    final have_eleHea=false,
    final have_eleCoo=false,
    final have_weaBus=false);

  package MediumA = Buildings.Media.Air   "Air medium model";
  package MediumW = Buildings.Media.Water "Water medium model";
  parameter Integer nZon = 6
    "Number of thermal zones";
  parameter Modelica.SIunits.TemperatureDifference delTBuiCoo=5
  "Nominal building supply and return chilled water temperature difference";
  parameter Integer facSca=1
    "Scaling factor to be applied to on each extensive quantity";
  parameter String idfPat=
    "modelica://Buildings/Resources/Data/ThermalZones/EnergyPlus/Validation/RefBldgSmallOfficeNew2004_Chicago.idf"
    "Path of the IDF file";
  parameter String weaPat=
    "modelica://Buildings/Resources/weatherdata/USA_IL_Chicago-OHare.Intl.AP.725300_TMY3.mos"
    "Path of the weather file";

  Buildings.Controls.OBC.CDL.Continuous.Sources.Constant minTSet[nZon](k=fill(
        20 + 273.15, nZon),
    each y(final unit="K", displayUnit="degC"))
    "Minimum temperature setpoint"
    annotation (Placement(transformation(extent={{-220,60},{-200,80}})));
  Buildings.Controls.OBC.CDL.Continuous.Sources.Constant maxTSet[nZon](k=fill(
        24 + 273.15, nZon),
    each y(final unit="K", displayUnit="degC"))
    "Maximum temperature setpoint"
    annotation (Placement(transformation(extent={{-220,20},{-200,40}})));

  Modelica.Blocks.Sources.Constant qConGai_flow(k=0) "Convective heat gain"
    annotation (Placement(transformation(extent={{-66,128},{-46,148}})));
  Modelica.Blocks.Sources.Constant qRadGai_flow(k=0) "Radiative heat gain"
    annotation (Placement(transformation(extent={{-66,168},{-46,188}})));
  Modelica.Blocks.Routing.Multiplex3 multiplex3_1
    annotation (Placement(transformation(extent={{-20,128},{0,148}})));
  Modelica.Blocks.Sources.Constant qLatGai_flow(k=0) "Latent heat gain"
    annotation (Placement(transformation(extent={{-66,88},{-46,108}})));
  Buildings.ThermalZones.EnergyPlus.ThermalZone znAttic(
    redeclare package Medium = MediumA,
    zoneName="Attic",
    nPorts=2) "Thermal zone"
    annotation (Placement(transformation(extent={{24,84},{64,124}})));
  Buildings.ThermalZones.EnergyPlus.ThermalZone znCore_ZN(
    redeclare package Medium = MediumA,
    zoneName="Core_ZN",
    nPorts=2) "Thermal zone"
    annotation (Placement(transformation(extent={{24,42},{64,82}})));
  Buildings.ThermalZones.EnergyPlus.ThermalZone znPerimeter_ZN_1(
    redeclare package Medium = MediumA,
    zoneName="Perimeter_ZN_1",
    nPorts=2) "Thermal zone"
    annotation (Placement(transformation(extent={{24,0},{64,40}})));
  Buildings.ThermalZones.EnergyPlus.ThermalZone znPerimeter_ZN_2(
    redeclare package Medium = MediumA,
    zoneName="Perimeter_ZN_2",
    nPorts=2) "Thermal zone"
    annotation (Placement(transformation(extent={{24,-40},{64,0}})));
  Buildings.ThermalZones.EnergyPlus.ThermalZone znPerimeter_ZN_3(
    redeclare package Medium = MediumA,
    zoneName="Perimeter_ZN_3",
    nPorts=2) "Thermal zone"
    annotation (Placement(transformation(extent={{24,-80},{64,-40}})));
  Buildings.ThermalZones.EnergyPlus.ThermalZone znPerimeter_ZN_4(
    redeclare package Medium = MediumA,
    zoneName="Perimeter_ZN_4",
    nPorts=2) "Thermal zone"
    annotation (Placement(transformation(extent={{24,-120},{64,-80}})));
  inner Buildings.ThermalZones.EnergyPlus.Building building(
    idfName=Modelica.Utilities.Files.loadResource(idfPat),
    weaName=Modelica.Utilities.Files.loadResource(weaPat))
    "Building outer component"
    annotation (Placement(transformation(extent={{30,138},{52,158}})));
  Buildings.Controls.OBC.CDL.Continuous.MultiSum mulSum(nin=6)
    annotation (Placement(transformation(extent={{260,110},{280,130}})));
  Buildings.Controls.OBC.CDL.Continuous.MultiSum mulSum3(nin=2) if have_pum
    annotation (Placement(transformation(extent={{260,70},{280,90}})));
  Buildings.Applications.DHC.Loads.Examples.BaseClasses.FanCoil4Pipe terUni[
    nZon](
    redeclare package Medium1 = MediumW,
    redeclare package Medium2 = MediumA,
    each facSca=facSca,
    QHea_flow_nominal={50000,10000,10000,10000,10000,10000},
    QCoo_flow_nominal={-10000,-10000,-10000,-10000,-10000,-10000},
    each T_aLoaHea_nominal=293.15,
    each T_aLoaCoo_nominal=297.15,
    each T_bHeaWat_nominal=308.15,
    each T_bChiWat_nominal=292.15,
    each T_aHeaWat_nominal=313.15,
    each T_aChiWat_nominal=287.15,
    each mLoaHea_flow_nominal=5,
    each mLoaCoo_flow_nominal=5) "Terminal unit"
    annotation (Placement(transformation(extent={{-140,-2},{-120,20}})));
  Buildings.Applications.DHC.Loads.BaseClasses.FlowDistribution disFloHea(
    redeclare package Medium = Medium,
    m_flow_nominal=sum(terUni.mHeaWat_flow_nominal .* terUni.facSca),
    have_pum=have_pum,
    dp_nominal=100000,
    nPorts_a1=nZon,
    nPorts_b1=nZon) if have_heaLoa
    "Heating water distribution system"
    annotation (Placement(transformation(extent={{-236,-188},{-216,-168}})));
  Buildings.Applications.DHC.Loads.BaseClasses.FlowDistribution disFloCoo(
    redeclare package Medium = MediumW,
    m_flow_nominal=sum(terUni.mChiWat_flow_nominal .* terUni.facSca),
    typDis=Buildings.Applications.DHC.Loads.Types.DistributionType.ChilledWater,
    have_pum=have_pum,
    dp_nominal=100000,
    nPorts_a1=nZon,
    nPorts_b1=nZon) if have_cooLoa
    "Chilled water distribution system"
    annotation (Placement(transformation(extent={{-160,-230},{-140,-210}})));
equation
  connect(qRadGai_flow.y,multiplex3_1.u1[1])
    annotation (Line(
      points={{-45,178},{-26,178},{-26,145},{-22,145}},
      color={0,0,127},
      smooth=Smooth.None));
  connect(qConGai_flow.y,multiplex3_1.u2[1])
    annotation (Line(
      points={{-45,138},{-22,138}},
      color={0,0,127},
      smooth=Smooth.None));
  connect(multiplex3_1.u3[1],qLatGai_flow.y)
    annotation (Line(points={{-22,131},{-26,131},{-26,98},{-45,98}},   color={0,0,127}));
  connect(multiplex3_1.y,znAttic.qGai_flow)
    annotation (Line(points={{1,138},{20,138},{20,114},{22,114}}, color={0,0,127}));
  connect(multiplex3_1.y,znCore_ZN.qGai_flow)
    annotation (Line(points={{1,138},{20,138},{20,72},{22,72}}, color={0,0,127}));
  connect(znAttic.ports[1], terUni[1].port_aLoa) annotation (Line(points={{42,84.9},
          {-8,84.9},{-8,18.1667},{-120,18.1667}},
                                             color={0,127,255}));
  connect(terUni[1].port_bLoa, znAttic.ports[2]) annotation (Line(points={{-140,
          18.1667},{-20,18.1667},{-20,84.9},{46,84.9}},
                                               color={0,127,255}));
  connect(znCore_ZN.ports[1], terUni[2].port_aLoa) annotation (Line(points={{42,42.9},
          {-8,42.9},{-8,18.1667},{-120,18.1667}},
                                             color={0,127,255}));
  connect(terUni[2].port_bLoa, znCore_ZN.ports[2]) annotation (Line(points={{-140,
          18.1667},{-20,18.1667},{-20,42.9},{46,42.9}},
                                               color={0,127,255}));

  connect(terUni[4].port_bLoa, znPerimeter_ZN_2.ports[2]) annotation (Line(points={{-140,
          18.1667},{-20,18.1667},{-20,-39.1},{46,-39.1}}, color={0,127,255}));

  connect(terUni[5].port_bLoa, znPerimeter_ZN_3.ports[2]) annotation (Line(points={{-140,
          18.1667},{-20,18.1667},{-20,-79.1},{46,-79.1}}, color={0,127,255}));

  connect(terUni[6].port_bLoa, znPerimeter_ZN_4.ports[2]) annotation (Line(points={{-140,
          18.1667},{-20,18.1667},{-20,-119.1},{46,-119.1}}, color={0,127,255}));

  connect(disFloCoo.ports_b1, terUni.port_aChiWat) annotation (Line(points={{-160,
          -214},{-260,-214},{-260,1.66667},{-140,1.66667}},
                                               color={0,127,255}));
  connect(terUni.port_bChiWat, disFloCoo.ports_a1) annotation (Line(points={{-120,
          1.66667},{-38,1.66667},{-38,-214},{-140,-214}},
                                           color={0,127,255}));
  connect(terUni.mReqChiWat_flow, disFloCoo.mReq_flow) annotation (Line(points={{
          -119.167,3.5},{-104,3.5},{-104,-80},{-180,-80},{-180,-224},{-161,-224}},
                                                          color={0,0,127}));
  connect(terUni.PFan, mulSum.u[1:6]) annotation (Line(points={{-119.167,9},{
          -100,9},{-100,220},{220,220},{220,118.333},{258,118.333}},
                                              color={0,0,127}));
  connect(mulSum.y, PFan) annotation (Line(points={{282,120},{320,120}},
                      color={0,0,127}));
 if have_pum then
  connect(PPum, mulSum3.y) annotation (Line(points={{320,80},{282,80}},
                                                 color={0,0,127}));
  connect(disFloHea.PPum, mulSum3.u[1]) annotation (Line(points={{-215,-186},{
          220.5,-186},{220.5,81},{258,81}}, color={0,0,127}));
  connect(disFloCoo.PPum, mulSum3.u[2]) annotation (Line(points={{-139,-228},{
          224,-228},{224,79},{258,79}}, color={0,0,127}));
 end if;
  connect(disFloCoo.QActTot_flow, QCoo_flow) annotation (Line(points={{-139,-226},
          {28,-226},{28,-218},{216,-218},{216,240},{320,240}},       color={0,0,
          127}));
  connect(znAttic.TAir, terUni[1].TSen) annotation (Line(points={{65,117.8},{80,
          117.8},{80,160},{-152,160},{-152,10.8333},{-140.833,10.8333}},
                                                                  color={0,0,
          127}));
  connect(znCore_ZN.TAir, terUni[2].TSen) annotation (Line(points={{65,75.8},{
          65,76},{80,76},{80,160},{-152,160},{-152,10.8333},{-140.833,10.8333}},
                                                                   color={0,0,
          127}));
  connect(minTSet.y, terUni.TSetHea) annotation (Line(points={{-198,70},{-160,
          70},{-160,14.5},{-140.833,14.5}},
                                        color={0,0,127}));
  connect(maxTSet.y, terUni.TSetCoo) annotation (Line(points={{-198,30},{-164,
          30},{-164,12.6667},{-140.833,12.6667}},
                                              color={0,0,127}));
  connect(multiplex3_1.y,znPerimeter_ZN_1.qGai_flow) annotation (Line(points={{1,138},{20,138},{20,30},{22,30}}, color={0,0,127}));
  connect(multiplex3_1.y,znPerimeter_ZN_2.qGai_flow) annotation (Line(points={{1,138},{20,138},{20,-10},{22,-10}}, color={0,0,127}));
  connect(multiplex3_1.y,znPerimeter_ZN_3.qGai_flow) annotation (Line(points={{1,138},{20,138},{20,-50},{22,-50}}, color={0,0,127}));
  connect(znPerimeter_ZN_1.ports[1], terUni[3].port_aLoa) annotation (Line(points={{42,0.9},
          {-8,0.9},{-8,18.1667},{-120,18.1667}},    color={0,127,255}));
  connect(terUni[3].port_bLoa, znPerimeter_ZN_1.ports[2]) annotation (Line(points={{-140,
          18.1667},{-20,18.1667},{-20,0.9},{46,0.9}}, color={0,127,255}));
  connect(znPerimeter_ZN_1.TAir, terUni[3].TSen) annotation (Line(points={{65,33.8},
          {72,33.8},{72,34},{80,34},{80,160},{-152,160},{-152,10.8333},{
          -140.833,10.8333}},
                color={0,0,127}));
  connect(znPerimeter_ZN_2.TAir, terUni[4].TSen) annotation (Line(points={{65,-6.2},
          {80,-6.2},{80,-140},{-152,-140},{-152,10.8333},{-140.833,10.8333}},
                                                                      color={0,
          0,127}));
  connect(znPerimeter_ZN_3.TAir, terUni[5].TSen) annotation (Line(points={{65,
          -46.2},{80,-46.2},{80,-140},{-152,-140},{-152,10.8333},{-140.833,
          10.8333}},                                                    color={
          0,0,127}));
  connect(znPerimeter_ZN_4.TAir, terUni[6].TSen) annotation (Line(points={{65,
          -86.2},{80,-86.2},{80,-140},{-152,-140},{-152,10.8333},{-140.833,
          10.8333}},                                                    color={
          0,0,127}));
  connect(multiplex3_1.y,znPerimeter_ZN_4.qGai_flow) annotation (Line(points={{1,138},{20,138},{20,-90},{22,-90}}, color={0,0,127}));
  connect(znPerimeter_ZN_2.ports[1], terUni[4].port_aLoa) annotation (Line(points={{42,
          -39.1},{-8,-39.1},{-8,18.1667},{-120,18.1667}},
                                                        color={0,127,255}));
  connect(znPerimeter_ZN_3.ports[1], terUni[5].port_aLoa) annotation (Line(points={{42,
          -79.1},{-8,-79.1},{-8,18.1667},{-120,18.1667}},
                                                        color={0,127,255}));
  connect(znPerimeter_ZN_4.ports[1], terUni[6].port_aLoa) annotation (Line(points={{42,
          -119.1},{-8,-119.1},{-8,18.1667},{-120,18.1667}},
                                                          color={0,127,255}));
if have_heaLoa then
  connect(disFloHea.port_a, secHeaSup[1]) annotation (Line(points={{-236,-178},{
          -266,-178},{-266,32},{-300,32}}, color={0,127,255}));
  connect(disFloHea.port_b, secHeaRet[1]) annotation (Line(points={{-216,-178},{
          260,-178},{260,32},{300,32}}, color={0,127,255}));
  connect(terUni.port_bHeaWat, disFloHea.ports_a1) annotation (Line(points={{-120,
          -0.166667},{-40,-0.166667},{-40,-172},{-216,-172}},color={0,127,255}));
  connect(disFloHea.ports_b1, terUni.port_aHeaWat) annotation (Line(points={{-236,
          -172},{-250,-172},{-250,-0.166667},{-140,-0.166667}},color={0,127,255}));
  connect(terUni.mReqHeaWat_flow, disFloHea.mReq_flow) annotation (Line(points={{
            -119.167,5.33333},{-100,5.33333},{-100,-90.5},{-237,-90.5},{-237,
            -182}},                                     color={0,0,127}));
  connect(disFloHea.QActTot_flow, QHea_flow) annotation (Line(points={{-215,-184},
          {-2,-184},{-2,-176},{212,-176},{212,280},{320,280}},       color={0,0,
          127}));
end if;
if have_cooLoa then
  connect(disFloCoo.port_a, secCooSup[1]) annotation (Line(points={{-160,-220},{
          -280,-220},{-280,-30},{-300,-30}}, color={0,127,255}));
  connect(disFloCoo.port_b, secCooRet[1]) annotation (Line(points={{-140,-220},{
          280,-220},{280,-30},{300,-30}}, color={0,127,255}));
end if;
annotation (
  Documentation(info="
<html>
<p>
This is a simplified six-zone building model based on EnergyPlus
building envelope model.
It was generated from translating a GeoJSON model specified within URBANopt UI.
The heating and cooling loads are computed with a four-pipe
fan coil unit model derived from
<a href=\"modelica://Buildings.Applications.DHC.Loads.BaseClasses.PartialTerminalUnit\">
Buildings.Applications.DHC.Loads.BaseClasses.PartialTerminalUnit</a>
and connected to the room model by means of fluid ports.
</p>
</html>
  "),
  Diagram(coordinateSystem(extent={{-300,-300},{300,300}})), Icon(
        coordinateSystem(extent={{-100,-100},{100,100}}), graphics={
          Bitmap(extent={{-108,-100},{92,100}},  fileName="modelica://Buildings/Resources/Images/ThermalZones/EnergyPlus/EnergyPlusLogo.png")}));
end BuildingSpawnZ6_modified_scalable;
