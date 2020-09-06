within Buildings.Applications.DHC.CentralPlants.Heating.Generation4.Validation;
model IndirectHeatingEts "Indirect heating ETS."
    extends Modelica.Icons.Example;

   package Medium = Buildings.Media.Water;

 parameter Buildings.Fluid.Movers.Data.Generic perHWPum(
    pressure=Buildings.Fluid.Movers.BaseClasses.Characteristics.flowParameters(
      V_flow=0.5
               /1000*{0,1},
      dp=(300000)*{1,0}))
    "Performance data for chilled water pumps";

  EnergyTransferStations.Heating.HeatingIndirect heaETS(
    show_T=true,
    redeclare package Medium = Medium,
    mDis_flow_nominal=0.5,
    mBui_flow_nominal=2,
    dpValve_nominal=6000,
    dp1_nominal=600,
    dp2_nominal=600,
    use_Q_flow_nominal=true,
    Q_flow_nominal=1*4200*5,
    T_a1_nominal(displayUnit="degC") = 328.15,
    T_a2_nominal(displayUnit="degC") = 313.15,
    eta=0.8,
    initType=Modelica.Blocks.Types.InitPID.InitialOutput,
    xi_start=1)
             annotation (Placement(transformation(extent={{0,10},{20,30}})));
  Fluid.Sources.MassFlowSource_T bui(
    redeclare package Medium = Medium,
    use_m_flow_in=false,
    m_flow=1,
    T=313.15,
    nPorts=1) annotation (Placement(transformation(extent={{70,-8},{50,12}})));
  Fluid.Sources.Boundary_pT                souHea1(
    redeclare package Medium = Medium,
    nPorts=1) annotation (Placement(transformation(extent={{-68,-10},{-48,10}})));
  Modelica.Blocks.Sources.RealExpression Tset(y=45 + 273.15)
    annotation (Placement(transformation(extent={{-58,70},{-38,90}})));
  Fluid.Sources.MassFlowSource_T dis(
    redeclare package Medium = Medium,
    use_m_flow_in=false,
    m_flow=7,
    T=328.15,
    nPorts=1) annotation (Placement(transformation(extent={{-68,30},{-48,50}})));
  Fluid.Sources.Boundary_pT                souHea3(redeclare package Medium =
        Medium, nPorts=1)
              annotation (Placement(transformation(extent={{60,28},{40,48}})));
equation
  connect(heaETS.port_a2, bui.ports[1]) annotation (Line(points={{20,14},{32,14},
          {32,2},{50,2}}, color={0,127,255}));
  connect(heaETS.port_b2,souHea1. ports[1]) annotation (Line(points={{0,14},{
          -16,14},{-16,0},{-48,0}},
                                 color={0,127,255}));
  connect(Tset.y,heaETS. TSetBuiSup) annotation (Line(points={{-37,80},{-14,80},
          {-14,20},{-2,20}},
                        color={0,0,127}));
  connect(dis.ports[1], heaETS.port_a1) annotation (Line(points={{-48,40},{-34,
          40},{-34,26},{0,26}}, color={0,127,255}));
  connect(souHea3.ports[1], heaETS.port_b1) annotation (Line(points={{40,38},{
          30,38},{30,26},{20,26}}, color={0,127,255}));
  annotation (Icon(coordinateSystem(preserveAspectRatio=false)), Diagram(
        coordinateSystem(preserveAspectRatio=false)),
    experiment(StopTime=3600, __Dymola_Algorithm="Dassl"));
end IndirectHeatingEts;
