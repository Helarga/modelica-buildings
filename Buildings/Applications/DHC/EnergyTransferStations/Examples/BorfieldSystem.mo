within Buildings.Applications.DHC.EnergyTransferStations.Examples;
model BorfieldSystem
  "The hydraluic connection of the borfield system integertaed with the ambient circuit controller."


  package Medium1 = Buildings.Media.Water "Medium model";
  package Medium2 = Buildings.Media.Water "Medium model";

 parameter Boolean show_T=true
      "= true, if actual temperature at port is computed"
      annotation (Dialog(group="Advanced"));

  Buildings.Controls.OBC.CDL.Logical.Sources.Constant con(k=true)
    annotation (Placement(transformation(extent={{-60,60},{-40,80}})));
  Fluid.Sources.MassFlowSource_T sou1(
    nPorts=1,
    redeclare package Medium = Medium1,
    use_T_in=true,
    m_flow=per.mCon_flow_nominal,
    T=298.15)
    annotation (Placement(transformation(extent={{-56,-6},{-36,14}})));
  Fluid.Sources.MassFlowSource_T  sou2(
    nPorts=1,
    redeclare package Medium = Medium2,
    use_T_in=true,
    m_flow=per.mEva_flow_nominal,
    T=291.15)
    annotation (Placement(transformation(extent={{64,-18},{44,2}})));
  Fluid.Sources.Boundary_pT sin1(redeclare package Medium = Medium1,
      nPorts=1)                         annotation (Placement(
        transformation(
        extent={{10,-10},{-10,10}},
        origin={74,28})));
  Fluid.Sources.Boundary_pT sin2(redeclare package Medium = Medium2,
      nPorts=1)                         annotation (Placement(
        transformation(
        extent={{-10,-10},{10,10}},
        origin={-66,-82})));
  Modelica.Blocks.Sources.Pulse TSet(
    amplitude=4,
    period=500,
    startTime=0,
    offset=273.15 + 6)
              "Set point for leaving chilled water temperature"
    annotation (Placement(transformation(extent={{-76,-52},{-56,-32}})));
  Modelica.Blocks.Sources.Constant TCon_in(k=25 + 273.15)
                      "Condenser inlet temperature"
    annotation (Placement(transformation(extent={{-92,-2},{-72,18}})));
  Modelica.Blocks.Sources.Constant TEva_in(k=12 + 273.15)
                   "Evaporator inlet temperature"
    annotation (Placement(transformation(extent={{54,-52},{74,-32}})));
  Fluid.FixedResistances.PressureDrop res1(
    redeclare package Medium = Medium1,
    m_flow_nominal=per.mCon_flow_nominal,
    dp_nominal=6000) "Flow resistance"
    annotation (Placement(transformation(extent={{36,18},{56,38}})));
  Fluid.FixedResistances.PressureDrop res2(
    dp_nominal=6000,
    redeclare package Medium = Medium2,
    m_flow_nominal=per.mEva_flow_nominal) "Flow resistance"
    annotation (Placement(transformation(extent={{-16,-92},{-36,-72}})));
  parameter Fluid.Chillers.Data.ElectricEIR.ElectricEIRChiller_McQuay_WSC_471kW_5_89COP_Vanes per
        "Chiller performance data"
    annotation (Placement(transformation(extent={{64,68},{84,88}})));
  Fluid.Chillers.ElectricEIR chi1(
    redeclare package Medium1 = Medium1,
    redeclare package Medium2 = Medium2,
    per=per,
    energyDynamics=Modelica.Fluid.Types.Dynamics.FixedInitial,
    dp1_nominal=6000,
    dp2_nominal=6000) "Chiller model"
    annotation (Placement(transformation(extent={{4,-12},{24,8}})));
equation

  connect(TCon_in.y,sou1. T_in) annotation (Line(
      points={{-71,8},{-58,8}},
      color={0,0,127},
      smooth=Smooth.None));
  connect(TEva_in.y,sou2. T_in) annotation (Line(
      points={{75,-42},{84,-42},{84,-4},{66,-4}},
      color={0,0,127},
      smooth=Smooth.None));
  connect(res1.port_b,sin1. ports[1]) annotation (Line(
      points={{56,28},{64,28}},
      color={0,127,255},
      smooth=Smooth.None));
  connect(res2.port_b,sin2. ports[1]) annotation (Line(
      points={{-36,-82},{-56,-82}},
      color={0,127,255},
      smooth=Smooth.None));
  connect(sou1.ports[1], chi1.port_a1) annotation (Line(
      points={{-36,4},{4,4}},
      color={0,127,255},
      smooth=Smooth.None));
  connect(chi1.port_b1, res1.port_a) annotation (Line(
      points={{24,4},{30,4},{30,28},{36,28}},
      color={0,127,255},
      smooth=Smooth.None));
  connect(sou2.ports[1], chi1.port_a2) annotation (Line(
      points={{44,-8},{24,-8}},
      color={0,127,255},
      smooth=Smooth.None));
  connect(chi1.port_b2, res2.port_a) annotation (Line(
      points={{4,-8},{-6,-8},{-6,-82},{-16,-82}},
      color={0,127,255},
      smooth=Smooth.None));
  connect(chi1.TSet, TSet.y) annotation (Line(
      points={{2,-5},{-26,-5},{-26,-42},{-55,-42}},
      color={0,0,127},
      smooth=Smooth.None));
  connect(con.y, chi1.on) annotation (Line(points={{-38,70},{-4,70},{-4,1},{2,1}},
        color={255,0,255}));
  annotation (Icon(coordinateSystem(preserveAspectRatio=false, extent={{-100,-120},
            {100,100}}),                                        graphics={
        Ellipse(lineColor = {75,138,73},
                fillColor={255,255,255},
                fillPattern = FillPattern.Solid,
                extent={{-98,-100},{98,98}}),
        Polygon(lineColor = {0,0,255},
                fillColor = {75,138,73},
                pattern = LinePattern.None,
                fillPattern = FillPattern.Solid,
                points={{-30,64},{70,4},{-30,-56},{-30,64}})}),  Diagram(coordinateSystem(preserveAspectRatio=false, extent={{-100,
            -100},{100,100}}),
        graphics={Line(points={{-22,22}}, color={28,108,200})}),
    experiment(
      StopTime=2000,
      Tolerance=1e-06,
      __Dymola_Algorithm="Dassl"),
    __Dymola_Commands(
  file="modelica://Buildings/Resources/Scripts/Dymola/Applications/DHC/EnergyTransferStations/Control/HeatpumpControllerHeaOnly.mos"
        "Simulate and plot"),
         experiment(Tolerance=1e-6, StopTime=14400));
end BorfieldSystem;
