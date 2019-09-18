within Buildings.Applications.DHC.EnergyTransferStations.Examples;
model HeatingRecoveryChillerController
  "Heating recovery chiller controller"

  package Medium = Buildings.Media.Water "Medium model";

  parameter Fluid.HeatPumps.Data.ReverseWaterToWater.Trane_Axiom_EXW2402 per
   "Performance data"
    annotation (Placement(transformation(extent={{34,78},{54,98}})));
  parameter Modelica.SIunits.MassFlowRate mEva_flow_nominal=per.mEva_flow_nominal
   "Source heat exchanger nominal mass flow rate";
  parameter Modelica.SIunits.MassFlowRate mCon_flow_nominal=per.mCon_flow_nominal
   "Load heat exchanger nominal mass flow rate";
  parameter Real scaling_factor= 1
   "Scaling factor for heatpump capacity";

  Control.HeatRecoveryChillerController heaPumCon
    annotation (Placement(transformation(extent={{-46,0},{-26,18}})));
  Modelica.Blocks.Sources.Constant TCooSet(k=7 + 273.15)
    "Cooling setpoint temperature"
    annotation (Placement(transformation(extent={{-94,-30},{-74,-10}})));
  Modelica.Blocks.Sources.Constant THeaSet(k=45 + 273.15)
   "Heating set point temperature"
    annotation (Placement(transformation(extent={{-94,30},{-74,50}})));
  Modelica.Blocks.Sources.BooleanConstant stepCoo
   "Step control"
    annotation (Placement(transformation(extent={{-94,-70},{-74,-50}})));
  Modelica.Blocks.Sources.BooleanConstant stepHea(k=false)
   "Step control"
    annotation (Placement(transformation(extent={{-94,70},{-74,90}})));
  Buildings.Controls.OBC.CDL.Continuous.Sources.Constant TConEnt(k=273.15 + 40)
    "Condenser entering water temperature"
    annotation (Placement(transformation(extent={{-48,66},{-28,86}})));
  Fluid.HeatPumps.EquationFitWaterToWater heaPum(
    redeclare package Medium1 = Medium,
    redeclare package Medium2 = Medium,
    scaling_factor=scaling_factor,
    per=per,
    T1_start = 273.15+40,
    T2_start = 273.15+7,
    energyDynamics=Modelica.Fluid.Types.Dynamics.FixedInitial,
    massDynamics=Modelica.Fluid.Types.Dynamics.FixedInitial,
    show_T=true,
    dp1_nominal=200,
    dp2_nominal=200)
    annotation (Placement(transformation(extent={{24,0},{44,20}})));
  Fluid.Sources.MassFlowSource_T conPum(
    use_m_flow_in=false,
    m_flow=mCon_flow_nominal,
    use_T_in=true,
    redeclare package Medium = Medium,
    nPorts=1) "Condenser water pump"
   annotation (Placement(transformation(
      extent={{10,-10},{-10,10}},
      rotation=180,
      origin={-4,80})));
  Fluid.Sources.MassFlowSource_T evaPum(
    m_flow=mEva_flow_nominal,
    nPorts=1,
    use_T_in=true,
    redeclare package Medium = Medium) "Evaporator water pump"
   annotation (Placement(transformation(
      extent={{-10,-10},{10,10}},
      rotation=180,
      origin={76,4})));
  Fluid.FixedResistances.PressureDrop
                                res1(
  redeclare package Medium = Medium,
    m_flow_nominal=mCon_flow_nominal,
    dp_nominal=6000)
   "Flow resistance"
   annotation (Placement(transformation(extent={{52,60},{72,80}})));
  Modelica.Fluid.Sources.FixedBoundary loaVol(redeclare package Medium =
      Medium, nPorts=1)
   "Volume for the load side"
   annotation (Placement(transformation(extent={{98,60},{78,80}})));
  Fluid.FixedResistances.PressureDrop res2(
    redeclare package Medium = Medium,
    m_flow_nominal=mEva_flow_nominal,
    dp_nominal=6000)
   "Flow resistance"
   annotation (Placement(transformation(extent={{6,-50},{-14,-30}})));
  Modelica.Fluid.Sources.FixedBoundary souVol(redeclare package Medium =
      Medium, nPorts=1)
   "Volume for source side"
   annotation (Placement(transformation(extent={{-44,-50},{-24,-30}})));
  Buildings.Controls.OBC.CDL.Continuous.Sources.Sine TEvaEnt(
    amplitude=3,
    freqHz=1/3000,
    offset=10 + 273.15,
    startTime=0) "Evaporator entering water temperature "
    annotation (Placement(transformation(extent={{54,-44},{74,-24}})));
equation
  connect(stepCoo.y, heaPumCon.ReqCoo)
    annotation (Line(points={{-73,-60},{-56,-60},{-56,1},{-47.4,1}},
                              color={255,0,255}));
  connect(heaPumCon.ReqHea, stepHea.y)
    annotation (Line(points={{-47.4,19},{-56,19},{-56,80},{-73,80}},
                               color={255,0,255}));
  connect(conPum.T_in,TConEnt. y)
    annotation (Line(points={{-16,76},{-26,76}}, color={0,0,127}));
  connect(res1.port_b,loaVol. ports[1])
    annotation (Line(points={{72,70},{78,70}},color={0,127,255}));
  connect(souVol.ports[1], res2.port_b)
    annotation (Line(points={{-24,-40},{-14,-40}}, color={0,127,255}));
  connect(TEvaEnt.y,evaPum. T_in) annotation (Line(points={{76,-34},{96,-34},{96,
          0},{88,0}}, color={0,0,127}));
  connect(THeaSet.y, heaPum.TConSet) annotation (Line(points={{-73,40},{8,40},{
          8,19},{24.5455,19}},
                             color={0,0,127}));
  connect(TCooSet.y, heaPum.TEvaSet) annotation (Line(points={{-73,-20},{-6,-20},
          {-6,1},{24.5455,1}}, color={0,0,127}));
  connect(heaPumCon.heaPumMod, heaPum.uMod)
    annotation (Line(points={{-24.6,10},{24.5455,10}}, color={255,127,0}));
  connect(conPum.ports[1], heaPum.port_a1) annotation (Line(points={{6,80},{18,
          80},{18,16},{25.8182,16}},
                                 color={0,127,255}));
  connect(heaPum.port_a2,evaPum. ports[1])
    annotation (Line(points={{44,4},{66,4}}, color={0,127,255}));
  connect(res1.port_a, heaPum.port_b1) annotation (Line(points={{52,70},{48,70},
          {48,16},{44,16}}, color={0,127,255}));
  connect(res2.port_a, heaPum.port_b2) annotation (Line(points={{6,-40},{12,-40},
          {12,4},{25.8182,4}}, color={0,127,255}));
  annotation (Icon(coordinateSystem(preserveAspectRatio=false), graphics={
        Ellipse(lineColor = {75,138,73},
                fillColor={255,255,255},
                fillPattern = FillPattern.Solid,
                extent={{-98,-100},{98,98}}),
        Polygon(lineColor = {0,0,255},
                fillColor = {75,138,73},
                pattern = LinePattern.None,
                fillPattern = FillPattern.Solid,
                points={{-30,64},{70,4},{-30,-56},{-30,64}})}),  Diagram(coordinateSystem(preserveAspectRatio=false)),
     __Dymola_Commands(
  file="modelica://Buildings/Resources/Scripts/Dymola/Applications/DHC/EnergyTransferStations/Control/HRChillerController.mos"
        "Simulate and plot"),
         experiment(Tolerance=1e-6, StopTime=14400));
end HeatingRecoveryChillerController;
