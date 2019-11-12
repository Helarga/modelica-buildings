within Buildings.Applications.DHC.EnergyTransferStations.Examples;
model EvaporatorCondenserPumpsController
  "Example of the evaporator and condenser pumps controller"

  package Medium = Buildings.Media.Water "Medium model";

  parameter Boolean show_T=true
    "= true, if actual temperature at port is computed"
     annotation (Dialog(group="Advanced"));
  parameter Modelica.SIunits.PressureDifference dpEva_nominal=10000
    "Nominal pressure raise";

  parameter Modelica.SIunits.MassFlowRate mCon_flow_nominal= 1.2
   "Evaporator nominal water flow rate";

  parameter Modelica.SIunits.MassFlowRate mEva_flow_nominal= 1.2
   "Evaporator nominal water flow rate";

  /*parameter Modelica.SIunits.MassFlowRate mSecHea_flow_nominal= mEva_flow_nominal*0.9
     "Secondary(building) heating circuit nominal water flow rate";
    parameter Modelica.SIunits.MassFlowRate mSecCoo_flow_nominal= mEva_flow_nominal*0.9
    "Secondary(building) cooling circuit nominal water flow rate";*/

  Buildings.Controls.OBC.CDL.Continuous.Sources.Constant conFloMin(k=0.3)
    "Primary pump control signal to maintain the condenser minimum flow rate recommended by the manufacturer. "
    annotation (Placement(transformation(extent={{16,18},{36,38}})));
  Modelica.Blocks.Sources.BooleanConstant heaMod(k=false) "Step control"
    annotation (Placement(transformation(extent={{88,48},{108,68}})));
  Modelica.Blocks.Sources.BooleanConstant CooMod(k=true)
                                                 "Step control"
    annotation (Placement(transformation(extent={{92,-78},{112,-58}})));
  Modelica.Blocks.Sources.Ramp mSecCoo(
    height=0.5,
    duration=200,
    offset=0.2) "Secondary(building side) circuit chilled water flow rate "
    annotation (Placement(transformation(extent={{44,-32},{64,-12}})));
  Modelica.Blocks.Sources.Ramp mSecHea(
    height=0.5,
    duration=200,
    offset=0.2) "Secondary(building side) circuit heating water flow rate "
    annotation (Placement(transformation(extent={{44,18},{64,38}})));
  Control.EvaporatorCondenserPumpsController priPumCon(mEva_flow_nominal=
        mEva_flow_nominal, mCon_flow_nominal=mCon_flow_nominal)
    "Primary pumps control block"
    annotation (Placement(transformation(extent={{120,-12},{140,8}})));
    //mSecHea_flow_nominal = mSecHea_flow_nominal,
    //mSecCoo_flow_nominal = mSecCoo_flow_nominal)
  Buildings.Controls.OBC.CDL.Continuous.Sources.Constant evaFloMin(k=0.3)
    "Primary pump control signal to maintain the evaporator minimum flow rate recommended by the manufacturer. "
    annotation (Placement(transformation(extent={{-10,18},{10,38}})));
   Fluid.Movers.SpeedControlled_y pumEva(
    redeclare package Medium = Medium,
    energyDynamics=Modelica.Fluid.Types.Dynamics.SteadyState,
    addPowerToMedium=false,
    show_T=show_T,
    per(pressure(dp={dpEva_nominal,0}, V_flow={0,mEva_flow_nominal/1000})),
    allowFlowReversal=false,
    use_inputFilter=false,
    riseTime=10)
    "Evaporator variable speed pump-primary circuit"
    annotation (Placement(transformation(
          extent={{-10,-10},{10,10}},
          rotation=0,
          origin={186,-28})));
  Modelica.Fluid.Sources.FixedBoundary sin(redeclare package Medium = Medium,
      nPorts=1) "Sink"
    annotation (Placement(transformation(extent={{296,-38},{276,-18}})));
  Modelica.Fluid.Sources.FixedBoundary sou(redeclare package Medium = Medium,
      nPorts=1) "Source volume"
    annotation (Placement(transformation(extent={{146,-38},{166,-18}})));
  Buildings.Controls.OBC.CDL.Continuous.Sources.Constant cooTanMin(k=0.4)
    "Primary pump speed signal to assure the minimum flow to the cold tank"
    annotation (Placement(transformation(extent={{0,-32},{20,-12}})));
  Buildings.Controls.OBC.CDL.Continuous.Sources.Constant heaTanMin(k=0.2)
    "Primary pump speed signal to assure the minimum flow to the hot tank"
    annotation (Placement(transformation(extent={{-36,18},{-16,38}})));
  Fluid.Sensors.MassFlowRate priCooFlo(redeclare package Medium = Media.Water)
    "Primary circuit evaporator side chilled water flow rate" annotation (
      Placement(transformation(
        extent={{-10,10},{10,-10}},
        rotation=0,
        origin={246,-28})));
   Fluid.Movers.SpeedControlled_y pumCon(
    redeclare package Medium = Medium,
    energyDynamics=Modelica.Fluid.Types.Dynamics.SteadyState,
    addPowerToMedium=false,
    show_T=show_T,
    per(pressure(dp={dpEva_nominal,0}, V_flow={0,mCon_flow_nominal/1000})),
    allowFlowReversal=false,
    use_inputFilter=false,
    riseTime=10) "Condenser variable speed pump-primary circuit" annotation (
      Placement(transformation(
        extent={{-10,10},{10,-10}},
        rotation=0,
        origin={186,60})));
  Modelica.Fluid.Sources.FixedBoundary sin1(redeclare package Medium = Medium,
      nPorts=1) "Sink"
    annotation (Placement(transformation(extent={{298,48},{278,68}})));
  Fluid.Sensors.MassFlowRate priHeaFlo(redeclare package Medium = Media.Water)
    "Primary circuit condenser side heating water flow rate" annotation (
      Placement(transformation(
        extent={{10,10},{-10,-10}},
        rotation=180,
        origin={246,60})));
  Modelica.Fluid.Sources.FixedBoundary sou1(redeclare package Medium = Medium,
      nPorts=1) "Source volume"
    annotation (Placement(transformation(extent={{148,50},{168,70}})));
equation
  connect(priPumCon.reqHea, heaMod.y) annotation (Line(points={{118.6,8},{112,8},
          {112,58},{109,58}}, color={255,0,255}));
  connect(CooMod.y, priPumCon.reqCoo) annotation (Line(points={{113,-68},{116,
          -68},{116,-11.8},{118.6,-11.8}}, color={255,0,255}));
  connect(conFloMin.y, priPumCon.conFloMin)
    annotation (Line(points={{38,28},{38,2},{119.2,2}}, color={0,0,127}));
  connect(mSecHea.y, priPumCon.mSecHea) annotation (Line(points={{65,28},{80,28},
          {80,4},{119.2,4}}, color={0,0,127}));
  connect(mSecCoo.y, priPumCon.mSecCoo) annotation (Line(points={{65,-22},{80,
          -22},{80,-7.6},{119.2,-7.6}}, color={0,0,127}));
  connect(priPumCon.yPumEva, pumEva.y) annotation (Line(
      points={{141,-10},{186,-10},{186,-16}},
      color={0,0,127},
      pattern=LinePattern.Dash));
  connect(pumEva.port_a, sou.ports[1]) annotation (Line(points={{176,-28},{166,
          -28}},               color={0,127,255}));
  connect(priCooFlo.port_a, pumEva.port_b)
    annotation (Line(points={{236,-28},{196,-28}}, color={0,127,255}));
  connect(priCooFlo.port_b, sin.ports[1])
    annotation (Line(points={{256,-28},{276,-28}}, color={0,127,255}));
  connect(priHeaFlo.port_a, pumCon.port_b)
    annotation (Line(points={{236,60},{196,60}}, color={0,127,255}));
  connect(priHeaFlo.port_b, sin1.ports[1]) annotation (Line(points={{256,60},{
          280,60},{280,58},{278,58}},
                                  color={0,127,255}));
  connect(priPumCon.yPumCon, pumCon.y) annotation (Line(
      points={{141,6},{186,6},{186,48}},
      color={0,0,127},
      pattern=LinePattern.Dash));
  connect(pumCon.port_a, sou1.ports[1])
    annotation (Line(points={{176,60},{168,60}}, color={0,127,255}));
  connect(priPumCon.mPriCoo, priCooFlo.m_flow) annotation (Line(
      points={{119.2,-10},{84,-10},{84,-90},{246,-90},{246,-39}},
      color={0,0,127},
      pattern=LinePattern.Dot));
  connect(priPumCon.cooTanMin, cooTanMin.y) annotation (Line(points={{119.2,
          -5.6},{24,-5.6},{24,-22},{22,-22}}, color={0,0,127}));
  connect(priPumCon.evaFloMin, evaFloMin.y) annotation (Line(points={{119.2,
          -0.4},{14,-0.4},{14,28},{12,28}}, color={0,0,127}));
  connect(priPumCon.heaTanMin, heaTanMin.y) annotation (Line(points={{119.2,
          -3.4},{-14,-3.4},{-14,28}}, color={0,0,127}));
  connect(priHeaFlo.m_flow, priPumCon.mPriHea) annotation (Line(
      points={{246,71},{246,86},{84,86},{84,6.4},{119.2,6.4}},
      color={0,0,127},
      pattern=LinePattern.Dot));
  annotation (Icon(coordinateSystem(preserveAspectRatio=false, extent={{-100,-100},
            {100,100}}),                                        graphics={
        Ellipse(lineColor = {75,138,73},
                fillColor={255,255,255},
                fillPattern = FillPattern.Solid,
                extent={{-98,-100},{98,98}}),
        Polygon(lineColor = {0,0,255},
                fillColor = {75,138,73},
                pattern = LinePattern.None,
                fillPattern = FillPattern.Solid,
                points={{-30,64},{70,4},{-30,-56},{-30,64}})}),  Diagram(coordinateSystem(preserveAspectRatio=false, extent={{-40,
            -100},{300,100}}),
        graphics={Line(points={{-22,22}}, color={28,108,200})}),
    experiment(StopTime=100000),
    __Dymola_Commands(
  file="modelica://Buildings/Resources/Scripts/Dymola/Applications/DHC/EnergyTransferStations/Control/EvaporatorCondenserPumpsController.mos"
        "Simulate and plot"),
         experiment(Tolerance=1e-6, StopTime=10000),
Documentation(info="<html>
<p>
This model validates the controller block
<a href=\"Buildings.Applications.DHC.EnergyTransferStations.Control.CondenserAndEvaporatorPumpsController\"> 
Buildings.Applications.DHC.EnergyTransferStations.Control.CondenserAndEvaporatorPumpsController</a>.
<p>

</html>", revisions="<html>
<ul>
<li>
 <br/>
</li>
</ul>
</html>"));
end EvaporatorCondenserPumpsController;
