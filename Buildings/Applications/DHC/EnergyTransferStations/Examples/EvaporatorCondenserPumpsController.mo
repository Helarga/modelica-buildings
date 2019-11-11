within Buildings.Applications.DHC.EnergyTransferStations.Examples;
model EvaporatorCondenserPumpsController
  "Example of the evaporator and condenser pumps controller"

  package Medium = Buildings.Media.Water "Medium model";

  parameter Boolean show_T=true
    "= true, if actual temperature at port is computed"
     annotation (Dialog(group="Advanced"));
  parameter Modelica.SIunits.PressureDifference dpEva_nominal=10000
    "Nominal pressure raise";

  parameter Modelica.SIunits.MassFlowRate mEva_flow_nominal= 1.5
     "Evaporator nominal water flow rate";

  Buildings.Controls.OBC.CDL.Continuous.Sources.Constant yPumMinLoa(k=0.2)
    "Minimum speed for the condenser side pump"
    annotation (Placement(transformation(extent={{-40,20},{-20,40}})));
  Modelica.Blocks.Sources.BooleanConstant heaMod(k=false) "Step control"
    annotation (Placement(transformation(extent={{60,60},{80,80}})));
  Modelica.Blocks.Sources.BooleanConstant CooMod(k=true)
                                                 "Step control"
    annotation (Placement(transformation(extent={{60,-80},{80,-60}})));
  Modelica.Blocks.Sources.Ramp mSecCoo_flow(
    height=0.5,
    duration=200,
    offset=1) "Secondary(building side) circuit chilled water flow rate "
    annotation (Placement(transformation(extent={{0,-40},{20,-20}})));
  Modelica.Blocks.Sources.Ramp mSecHea_flow(
    height=1.2,
    duration=200,
    offset=0.5) "Secondary(building side) circuit heating water flow rate "
    annotation (Placement(transformation(extent={{2,20},{22,40}})));
  Control.EvaporatorCondenserPumpsController pumCon
    annotation (Placement(transformation(extent={{90,-10},{110,10}})));
  Modelica.Blocks.Sources.Ramp mCon_flow(
    height=0.5,
    duration=200,
    offset=0.5) "Condenser water flow rate"
    annotation (Placement(transformation(extent={{40,20},{60,40}})));
  Modelica.Blocks.Sources.Ramp mEva_flow(
    height=0.5,
    duration=200,
    offset=0.5) "Evaporator water flow rate"
    annotation (Placement(transformation(extent={{40,-40},{60,-20}})));
  Buildings.Controls.OBC.CDL.Continuous.Sources.Constant yPumMinSoa(k=0.2)
    "Minimum speed for the evaporator side pump"
    annotation (Placement(transformation(extent={{-40,-42},{-20,-22}})));
   Fluid.Movers.SpeedControlled_y           pumEva(
    redeclare package Medium = Medium,
    energyDynamics=Modelica.Fluid.Types.Dynamics.SteadyState,
    addPowerToMedium=false,
    show_T=show_T,
    per(pressure(dp={2*dpEva_nominal,0}, V_flow={0,2*mEva_flow_nominal/1000})),
    allowFlowReversal=false,
    use_inputFilter=false,
    riseTime=10)
    "Evaporator variable speed pump-primary circuit"
    annotation (Placement(transformation(
          extent={{-10,-10},{10,10}},
          rotation=0,
          origin={130,-28})));
  Modelica.Fluid.Sources.FixedBoundary heaLoa(redeclare package Medium = Medium,
      nPorts=1) "Volume for the heating load"
   annotation (Placement(transformation(extent={{160,18},{140,38}})));
  Modelica.Fluid.Sources.FixedBoundary heaLoa1(redeclare package Medium =
        Medium, nPorts=1)
                "Volume for the heating load"
   annotation (Placement(transformation(extent={{122,-60},{102,-40}})));
equation
  connect(pumCon.ReqHea, heaMod.y) annotation (Line(points={{88.6,10},{88,10},{88,
          70},{81,70}},     color={255,0,255}));
  connect(CooMod.y, pumCon.ReqCoo) annotation (Line(points={{81,-70},{86,-70},{
          86,-9.8},{88.6,-9.8}},
                              color={255,0,255}));
  connect(yPumMinLoa.y, pumCon.minConMasFlo) annotation (Line(points={{-18,30},{
          -18,1.4},{89.2,1.4}},     color={0,0,127}));
  connect(mCon_flow.y, pumCon.mPriHea) annotation (Line(points={{61,30},{68,30},
          {68,8.4},{89.2,8.4}}, color={0,0,127}));
  connect(mSecHea_flow.y, pumCon.mSecHea) annotation (Line(points={{23,30},{30,30},
          {30,5.8},{89.2,5.8}}, color={0,0,127}));
  connect(mEva_flow.y, pumCon.mPriEva) annotation (Line(points={{61,-30},{68,-30},
          {68,-8},{89.2,-8}}, color={0,0,127}));
  connect(yPumMinSoa.y, pumCon.minEvaMasFlo)
    annotation (Line(points={{-18,-32},{-18,-1.8},{89.2,-1.8}},
                                                         color={0,0,127}));
  connect(mSecCoo_flow.y, pumCon.mSecCoo) annotation (Line(points={{21,-30},{30,
          -30},{30,-5.4},{89.2,-5.4}}, color={0,0,127}));
  connect(pumCon.yPumEva, pumEva.y)
    annotation (Line(points={{111,-8},{130,-8},{130,-16}}, color={0,0,127}));
  connect(pumEva.port_b, heaLoa.ports[1]) annotation (Line(points={{140,-28},{146,
          -28},{146,28},{140,28}}, color={0,127,255}));
  connect(pumEva.port_a, heaLoa1.ports[1]) annotation (Line(points={{120,-28},{94,
          -28},{94,-50},{102,-50}}, color={0,127,255}));
  annotation (Icon(coordinateSystem(preserveAspectRatio=false), graphics={
        Ellipse(lineColor = {75,138,73},
                fillColor={255,255,255},
                fillPattern = FillPattern.Solid,
                extent={{-98,-100},{98,98}}),
        Polygon(lineColor = {0,0,255},
                fillColor = {75,138,73},
                pattern = LinePattern.None,
                fillPattern = FillPattern.Solid,
                points={{-30,64},{70,4},{-30,-56},{-30,64}})}),  Diagram(coordinateSystem(preserveAspectRatio=false, extent={
            {-100,-100},{160,100}}),
        graphics={Line(points={{-22,22}}, color={28,108,200})}),
    experiment(StopTime=10000),
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
