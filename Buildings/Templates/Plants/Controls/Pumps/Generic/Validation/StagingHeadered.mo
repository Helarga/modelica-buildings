within Buildings.Templates.Plants.Controls.Pumps.Generic.Validation;
model StagingHeadered
  "Validation model for staging of headered pumps"
  parameter Integer nEqu=3
    "Number of plant equipment";
  parameter Integer nPum=nEqu
    "Number of pumps that operate at design conditions";
  parameter Real V_flow_nominal=0.1
    "Design flow rate";
  Buildings.Templates.Plants.Controls.Pumps.Generic.StagingHeadered staPumPriDp(
    is_pri=true,
    is_hdr=true,
    is_ctlDp=true,
    have_valInlIso=true,
    have_valOutIso=true,
    final nEqu=nEqu,
    final nPum=nPum,
    nSenDp=1,
    final V_flow_nominal=V_flow_nominal)
    "Pump staging – Headered primary pumps with ∆p control"
    annotation (Placement(transformation(extent={{0,-50},{20,-30}})));
  Buildings.Controls.OBC.CDL.Reals.Sources.TimeTable ratFlo(table=[0,0; 1,1; 1.5,
        1; 2,0], timeScale=3600) "Flow ratio to design value"
    annotation (Placement(transformation(extent={{-110,-10},{-90,10}})));
  Buildings.Controls.OBC.CDL.Reals.MultiplyByParameter VPri_flow(final k=
        V_flow_nominal)
    "Flow rate"
    annotation (Placement(transformation(extent={{-68,-10},{-48,10}})));
  Buildings.Controls.OBC.CDL.Logical.Sources.TimeTable u1(
    table=[0,0,0,0; 0.1,1,0,0; 0.5,1,1,0; 1,1,1,1; 1.5,1,1,1; 2,0,1,1; 2.5,0,0,1;
        3,0,0,0],
    timeScale=3600,
    period=10800)
    "Command signal – Plant, equipment or isolation valve depending on tested configuration"
    annotation (Placement(transformation(extent={{-110,30},{-90,50}})));
  Buildings.Templates.Plants.Controls.Pumps.Generic.StagingHeadered staPumSecDp(
    is_pri=false,
    is_hdr=true,
    is_ctlDp=true,
    have_valInlIso=true,
    have_valOutIso=true,
    final nEqu=nEqu,
    final nPum=nPum,
    nSenDp=1,
    final V_flow_nominal=V_flow_nominal)
    "Pump staging – Headered secondary pumps with ∆p control"
    annotation (Placement(transformation(extent={{0,-90},{20,-70}})));
  Buildings.Templates.Plants.Controls.Pumps.Generic.StagingHeadered staPumPriNoDp(
    is_pri=true,
    is_hdr=true,
    is_ctlDp=false,
    have_valInlIso=true,
    have_valOutIso=false,
    final nEqu=nEqu,
    final nPum=nPum,
    final V_flow_nominal=V_flow_nominal)
    "Pump staging – Headered primary pumps without ∆p control"
    annotation (Placement(transformation(extent={{0,-10},{20,10}})));
  Buildings.Templates.Plants.Controls.Pumps.Generic.StagingHeadered staPumPriDed(
    is_pri=true,
    is_hdr=false,
    is_ctlDp=false,
    have_valInlIso=true,
    have_valOutIso=false,
    final nEqu=nEqu,
    final nPum=nPum,
    final V_flow_nominal=V_flow_nominal)
    "Pump staging – Dedicated primary pumps"
    annotation (Placement(transformation(extent={{0,30},{20,50}})));
  Buildings.Controls.OBC.CDL.Reals.Sources.Constant dpSet(k=Buildings.Templates.Data.Defaults.dpChiWatLocSet_max)
    "Loop differential pressure setpoint"
    annotation (Placement(transformation(extent={{-110,-50},{-90,-30}})));
  Buildings.Controls.OBC.CDL.Reals.Sources.Constant yPum(k=0.6)
    "Pump speed command"
    annotation (Placement(transformation(extent={{-110,-90},{-90,-70}})));
  Components.Controls.StatusEmulator y1Pum_actual[nPum] "Pump Status"
    annotation (Placement(transformation(extent={{20,70},{0,90}})));
equation
  connect(ratFlo.y[1],VPri_flow. u)
    annotation (Line(points={{-88,0},{-70,0}},color={0,0,127}));
  connect(VPri_flow.y, staPumPriDp.V_flow) annotation (Line(points={{-46,0},{-40,
          0},{-40,-42},{-2,-42}}, color={0,0,127}));
  connect(u1.y, staPumPriDp.u1ValInlIso) annotation (Line(points={{-88,40},{-30,
          40},{-30,-34},{-2,-34}}, color={255,0,255}));
  connect(u1.y, staPumPriDp.u1ValOutIso) annotation (Line(points={{-88,40},{-30,
          40},{-30,-36},{-2,-36}}, color={255,0,255}));
  connect(u1.y[1], staPumSecDp.u1Pla) annotation (Line(points={{-88,40},{-30,40},
          {-30,-72},{-2,-72}}, color={255,0,255}));
  connect(VPri_flow.y, staPumSecDp.V_flow) annotation (Line(points={{-46,0},{-40,
          0},{-40,-82},{-2,-82}}, color={0,0,127}));
  connect(u1.y, staPumPriNoDp.u1ValInlIso) annotation (Line(points={{-88,40},{-30,
          40},{-30,6},{-2,6}}, color={255,0,255}));
  connect(u1.y, staPumPriNoDp.u1Pum) annotation (Line(points={{-88,40},{-30,40},
          {-30,2},{-2,2}},   color={255,0,255}));
  connect(u1.y, staPumPriDed.u1Pum) annotation (Line(points={{-88,40},{-30,40},{
          -30,42},{-2,42}}, color={255,0,255}));
  connect(yPum.y, staPumPriDp.y) annotation (Line(points={{-88,-80},{-60,-80},{-60,
          -48},{-2,-48}}, color={0,0,127}));
  connect(yPum.y, staPumSecDp.y) annotation (Line(points={{-88,-80},{-60,-80},{-60,
          -88},{-2,-88}}, color={0,0,127}));
  connect(dpSet.y, staPumPriDp.dp[1]) annotation (Line(points={{-88,-40},{-80,-40},
          {-80,-46},{-2,-46}}, color={0,0,127}));
  connect(dpSet.y, staPumPriDp.dpSet[1]) annotation (Line(points={{-88,-40},{-80,-40},
          {-80,-44},{-2,-44}}, color={0,0,127}));
  connect(dpSet.y, staPumSecDp.dp[1]) annotation (Line(points={{-88,-40},{-80,-40},
          {-80,-86},{-2,-86}}, color={0,0,127}));
  connect(dpSet.y, staPumSecDp.dpSet[1]) annotation (Line(points={{-88,-40},{-80,-40},
          {-80,-84},{-2,-84}}, color={0,0,127}));
  connect(staPumPriDp.y1, y1Pum_actual.y1) annotation (Line(points={{22,-40},{
          40,-40},{40,80},{22,80}}, color={255,0,255}));
  connect(y1Pum_actual.y1_actual, staPumPriDed.u1Pum_actual) annotation (Line(
        points={{-2,80},{-20,80},{-20,40},{-2,40}}, color={255,0,255}));
  connect(y1Pum_actual.y1_actual, staPumPriNoDp.u1Pum_actual) annotation (Line(
        points={{-2,80},{-20,80},{-20,0},{-2,0}}, color={255,0,255}));
  connect(y1Pum_actual.y1_actual, staPumPriDp.u1Pum_actual) annotation (Line(
        points={{-2,80},{-20,80},{-20,-40},{-2,-40}}, color={255,0,255}));
  connect(y1Pum_actual.y1_actual, staPumSecDp.u1Pum_actual) annotation (Line(
        points={{-2,80},{-20,80},{-20,-80},{-2,-80}}, color={255,0,255}));
  annotation (
    __Dymola_Commands(
      file=
        "modelica://Buildings/Resources/Scripts/Dymola/Templates/Plants/Controls/Pumps/Generic/Validation/StagingHeadered.mos"
        "Simulate and plot"),
    experiment(
      StopTime=10800.0,
      Tolerance=1e-06),
    Documentation(
      info="<html>
<p>
This model validates
<a href=\"modelica://Buildings.Templates.Plants.Controls.Pumps.Generic.StagingHeadered\">
Buildings.Templates.Plants.Controls.Pumps.Generic.StagingHeadered</a>
with three plant equipment and three pumps and for the following configurations.
</p>
<ul>
<li>
Dedicated primary pumps (component <code>staPumPriDed</code>):
the number of pumps commanded on matches the number of operating
equipment with a one-to-one relationship.
</li>
<li>
Headered primary pumps without ∆p control (component <code>staPumPriNoDp</code>):
the number of pumps commanded on matches the number of operating
equipment without a one-to-one relationship.
</li>
<li>
Headered primary pumps with ∆p control (component <code>staPumPriDp</code>):
the number of pumps commanded on does not match the number of operating
equipment but is rather related to the fractional flow rate. 
The simulation also exhibits the lead/lag rotation of the primary pumps.
</li>
<li>
Headered secondary pumps with ∆p control (component <code>staPumSecDp</code>):
the number of pumps commanded on does not match the number of operating
equipment but is rather related to the fractional flow rate.
All pumps are disabled when the plant is disabled.
</li>
</ul>
</html>",
      revisions="<html>
<ul>
<li>
July 10, 2024, by Antoine Gautier:<br/>
Updated the model with <code>StatusEmulator</code>.
</li>
<li>
March 29, 2024, by Antoine Gautier:<br/>
First implementation.
</li>
</ul>
</html>"),
    Icon(
      graphics={
        Ellipse(
          lineColor={75,138,73},
          fillColor={255,255,255},
          fillPattern=FillPattern.Solid,
          extent={{-100,-100},{100,100}}),
        Polygon(
          lineColor={0,0,255},
          fillColor={75,138,73},
          pattern=LinePattern.None,
          fillPattern=FillPattern.Solid,
          points={{-36,60},{64,0},{-36,-60},{-36,60}})}),
    Diagram(
      coordinateSystem(
        extent={{-120,-100},{120,100}}),
      graphics={
        Polygon(
          points={{214,66},{214,66}},
          lineColor={28,108,200})}));
end StagingHeadered;
