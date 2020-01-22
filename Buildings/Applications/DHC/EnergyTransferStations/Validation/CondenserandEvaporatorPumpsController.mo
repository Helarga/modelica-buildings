within Buildings.Applications.DHC.EnergyTransferStations.Validation;
model CondenserandEvaporatorPumpsController
  Buildings.Applications.DHC.EnergyTransferStations.Control.CondenserAndEvaporatorPumpsController pumEvaConCon
    "Evaporator and condenser pumps controller"
    annotation (Placement(transformation(extent={{64,-16},{90,16}})));
  Buildings.Controls.OBC.CDL.Continuous.Sources.Ramp uMod(
    height=10,
    duration(displayUnit="h") = 14400,
    offset=-5,
    startTime=0)
      "HeatPump operational mode input signal"
       annotation (Placement(transformation(extent={{-96,-10},{-76,10}})));
  Modelica.Blocks.Sources.Constant TConLvg(k=39 + 273.15)
    annotation (Placement(transformation(extent={{-40,10},{-20,30}})));
  Modelica.Blocks.Sources.Constant THeaSet(k=45 + 273.15)
    annotation (Placement(transformation(extent={{-40,74},{-20,94}})));
  Modelica.Blocks.Sources.Constant TConEnt(k=30 + 273.15)
    annotation (Placement(transformation(extent={{-40,42},{-20,62}})));
  Modelica.Blocks.Sources.Constant TEvaEnt(k=12 + 273.15)
    annotation (Placement(transformation(extent={{-40,-30},{-20,-10}})));
  Modelica.Blocks.Sources.Constant TEvaLvg(k=8 + 273.15)
    annotation (Placement(transformation(extent={{-40,-60},{-20,-40}})));
  Modelica.Blocks.Sources.Constant TCooSet(k=6 + 273.15)
    annotation (Placement(transformation(extent={{-40,-90},{-20,-70}})));
  Modelica.Blocks.Sources.Constant minPumEva(k=0.2)
    "Minimum speed of the evaporator pump to charge the cold buffer tank"
    annotation (Placement(transformation(extent={{20,-40},{40,-20}})));
  Modelica.Blocks.Sources.Constant minPumCon(k=0.2)
    "Minimum speed of the condenser pump to charge the hot buffer tank"
    annotation (Placement(transformation(extent={{20,20},{40,40}})));
  Modelica.Blocks.Math.RealToInteger realToInteger
    annotation (Placement(transformation(extent={{-68,-10},{-48,10}})));
equation
  connect(pumEvaConCon.TSetHea, THeaSet.y) annotation (Line(points={{63.09,
          10.08},{-10,10.08},{-10,84},{-19,84}}, color={0,0,127}));
  connect(pumEvaConCon.TConEnt, TConEnt.y) annotation (Line(points={{63.09,7.2},
          {-14,7.2},{-14,52},{-19,52}}, color={0,0,127}));
  connect(TEvaEnt.y, pumEvaConCon.TEvaEnt) annotation (Line(points={{-19,-20},{
          -14,-20},{-14,-3.04},{63.09,-3.04}}, color={0,0,127}));
  connect(pumEvaConCon.TEvaLvg, TEvaLvg.y) annotation (Line(points={{63.09,-6.24},
          {28,-6.24},{28,-6},{-12,-6},{-12,-50},{-19,-50}}, color={0,0,127}));
  connect(pumEvaConCon.TSetCoo, TCooSet.y) annotation (Line(points={{63.09,-8.8},
          {-8,-8.8},{-8,-80},{-19,-80}}, color={0,0,127}));
  connect(pumEvaConCon.pumEvaMin, minPumEva.y) annotation (Line(points={{63.09,
          -12.64},{50,-12.64},{50,-30},{41,-30}}, color={0,0,127}));
  connect(pumEvaConCon.pumConMin, minPumCon.y) annotation (Line(points={{63.09,
          14.56},{48,14.56},{48,30},{41,30}}, color={0,0,127}));
  connect(TConLvg.y, pumEvaConCon.TConLvg) annotation (Line(points={{-19,20},{-16,
          20},{-16,4},{63.09,4}}, color={0,0,127}));
  connect(uMod.y, realToInteger.u)
    annotation (Line(points={{-74,0},{-70,0}}, color={0,0,127}));
  connect(pumEvaConCon.heaPumMod, realToInteger.y) annotation (Line(points={{
          62.31,0.16},{12,0.16},{12,0},{-47,0}}, color={255,127,0}));
  annotation (Icon(coordinateSystem(preserveAspectRatio=false), graphics={
        Ellipse(lineColor = {75,138,73},
                fillColor={255,255,255},
                fillPattern = FillPattern.Solid,
                extent={{-98,-100},{98,98}}),
        Polygon(lineColor = {0,0,255},
                fillColor = {75,138,73},
                pattern = LinePattern.None,
                fillPattern = FillPattern.Solid,
                points={{-30,64},{70,4},{-30,-56},{-30,64}})}), Diagram(
        coordinateSystem(preserveAspectRatio=false)));
end CondenserandEvaporatorPumpsController;
