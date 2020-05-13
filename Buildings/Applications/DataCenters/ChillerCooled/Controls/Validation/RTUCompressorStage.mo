within Buildings.Applications.DataCenters.ChillerCooled.Controls.Validation;
model RTUCompressorStage
  "Test of the compressor stageing controller."
  extends Modelica.Icons.Example;

  Buildings.Applications.DataCenters.ChillerCooled.Controls.RTUCompressorStage chiStaCon(tWai=1)
    "Staging controller for chillers"
    annotation (Placement(transformation(extent={{-40,-2},{-20,18}})));
  Modelica.Blocks.Sources.Constant TCooSet(
     k=24 + 273.15,
     y(unit= "K",
       displayUnit="degC")) "Indoor cooling setpoint temperature."
    annotation (Placement(transformation(extent={{-80,20},{-60,40}})));
  Modelica.Blocks.Sources.Pulse TIndMea(
    amplitude=-1*(0.5/(1.8)),
    period=200,
    offset=24 + 273.15) "Measured value of the indoor temperarture. "
    annotation (Placement(transformation(extent={{-80,-40},{-60,-20}})));
  Buildings.Controls.OBC.CDL.Logical.Switch stage2 "Mode index"
    annotation (Placement(transformation(extent={{60,10},{80,30}})));
  Buildings.Controls.OBC.CDL.Logical.Switch stage1 "Mode index"
    annotation (Placement(transformation(extent={{26,-70},{46,-50}})));
  Modelica.Blocks.Sources.Constant comOff(k=0) "The compressor is off."
    annotation (Placement(transformation(extent={{-40,-90},{-20,-70}})));
  Modelica.Blocks.Sources.Constant sta1(k=1) "The compressor run on stage1 "
    annotation (Placement(transformation(extent={{-40,60},{-20,80}})));
  Modelica.Blocks.Sources.Constant sta2(k=0.6) "The compressor run on stage1 "
    annotation (Placement(transformation(extent={{-40,-50},{-20,-30}})));
equation
  connect(chiStaCon.TSet, TCooSet.y) annotation (Line(points={{-41,4},{-48,4},{
          -48,30},{-59,30}},
                           color={0,0,127}));
  connect(chiStaCon.TIndMea, TIndMea.y) annotation (Line(points={{-41,0.2},{-48,
          0.2},{-48,-30},{-59,-30}},color={0,0,127}));
  connect(chiStaCon.stage2Boo, stage2.u2) annotation (Line(points={{-19,14},{-4,
          14},{-4,20},{58,20}},color={255,0,255}));
  connect(chiStaCon.stage1Boo1, stage1.u2) annotation (Line(points={{-19,2},{-4,
          2},{-4,-60},{24,-60}},     color={255,0,255}));
  connect(stage1.u3, comOff.y) annotation (Line(points={{24,-68},{0,-68},{0,-80},
          {-19,-80}}, color={0,0,127}));
  connect(stage1.y, stage2.u3) annotation (Line(points={{48,-60},{60,-60},{60,
          -10},{40,-10},{40,12},{58,12}}, color={0,0,127}));
  connect(sta2.y, stage1.u1) annotation (Line(points={{-19,-40},{0,-40},{0,-52},
          {24,-52}}, color={0,0,127}));
  connect(sta1.y, stage2.u1) annotation (Line(points={{-19,70},{0,70},{0,28},{
          58,28}}, color={0,0,127}));
  annotation (    __Dymola_Commands(file=
          "modelica://Buildings/Resources/Scripts/Dymola/Applications/DataCenters/ChillerCooled/Controls/Validation/RTUComperssorStage.mos"
        "Simulate and plot"),
    Documentation(info="<html>
<p>
This example test the RTU compressor staging controller implemented in
<a href=\"modelica://\">
</a>.
</p>
<p>

</p>
</html>", revisions="<html>
<ul>
<li>
May 13, 2020, by Hagar Elarga:<br/>
First implementation.
</li>
</ul>
</html>"),
experiment(
      StartTime=0,
      StopTime=1440,
      Tolerance=1e-06));
end RTUCompressorStage;
