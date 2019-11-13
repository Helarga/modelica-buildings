within Buildings.Applications.DHC.EnergyTransferStations.Examples;
model HeatpumpControllerBlock
  "Reverse heatpump controller operates in heating mode only"
  package Medium = Buildings.Media.Water "Medium model";

  Control.HeatPumpController heaPumCon
    annotation (Placement(transformation(extent={{74,0},{94,20}})));

  Modelica.Blocks.Sources.BooleanPulse heaMod(width=50, period=500)
    "Step control"
    annotation (Placement(transformation(extent={{0,60},{20,80}})));
  Modelica.Blocks.Sources.BooleanPulse cooMod(
    width=50,
    period=500,
    startTime=125)
    "Step control"
    annotation (Placement(transformation(extent={{0,30},{20,50}})));
  Modelica.Blocks.Sources.Constant TCooSet(k=7 + 273.15)
    "Cooling setpoint temperature"
    annotation (Placement(transformation(extent={{-40,36},{-20,56}})));
  Modelica.Blocks.Sources.Constant THeaSetMax(k=50 + 273.15)
    "Maximum heating set point temperature"
    annotation (Placement(transformation(extent={{-40,2},{-20,22}})));
  Modelica.Blocks.Sources.Constant TEvaLvg(k=8 + 273.15)
    "Evaporator leaving water temperature"
    annotation (Placement(transformation(extent={{0,-80},{20,-60}})));
  Modelica.Blocks.Sources.Constant THeaSet(k=40 + 273.15)
    "Heating set point temperature"
    annotation (Placement(transformation(extent={{-40,70},{-20,90}})));
  Modelica.Blocks.Sources.Constant TMinConEnt(k=25 + 273.15)
    "Minimum condenser entering temperature"
    annotation (Placement(transformation(extent={{-40,-40},{-20,-20}})));
  Modelica.Blocks.Sources.Constant TMaxEvaEnt(k=18 + 273.15)
    "Maximum Evaporator entering temperature."
    annotation (Placement(transformation(extent={{-40,-80},{-20,-60}})));
  Modelica.Blocks.Sources.Constant TEvaEnt(k=19 + 273.15)
    "Evaporator entering temperature"
    annotation (Placement(transformation(extent={{0,-40},{20,-20}})));
  Modelica.Blocks.Sources.Constant TConEnt(k=26 + 273.15)
    "Condenser entering temperature"
    annotation (Placement(transformation(extent={{0,-120},{20,-100}})));
equation
  connect(heaPumCon.reqHea, heaMod.y)
  annotation (Line(points={{72.6,19},{70,19},{70,70},{21,70}},
                              color={255,0,255}));
  connect(heaPumCon.reqCoo,cooMod. y) annotation (Line(points={{72.6,16},{68,16},
          {68,40},{21,40}},     color={255,0,255}));
  connect(heaPumCon.TSetCoo, TCooSet.y) annotation (Line(points={{73,11},{-6,11},
          {-6,14},{-10,14},{-10,46},{-19,46}},
                                      color={0,0,127}));
  connect(THeaSetMax.y, heaPumCon.TSetHeaMax) annotation (Line(points={{-19,12},
          {-12,12},{-12,8.8},{73,8.8}},   color={0,0,127}));
  connect(TEvaLvg.y,heaPumCon.TEvaLvg)  annotation (Line(points={{21,-70},{36,
          -70},{36,1.6},{73,1.6}}, color={0,0,127}));
  connect(THeaSet.y, heaPumCon.TSetHea) annotation (Line(points={{-19,80},{-6,
          80},{-6,13},{73,13}},    color={0,0,127}));
  connect(TMinConEnt.y, heaPumCon.TMinConEnt) annotation (Line(points={{-19,-30},
          {-12,-30},{-12,7.2},{73,7.2}},   color={0,0,127}));
  connect(TMaxEvaEnt.y, heaPumCon.TMaxEvaEnt) annotation (Line(points={{-19,-70},
          {-6,-70},{-6,5.4},{73,5.4}},   color={0,0,127}));
  connect(TEvaEnt.y, heaPumCon.TEvaEnt) annotation (Line(points={{21,-30},{26,
          -30},{26,3.4},{73,3.4}},
                                color={0,0,127}));
  connect(TConEnt.y, heaPumCon.TConEnt) annotation (Line(points={{21,-110},{38,
          -110},{38,0.2},{73,0.2}},
                                color={0,0,127}));
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
            {-100,-140},{100,100}}),
        graphics={Line(points={{-22,22}}, color={28,108,200})}),
    experiment(StopTime=86400),
    __Dymola_Commands(
  file="modelica://Buildings/Resources/Scripts/Dymola/Applications/DHC/EnergyTransferStations/Control/HeatpumpControllerBlock.mos"
        "Simulate and plot"),
         experiment(Tolerance=1e-6, StopTime=2000),
Documentation(info="<html>
<p>
This model validates the controller block
<a href=\"Buildings.Applications.DHC.EnergyTransferStations.Control.HeatPumpController\"> 
Buildings.Applications.DHC.EnergyTransferStations.Control.HeatPumpController</a>.
<p>

</html>", revisions="<html>
<ul>
<li>
 <br/>
</li>
</ul>
</html>"));
end HeatpumpControllerBlock;
