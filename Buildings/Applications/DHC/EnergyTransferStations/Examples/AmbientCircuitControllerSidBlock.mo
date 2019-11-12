within Buildings.Applications.DHC.EnergyTransferStations.Examples;
model AmbientCircuitControllerSidBlock
  "AmbientCircuitControllerValidation"
  package Medium = Buildings.Media.Water "Medium model";

  parameter Modelica.SIunits.TemperatureDifference dTHex = 5
    "Temperature difference in and out of substation heat exchanger";
   parameter Modelica.SIunits.TemperatureDifference dTGeo= 5
    "Temperature difference in and out of borefield";

  Modelica.Blocks.Sources.BooleanPulse valHea(width=50, period=500)
   "Heating side valve status"
    annotation (Placement(transformation(extent={{-20,40},{0,60}})));
  Modelica.Blocks.Sources.BooleanConstant valCoo(k=false)
    "Cooling side valve status"
    annotation (Placement(transformation(extent={{-20,10},{0,30}})));
  Modelica.Blocks.Sources.Constant TBorMaxEnt(k=30 + 273.15)
    "Borefield Maximum entering water temperature"
    annotation (Placement(transformation(extent={{-20,-58},{0,-38}})));
  Modelica.Blocks.Sources.BooleanPulse rejFulHea(period=500)
    "Rejection full heat load mode."
    annotation (Placement(transformation(extent={{-56,10},{-36,30}})));
  Modelica.Blocks.Sources.BooleanConstant reqHea "Heating is required"
    annotation (Placement(transformation(extent={{-20,72},{0,92}})));
  Modelica.Blocks.Sources.Constant TDisHexEnt(k=18 + 273.15)
    "District heat exchnager entering water temperature"
    annotation (Placement(transformation(extent={{-20,-88},{0,-68}})));
  Control.AmbientCircuitSid ambCirCon(
      dTHex=dTHex,
      dTGeo=dTGeo)
  "Ambient water circuit control"
    annotation (Placement(transformation(extent={{32,-10},{52,10}})));
  Modelica.Blocks.Sources.Constant TDisHexLvg(k=12 + 273.15)
    "District heat exchnager leaving water temperature"
    annotation (Placement(transformation(extent={{-20,-118},{0,-98}})));
  Modelica.Blocks.Sources.Constant TBorEnt(k=12 + 273.15)
    "Borefiled entering water temperature"
    annotation (Placement(transformation(extent={{-20,-152},{0,-132}})));
  Modelica.Blocks.Sources.BooleanConstant reqCoo(k=false)
    "Cooling is required."
    annotation (Placement(transformation(extent={{-56,-50},{-36,-30}})));
  Modelica.Blocks.Sources.BooleanPulse rejFulCoo(
    width=1,
    period=500,
    startTime=0) "Reject full cooling load mode."
    annotation (Placement(transformation(extent={{-56,-20},{-36,0}})));
equation
  connect(valCoo.y,ambCirCon. valCoo) annotation (Line(points={{1,20},{12,20},{12,
          5.2},{31,5.2}},    color={255,0,255}));
  connect(valHea.y,ambCirCon. valHea) annotation (Line(points={{1,50},{14,50},{14,
          7.6},{31,7.6}},   color={255,0,255}));
  connect(TDisHexLvg.y,ambCirCon. TDisHexLvg) annotation (Line(points={{1,-108},
          {26,-108},{26,-7.2},{31,-7.2}},
                                        color={0,0,127}));
  connect(TDisHexEnt.y,ambCirCon. TDisHexEnt) annotation (Line(points={{1,-78},{
          24,-78},{24,-5.2},{31,-5.2}},  color={0,0,127}));
  connect(ambCirCon.TBorMaxEnt, TBorMaxEnt.y) annotation (Line(points={{31,-3},{
          16,-3},{16,-48},{1,-48}}, color={0,0,127}));
  connect(TBorEnt.y, ambCirCon.TBorEnt) annotation (Line(points={{1,-142},{28,-142},
          {28,-9.8},{31,-9.8}}, color={0,0,127}));
  connect(reqHea.y, ambCirCon.requireHeat) annotation (Line(points={{1,82},{20,82},
          {20,9.8},{31,9.8}}, color={255,0,255}));
  connect(rejFulHea.y, ambCirCon.rejHeaFulLoa) annotation (Line(points={{-35,20},
          {-24,20},{-24,3},{31,3}}, color={255,0,255}));
  connect(reqCoo.y, ambCirCon.requireCold) annotation (Line(points={{-35,-40},{-24,
          -40},{-24,-1},{31,-1}}, color={255,0,255}));
  connect(ambCirCon.rejColFulLoa, rejFulCoo.y) annotation (Line(points={{31,1},{
          -30,1},{-30,-10},{-35,-10}}, color={255,0,255}));
  annotation (Icon(coordinateSystem(preserveAspectRatio=false), graphics={
        Ellipse(lineColor = {75,138,73},
                fillColor={255,255,255},
                fillPattern = FillPattern.Solid,
                extent={{-98,-100},{98,98}}),
        Polygon(lineColor = {0,0,255},
                fillColor = {75,138,73},
                pattern = LinePattern.None,
                fillPattern = FillPattern.Solid,
                points={{-30,64},{70,4},{-30,-56},{-30,64}})}),  Diagram(coordinateSystem(preserveAspectRatio=false, extent={{-100,
            -160},{100,100}}),
        graphics={Line(points={{-22,22}}, color={28,108,200})}),
    experiment(StopTime=1500),
    __Dymola_Commands(
  file="modelica://Buildings/Resources/Scripts/Dymola/Applications/DHC/EnergyTransferStations/Control/AmbientCircuitControllerBlock.mos"
        "Simulate and plot"),
         experiment(Tolerance=1e-6, StopTime=14400),
         Documentation(info="<html>
<p>
This model validates the controller block
<a href=\"Buildings.Applications.DHC.EnergyTransferStations.Control.AmbientCircuitController\"> 
Buildings.Applications.DHC.EnergyTransferStations.Control.AmbientCircuitController</a>.
<p>

</html>", revisions="<html>
<ul>
<li>
 <br/>
</li>
</ul>
</html>"));
end AmbientCircuitControllerSidBlock;
