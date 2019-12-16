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
    annotation (Placement(transformation(extent={{-42,40},{-22,60}})));
  Modelica.Blocks.Sources.BooleanConstant valCoo(k=false)
    "Cooling side valve status"
    annotation (Placement(transformation(extent={{-42,10},{-22,30}})));
  Modelica.Blocks.Sources.Constant TBorMaxEnt(k=30 + 273.15)
    "Borefield Maximum entering water temperature"
    annotation (Placement(transformation(extent={{-42,-58},{-22,-38}})));
  Modelica.Blocks.Sources.BooleanPulse rejFulHea(period=500)
    "Rejection full heat load mode."
    annotation (Placement(transformation(extent={{-80,10},{-60,30}})));
  Modelica.Blocks.Sources.BooleanConstant reqHea "Heating is required"
    annotation (Placement(transformation(extent={{-42,72},{-22,92}})));
  Modelica.Blocks.Sources.Constant TDisHexEnt(k=18 + 273.15)
    "District heat exchnager entering water temperature"
    annotation (Placement(transformation(extent={{-42,-88},{-22,-68}})));
  Control.AmbientCircuitSid ambCirCon(
      dTHex=dTHex,
      dTGeo=dTGeo)
  "Ambient water circuit control"
    annotation (Placement(transformation(extent={{10,-10},{30,10}})));
  Modelica.Blocks.Sources.Constant TDisHexLvg(k=12 + 273.15)
    "District heat exchnager leaving water temperature"
    annotation (Placement(transformation(extent={{-42,-118},{-22,-98}})));
  Modelica.Blocks.Sources.Constant TBorEnt(k=12 + 273.15)
    "Borefiled entering water temperature"
    annotation (Placement(transformation(extent={{-42,-152},{-22,-132}})));
  Modelica.Blocks.Sources.BooleanConstant reqCoo(k=false)
    "Cooling is required."
    annotation (Placement(transformation(extent={{-80,-50},{-60,-30}})));
  Modelica.Blocks.Sources.BooleanPulse rejFulCoo(
    width=1,
    period=500,
    startTime=0) "Reject full cooling load mode."
    annotation (Placement(transformation(extent={{-80,-20},{-60,0}})));
equation
  connect(valCoo.y,ambCirCon. valCoo) annotation (Line(points={{-21,20},{-10,20},
          {-10,5.2},{9,5.2}},color={255,0,255}));
  connect(valHea.y,ambCirCon. valHea) annotation (Line(points={{-21,50},{-8,50},
          {-8,7.6},{9,7.6}},color={255,0,255}));
  connect(TDisHexLvg.y,ambCirCon. TDisHexLvg) annotation (Line(points={{-21,
          -108},{4,-108},{4,-7.2},{9,-7.2}},
                                        color={0,0,127}));
  connect(TDisHexEnt.y,ambCirCon. TDisHexEnt) annotation (Line(points={{-21,-78},
          {2,-78},{2,-5.2},{9,-5.2}},    color={0,0,127}));
  connect(ambCirCon.TBorMaxEnt, TBorMaxEnt.y) annotation (Line(points={{9,-3},{
          -6,-3},{-6,-48},{-21,-48}},
                                    color={0,0,127}));
  connect(TBorEnt.y, ambCirCon.TBorEnt) annotation (Line(points={{-21,-142},{6,
          -142},{6,-9.8},{9,-9.8}},
                                color={0,0,127}));
  connect(reqHea.y, ambCirCon.reqHea) annotation (Line(points={{-21,82},{-2,82},
          {-2,9.8},{9,9.8}}, color={255,0,255}));
  connect(rejFulHea.y, ambCirCon.rejHeaFulLoa) annotation (Line(points={{-59,20},
          {-46,20},{-46,3},{9,3}},  color={255,0,255}));
  connect(reqCoo.y, ambCirCon.reqCoo) annotation (Line(points={{-59,-40},{-46,
          -40},{-46,-1},{9,-1}},
                             color={255,0,255}));
  connect(ambCirCon.rejCooFulLoa, rejFulCoo.y) annotation (Line(points={{9,1},{
          -52,1},{-52,-10},{-59,-10}}, color={255,0,255}));
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
  file="modelica://Buildings/Resources/Scripts/Dymola/Applications/DHC/EnergyTransferStations/Control/AmbientCircuitControllerSidBlock.mos"
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
