within Buildings.Applications.DHC.EnergyTransferStations.Validation;
model AmbientCircuitControllerSidBlock
  "AmbientCircuitControllerValidation"
  package Medium = Buildings.Media.Water "Medium model";

  parameter Modelica.SIunits.TemperatureDifference dTHex = 5
    "Temperature difference in and out of substation heat exchanger";
   parameter Modelica.SIunits.TemperatureDifference dTGeo= 5
    "Temperature difference in and out of borefield";

  Modelica.Blocks.Sources.BooleanPulse valHea(width=50, period=500)
   "Heating side valve status"
    annotation (Placement(transformation(extent={{-40,40},{-20,60}})));
  Modelica.Blocks.Sources.BooleanConstant valCoo(k=false)
    "Cooling side valve status"
    annotation (Placement(transformation(extent={{-42,10},{-22,30}})));
  Modelica.Blocks.Sources.Constant TBorMaxEnt(k=30 + 273.15)
    "Borefield Maximum entering water temperature"
    annotation (Placement(transformation(extent={{-40,-40},{-20,-20}})));
  Modelica.Blocks.Sources.BooleanPulse rejFulHea(
    width=25,
    period=500,
    startTime=0)
    "Rejection full heat load mode."
    annotation (Placement(transformation(extent={{-80,10},{-60,30}})));
  Modelica.Blocks.Sources.BooleanConstant reqHea "Heating is required"
    annotation (Placement(transformation(extent={{-40,70},{-20,90}})));
  Modelica.Blocks.Sources.Constant TDisHexEnt(k=18 + 273.15)
    "District heat exchnager entering water temperature"
    annotation (Placement(transformation(extent={{-40,-70},{-20,-50}})));
  Control.AmbientCircuitSid ambCirCon(
      dTHex=dTHex,
      dTGeo=dTGeo)
  "Ambient water circuit control"
    annotation (Placement(transformation(extent={{40,-20},{60,0}})));
  Modelica.Blocks.Sources.Constant TDisHexLvg(k=12 + 273.15)
    "District heat exchnager leaving water temperature"
    annotation (Placement(transformation(extent={{-80,-90},{-60,-70}})));
  Modelica.Blocks.Sources.Constant TBorEnt(k=12 + 273.15)
    "Borefiled entering water temperature"
    annotation (Placement(transformation(extent={{-40,-140},{-20,-120}})));
  Modelica.Blocks.Sources.BooleanConstant reqCoo(k=false)
    "Cooling is required."
    annotation (Placement(transformation(extent={{-80,-50},{-60,-30}})));
  Modelica.Blocks.Sources.BooleanConstant
                                       rejFulCoo(k=false)
                 "Reject full cooling load mode."
    annotation (Placement(transformation(extent={{-80,-20},{-60,0}})));
  Modelica.Blocks.Sources.Constant TBorLvg(k=20 + 273.15)
    "Borefiled Leaving water temperature"
    annotation (Placement(transformation(extent={{-80,-122},{-60,-102}})));
equation
  connect(valCoo.y,ambCirCon. valCoo) annotation (Line(points={{-21,20},{-10,
          20},{-10,-4.8},{39,-4.8}},
                             color={255,0,255}));
  connect(valHea.y,ambCirCon. valHea) annotation (Line(points={{-19,50},{-8,
          50},{-8,-2.4},{39,-2.4}},
                            color={255,0,255}));
  connect(TDisHexEnt.y,ambCirCon. TDisHexEnt) annotation (Line(points={{-19,-60},
          {2,-60},{2,-15},{39,-15}},     color={0,0,127}));
  connect(ambCirCon.TBorMaxEnt, TBorMaxEnt.y) annotation (Line(points={{39,-13},
          {-6,-13},{-6,-30},{-19,-30}},
                                    color={0,0,127}));
  connect(TBorEnt.y, ambCirCon.TBorEnt) annotation (Line(points={{-19,-130},{
          30,-130},{30,-20.8},{39,-20.8}},
                                color={0,0,127}));
  connect(reqHea.y, ambCirCon.reqHea) annotation (Line(points={{-19,80},{-2,
          80},{-2,-0.2},{39,-0.2}},
                             color={255,0,255}));
  connect(rejFulHea.y, ambCirCon.rejHeaFulLoa) annotation (Line(points={{-59,20},
          {-46,20},{-46,-7},{39,-7}},
                                    color={255,0,255}));
  connect(reqCoo.y, ambCirCon.reqCoo) annotation (Line(points={{-59,-40},{-46,
          -40},{-46,-11},{39,-11}},
                             color={255,0,255}));
  connect(ambCirCon.rejCooFulLoa, rejFulCoo.y) annotation (Line(points={{39,-9},
          {-52,-9},{-52,-10},{-59,-10}},
                                       color={255,0,255}));
  connect(ambCirCon.TBorLvg, TBorLvg.y) annotation (Line(points={{39,-18.8},{
          20,-18.8},{20,-112},{-59,-112}}, color={0,0,127}));
  connect(ambCirCon.TDisHexLvg, TDisHexLvg.y) annotation (Line(points={{39,
          -17},{12,-17},{12,-80},{-59,-80}}, color={0,0,127}));
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
  file="modelica://Buildings/Resources/Scripts/Dymola/Applications/DHC/EnergyTransferStations/Validation/AmbientCircuitControllerSidBlock.mos"
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
