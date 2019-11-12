within Buildings.Applications.DHC.EnergyTransferStations.Examples;
model BufferTanksValidation
  "Validation of hot and cold buffer tanks."

  Control.ETSController
    ETSCon    annotation (Placement(transformation(extent={{50,-10},{70,8}})));

  Buildings.Controls.OBC.CDL.Continuous.Sources.Sine     TTanhotTop(
    amplitude=10,
    freqHz=1/3600,
    offset=30 + 273.15)
    "Load heat exchanger entering water temperature"
    annotation (Placement(transformation(extent={{8,60},{28,80}})));
  Modelica.Blocks.Sources.Sine     TTanCooBot(
    amplitude=2,
    freqHz=1/3600,
    offset=6 + 273.15)
    "Cooling setpoint temperature"
    annotation (Placement(transformation(extent={{-6,-66},{14,-46}})));
  Modelica.Blocks.Sources.Constant THeaSet(k=35 + 273.15)
    "Heating set point temperature"
    annotation (Placement(transformation(extent={{-38,12},{-18,32}})));
  Modelica.Blocks.Sources.Constant TTanCooTop(k=6 + 273.15)
    "Maximum heating set point temperature"
    annotation (Placement(transformation(extent={{20,-82},{40,-62}})));
  Modelica.Blocks.Sources.Constant TTanHotBot(k=40 + 273.15)
    "Minimum heating set point temperature"
    annotation (Placement(transformation(extent={{-16,32},{4,52}})));
  Modelica.Blocks.Sources.Constant TCooSet(k=7 + 273.15)
    "Cooling setpoint temperature"
    annotation (Placement(transformation(extent={{-30,-50},{-10,-30}})));
  Modelica.Blocks.Sources.Constant mhotNor(k=0.2)
    "Heating set point temperature"
    annotation (Placement(transformation(extent={{-68,-4},{-48,16}})));
  Modelica.Blocks.Sources.Constant mColNor(k=0.2)
    "Heating set point temperature"
    annotation (Placement(transformation(extent={{-68,-34},{-48,-14}})));
equation
  connect(THeaSet.y, ETSCon.TSetHea) annotation (Line(points={{-17,22},{26,22},{
          26,3.5},{49,3.5}}, color={0,0,127}));
  connect(TTanhotTop.y, ETSCon.TTanHeaTop) annotation (Line(points={{30,70},{40,
          70},{40,7.1},{49,7.1}}, color={0,0,127}));
  connect(TTanHotBot.y, ETSCon.TTanHeaBot) annotation (Line(points={{5,42},{32,42},
          {32,5.3},{49,5.3}}, color={0,0,127}));
  connect(ETSCon.TTanCooTop, TTanCooTop.y) annotation (Line(points={{49,-9.1},{46,
          -9.1},{46,-72},{41,-72}}, color={0,0,127}));
  connect(TTanCooBot.y, ETSCon.TTanCooBot) annotation (Line(points={{15,-56},{36,
          -56},{36,-7.3},{49,-7.3}}, color={0,0,127}));
  connect(TCooSet.y, ETSCon.TSetCoo) annotation (Line(points={{-9,-40},{28,-40},
          {28,-5.5},{49,-5.5}}, color={0,0,127}));
  connect(mhotNor.y, ETSCon.mTanHotNor) annotation (Line(points={{-47,6},{24,6},
          {24,0},{49,0},{49,-0.1}}, color={0,0,127}));
  connect(mColNor.y, ETSCon.mTanColNor) annotation (Line(points={{-47,-24},{24,
          -24},{24,-1.9},{49,-1.9}},
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
    experiment(StopTime=14400),
    __Dymola_Commands(
  file="modelica://Buildings/Resources/Scripts/Dymola/Applications/DHC/EnergyTransferStations/Control/HeatpumpController.mos"
        "Simulate and plot"),
         experiment(Tolerance=1e-6, StopTime=14400));
end BufferTanksValidation;
