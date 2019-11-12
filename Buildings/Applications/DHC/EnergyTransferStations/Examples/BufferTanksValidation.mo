within Buildings.Applications.DHC.EnergyTransferStations.Examples;
model BufferTanksValidation
  "Validation of hot and cold buffer tanks."

  Control.ETSController
    ETSCon(THys=1)
              annotation (Placement(transformation(extent={{42,-34},{62,-16}})));
  Buildings.Controls.OBC.CDL.Continuous.Sources.Constant TTanhotTop1(k=38 +
        273.15)
    "Load heat exchanger entering water temperature"
    annotation (Placement(transformation(extent={{0,36},{20,56}})));
  Modelica.Blocks.Sources.Constant TTanCooBot(k=8 + 273.15)
    "Cooling setpoint temperature"
    annotation (Placement(transformation(extent={{-14,-90},{6,-70}})));
  Modelica.Blocks.Sources.Constant THeaSet1(k=35 + 273.15)
    "Heating set point temperature"
    annotation (Placement(transformation(extent={{-46,-12},{-26,8}})));
  Modelica.Blocks.Sources.Constant TTanCooTop(k=12 + 273.15)
    "Maximum heating set point temperature"
    annotation (Placement(transformation(extent={{12,-106},{32,-86}})));
  Modelica.Blocks.Sources.Constant TTanHotBot1(k=40 + 273.15)
    "Minimum heating set point temperature"
    annotation (Placement(transformation(extent={{-24,8},{-4,28}})));
  Modelica.Blocks.Sources.Constant TCooSet(k=7 + 273.15)
    "Cooling setpoint temperature"
    annotation (Placement(transformation(extent={{-38,-74},{-18,-54}})));
  Modelica.Blocks.Sources.Constant mhotNor(k=0.2)
    "Heating set point temperature"
    annotation (Placement(transformation(extent={{-76,-28},{-56,-8}})));
  Modelica.Blocks.Sources.Constant mColNor(k=0.2)
    "Heating set point temperature"
    annotation (Placement(transformation(extent={{-76,-58},{-56,-38}})));
equation
  connect(THeaSet1.y, ETSCon.TSetHea) annotation (Line(points={{-25,-2},{18,-2},
          {18,-20.5},{41,-20.5}}, color={0,0,127}));
  connect(TTanhotTop1.y, ETSCon.TTanHeaTop) annotation (Line(points={{22,46},{
          32,46},{32,-16.9},{41,-16.9}}, color={0,0,127}));
  connect(TTanHotBot1.y, ETSCon.TTanHeaBot) annotation (Line(points={{-3,18},{
          24,18},{24,-18.7},{41,-18.7}}, color={0,0,127}));
  connect(ETSCon.TTanCooTop,TTanCooTop. y) annotation (Line(points={{41,-33.1},
          {38,-33.1},{38,-96},{33,-96}},
                                    color={0,0,127}));
  connect(TTanCooBot.y,ETSCon. TTanCooBot) annotation (Line(points={{7,-80},{28,
          -80},{28,-31.3},{41,-31.3}},
                                     color={0,0,127}));
  connect(TCooSet.y,ETSCon. TSetCoo) annotation (Line(points={{-17,-64},{20,-64},
          {20,-29.5},{41,-29.5}},
                                color={0,0,127}));
  connect(mhotNor.y,ETSCon. mTanHotNor) annotation (Line(points={{-55,-18},{16,
          -18},{16,-24},{41,-24},{41,-24.1}},
                                    color={0,0,127}));
  connect(mColNor.y,ETSCon. mTanColNor) annotation (Line(points={{-55,-48},{16,
          -48},{16,-25.9},{41,-25.9}},
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
