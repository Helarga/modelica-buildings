within Buildings.Applications.DHC.EnergyTransferStations.Validation;
model ETSController "Validation of ETS controller."

  Control.SubstationMainController
    ETSCon(THys=1)
              annotation (Placement(transformation(extent={{40,-10},{60,10}})));
  BaseClasses.Constants cons(
    k={38 + 273.15,40 + 273.15,35 + 273.15,7 + 273.15,8 + 273.15,12 + 273.15},
    conNam={"TTanHotTop","TTanHotBot","TSetHea","TSetCoo","TTanCooTop",
        "TTanCooBot"},
    nCon=6) annotation (Placement(transformation(extent={{-60,-10},{-40,10}})));
equation
  connect(cons.y[1], ETSCon.TTanHeaTop) annotation (Line(points={{-39,-0.83333},
          {-38,-0.83333},{-38,9},{39,9}}, color={0,0,127}));
  connect(cons.y[2], ETSCon.TTanHeaBot) annotation (Line(points={{-39,-0.5},{
          -38,-0.5},{-38,7},{39,7}}, color={0,0,127}));
  connect(cons.y[3], ETSCon.TSetHea) annotation (Line(points={{-39,-0.16667},{
          -38,-0.16667},{-38,5},{39,5}}, color={0,0,127}));
  connect(cons.y[4], ETSCon.TSetCoo) annotation (Line(points={{-39,0.16667},{
          -38,0.16667},{-38,-5},{39,-5}}, color={0,0,127}));
  connect(cons.y[5], ETSCon.TTanCooTop) annotation (Line(points={{-39,0.5},{-38,
          0.5},{-38,-7.2},{39,-7.2}}, color={0,0,127}));
  connect(cons.y[6], ETSCon.TTanCooBot) annotation (Line(points={{-39,0.83333},
          {-38,0.83333},{-38,-9.4},{39,-9.4}}, color={0,0,127}));
  annotation (Icon(coordinateSystem(preserveAspectRatio=false, extent={{-100,
            -120},{100,100}}),                                  graphics={
        Ellipse(lineColor = {75,138,73},
                fillColor={255,255,255},
                fillPattern = FillPattern.Solid,
                extent={{-98,-100},{98,98}}),
        Polygon(lineColor = {0,0,255},
                fillColor = {75,138,73},
                pattern = LinePattern.None,
                fillPattern = FillPattern.Solid,
                points={{-30,64},{70,4},{-30,-56},{-30,64}})}),  Diagram(coordinateSystem(preserveAspectRatio=false, extent={{-100,
            -100},{100,100}}),
        graphics={Line(points={{-22,22}}, color={28,108,200})}),
    experiment(StopTime=14400),
    __Dymola_Commands(
  file="modelica://Buildings/Resources/Scripts/Dymola/Applications/DHC/EnergyTransferStations/Validation/ETSController.mos"
        "Simulate and plot"),
         experiment(Tolerance=1e-6, StopTime=14400));
end ETSController;
