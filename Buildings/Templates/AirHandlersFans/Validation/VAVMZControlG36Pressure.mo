within Buildings.Templates.AirHandlersFans.Validation;
model VAVMZControlG36Pressure
  "Validation model for multiple-zone VAV"
  extends VAVMZBase(
    datAll(redeclare model VAV =
      UserProject.AirHandlersFans.VAVMZControlG36Pressure),
    redeclare UserProject.AirHandlersFans.VAVMZControlG36Pressure VAV_1);

  annotation (
  __Dymola_Commands(
  file="modelica://Buildings/Resources/Scripts/Dymola/Templates/AirHandlersFans/Validation/VAVMZControlG36Pressure.mos"
  "Simulate and plot"),
  experiment(Tolerance=1e-6, StopTime=10000), Documentation(info="<html>
<p>
This is a validation model for the configuration represented by
<a href=\"modelica://Buildings.Templates.AirHandlersFans.Validation.UserProject.AirHandlersFans.VAVMZControlG36Pressure\">
Buildings.Templates.AirHandlersFans.Validation.UserProject.AirHandlersFans.VAVMZControlG36Pressure</a>.
It is intended to check whether the template model is well-defined for
this particular system configuration.
</p>
</html>"));
end VAVMZControlG36Pressure;