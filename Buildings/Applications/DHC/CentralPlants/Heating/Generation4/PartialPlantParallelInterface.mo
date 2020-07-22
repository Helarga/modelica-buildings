within Buildings.Applications.DHC.CentralPlants.Heating.Generation4;
partial model PartialPlantParallelInterface
  "Partial model that implements the interface for parallel plants"
  extends Buildings.Fluid.Interfaces.PartialTwoPortInterface;
  extends Buildings.Fluid.Interfaces.TwoPortFlowResistanceParameters(
     final computeFlowResistance=true);

  parameter Integer num(min=1)=2 "Number of equipment";

  Modelica.Blocks.Math.BooleanToReal booToRea[num](
    each final realTrue=1,
    each final realFalse=0)
    "Boolean to real (if true then 1 else 0)"
    annotation (Placement(transformation(extent={{-80,48},{-68,60}})));
  Modelica.Blocks.Interfaces.BooleanInput on[num]
    "Set to true to enable equipment, or false to disable equipment"
    annotation (Placement(transformation(extent={{-140,34},{-100,74}}),
        iconTransformation(extent={{-140,34},{-100,74}})));
equation
  connect(on, booToRea.u)
    annotation (Line(points={{-120,54},{-81.2,54}},
      color={255,0,255}));
  annotation (Icon(coordinateSystem(preserveAspectRatio=false), graphics={
        Rectangle(
          extent={{-80,80},{80,-80}},
          lineColor={0,0,255},
          pattern=LinePattern.None,
          fillColor={95,95,95},
          fillPattern=FillPattern.Solid)}),    Documentation(revisions="<html>
<ul>
<li>
June 30, 2017, by Yangyang Fu:<br/>
First implementation.
</li>
</ul>
</html>",
        info="<html>
This model implements the interface for the parallel plants
in the <a href=\"modelica://Buildings.Applications.DataCenters.ChillerCooled\">Buildings.Applications.DataCenters.ChillerCooled</a> package.
The parallel plants contain <code>num</code> identical plants
that share the same temperature setpoint at <code>port_b2</code>.
These plants can be operated individually by specifying different on/off signals.
</html>"));
end PartialPlantParallelInterface;
