within Buildings.Applications.DHC.CentralPlants.Heating.Generation4.Subsystems;
model FlowMachine_y "Identical speed controlled flow machines"
  extends
    Buildings.Applications.DHC.CentralPlants.Heating.Generation4.Subsystems.PartialPumpParallel(
    redeclare final Buildings.Fluid.Movers.SpeedControlled_y pum,
    rhoStd=Medium.density_pTX(101325, 273.15+4, Medium.X_default));

equation
  connect(booToRea.y, pum.y) annotation (Line(points={{2,-40},{10,-40},{10,-20},
          {-32,-20},{-32,28},{-10,28},{-10,12}}, color={0,0,127}));
  annotation (    Documentation(revisions="<html>
<ul>
<li>
July 27, 2017, by Yangyang Fu:<br/>
First implementation.
</li>
</ul>
</html>", info="<html>
<p>This model implements a parallel of identical pumps with speed being controlled.
The number can be specified by setting a value of <code>num</code>.
The shutoff valves are used to avoid circulating flow among pumps.
</p>
</html>"));
end FlowMachine_y;
