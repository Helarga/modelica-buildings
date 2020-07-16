within Buildings.Applications.DataCenters.ChillerCooled.Equipment;
model FlowMachine_dp "Identical speed controlled flow machines"
  extends
    Buildings.Applications.DataCenters.ChillerCooled.Equipment.BaseClasses.PartialPumpParallel(
    redeclare final Buildings.Fluid.Movers.FlowControlled_dp pum(
      each final m_flow_nominal= m_flow_nominal,
      prescribeSystemPressure=true),
    rhoStd=Medium.density_pTX(101325, 273.15+4, Medium.X_default));
      //each dp_nominal=10000,
      //each final per=per,

  Modelica.Blocks.Interfaces.RealInput dpSet[num]
    "Continuous input signal for the flow machine" annotation (Placement(
        transformation(extent={{-140,54},{-100,94}}), iconTransformation(extent=
           {{-140,-42},{-100,-2}})));
  Modelica.Blocks.Interfaces.RealInput dpMea[num]
    "Continuous input signal for the flow machine" annotation (Placement(
        transformation(extent={{-140,-100},{-100,-60}}), iconTransformation(
          extent={{-140,-100},{-100,-60}})));
equation
  connect(dpMea, pum.dpMea) annotation (Line(points={{-120,-80},{-60,-80},{-60,
          28},{-8,28},{-8,12}}, color={0,0,127}));
  connect(dpSet, pum.dp_in) annotation (Line(points={{-120,74},{-72,74},{-72,36},
          {0,36},{0,12}}, color={0,0,127}));
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
end FlowMachine_dp;
