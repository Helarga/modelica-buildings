within Buildings.Applications.DHC.CentralPlants.Heating.Generation4;
model BoilerParallel "Multiple identical boiler"
  extends
    Buildings.Applications.DHC.CentralPlants.Heating.Generation4.PartialPlantParallel(
      val(dpFixed_nominal=7000));
  parameter Modelica.SIunits.MassFlowRate mBoi_flow_nominal;
  parameter Modelica.SIunits.HeatFlowRate Q_flow_nominal;
  parameter Modelica.SIunits.PressureDiffernce dp_nominalBoi;


  replaceable Buildings.Fluid.Boilers.BoilerPolynomial boi[num](
    m_flow_nominal=mBoi_flow_nominal,
    dp_nominal=dp_nominalBoi,
    energyDynamics=Modelica.Fluid.Types.Dynamics.FixedInitial,
    Q_flow_nominal=Q_flow_nominal,
    fue=Buildings.Fluid.Data.Fuels.HeatingOilLowerHeatingValue())
    "Hot water boiler"
    annotation (Placement(transformation(extent={{-12,-10},{8,10}})));
  Modelica.Blocks.Interfaces.RealInput PLR[num] "Part load ratio signal."
    annotation (Placement(transformation(extent={{-142,0},{-102,40}}),
        iconTransformation(extent={{-142,0},{-102,40}})));





equation
  connect(port_a, boi.port_a)
    annotation (Line(points={{-100,0},{-12,0}}, color={0,127,255}));
  connect(boi.port_b, val.port_a)
    annotation (Line(points={{8,0},{36,0}}, color={0,127,255}));
  for i in 1:num loop
    connect(val[i].port_b, port_b)
      annotation (Line(points={{56,0},{100,0}}, color={0,127,255}));
  end for;
  connect(PLR, boi.y) annotation (Line(points={{-122,20},{-86,20},{-86,8},{-14,8}},
        color={0,0,127}));
  annotation (    Documentation(info="<html>
<p>
This model implements a chiller parallel with <code>num</code> identical chillers. For the chiller model please see
<a href=\"modelica://Buildings.Fluid.Chillers.ElectricEIR\">Buildings.Fluid.Chillers.ElectricEIR</a>.
</p>
<p>
Note that although the chillers have identical nominal conditions, they can have different
performance curves specified in performance data <code>per</code>.
</p>
</html>", revisions="<html>
<ul>
<li>
June 30, 2017, by Yangyang Fu:<br/>
First implementation.
</li>
</ul>
</html>"));
end BoilerParallel;
