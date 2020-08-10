within Buildings.Applications.DHC.CentralPlants.Heating.Generation4;
model BoilerParallel "Multiple identical boiler"
  extends
    Buildings.Applications.DHC.CentralPlants.Heating.Generation4.PartialPlantParallel(
        redeclare Buildings.Fluid.Boilers.BoilerPolynomial boi[num](
            each energyDynamics=Modelica.Fluid.Types.Dynamics.FixedInitial,
            each Q_flow_nominal= Q_flow_nominal,
            each m_flow_nominal=m_flow_nominal,
            each dp_nominal=dp_nominal,
            each effCur=Buildings.Fluid.Types.EfficiencyCurves.Constant,
            each a={0.9},
            each fue=Fluid.Data.Fuels.NaturalGasHigherHeatingValue()));
           // each a={0.9},

  parameter Modelica.SIunits.MassFlowRate m_flow_nominal;
  parameter Modelica.SIunits.PressureDifference dp_nominal;
  parameter Modelica.SIunits.HeatFlowRate Q_flow_nominal;

  Modelica.Blocks.Interfaces.RealInput PLR[num]
   "Part load ratio signal."
    annotation (Placement(transformation(extent={{-120,30},{-100,50}}),
        iconTransformation(extent={{-120,30},{-100,50}})));
  Modelica.Blocks.Interfaces.RealOutput TBoiLvg[num]
   "Boiler leaving water temperature."
    annotation (Placement(transformation(
          extent={{100,30},{120,50}}), iconTransformation(extent={{100,30},{120,
            50}})));
equation
  connect(boi.port_b, val.port_a)  annotation (Line(points={{0,0},{36,0}}, color={0,127,255}));
  for i in 1:num loop
    connect(val[i].port_b, port_b) annotation (Line(points={{56,0},{100,0}}, color={0,127,255}));
    connect(port_a, boi[i].port_a) annotation (Line(points={{-100,0},{-20,0}}, color={0,127,255}));
  end for;
  connect(PLR, filter.u) annotation (Line(points={{-110,40},{-80,40},{-80,84},{
          -55.2,84}},
                color={0,0,127}));
  connect(PLR, boi.y) annotation (Line(points={{-110,40},{-80,40},{-80,8},{-22,8}},
               color={0,0,127}));
  connect(boi.T, TBoiLvg) annotation (Line(points={{1,8},{20,8},{20,40},{110,40}},
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
</html>"), Icon(graphics={
        Line(
          points={{-92,0},{0,0}},
          color={28,108,200},
          thickness=1),
        Line(
          points={{0,0},{92,0}},
          color={238,46,47},
          thickness=1),
        Rectangle(extent={{-54,54},{54,-54}}, lineColor={102,44,145})}));
end BoilerParallel;
