within Buildings.Applications.DHC.CentralPlants.Heating.Generation4;
partial model PartialPlantParallel
  "Partial source plant model with associated valves"
  extends
    Buildings.Applications.DHC.CentralPlants.Heating.Generation4.PartialPlantParallelInterface;
  extends
    Buildings.Applications.DHC.CentralPlants.Heating.Generation4.ValveParameters(
    final deltaM= 0.1,
    rhoStd = Medium.density_pTX(101325, 273.15+50, Medium.X_default));

  extends
    Buildings.Applications.DataCenters.ChillerCooled.Equipment.BaseClasses.SignalFilter(
    final numFil=num);

  constant Boolean homotopyInitialization = true "= true, use homotopy method"
    annotation(HideResult=true);

  // Isolation valve parameters
  parameter Real l( min=1e-10, max=1) = 0.0001
    "Valve leakage, l=Kv(y=0)/Kv(y=1)"
    annotation(Dialog(group="Two-way valve"));
  parameter Real kFixed(unit="", min=0)= m_flow_nominal ./ sqrt( dp_nominal)
    "Flow coefficient of fixed resistance that may be in series with valve 1, k=m_flow/sqrt(dp), with unit=(kg.m)^(1/2)."
   annotation(Dialog(group="Two-way valve"));

  Buildings.Fluid.Actuators.Valves.TwoWayLinear val[num](
    redeclare each package Medium = Medium,
    each final allowFlowReversal=allowFlowReversal,
    each final m_flow_nominal=m_flow_nominal,
    each final deltaM=deltaM,
    each dpFixed_nominal=dp_nominal,
    each final show_T=show_T,
    each final homotopyInitialization=homotopyInitialization,
    each final use_inputFilter=false,
    each final riseTime=riseTimeValve,
    each final init=initValve,
    final y_start=yValve_start,
    each final l=l,
    each final kFixed=kFixed,
    each final dpValve_nominal=dpValve_nominal,
    each final CvData=Buildings.Fluid.Types.CvTypes.OpPoint,
    each final from_dp=from_dp,
    each final linearized=linearizeFlowResistance,
    each final rhoStd=rhoStd) "Isolation valves for on/off use" annotation (
      Placement(transformation(
        extent={{-10,-10},{10,10}},
        rotation=0,
        origin={46,0})));

  replaceable Buildings.Fluid.Boilers.BoilerPolynomial boi[num](
    redeclare each final package Medium = Medium,
    each effCur=Buildings.Fluid.Types.EfficiencyCurves.Constant,
    each from_dp=true,
    each T_start=293.15,
    each a={0.9},
    each fue=Fluid.Data.Fuels.NaturalGasHigherHeatingValue())
    "Hot water boiler"
    annotation (Placement(transformation(extent={{-20,-10},{0,10}})));

initial equation
  assert(homotopyInitialization, "In " + getInstanceName() +
    ": The constant homotopyInitialization has been modified from its default value. This constant will be removed in future releases.",
    level = AssertionLevel.warning);

equation

  if use_inputFilter then
  else
  end if;
  connect(y_actual, val.y)
    annotation (Line(points={{-20,74},{46,74},{46,12}}, color={0,0,127}));
  annotation (    Documentation(info="<html>
<p>
Partial model that can be extended to construct parallel chillers such as
<a href=\"modelica://Buildings.Applications.DataCenters.ChillerCooled.Equipment.ElectricChillerParallel\">
Buildings.Applications.DataCenters.ChillerCooled.Equipment.ElectricChillerParallel</a>
and water-side economizers <a href=\"modelica://Buildings.Applications.DataCenters.ChillerCooled.Equipment.WatersideEconomizer\">
Buildings.Applications.DataCenters.ChillerCooled.Equipment.WatersideEconomizer</a>.
</p>
<p>
The associated valve group <code>val1</code> and <code>val2</code>
on <code>medium 1</code> and <code>medium 2</code> side are for on/off use only.
The number of valves in each group is specified by the parameter <code>n</code>.
The valve parameters can be specified differently.
</p>
<p>
The signal filter is used to smoothe the on/off signal for the valves.
</p>
</html>",
        revisions="<html>
<ul>
<li>
April 14, 2020, by Michael Wetter:<br/>
Changed <code>homotopyInitialization</code> to a constant.<br/>
This is for
<a href=\"https://github.com/ibpsa/modelica-ibpsa/issues/1341\">Buildings, #1341</a>.
</li>
<li>
June 30, 2017, by Yangyang Fu:<br/>
First implementation.
</li>
</ul>
</html>"));
end PartialPlantParallel;
