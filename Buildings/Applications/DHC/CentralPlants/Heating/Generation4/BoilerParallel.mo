within Buildings.Applications.DHC.CentralPlants.Heating.Generation4;
model BoilerParallel "Multiple identical boiler"
  extends
    Buildings.Applications.DHC.CentralPlants.Heating.Generation4.PartialPlantParallel(
      redeclare Buildings.Fluid.Boilers.BoilerPolynomial hea(
            each energyDynamics=Modelica.Fluid.Types.Dynamics.FixedInitial,
            each Q_flow_nominal= Q_flow_nominal,
            each m_flow_nominal=m_flow_nominal,
            each dp_nominal=dp_nominal,
            each effCur=Buildings.Fluid.Types.EfficiencyCurves.Constant,
            each a={0.9},
            each fue=Fluid.Data.Fuels.NaturalGasHigherHeatingValue()));

  parameter Modelica.SIunits.TemperatureDifference delT_nominal;
  parameter Modelica.SIunits.MassFlowRate m_flow_nominal;
  parameter Modelica.SIunits.PressureDifference dp_nominal;
  parameter Modelica.SIunits.HeatFlowRate Q_flow_nominal;
  parameter Integer numBoi;

  Modelica.Blocks.Interfaces.RealInput PLR[num]
   "Part load ratio signal."
    annotation (Placement(transformation(extent={{-120,-54},{-100,-34}}),
        iconTransformation(extent={{-120,-54},{-100,-34}})));
  Modelica.Blocks.Interfaces.RealOutput TBoiLvg[num]
   "Boiler leaving water temperature."
    annotation (Placement(transformation(
          extent={{100,30},{120,50}}), iconTransformation(extent={{100,30},{120,
            50}})));
  Modelica.Blocks.Math.BooleanToReal booToRea[num](each final realTrue=1, each final
            realFalse=0)
    "Boolean to real (if true then 1 else 0)"
    annotation (Placement(transformation(extent={{-80,40},{-60,60}})));
  Modelica.Blocks.Interfaces.BooleanInput on[num] "On signal of the plant"
    annotation (Placement(transformation(extent={{-120,40},{-100,60}}),
        iconTransformation(extent={{-140,60},{-100,100}})));
  Fluid.Actuators.Valves.TwoWayEqualPercentage           valByp(
    redeclare package Medium = Medium,
    m_flow_nominal=m_flow_nominal*0.0001,
    dpValve_nominal=dp_nominal,
    use_inputFilter=false) "Condenser water bypass valve"
    annotation (Placement(transformation(
        extent={{-10,-10},{10,10}},
        origin={50,-82})));
  Modelica.Blocks.Sources.Constant TSetByPas(k=70 + 273.15)
    "Bypass loop temperature setpoint"
    annotation (Placement(transformation(extent={{-96,-42},{-76,-22}})));
  Buildings.Controls.Continuous.LimPID bypValCon(
    u_s(unit="K", displayUnit="degC"),
    u_m(unit="K", displayUnit="degC"),
    controllerType=Modelica.Blocks.Types.SimpleController.PI,
    k=1,
    Ti=60,
    reverseActing=false,
    reset=Buildings.Types.Reset.Parameter,
    y_reset=0)
           "Bypass valve controller"
    annotation (Placement(transformation(extent={{-60,-40},{-40,-20}})));
  Modelica.Blocks.Interfaces.RealInput THWSup(unit="K")
    "Heating water supplu temperature." annotation (Placement(transformation(
          extent={{-122,-100},{-102,-80}}),
                                         iconTransformation(extent={{-120,-90},
            {-100,-70}})));
equation

 for i in 1:numBoi loop
    connect(port_a, hea[i].port_a)
      annotation (Line(points={{-100,0},{-20,0}}, color={0,127,255}));
  connect(val[i].port_b, port_b)
    annotation (Line(points={{56,0},{100,0}}, color={0,127,255}));
 end for;
  connect(filter.u, booToRea.y) annotation (Line(points={{-55.2,84},{-58,84},{-58,
          50},{-59,50}}, color={0,0,127}));
  connect(on, booToRea.u) annotation (Line(points={{-110,50},{-82,50}}, color={255,0,255}));
  connect(hea.port_b, val.port_a) annotation (Line(points={{0,0},{36,0}}, color={0,127,255}));
  connect(PLR,hea.y)  annotation (Line(points={{-110,-44},{-74,-44},{-74,8},{
          -22,8}},
        color={0,0,127}));
  connect(hea.T, TBoiLvg) annotation (Line(points={{1,8},{14,8},{14,40},{110,40}}, color={0,0,127}));
  connect(TSetByPas.y,bypValCon. u_s) annotation (Line(points={{-75,-32},{-72,
          -32},{-72,-30},{-62,-30}},                                                 color={0,0,127}));
  connect(bypValCon.y,valByp. y) annotation (Line(points={{-39,-30},{50,-30},{
          50,-70}},              color={0,0,127}));
  connect(valByp.port_a, port_a) annotation (Line(points={{40,-82},{-30,-82},{
          -30,0},{-100,0}},
                        color={0,127,255}));
  connect(valByp.port_b, port_b) annotation (Line(points={{60,-82},{64,-82},{64,
          0},{100,0}}, color={0,127,255}));
  connect(THWSup,bypValCon. u_m) annotation (Line(points={{-112,-90},{-50,-90},
          {-50,-42}},color={0,0,127}));
  connect(on[1], bypValCon.trigger) annotation (Line(points={{-110,45},{-100,45},
          {-100,-66},{-56,-66},{-56,-42},{-58,-42}}, color={255,0,255}));
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
