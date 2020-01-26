within Buildings.Fluid.HeatPumps.BaseClasses;
partial model PartialReversible
  "Partial model for water to water heat pumps"
  extends Buildings.Fluid.Interfaces.FourPortHeatMassExchanger(
        dp2_nominal=200,
        dp1_nominal=200,
        show_T=true,
      redeclare final Buildings.Fluid.MixingVolumes.MixingVolume
        vol2( V=m2_flow_nominal*tau2/rho2_nominal,
              nPorts=2,
              final prescribedHeatFlowRate=true),
        vol1( V=m1_flow_nominal*tau1/rho1_nominal,
              nPorts=2,
              final prescribedHeatFlowRate=true));

  parameter Real scaling_factor = 1
   "Scaling factor for heat pump capacity";

  Modelica.Blocks.Interfaces.IntegerInput uMod
   "Control input signal, cooling mode=-1, off=0, heating mode=+1"
    annotation (Placement(transformation(extent={{-124,-12},{-100,12}}),
          iconTransformation(extent={{-120,-10},{-100,10}})));
  Modelica.Blocks.Interfaces.RealInput TSet(final unit="K", displayUnit="degC")
    "Set point for leaving fluid temperature at port b1"
    annotation (Placement(transformation(extent={{-120,74},{-100,94}}),
          iconTransformation(extent={{-128,76},{-100,104}})));
  Modelica.Blocks.Interfaces.RealOutput P(final unit="W")
   "Compressor power "
    annotation (Placement(transformation(extent={{100,-10},{120,10}}),
        iconTransformation(extent={{100,-12},{120,8}})));
  Modelica.Blocks.Interfaces.RealOutput QSou_flow(final unit="W")
   "Heat flow rate at the source heat exchanger"
    annotation (Placement(transformation(extent={{100,-98},{120,-78}}),
        iconTransformation(extent={{100,-100},{120,-80}})));
  Modelica.Blocks.Interfaces.RealOutput QLoa_flow(final unit="W")
   "Heat flow rate at the load heat exchanger"
    annotation (Placement(transformation(extent={{100,78},{120,98}}),
        iconTransformation(extent={{100,80},{120,100}})));
  Modelica.Blocks.Interfaces.RealOutput COP(final min=0, final unit="1")
    "Coefficient of performance, assuming useful heat is at load side (at Medium 1)"
    annotation (Placement(transformation(extent={{100,-50},{120,-30}}),
        iconTransformation(extent={{100,-40},{120,-20}})));
  ReSetTSetCoo reSet
    annotation (Placement(transformation(extent={{-48,50},{-28,70}})));
   Modelica.Blocks.Interfaces.RealInput TMinEntSou(final unit="K", displayUnit="degC")
    "Minimum leaving water temperature at the source side" annotation (
      Placement(transformation(extent={{-120,88},{-100,108}}),
        iconTransformation(extent={{-120,70},{-100,90}})));
   Modelica.Blocks.Interfaces.RealInput TMaxEntSou(final unit="K", displayUnit="degC")
    "Maximum leaving water temperature at the source side" annotation (
      Placement(transformation(extent={{-120,40},{-100,60}}),
        iconTransformation(extent={{-120,-50},{-100,-30}})));
protected
  constant Modelica.SIunits.SpecificEnergy h1_default=
     Medium1.specificEnthalpy_pTX(
       Medium1.p_default,
       Medium1.T_default,
       Medium1.X_default)
  "Default enthalpy for Medium 1";

  Modelica.SIunits.SpecificEnthalpy hSet=if uMod == 0 then h1_default else
      Medium1.specificEnthalpy_pTX(
      p=port_b1.p,
      T=reSet.chiTSet,
      X=cat(
        1,
        port_b1.Xi_outflow,
        {1 - sum(port_b1.Xi_outflow)})) "Enthalpy corresponding to set point";
  Modelica.Blocks.Sources.RealExpression TSouEnt(
    final y=Medium2.temperature(
      Medium2.setState_phX(port_a2.p,
                           inStream(port_a2.h_outflow),
                           inStream(port_a2.Xi_outflow))))
   "Source side entering fluid temperature"
    annotation (Placement(transformation(extent={{-80,-36},{-60,-16}})));

  Modelica.Blocks.Sources.RealExpression Q_flow_set(
    final y= if (uMod == 0)
      then
        0
      else
        m1_flow*(hSet - inStream(port_a1.h_outflow)))
    "Required heat flow rate to meet set point"
    annotation (Placement(transformation(extent={{-80,30},{-60,50}})));

  Buildings.HeatTransfer.Sources.PrescribedHeatFlow preHeaFloLoa
   "Prescribed load side heat flow rate"
    annotation (Placement(transformation(extent={{61,10},{41,30}})));
  Buildings.HeatTransfer.Sources.PrescribedHeatFlow preHeaFloSou
   "Prescribed source side heat flow rate"
    annotation (Placement(transformation(extent={{59,-70},{39,-50}})));

equation
  connect(preHeaFloLoa.port, vol1.heatPort) annotation (Line(points={{41,20},{
          -20,20},{-20,60},{-10,60}}, color={191,0,0}));
  connect(preHeaFloSou.port, vol2.heatPort)
    annotation (Line(points={{39,-60},{12,-60}}, color={191,0,0}));
  connect(TSet, reSet.TSet) annotation (Line(points={{-110,84},{-54,84},{-54,
          62.6},{-49,62.6}},
                     color={0,0,127}));
  connect(TMinEntSou,reSet.TMinLvgLoa)  annotation (Line(points={{-110,98},{-52,
          98},{-52,68},{-49,68}}, color={0,0,127}));
  connect(TMaxEntSou,reSet.TMaxLvgLoa)  annotation (Line(points={{-110,50},{-96,
          50},{-96,56},{-49,56}}, color={0,0,127}));
  connect(uMod, reSet.uMod) annotation (Line(points={{-112,0},{-88,0},{-88,60},
          {-49,60}},color={255,127,0}));
  annotation (Icon(coordinateSystem(preserveAspectRatio=false,extent={{-100,
            -100},{100,100}}), graphics={
        Line(points={{112,-6}}, color={28,108,200}),
        Line(points={{2,-90}}, color={28,108,200}),
        Rectangle(
          extent={{-99,64},{102,54}},
          lineColor={0,0,255},
          pattern=LinePattern.None,
          fillColor={0,0,0},
          fillPattern=FillPattern.Solid),
        Rectangle(
          extent={{-99,-56},{102,-66}},
          lineColor={0,0,255},
          pattern=LinePattern.None,
          fillColor={0,0,0},
          fillPattern=FillPattern.Solid),
        Ellipse(
          extent={{32,12},{68,-22}},
          lineColor={0,0,0},
          fillColor={135,135,135},
          fillPattern=FillPattern.Solid,
          lineThickness=0.5),
        Rectangle(
          extent={{-56,68},{58,50}},
          lineColor={0,0,0},
          fillColor={135,135,135},
          fillPattern=FillPattern.Solid),
        Rectangle(
          extent={{-56,-52},{58,-70}},
          lineColor={0,0,0},
          fillColor={135,135,135},
          fillPattern=FillPattern.Solid),
        Rectangle(
          extent={{-103,64},{98,54}},
          lineColor={0,0,255},
          pattern=LinePattern.None,
          fillColor={0,0,255},
          fillPattern=FillPattern.Solid),
        Rectangle(
          extent={{-2,54},{98,64}},
          lineColor={0,0,255},
          pattern=LinePattern.None,
          fillColor={255,0,0},
          fillPattern=FillPattern.Solid),
        Rectangle(
          extent={{-101,-56},{100,-66}},
          lineColor={0,0,255},
          pattern=LinePattern.None,
          fillColor={0,0,255},
          fillPattern=FillPattern.Solid),
        Rectangle(
          extent={{-100,-66},{0,-56}},
          lineColor={0,0,127},
          pattern=LinePattern.None,
          fillColor={0,0,127},
          fillPattern=FillPattern.Solid),
        Polygon(
          points={{-42,0},{-52,-12},{-32,-12},{-42,0}},
          lineColor={0,0,0},
          fillColor={135,135,135},
          fillPattern=FillPattern.Solid,
          lineThickness=0.5),
        Polygon(
          points={{-42,0},{-52,10},{-32,10},{-42,0}},
          lineColor={0,0,0},
          fillColor={135,135,135},
          fillPattern=FillPattern.Solid,
          lineThickness=0.5),
        Rectangle(
          extent={{-44,50},{-40,10}},
          lineColor={ERROR,
                          0,0},
          fillColor={135,135,135},
          fillPattern=FillPattern.Solid),
        Rectangle(
          extent={{-44,-12},{-40,-52}},
          lineColor={0,0,0},
          fillColor={135,135,135},
          fillPattern=FillPattern.Solid),
        Line(points={{0,68},{0,90},{-100,90}},   color={0,0,255}),
        Line(points={{2,68},{2,90},{100,90},{102,86}}, color={28,108,200}),
        Line(points={{70,0},{108,0}}, color={28,108,200}),
        Line(points={{2,-70},{2,-90},{106,-90}}, color={28,108,200}),
        Line(points={{24,-18},{6,-18},{6,-52}}, color={238,46,47}),
        Line(points={{24,6},{6,6},{6,50}}, color={238,46,47}),
        Rectangle(
          extent={{24,18},{26,-24}},
          lineColor={0,0,0},
          fillColor={135,135,135},
          fillPattern=FillPattern.Solid),
        Polygon(
          points={{-1,12},{-17,-12},{15,-12},{-1,12}},
          lineColor={0,0,0},
          fillColor={0,0,0},
          fillPattern=FillPattern.Solid,
          origin={44,-3},
          rotation=90),
        Ellipse(extent={{22,2},{28,10}}, lineColor={255,0,0}),
        Ellipse(extent={{22,-22},{28,-14}}, lineColor={255,0,0}),
        Line(points={{32,-4},{26,-4},{26,-4}}, color={255,0,0}),
        Line(points={{70,-30},{108,-30}},
                                      color={28,108,200})}),
  defaultComponentName="heaPum",
  Documentation(info="<html>
  <p>
  Model for a water to water heat pump using the equation fit model as described
  in the EnergyPlus9.0.1 engineering reference documentation section 16.6.1: Water to water heat pump model. The model is based on J.Hui (2002), S.Arun. (2004) and C.Tang (2005).
  </p>
  <p>
  The model uses four non-dimensional equations or curves stated in <a href=\"Buildings.Fluid.HeatPumps.BaseClasses.EquationFitMethod\">
  Buildings.Fluid.HeatPumps.BaseClasses.EquationFitMethod</a> to predict the heat pump performance in either cooling or
  heating modes. The methodology involved using the generalized least square method to create a set of performance
  coefficients for the heating and cooling load ratios <code>HLRC</code>, <code>CLRC</code> and for the compressor power ratios <code>PHC</code>, <code>PCC</code> for heating and cooling modes respectively from the catalog data at indicated reference conditions. These respective coefficients
  and indicated reference conditions are used in the model to simulate the heat pump performance.
  The variables include load side inlet temperature, source side inlet temperature,
  load side water flow rate and source side water flow rate. Source and load sides are terms which
  separates between thermal source and building load sides within the heat pump. For ex. when the control integer signal <code>uMod</code>= 1,
  the heat pump is controlled to meet the condenser outlet temperature i.e. supply heating temperature to the building,
  the source side is the evaporator and the load side is the condenser.
  Likewise, in case of <code>uMod</code>=-1, the heat pump is controlled to meet the evaporator leaving water temperature,
  accordingly, the source side is the condenser and the load side is the evaporator.
  </p>
  <p>
  The heating and cooling performance coefficients are stored in the data record <code>per</code> and are available from <a href=\"Buildings.Fluid.HeatPumps.Data.EquationFitWaterToWater\">
  Buildings.Fluid.HeatPumps.Data.EquationFitWaterToWater</a>.
  </p>
  <p>
  The model takes as input signals; the set point for either the leaving water temperature for the
  condenser or the evaporator which is met if the heat pump has sufficient capacity and the integer input <code>uMod</code> which identifies the heat pump operational mode.
  </p>
  <p>
  The electric power only includes the power for the compressor, but not any power for pumps or fans.
  </p>
  <h4>References</h4>
  <p>
  C.Tang
   <i>
  Equation fit based models of water source heat pumps.
  </i>
  Master Thesis. Oklahoma State University, Oklahoma, USA. 2005.
  </p>
    </html>", revisions="<html>
  <ul>
  <li>
  May 19, 2019, by Hagar Elarga:<br/>
  First implementation.
  </li>
  </ul>
</html>"),
    Diagram(coordinateSystem(extent={{-100,-100},{100,100}})));
end PartialReversible;
