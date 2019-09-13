within Buildings.Applications.DHC.EnergyTransferStations.BaseClasses;
model HeatingSupplyHeader_volume
  "Hydraulic supply water header to connect the heat pump and solar system to building heating load"
 replaceable package Medium = Modelica.Media.Interfaces.PartialMedium;

  parameter Modelica.SIunits.MassFlowRate m_flow_nominal;
  Fluid.FixedResistances.LosslessPipe pip(
    redeclare package Medium = Medium,
     m_flow_nominal=m_flow_nominal)
     annotation (Placement(transformation(extent={{8,22},{-12,42}})));
  Buildings.Fluid.Delays.DelayFirstOrder vol1(
    redeclare final package Medium = Medium,
    final energyDynamics=Modelica.Fluid.Types.Dynamics.FixedInitial,
    tau=10*60,
    final m_flow_nominal=m_flow_nominal,
    nPorts=3)
     annotation (Placement(transformation(extent={{50,32},{70,52}})));
 Buildings.Fluid.Delays.DelayFirstOrder vol2(
   redeclare final package Medium = Medium,
    final energyDynamics=Modelica.Fluid.Types.Dynamics.FixedInitial,
    tau=10*60,
    final m_flow_nominal=m_flow_nominal,
    nPorts=3)
     annotation (Placement(transformation(extent={{-76,32},{-56,52}})));
  Modelica.Fluid.Interfaces.FluidPort_a heaPumIn(
    p(start=Medium.p_default),
    redeclare final package Medium = Medium,
    h_outflow(start=Medium.h_default, nominal=Medium.h_default))
    "water supply header inlet from heatpump"
     annotation (Placement(
        transformation(extent={{-110,22},{-90,42}}), iconTransformation(extent={
            {-58,6},{-38,26}})));
  Modelica.Fluid.Interfaces.FluidPort_b borSysOut(
    p(start=Medium.p_default),
    redeclare final package Medium = Medium,
    h_outflow(start=Medium.h_default, nominal=Medium.h_default))
    "Supply water outlet from the cold header to the cold buffer tank"
     annotation (Placement(transformation(extent={{-110,-12},{-90,8}}),
        iconTransformation(extent={{-60,-28},{-40,-8}})));
  Modelica.Fluid.Interfaces.FluidPort_a solSysIn(
    p(start=Medium.p_default),
    redeclare final package Medium = Medium,
    h_outflow(start=Medium.h_default, nominal=Medium.h_default))
    "water supply header inlet from heatpump"
     annotation (Placement(
        transformation(extent={{92,22},{112,42}}), iconTransformation(extent={{42,
            6},{62,26}})));
  Modelica.Fluid.Interfaces.FluidPort_b hotTanOut(
    p(start=Medium.p_default),
    redeclare final package Medium = Medium,
    h_outflow(start=Medium.h_default, nominal=Medium.h_default))
    "Supply water outlet from the cold header to the cold buffer tank"
    annotation (Placement(transformation(extent={{92,-10},{112,10}}),
        iconTransformation(extent={{44,-28},{64,-8}})));

equation
  connect(heaPumIn, vol2.ports[1]) annotation (Line(points={{-100,32},{-68.6667,
          32}},                   color={0,127,255}));
  connect(borSysOut, vol2.ports[2])
    annotation (Line(points={{-100,-2},{-66,-2},{-66,32}}, color={0,127,255}));
  connect(vol2.ports[3], pip.port_b) annotation (Line(points={{-63.3333,32},{
          -12,32}},      color={0,127,255}));
  connect(pip.port_a, vol1.ports[1])
    annotation (Line(points={{8,32},{57.3333,32}}, color={0,127,255}));
  connect(vol1.ports[2], solSysIn)
    annotation (Line(points={{60,32},{102,32}}, color={0,127,255}));
  connect(vol1.ports[3], hotTanOut) annotation (Line(points={{62.6667,32},{
          62.6667,0},{102,0}},
                       color={0,127,255}));
  annotation (Icon(graphics={
        Rectangle(
          extent={{-98,18},{-86,-22}},
          lineColor={238,46,47},
          lineThickness=0.5,
          fillColor={238,46,47},
          fillPattern=FillPattern.Solid),
        Rectangle(
          extent={{-86,6},{90,-8}},
          lineColor={238,46,47},
          lineThickness=0.5,
          fillColor={238,46,47},
          fillPattern=FillPattern.Solid),
        Rectangle(
          extent={{88,20},{100,-20}},
          lineColor={238,46,47},
          lineThickness=0.5,
          fillColor={238,46,47},
          fillPattern=FillPattern.Solid)}));
end HeatingSupplyHeader_volume;
