within Buildings.Applications.DHC.EnergyTransferStations.BaseClasses;
model AmbientReturnHeader "Ambient circuit return water header"
replaceable package Medium = Modelica.Media.Interfaces.PartialMedium;

  parameter Modelica.SIunits.MassFlowRate m_flow_nominal
   "Nominal mass flow rate";
  parameter Integer nPorts_a=1
   "Number of ports"
     annotation(Evaluate=true, Dialog(connectorSizing=true, tab="General",group="Ports"));
  parameter Integer nPorts_b=1
   "Number of ports"
     annotation(Evaluate=true, Dialog(connectorSizing=true, tab="General",group="Ports"));
 Fluid.FixedResistances.LosslessPipe pip(
    redeclare package Medium = Medium,
     m_flow_nominal=m_flow_nominal)
     annotation (Placement(transformation(extent={{8,-10},{-12,10}})));
  Modelica.Fluid.Interfaces.FluidPorts_a ports_a[nPorts_a](
    redeclare package Medium = Medium)
    "The ports which connects the heat pump outlet to the borefield inlet"
     annotation (Placement(
        transformation(extent={{-110,-40},{-90,40}}),iconTransformation(extent={{-10,-40},
            {10,40}},
        rotation=180,
        origin={-108,0})));
  Modelica.Fluid.Interfaces.FluidPorts_b ports_b[nPorts_b](
    redeclare package Medium = Medium)
     annotation (Placement(
        transformation(extent={{86,-40},{106,40}}),iconTransformation(extent={{-10,-40},
            {10,40}},
        rotation=0,
        origin={110,0})));
equation
  if nPorts_b>0 then
    for i in 1:nPorts_b loop
     connect(ports_b[nPorts_b], pip.port_a)
    annotation (Line(points={{96,0},{8,0}},   color={0,127,255}));
    end for;
  end if;
  if nPorts_a>0 then
    for i in  1:nPorts_a loop
     connect(pip.port_b, ports_a[nPorts_a])
    annotation (Line(points={{-12,0},{-100,0}}, color={0,127,255}));
    end for;
  end if;

  annotation (Icon(coordinateSystem(preserveAspectRatio=false), graphics={
        Rectangle(
          extent={{-86,6},{90,-8}},
          lineColor={28,108,200},
          lineThickness=0.5,
          fillColor={28,108,200},
          fillPattern=FillPattern.Solid),
        Rectangle(
          extent={{-98,18},{-86,-22}},
          lineColor={28,108,200},
          lineThickness=0.5,
          fillColor={28,108,200},
          fillPattern=FillPattern.Solid),
        Rectangle(
          extent={{88,20},{100,-20}},
          lineColor={28,108,200},
          lineThickness=0.5,
          fillColor={28,108,200},
          fillPattern=FillPattern.Solid),Text(
        extent={{-150,140},{150,100}},
        lineColor={0,0,255}),    Text(
          extent={{-149,95},{151,55}},
          lineColor={0,0,255},
          fillPattern=FillPattern.HorizontalCylinder,
          fillColor={0,127,255},
          textString="%name")}),
                               Diagram(
        coordinateSystem(preserveAspectRatio=false)), defaultComponentName="cooSupHed");
end AmbientReturnHeader;
