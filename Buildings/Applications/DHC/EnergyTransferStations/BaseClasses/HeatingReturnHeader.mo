within Buildings.Applications.DHC.EnergyTransferStations.BaseClasses;
model HeatingReturnHeader
  "Hydraulic supply water header to connect the heat pump and solar system to building heating load"
 replaceable package Medium = Modelica.Media.Interfaces.PartialMedium;

 parameter Modelica.SIunits.MassFlowRate m_flow_nominal;
 parameter Integer nPorts_a=1
  "Number of ports"
    annotation(Evaluate=true, Dialog(connectorSizing=true, tab="General",group="Ports"));
 parameter Integer nPorts_b=1
  "Number of ports"
    annotation(Evaluate=true, Dialog(connectorSizing=true, tab="General",group="Ports"));
 Fluid.FixedResistances.LosslessPipe pip(redeclare package Medium = Medium,
      m_flow_nominal=m_flow_nominal)
    annotation (Placement(transformation(extent={{10,-10},{-10,10}})));
 Modelica.Fluid.Interfaces.FluidPorts_a ports_a[nPorts_a](redeclare package
      Medium = Medium)
   "The ports which connects the heat pump outlet to the borefield inlet"
    annotation (Placement(
       transformation(extent={{-108,-40},{-88,40}}),iconTransformation(extent={{-10,-40},
           {10,40}},
       rotation=180,
       origin={-110,0})));
 Modelica.Fluid.Interfaces.FluidPorts_b ports_b[nPorts_b](redeclare package
      Medium = Medium)
    annotation (Placement(
       transformation(extent={{88,-40},{108,40}}),iconTransformation(extent={{-10,-40},
           {10,40}},
       rotation=0,
       origin={110,0})));
equation

  if nPorts_b>0 then
    for i in 1:nPorts_b loop
     connect(ports_b[nPorts_b], pip.port_a)
    annotation (Line(points={{98,0},{10,0}},  color={0,127,255}));
    end for;
  end if;
  if nPorts_a>0 then
    for i in  1:nPorts_a loop
     connect(pip.port_b, ports_a[nPorts_a])
    annotation (Line(points={{-10,0},{-98,0}},  color={0,127,255}));
    end for;
  end if;
  connect(ports_a[1], pip.port_b)
    annotation (Line(points={{-98,0},{-10,0}}, color={0,127,255}));
  connect(pip.port_a, ports_b[1])
    annotation (Line(points={{10,0},{98,0}}, color={0,127,255}));
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
          extent={{86,20},{98,-20}},
          lineColor={28,108,200},
          lineThickness=0.5,
          fillColor={28,108,200},
          fillPattern=FillPattern.Solid),
                                 Text(
          extent={{-149,95},{151,55}},
          lineColor={0,0,255},
          fillPattern=FillPattern.HorizontalCylinder,
          fillColor={0,127,255},
          textString="%name")}),                                 Diagram(
        coordinateSystem(preserveAspectRatio=false)),defaultComponentName="heaRetHed");
end HeatingReturnHeader;
