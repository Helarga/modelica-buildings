within Buildings.Applications.DHC.EnergyTransferStations.BaseClasses;
model HeatingSupplyHeader
  "Hydraulic supply water header to connect the heat pump and solar system to building heating load"
 replaceable package Medium = Modelica.Media.Interfaces.PartialMedium " Medium package";

  parameter Modelica.SIunits.MassFlowRate m_flow_nominal;
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
  Modelica.Fluid.Interfaces.FluidPorts_a ports_a[nPorts_a](redeclare package
      Medium = Medium)
    "The ports which connects the heat pump outlet to the borefield inlet"
    annotation (Placement(transformation(extent={{-110,-40},{-90,40}}),
        iconTransformation(
        extent={{-10,-40},{10,40}},
        rotation=180,
        origin={-108,0})));
  Modelica.Fluid.Interfaces.FluidPorts_b ports_b [nPorts_b](redeclare package
      Medium = Medium)
    "The ports which connects the heat pump outlet to the borefield inlet"
     annotation (Placement(
        transformation(extent={{90,-40},{110,40}}),  iconTransformation(extent={{-10,-40},
            {10,40}},
        rotation=180,
        origin={110,0})));
equation
  if nPorts_b>0 then
     for i in 1:nPorts_b loop
      connect(pip.port_a, ports_b[nPorts_b])
        annotation (Line(points={{8,0},{100,0}}, color={0,127,255}));
     end for;
  end if;
  if nPorts_a>0 then
    for i in  1:nPorts_a loop
      connect(pip.port_b, ports_a[nPorts_a])
       annotation (Line(points={{-12,0},{-100,0}}, color={0,127,255}));
    end for;
  end if;

  connect(pip.port_b, ports_a[1])
    annotation (Line(points={{-12,0},{-100,0}}, color={0,127,255}));
  connect(pip.port_a, ports_b[1])
    annotation (Line(points={{8,0},{100,0}}, color={0,127,255}));

    annotation (Icon(graphics={
       Rectangle(
         extent={{-92,8},{88,-6}},
         lineColor={238,46,47},
         lineThickness=0.5,
         fillColor={238,46,47},
         fillPattern=FillPattern.Solid),
       Rectangle(
         extent={{-98,20},{-86,-20}},
         lineColor={238,46,47},
         lineThickness=0.5,
         fillColor={238,46,47},
         fillPattern=FillPattern.Solid),
       Rectangle(
         extent={{88,20},{100,-20}},
         lineColor={238,46,47},
         lineThickness=0.5,
         fillColor={238,46,47},
         fillPattern=FillPattern.Solid),
                                Text(
         extent={{-149,93},{151,53}},
         lineColor={0,0,255},
         fillPattern=FillPattern.HorizontalCylinder,
         fillColor={0,127,255},
         textString="%name")}),
         Documentation(info="<html>
 <h4> Water hydraulic header </h4>
 <p>
 The model represents a hydraulic header or a common pipe which connects different components at the system such as the heat pump, solar module etc. and it
 assures the delivery of the flow in the quantities required at design conditions and at all other operational modes.
</p>

</html>", revisions="<html>
<ul>
<li>
 <br/>
</li>
</ul>
</html>"));
end HeatingSupplyHeader;
