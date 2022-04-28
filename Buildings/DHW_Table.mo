within Buildings;
package DHW_Table

  model DHW_Profile
      extends Modelica.Icons.Example;

    package Medium =Buildings.Media.Water;

    Fluid.Sources.MassFlowSource_T           dhwFlo(
      redeclare package Medium = Medium,
      use_m_flow_in=true,
      use_T_in=false,
      nPorts=1) "Source side water pump" annotation (Placement(transformation(
          extent={{-10,10},{10,-10}},
          rotation=180,
          origin={-42,-8})));
    Modelica.Blocks.Math.Gain DHW(k=-1) "Pump mass flow rate" annotation (
        Placement(transformation(
          extent={{10,-10},{-10,10}},
          rotation=0,
          origin={2,0})));
  equation
    connect(DHW.y,dhwFlo. m_flow_in) annotation (Line(points={{-9,0},{-30,-8.88178e-16}},
                             color={0,0,127}));
    annotation (Icon(coordinateSystem(preserveAspectRatio=false)), Diagram(
          coordinateSystem(preserveAspectRatio=false)));
  end DHW_Profile;
end DHW_Table;
