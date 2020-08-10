within Buildings.Applications.DHC.CentralPlants.Heating.Generation4.Controls;
model HeatingWaterPumpSpeed "Controller for variable speed heating water pumps"

  parameter Integer numPum(min=1, max=2)=2 "Number of chilled water pumps, maximum is 2";

  parameter Modelica.SIunits.PressureDifference dpSetPoi(displayUnit="Pa")
    "Pressure difference setpoint";

  parameter Modelica.SIunits.Time tWai "Waiting time";

  parameter Modelica.SIunits.MassFlowRate m_flow_nominal
    "Nominal mass flow rate of single chilled water pump";

  parameter Real minSpe=0.05
    "Minimum speed ratio required by chilled water pumps";

  parameter Modelica.Blocks.Types.SimpleController controllerType=Modelica.Blocks.Types.SimpleController.PI
    "Type of pump speed controller";

  parameter Real k(unit="1", min=0) = 0.1 "Gain of controller";

  parameter Modelica.SIunits.Time Ti(min=Modelica.Constants.small)=60
    "Time constant of Integrator block" annotation (Dialog(enable=
          controllerType == Modelica.Blocks.Types.SimpleController.PI or
          controllerType == Modelica.Blocks.Types.SimpleController.PID));

  parameter Modelica.SIunits.Time Td(min=0)=0.1
    "Time constant of Derivative block" annotation (Dialog(enable=
          controllerType == Modelica.Blocks.Types.SimpleController.PD or
          controllerType == Modelica.Blocks.Types.SimpleController.PID));

  Modelica.Blocks.Interfaces.RealInput masFloPum(
    final unit="kg/s")
    "Total mass flowrate of chilled water pumps"
    annotation (Placement(transformation(extent={{-140,46},{-100,86}})));

  Modelica.Blocks.Interfaces.RealInput dpMea(
    final unit="Pa")
    "Measured pressure difference"
    annotation (Placement(transformation(extent={{-140,-60},{-100,-20}})));

  Modelica.Blocks.Interfaces.RealOutput y[numPum](
    unit="1",min=0,max=1) "Pump speed signal"
    annotation (Placement(transformation(extent={{100,-10},{120,10}})));

  Modelica.Blocks.Math.Product pumSpe[numPum] "Output pump speed"
    annotation (Placement(transformation(extent={{40,-10},{60,10}})));

  Buildings.Applications.DataCenters.ChillerCooled.Controls.VariableSpeedPumpStage pumStaCon(
    tWai=tWai,
    m_flow_nominal=m_flow_nominal,
    minSpe=minSpe)
    "Chilled water pump staging control"
    annotation (Placement(transformation(extent={{-10,-10},{10,10}})));

  Buildings.Controls.Continuous.LimPID conPID(
    controllerType=Modelica.Blocks.Types.SimpleController.PI,
    Ti=300,
    k=k,
    Td=60,
    yMax=1,
    yMin=0,
    reverseActing=false)
     "PID controller of pump speed"
    annotation (Placement(transformation(extent={{-60,-10},{-40,10}})));
  Modelica.Blocks.Sources.Constant dpSetSca(k=dpSetPoi)
    "Scaled differential pressure setpoint"
    annotation (Placement(transformation(extent={{-100,-10},{-80,10}})));
  Modelica.Blocks.Math.Gain gai(k=1/dpSetPoi)
    "Multiplier gain for normalizing dp input"
    annotation (Placement(transformation(extent={{-80,-50},{-60,-30}})));
  Modelica.Blocks.Sources.RealExpression setMas(y=if masFloPum <=
        m_flow_nominal then m_flow_nominal else numPum*m_flow_nominal)
    "SetPoint of the heating water mass flow rate."
    annotation (Placement(transformation(extent={{0,30},{20,50}})));
  Modelica.Blocks.Interfaces.RealOutput PLR "PLR of the boiler"
    annotation (Placement(transformation(extent={{100,50},{120,70}})));
  Modelica.Blocks.Math.Division ratMas
    annotation (Placement(transformation(extent={{34,50},{54,70}})));
equation
  connect(pumStaCon.masFloPum, masFloPum) annotation (Line(points={{-12,8},{-22,
          8},{-22,66},{-120,66}}, color={0,0,127}));
  connect(conPID.y, pumStaCon.speSig) annotation (Line(points={{-39,0},{-20,0},{
          -20,4},{-12,4}}, color={0,0,127}));
  connect(pumSpe.y, y) annotation (Line(points={{61,0},{110,0}}, color={0,0,127}));
  connect(dpSetSca.y, conPID.u_s)
    annotation (Line(points={{-79,0},{-62,0}}, color={0,0,127}));
  connect(pumStaCon.y, pumSpe.u1)
    annotation (Line(points={{11,0},{22,0},{22,6},{38,6}}, color={0,0,127}));
  connect(conPID.y, pumSpe[1].u2) annotation (Line(points={{-39,0},{-20,0},{-20,
          -32},{22,-32},{22,-6},{38,-6}}, color={0,0,127}));
  connect(conPID.y, pumSpe[2].u2) annotation (Line(points={{-39,0},{-20,0},{-20,
          -32},{22,-32},{22,-6},{38,-6}}, color={0,0,127}));
  connect(dpMea, gai.u)
    annotation (Line(points={{-120,-40},{-82,-40}}, color={0,0,127}));
  connect(conPID.u_m, gai.y)
    annotation (Line(points={{-50,-12},{-50,-40},{-59,-40}}, color={0,0,127}));
  connect(ratMas.u2, setMas.y) annotation (Line(points={{32,54},{26,54},{26,40},
          {21,40}}, color={0,0,127}));
  connect(masFloPum, ratMas.u1)
    annotation (Line(points={{-120,66},{32,66}}, color={0,0,127}));
  connect(ratMas.y, PLR)
    annotation (Line(points={{55,60},{110,60}}, color={0,0,127}));
  connect(PLR, PLR)
    annotation (Line(points={{110,60},{110,60}}, color={0,0,127}));
  annotation (Icon(coordinateSystem(preserveAspectRatio=false), graphics={
          Rectangle(
          extent={{-100,100},{100,-100}},
          lineColor={0,0,127},
          fillColor={255,255,255},
          fillPattern=FillPattern.Solid),
                                        Text(
        extent={{-150,150},{150,110}},
        textString="%name",
        lineColor={0,0,255})}),             Diagram(coordinateSystem(
          preserveAspectRatio=false)));
end HeatingWaterPumpSpeed;
