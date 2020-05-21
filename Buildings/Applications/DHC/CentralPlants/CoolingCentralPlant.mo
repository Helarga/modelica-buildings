within Buildings.Applications.DHC.CentralPlants;
model CoolingCentralPlant "Fourth generation district cooling plant."
  Fluid.Chillers.ElectricEIR chi[nUin] "Water cooled chiller."
    annotation (Placement(transformation(extent={{20,-80},{40,-60}})));
  Fluid.HeatExchangers.CoolingTowers.Merkel merkel[nUni] "Cooling tower."
    annotation (Placement(transformation(extent={{-10,60},{10,80}})));
  EnergyTransferStations.HydraulicHeader cooWarDelHea(nPorts_b=1)
    "Cooling water pumps suction header."
    annotation (Placement(transformation(extent={{-40,40},{-60,60}})));
  BoundaryConditions.WeatherData.Bus weaBus annotation (Placement(
        transformation(extent={{-100,80},{-80,100}}),iconTransformation(extent={{-108,-8},
            {-88,12}})));

  Fluid.Movers.SpeedControlled_y pum[nUni](
    redeclare each final replaceable package Medium = Medium,
    each final inputType=Buildings.Fluid.Types.InputType.Continuous,
    final per=per,
    each final addPowerToMedium=addPowerToMedium,
    each final allowFlowReversal=allowFlowReversal,
    each final m_flow_small=m_flow_small,
    each final show_T=show_T,
    each final tau=tau,
    each final use_inputFilter=use_inputFilter,
    each final riseTime=riseTimePump,
    each final init=init,
    final y_start=yPump_start,
    each final energyDynamics=energyDynamics,
    each final massDynamics=massDynamics,
    each final p_start=p_start,
    each final T_start=T_start,
    each final X_start=X_start,
    each final C_start=C_start,
    each final C_nominal=C_nominal) "Condenser water pump."
    annotation (Placement(transformation(extent={{-66,-70},{-46,-50}})));
  EnergyTransferStations.HydraulicHeader cooWatSucHea(nPorts_a=1)
    "Cooling water pumps suction header."
    annotation (Placement(transformation(extent={{60,40},{40,60}})));
  EnergyTransferStations.HydraulicHeader conLeaWatHed(nPorts_b=1)
    "Condenser leaving water header."
    annotation (Placement(transformation(extent={{46,-28},{26,-8}})));
equation
  connect(cooWarDelHea.ports_b[1], merkel.port_a) annotation (Line(points={{-60,
          50},{-80,50},{-80,70},{-10,70}}, color={0,127,255}));
  connect(weaBus.TWetBul, merkel.TAir) annotation (Line(
      points={{-90,90},{-80,90},{-80,74},{-12,74}},
      color={255,204,51},
      thickness=0.5), Text(
      string="%first",
      index=-1,
      extent={{-6,3},{-6,3}},
      horizontalAlignment=TextAlignment.Right));
  connect(merkel.port_b, cooWatSucHea.ports_a[1]) annotation (Line(points={{10,
          70},{80,70},{80,50},{60,50}}, color={0,127,255}));
  annotation (Icon(coordinateSystem(preserveAspectRatio=false), graphics={
          Rectangle(extent={{-100,100},{100,-100}}, lineColor={28,108,200})}),
                                                                 Diagram(
        coordinateSystem(preserveAspectRatio=false)));
end CoolingCentralPlant;
