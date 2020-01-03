within Buildings.Applications.DHC.EnergyTransferStations;
package Validation
  "Collection of models that validate the ETS controllers and systems"
    extends Modelica.Icons.ExamplesPackage;

  model CondenserandEvaporatorPumpsController
    Buildings.Applications.DHC.EnergyTransferStations.Control.CondenserAndEvaporatorPumpsController pumEvaConCon
      "Evaporator and condenser pumps controller"
      annotation (Placement(transformation(extent={{64,-16},{90,16}})));
    Buildings.Controls.OBC.CDL.Continuous.Sources.Ramp uMod(
      height=10,
      duration(displayUnit="h") = 14400,
      offset=-5,
      startTime=0)
        "HeatPump operational mode input signal"
         annotation (Placement(transformation(extent={{-96,-10},{-76,10}})));
    Modelica.Blocks.Sources.Constant TConLvg(k=39 + 273.15)
      annotation (Placement(transformation(extent={{-40,10},{-20,30}})));
    Modelica.Blocks.Sources.Constant THeaSet(k=45 + 273.15)
      annotation (Placement(transformation(extent={{-40,74},{-20,94}})));
    Modelica.Blocks.Sources.Constant TConEnt(k=30 + 273.15)
      annotation (Placement(transformation(extent={{-40,42},{-20,62}})));
    Modelica.Blocks.Sources.Constant TEvaEnt(k=12 + 273.15)
      annotation (Placement(transformation(extent={{-40,-30},{-20,-10}})));
    Modelica.Blocks.Sources.Constant TEvaLvg(k=8 + 273.15)
      annotation (Placement(transformation(extent={{-40,-60},{-20,-40}})));
    Modelica.Blocks.Sources.Constant TCooSet(k=6 + 273.15)
      annotation (Placement(transformation(extent={{-40,-90},{-20,-70}})));
    Modelica.Blocks.Sources.Constant minPumEva(k=0.2)
      "Minimum speed of the evaporator pump to charge the cold buffer tank"
      annotation (Placement(transformation(extent={{20,-40},{40,-20}})));
    Modelica.Blocks.Sources.Constant minPumCon(k=0.2)
      "Minimum speed of the condenser pump to charge the hot buffer tank"
      annotation (Placement(transformation(extent={{20,20},{40,40}})));
    Modelica.Blocks.Math.RealToInteger realToInteger
      annotation (Placement(transformation(extent={{-68,-10},{-48,10}})));
  equation
    connect(pumEvaConCon.TSetHea, THeaSet.y) annotation (Line(points={{63.09,
            10.08},{-10,10.08},{-10,84},{-19,84}}, color={0,0,127}));
    connect(pumEvaConCon.TConEnt, TConEnt.y) annotation (Line(points={{63.09,7.2},
            {-14,7.2},{-14,52},{-19,52}}, color={0,0,127}));
    connect(TEvaEnt.y, pumEvaConCon.TEvaEnt) annotation (Line(points={{-19,-20},{
            -14,-20},{-14,-3.04},{63.09,-3.04}}, color={0,0,127}));
    connect(pumEvaConCon.TEvaLvg, TEvaLvg.y) annotation (Line(points={{63.09,-6.24},
            {28,-6.24},{28,-6},{-12,-6},{-12,-50},{-19,-50}}, color={0,0,127}));
    connect(pumEvaConCon.TSetCoo, TCooSet.y) annotation (Line(points={{63.09,-8.8},
            {-8,-8.8},{-8,-80},{-19,-80}}, color={0,0,127}));
    connect(pumEvaConCon.pumEvaMin, minPumEva.y) annotation (Line(points={{63.09,
            -12.64},{50,-12.64},{50,-30},{41,-30}}, color={0,0,127}));
    connect(pumEvaConCon.pumConMin, minPumCon.y) annotation (Line(points={{63.09,
            14.56},{48,14.56},{48,30},{41,30}}, color={0,0,127}));
    connect(TConLvg.y, pumEvaConCon.TConLvg) annotation (Line(points={{-19,20},{-16,
            20},{-16,4},{63.09,4}}, color={0,0,127}));
    connect(uMod.y, realToInteger.u)
      annotation (Line(points={{-74,0},{-70,0}}, color={0,0,127}));
    connect(pumEvaConCon.heaPumMod, realToInteger.y) annotation (Line(points={{
            62.31,0.16},{12,0.16},{12,0},{-47,0}}, color={255,127,0}));
    annotation (Icon(coordinateSystem(preserveAspectRatio=false), graphics={
          Ellipse(lineColor = {75,138,73},
                  fillColor={255,255,255},
                  fillPattern = FillPattern.Solid,
                  extent={{-98,-100},{98,98}}),
          Polygon(lineColor = {0,0,255},
                  fillColor = {75,138,73},
                  pattern = LinePattern.None,
                  fillPattern = FillPattern.Solid,
                  points={{-30,64},{70,4},{-30,-56},{-30,64}})}), Diagram(
          coordinateSystem(preserveAspectRatio=false)));
  end CondenserandEvaporatorPumpsController;

  model EvaporatorCondenserPumpsController
    "Example of the evaporator and condenser pumps controller"

    package Medium = Buildings.Media.Water "Medium model";

    parameter Boolean show_T=true
      "= true, if actual temperature at port is computed"
       annotation (Dialog(group="Advanced"));
    parameter Modelica.SIunits.PressureDifference dpEva_nominal=10000
      "Nominal pressure raise";

    parameter Modelica.SIunits.MassFlowRate mCon_flow_nominal= 1.2
     "Evaporator nominal water flow rate";

    parameter Modelica.SIunits.MassFlowRate mEva_flow_nominal= 1.2
     "Evaporator nominal water flow rate";

    /*parameter Modelica.SIunits.MassFlowRate mSecHea_flow_nominal= mEva_flow_nominal*0.9
     "Secondary(building) heating circuit nominal water flow rate";
    parameter Modelica.SIunits.MassFlowRate mSecCoo_flow_nominal= mEva_flow_nominal*0.9
    "Secondary(building) cooling circuit nominal water flow rate";*/

    Buildings.Controls.OBC.CDL.Continuous.Sources.Constant conFloMin(k=0.3)
      "Primary pump control signal to maintain the condenser minimum flow rate recommended by the manufacturer. "
      annotation (Placement(transformation(extent={{16,18},{36,38}})));
    Modelica.Blocks.Sources.BooleanConstant heaMod(k=false) "Step control"
      annotation (Placement(transformation(extent={{88,48},{108,68}})));
    Modelica.Blocks.Sources.BooleanConstant CooMod(k=true)
                                                   "Step control"
      annotation (Placement(transformation(extent={{92,-78},{112,-58}})));
    Modelica.Blocks.Sources.Ramp mSecCoo(
      height=0.5,
      duration=200,
      offset=0.2) "Secondary(building side) circuit chilled water flow rate "
      annotation (Placement(transformation(extent={{44,-32},{64,-12}})));
    Modelica.Blocks.Sources.Ramp mSecHea(
      height=0.5,
      duration=200,
      offset=0.2) "Secondary(building side) circuit heating water flow rate "
      annotation (Placement(transformation(extent={{44,18},{64,38}})));
    Control.EvaporatorCondenserPumpsController priPumCon(mEva_flow_nominal=
          mEva_flow_nominal, mCon_flow_nominal=mCon_flow_nominal)
      "Primary pumps control block"
      annotation (Placement(transformation(extent={{120,-12},{140,8}})));
      //mSecHea_flow_nominal = mSecHea_flow_nominal,
      //mSecCoo_flow_nominal = mSecCoo_flow_nominal)
    Buildings.Controls.OBC.CDL.Continuous.Sources.Constant evaFloMin(k=0.3)
      "Primary pump control signal to maintain the evaporator minimum flow rate recommended by the manufacturer. "
      annotation (Placement(transformation(extent={{-10,18},{10,38}})));
     Fluid.Movers.SpeedControlled_y pumEva(
      redeclare package Medium = Medium,
      energyDynamics=Modelica.Fluid.Types.Dynamics.SteadyState,
      addPowerToMedium=false,
      show_T=show_T,
      per(pressure(dp={dpEva_nominal,0}, V_flow={0,mEva_flow_nominal/1000})),
      allowFlowReversal=false,
      use_inputFilter=false,
      riseTime=10)
      "Evaporator variable speed pump-primary circuit"
      annotation (Placement(transformation(
            extent={{-10,-10},{10,10}},
            rotation=0,
            origin={186,-28})));
    Modelica.Fluid.Sources.FixedBoundary sin(redeclare package Medium = Medium,
        nPorts=1) "Sink"
      annotation (Placement(transformation(extent={{296,-38},{276,-18}})));
    Modelica.Fluid.Sources.FixedBoundary sou(redeclare package Medium = Medium,
        nPorts=1) "Source volume"
      annotation (Placement(transformation(extent={{146,-38},{166,-18}})));
    Buildings.Controls.OBC.CDL.Continuous.Sources.Constant cooTanMin(k=0.4)
      "Primary pump speed signal to assure the minimum flow to the cold tank"
      annotation (Placement(transformation(extent={{0,-32},{20,-12}})));
    Buildings.Controls.OBC.CDL.Continuous.Sources.Constant heaTanMin(k=0.2)
      "Primary pump speed signal to assure the minimum flow to the hot tank"
      annotation (Placement(transformation(extent={{-36,18},{-16,38}})));
    Fluid.Sensors.MassFlowRate priCooFlo(redeclare package Medium = Media.Water)
      "Primary circuit evaporator side chilled water flow rate" annotation (
        Placement(transformation(
          extent={{-10,10},{10,-10}},
          rotation=0,
          origin={246,-28})));
     Fluid.Movers.SpeedControlled_y pumCon(
      redeclare package Medium = Medium,
      energyDynamics=Modelica.Fluid.Types.Dynamics.SteadyState,
      addPowerToMedium=false,
      show_T=show_T,
      per(pressure(dp={dpEva_nominal,0}, V_flow={0,mCon_flow_nominal/1000})),
      allowFlowReversal=false,
      use_inputFilter=false,
      riseTime=10) "Condenser variable speed pump-primary circuit" annotation (
        Placement(transformation(
          extent={{-10,10},{10,-10}},
          rotation=0,
          origin={186,60})));
    Modelica.Fluid.Sources.FixedBoundary sin1(redeclare package Medium = Medium,
        nPorts=1) "Sink"
      annotation (Placement(transformation(extent={{298,48},{278,68}})));
    Fluid.Sensors.MassFlowRate priHeaFlo(redeclare package Medium = Media.Water)
      "Primary circuit condenser side heating water flow rate" annotation (
        Placement(transformation(
          extent={{10,10},{-10,-10}},
          rotation=180,
          origin={246,60})));
    Modelica.Fluid.Sources.FixedBoundary sou1(redeclare package Medium = Medium,
        nPorts=1) "Source volume"
      annotation (Placement(transformation(extent={{148,50},{168,70}})));
  equation
    connect(priPumCon.reqHea, heaMod.y) annotation (Line(points={{118.6,8},{112,8},
            {112,58},{109,58}}, color={255,0,255}));
    connect(CooMod.y, priPumCon.reqCoo) annotation (Line(points={{113,-68},{116,
            -68},{116,-11.8},{118.6,-11.8}}, color={255,0,255}));
    connect(conFloMin.y, priPumCon.conFloMin)
      annotation (Line(points={{38,28},{38,2},{119.2,2}}, color={0,0,127}));
    connect(mSecHea.y, priPumCon.mSecHea) annotation (Line(points={{65,28},{80,28},
            {80,4},{119.2,4}}, color={0,0,127}));
    connect(mSecCoo.y, priPumCon.mSecCoo) annotation (Line(points={{65,-22},{80,
            -22},{80,-7.6},{119.2,-7.6}}, color={0,0,127}));
    connect(priPumCon.yPumEva, pumEva.y) annotation (Line(
        points={{141,-10},{186,-10},{186,-16}},
        color={0,0,127},
        pattern=LinePattern.Dash));
    connect(pumEva.port_a, sou.ports[1]) annotation (Line(points={{176,-28},{166,
            -28}},               color={0,127,255}));
    connect(priCooFlo.port_a, pumEva.port_b)
      annotation (Line(points={{236,-28},{196,-28}}, color={0,127,255}));
    connect(priCooFlo.port_b, sin.ports[1])
      annotation (Line(points={{256,-28},{276,-28}}, color={0,127,255}));
    connect(priHeaFlo.port_a, pumCon.port_b)
      annotation (Line(points={{236,60},{196,60}}, color={0,127,255}));
    connect(priHeaFlo.port_b, sin1.ports[1]) annotation (Line(points={{256,60},{
            280,60},{280,58},{278,58}},
                                    color={0,127,255}));
    connect(priPumCon.yPumCon, pumCon.y) annotation (Line(
        points={{141,6},{186,6},{186,48}},
        color={0,0,127},
        pattern=LinePattern.Dash));
    connect(pumCon.port_a, sou1.ports[1])
      annotation (Line(points={{176,60},{168,60}}, color={0,127,255}));
    connect(priPumCon.mPriCoo, priCooFlo.m_flow) annotation (Line(
        points={{119.2,-10},{84,-10},{84,-90},{246,-90},{246,-39}},
        color={0,0,127},
        pattern=LinePattern.Dot));
    connect(priPumCon.cooTanMin, cooTanMin.y) annotation (Line(points={{119.2,
            -5.6},{24,-5.6},{24,-22},{22,-22}}, color={0,0,127}));
    connect(priPumCon.evaFloMin, evaFloMin.y) annotation (Line(points={{119.2,
            -0.4},{14,-0.4},{14,28},{12,28}}, color={0,0,127}));
    connect(priPumCon.heaTanMin, heaTanMin.y) annotation (Line(points={{119.2,
            -3.4},{-14,-3.4},{-14,28}}, color={0,0,127}));
    connect(priHeaFlo.m_flow, priPumCon.mPriHea) annotation (Line(
        points={{246,71},{246,86},{84,86},{84,6.4},{119.2,6.4}},
        color={0,0,127},
        pattern=LinePattern.Dot));
    annotation (Icon(coordinateSystem(preserveAspectRatio=false, extent={{-100,-100},
              {100,100}}),                                        graphics={
          Ellipse(lineColor = {75,138,73},
                  fillColor={255,255,255},
                  fillPattern = FillPattern.Solid,
                  extent={{-98,-100},{98,98}}),
          Polygon(lineColor = {0,0,255},
                  fillColor = {75,138,73},
                  pattern = LinePattern.None,
                  fillPattern = FillPattern.Solid,
                  points={{-30,64},{70,4},{-30,-56},{-30,64}})}),  Diagram(coordinateSystem(preserveAspectRatio=false, extent={{-40,
              -100},{300,100}}),
          graphics={Line(points={{-22,22}}, color={28,108,200})}),
      experiment(StopTime=100000),
      __Dymola_Commands(
    file="modelica://Buildings/Resources/Scripts/Dymola/Applications/DHC/EnergyTransferStations/Control/EvaporatorCondenserPumpsController.mos"
          "Simulate and plot"),
           experiment(Tolerance=1e-6, StopTime=10000),
  Documentation(info="<html>
<p>
This model validates the controller block
<a href=\"Buildings.Applications.DHC.EnergyTransferStations.Control.CondenserAndEvaporatorPumpsController\"> 
Buildings.Applications.DHC.EnergyTransferStations.Control.CondenserAndEvaporatorPumpsController</a>.
<p>

</html>",   revisions="<html>
<ul>
<li>
 <br/>
</li>
</ul>
</html>"));
  end EvaporatorCondenserPumpsController;

  model HeatpumpControllerBlock
    "Reverse heatpump controller operates in heating mode only"
    package Medium = Buildings.Media.Water "Medium model";

    Control.HeatPumpController heaPumCon
      annotation (Placement(transformation(extent={{74,0},{94,20}})));

    Modelica.Blocks.Sources.BooleanPulse heaMod(width=50, period=500)
      "Step control"
      annotation (Placement(transformation(extent={{0,60},{20,80}})));
    Modelica.Blocks.Sources.BooleanPulse cooMod(
      width=50,
      period=500,
      startTime=500)
      "Step control"
      annotation (Placement(transformation(extent={{0,30},{20,50}})));
    Modelica.Blocks.Sources.Constant TCooSet(k=7 + 273.15)
      "Cooling setpoint temperature"
      annotation (Placement(transformation(extent={{-40,36},{-20,56}})));
    Modelica.Blocks.Sources.Constant THeaSetMax(k=50 + 273.15)
      "Maximum heating set point temperature"
      annotation (Placement(transformation(extent={{-40,2},{-20,22}})));
    Modelica.Blocks.Sources.Constant TEvaLvg(k=8 + 273.15)
      "Evaporator leaving water temperature"
      annotation (Placement(transformation(extent={{0,-80},{20,-60}})));
    Modelica.Blocks.Sources.Constant THeaSet(k=40 + 273.15)
      "Heating set point temperature"
      annotation (Placement(transformation(extent={{-40,70},{-20,90}})));
    Modelica.Blocks.Sources.Constant TMinConEnt(k=25 + 273.15)
      "Minimum condenser entering temperature"
      annotation (Placement(transformation(extent={{-40,-40},{-20,-20}})));
    Modelica.Blocks.Sources.Constant TMaxEvaEnt(k=18 + 273.15)
      "Maximum Evaporator entering temperature."
      annotation (Placement(transformation(extent={{-40,-80},{-20,-60}})));
    Modelica.Blocks.Sources.Constant TEvaEnt(k=19 + 273.15)
      "Evaporator entering temperature"
      annotation (Placement(transformation(extent={{0,-40},{20,-20}})));
    Modelica.Blocks.Sources.Constant TConEnt(k=26 + 273.15)
      "Condenser entering temperature"
      annotation (Placement(transformation(extent={{0,-120},{20,-100}})));
  equation
    connect(heaPumCon.reqHea, heaMod.y)
    annotation (Line(points={{72.6,19},{70,19},{70,70},{21,70}},
                                color={255,0,255}));
    connect(heaPumCon.reqCoo,cooMod. y) annotation (Line(points={{72.6,16},{68,16},
            {68,40},{21,40}},     color={255,0,255}));
    connect(heaPumCon.TSetCoo, TCooSet.y) annotation (Line(points={{73,11},{-6,11},
            {-6,14},{-10,14},{-10,46},{-19,46}},
                                        color={0,0,127}));
    connect(THeaSetMax.y, heaPumCon.TSetHeaMax) annotation (Line(points={{-19,12},
            {-12,12},{-12,8.8},{73,8.8}},   color={0,0,127}));
    connect(TEvaLvg.y,heaPumCon.TEvaLvg)  annotation (Line(points={{21,-70},{36,
            -70},{36,1.6},{73,1.6}}, color={0,0,127}));
    connect(THeaSet.y, heaPumCon.TSetHea) annotation (Line(points={{-19,80},{-6,
            80},{-6,13},{73,13}},    color={0,0,127}));
    connect(TMinConEnt.y, heaPumCon.TMinConEnt) annotation (Line(points={{-19,-30},
            {-12,-30},{-12,7.2},{73,7.2}},   color={0,0,127}));
    connect(TMaxEvaEnt.y, heaPumCon.TMaxEvaEnt) annotation (Line(points={{-19,-70},
            {-6,-70},{-6,5.4},{73,5.4}},   color={0,0,127}));
    connect(TEvaEnt.y, heaPumCon.TEvaEnt) annotation (Line(points={{21,-30},{26,
            -30},{26,3.4},{73,3.4}},
                                  color={0,0,127}));
    connect(TConEnt.y, heaPumCon.TConEnt) annotation (Line(points={{21,-110},{38,
            -110},{38,0.2},{73,0.2}},
                                  color={0,0,127}));
    annotation (Icon(coordinateSystem(preserveAspectRatio=false), graphics={
          Ellipse(lineColor = {75,138,73},
                  fillColor={255,255,255},
                  fillPattern = FillPattern.Solid,
                  extent={{-98,-100},{98,98}}),
          Polygon(lineColor = {0,0,255},
                  fillColor = {75,138,73},
                  pattern = LinePattern.None,
                  fillPattern = FillPattern.Solid,
                  points={{-30,64},{70,4},{-30,-56},{-30,64}})}),  Diagram(coordinateSystem(preserveAspectRatio=false, extent={
              {-100,-140},{100,100}}),
          graphics={Line(points={{-22,22}}, color={28,108,200})}),
      experiment(StopTime=86400),
      __Dymola_Commands(
    file="modelica://Buildings/Resources/Scripts/Dymola/Applications/DHC/EnergyTransferStations/Control/HeatpumpControllerBlock.mos"
          "Simulate and plot"),
           experiment(Tolerance=1e-6, StopTime=2000),
  Documentation(info="<html>
<p>
This model validates the controller block
<a href=\"Buildings.Applications.DHC.EnergyTransferStations.Control.HeatPumpController\"> 
Buildings.Applications.DHC.EnergyTransferStations.Control.HeatPumpController</a>.
<p>

</html>",   revisions="<html>
<ul>
<li>
 <br/>
</li>
</ul>
</html>"));
  end HeatpumpControllerBlock;

  model AmbientCircuitControllerBlock
    "AmbientCircuitControllerValidation"
    package Medium = Buildings.Media.Water "Medium model";

    parameter Modelica.SIunits.TemperatureDifference dTHex = 5
      "Temperature difference in and out of substation heat exchanger";
    Modelica.Blocks.Sources.BooleanPulse valHea(width=50, period=500)
     "Heating side valve status"
      annotation (Placement(transformation(extent={{-20,10},{0,30}})));
    Modelica.Blocks.Sources.BooleanConstant
                                         valCoo(k=false)
      "Cooling side valve status"
      annotation (Placement(transformation(extent={{-20,-20},{0,0}})));
    Modelica.Blocks.Sources.Constant TBorOut(k=30 + 273.15)
      "Borefield leaving water temperature"
      annotation (Placement(transformation(extent={{-20,-80},{0,-60}})));
    Modelica.Blocks.Sources.Constant TEntEva(k=12 + 273.15)
      "Evaporator entering water temperature"
      annotation (Placement(transformation(extent={{-20,40},{0,60}})));
    Modelica.Blocks.Sources.Constant TEntCon(k=35 + 273.15)
      "Condenser entering water temperature"
      annotation (Placement(transformation(extent={{-20,72},{0,92}})));
    Modelica.Blocks.Sources.Constant TDisHexEnt(k=18 + 273.15)
      "District heat exchnager entering water temperature"
      annotation (Placement(transformation(extent={{-20,-110},{0,-90}})));
    Modelica.Blocks.Sources.Pulse    TBorIn(
      amplitude=-5,
      width=50,
      period=250,
      offset=30 + 273.15)
      "Borefield entering water temperature"
      annotation (Placement(transformation(extent={{-20,-50},{0,-30}})));
    Control.AmbientCircuitController AmbCirCon( dTHex=dTHex)
    "Ambient water circuit control"
      annotation (Placement(transformation(extent={{32,-10},{52,10}})));
    Modelica.Blocks.Sources.Constant TDisHexLvg(k=12 + 273.15)
      "District heat exchnager leaving water temperature"
      annotation (Placement(transformation(extent={{-20,-140},{0,-120}})));
  equation
    connect(TEntEva.y, AmbCirCon.TEntEva) annotation (Line(points={{1,50},{16,50},
            {16,5.8},{31,5.8}}, color={0,0,127}));
    connect(TEntCon.y, AmbCirCon.TEntCon) annotation (Line(points={{1,82},{26,82},
            {26,8.2},{31,8.2}}, color={0,0,127}));
    connect(valCoo.y, AmbCirCon.valCoo) annotation (Line(points={{1,-10},{12,-10},
            {12,1.2},{31,1.2}},color={255,0,255}));
    connect(valHea.y, AmbCirCon.valHea) annotation (Line(points={{1,20},{12,20},{
            12,3.2},{31,3.2}},color={255,0,255}));
    connect(TBorIn.y, AmbCirCon.TBorIn) annotation (Line(points={{1,-40},{16,-40},
            {16,-2},{31,-2}}, color={0,0,127}));
    connect(TBorOut.y, AmbCirCon.TBorOut) annotation (Line(points={{1,-70},{20,-70},
            {20,-4.6},{31,-4.6}}, color={0,0,127}));
    connect(TDisHexLvg.y, AmbCirCon.TDisHexLvg) annotation (Line(points={{1,-130},
            {28,-130},{28,-10},{31,-10}}, color={0,0,127}));
    connect(TDisHexEnt.y, AmbCirCon.TDisHexEnt) annotation (Line(points={{1,-100},
            {24,-100},{24,-7.6},{31,-7.6}},color={0,0,127}));
    annotation (Icon(coordinateSystem(preserveAspectRatio=false), graphics={
          Ellipse(lineColor = {75,138,73},
                  fillColor={255,255,255},
                  fillPattern = FillPattern.Solid,
                  extent={{-98,-100},{98,98}}),
          Polygon(lineColor = {0,0,255},
                  fillColor = {75,138,73},
                  pattern = LinePattern.None,
                  fillPattern = FillPattern.Solid,
                  points={{-30,64},{70,4},{-30,-56},{-30,64}})}),  Diagram(coordinateSystem(preserveAspectRatio=false, extent={{-100,
              -160},{100,100}}),
          graphics={Line(points={{-22,22}}, color={28,108,200})}),
      experiment(StopTime=1500),
      __Dymola_Commands(
    file="modelica://Buildings/Resources/Scripts/Dymola/Applications/DHC/EnergyTransferStations/Control/AmbientCircuitControllerBlock.mos"
          "Simulate and plot"),
           experiment(Tolerance=1e-6, StopTime=14400),
           Documentation(info="<html>
<p>
This model validates the controller block
<a href=\"Buildings.Applications.DHC.EnergyTransferStations.Control.AmbientCircuitController\"> 
Buildings.Applications.DHC.EnergyTransferStations.Control.AmbientCircuitController</a>.
<p>

</html>",   revisions="<html>
<ul>
<li>
 <br/>
</li>
</ul>
</html>"));
  end AmbientCircuitControllerBlock;

  model AmbientCircuitControllerSidBlock
    "AmbientCircuitControllerValidation"
    package Medium = Buildings.Media.Water "Medium model";

    parameter Modelica.SIunits.TemperatureDifference dTHex = 5
      "Temperature difference in and out of substation heat exchanger";
     parameter Modelica.SIunits.TemperatureDifference dTGeo= 5
      "Temperature difference in and out of borefield";

    Modelica.Blocks.Sources.BooleanPulse valHea(width=50, period=500)
     "Heating side valve status"
      annotation (Placement(transformation(extent={{-42,40},{-22,60}})));
    Modelica.Blocks.Sources.BooleanConstant valCoo(k=false)
      "Cooling side valve status"
      annotation (Placement(transformation(extent={{-42,10},{-22,30}})));
    Modelica.Blocks.Sources.Constant TBorMaxEnt(k=30 + 273.15)
      "Borefield Maximum entering water temperature"
      annotation (Placement(transformation(extent={{-42,-58},{-22,-38}})));
    Modelica.Blocks.Sources.BooleanPulse rejFulHea(period=500)
      "Rejection full heat load mode."
      annotation (Placement(transformation(extent={{-80,10},{-60,30}})));
    Modelica.Blocks.Sources.BooleanConstant reqHea "Heating is required"
      annotation (Placement(transformation(extent={{-42,72},{-22,92}})));
    Modelica.Blocks.Sources.Constant TDisHexEnt(k=18 + 273.15)
      "District heat exchnager entering water temperature"
      annotation (Placement(transformation(extent={{-42,-88},{-22,-68}})));
    Control.AmbientCircuitSid ambCirCon(
        dTHex=dTHex,
        dTGeo=dTGeo)
    "Ambient water circuit control"
      annotation (Placement(transformation(extent={{10,-10},{30,10}})));
    Modelica.Blocks.Sources.Constant TDisHexLvg(k=12 + 273.15)
      "District heat exchnager leaving water temperature"
      annotation (Placement(transformation(extent={{-42,-118},{-22,-98}})));
    Modelica.Blocks.Sources.Constant TBorEnt(k=12 + 273.15)
      "Borefiled entering water temperature"
      annotation (Placement(transformation(extent={{-42,-152},{-22,-132}})));
    Modelica.Blocks.Sources.BooleanConstant reqCoo(k=false)
      "Cooling is required."
      annotation (Placement(transformation(extent={{-80,-50},{-60,-30}})));
    Modelica.Blocks.Sources.BooleanPulse rejFulCoo(
      width=1,
      period=500,
      startTime=0) "Reject full cooling load mode."
      annotation (Placement(transformation(extent={{-80,-20},{-60,0}})));
  equation
    connect(valCoo.y,ambCirCon. valCoo) annotation (Line(points={{-21,20},{-10,20},
            {-10,5.2},{9,5.2}},color={255,0,255}));
    connect(valHea.y,ambCirCon. valHea) annotation (Line(points={{-21,50},{-8,50},
            {-8,7.6},{9,7.6}},color={255,0,255}));
    connect(TDisHexLvg.y,ambCirCon. TDisHexLvg) annotation (Line(points={{-21,
            -108},{4,-108},{4,-7.2},{9,-7.2}},
                                          color={0,0,127}));
    connect(TDisHexEnt.y,ambCirCon. TDisHexEnt) annotation (Line(points={{-21,-78},
            {2,-78},{2,-5.2},{9,-5.2}},    color={0,0,127}));
    connect(ambCirCon.TBorMaxEnt, TBorMaxEnt.y) annotation (Line(points={{9,-3},{
            -6,-3},{-6,-48},{-21,-48}},
                                      color={0,0,127}));
    connect(TBorEnt.y, ambCirCon.TBorEnt) annotation (Line(points={{-21,-142},{6,
            -142},{6,-9.8},{9,-9.8}},
                                  color={0,0,127}));
    connect(reqHea.y, ambCirCon.reqHea) annotation (Line(points={{-21,82},{-2,82},
            {-2,9.8},{9,9.8}}, color={255,0,255}));
    connect(rejFulHea.y, ambCirCon.rejHeaFulLoa) annotation (Line(points={{-59,20},
            {-46,20},{-46,3},{9,3}},  color={255,0,255}));
    connect(reqCoo.y, ambCirCon.reqCoo) annotation (Line(points={{-59,-40},{-46,
            -40},{-46,-1},{9,-1}},
                               color={255,0,255}));
    connect(ambCirCon.rejCooFulLoa, rejFulCoo.y) annotation (Line(points={{9,1},{
            -52,1},{-52,-10},{-59,-10}}, color={255,0,255}));
    annotation (Icon(coordinateSystem(preserveAspectRatio=false), graphics={
          Ellipse(lineColor = {75,138,73},
                  fillColor={255,255,255},
                  fillPattern = FillPattern.Solid,
                  extent={{-98,-100},{98,98}}),
          Polygon(lineColor = {0,0,255},
                  fillColor = {75,138,73},
                  pattern = LinePattern.None,
                  fillPattern = FillPattern.Solid,
                  points={{-30,64},{70,4},{-30,-56},{-30,64}})}),  Diagram(coordinateSystem(preserveAspectRatio=false, extent={{-100,
              -160},{100,100}}),
          graphics={Line(points={{-22,22}}, color={28,108,200})}),
      experiment(StopTime=1500),
      __Dymola_Commands(
    file="modelica://Buildings/Resources/Scripts/Dymola/Applications/DHC/EnergyTransferStations/Control/AmbientCircuitControllerSidBlock.mos"
          "Simulate and plot"),
           experiment(Tolerance=1e-6, StopTime=14400),
           Documentation(info="<html>
<p>
This model validates the controller block
<a href=\"Buildings.Applications.DHC.EnergyTransferStations.Control.AmbientCircuitController\"> 
Buildings.Applications.DHC.EnergyTransferStations.Control.AmbientCircuitController</a>.
<p>

</html>",   revisions="<html>
<ul>
<li>
 <br/>
</li>
</ul>
</html>"));
  end AmbientCircuitControllerSidBlock;

  model BufferTanksValidation
    "Validation of hot and cold buffer tanks."

    Control.ETSController
      ETSCon(THys=1)
                annotation (Placement(transformation(extent={{42,-34},{62,-16}})));
    Buildings.Controls.OBC.CDL.Continuous.Sources.Constant TTanhotTop1(k=38 +
          273.15)
      "Load heat exchanger entering water temperature"
      annotation (Placement(transformation(extent={{0,36},{20,56}})));
    Modelica.Blocks.Sources.Constant TTanCooBot(k=8 + 273.15)
      "Cooling setpoint temperature"
      annotation (Placement(transformation(extent={{-14,-90},{6,-70}})));
    Modelica.Blocks.Sources.Constant THeaSet1(k=35 + 273.15)
      "Heating set point temperature"
      annotation (Placement(transformation(extent={{-46,-12},{-26,8}})));
    Modelica.Blocks.Sources.Constant TTanCooTop(k=12 + 273.15)
      "Maximum heating set point temperature"
      annotation (Placement(transformation(extent={{12,-106},{32,-86}})));
    Modelica.Blocks.Sources.Constant TTanHotBot1(k=40 + 273.15)
      "Minimum heating set point temperature"
      annotation (Placement(transformation(extent={{-24,8},{-4,28}})));
    Modelica.Blocks.Sources.Constant TCooSet(k=7 + 273.15)
      "Cooling setpoint temperature"
      annotation (Placement(transformation(extent={{-38,-74},{-18,-54}})));
    Modelica.Blocks.Sources.Constant mhotNor(k=0.2)
      "Heating set point temperature"
      annotation (Placement(transformation(extent={{-76,-28},{-56,-8}})));
    Modelica.Blocks.Sources.Constant mColNor(k=0.2)
      "Heating set point temperature"
      annotation (Placement(transformation(extent={{-76,-58},{-56,-38}})));
  equation
    connect(THeaSet1.y, ETSCon.TSetHea) annotation (Line(points={{-25,-2},{18,-2},
            {18,-20.5},{41,-20.5}}, color={0,0,127}));
    connect(TTanhotTop1.y, ETSCon.TTanHeaTop) annotation (Line(points={{22,46},{
            32,46},{32,-16.9},{41,-16.9}}, color={0,0,127}));
    connect(TTanHotBot1.y, ETSCon.TTanHeaBot) annotation (Line(points={{-3,18},{
            24,18},{24,-18.7},{41,-18.7}}, color={0,0,127}));
    connect(ETSCon.TTanCooTop,TTanCooTop. y) annotation (Line(points={{41,-33.1},
            {38,-33.1},{38,-96},{33,-96}},
                                      color={0,0,127}));
    connect(TTanCooBot.y,ETSCon. TTanCooBot) annotation (Line(points={{7,-80},{28,
            -80},{28,-31.3},{41,-31.3}},
                                       color={0,0,127}));
    connect(TCooSet.y,ETSCon. TSetCoo) annotation (Line(points={{-17,-64},{20,-64},
            {20,-29.5},{41,-29.5}},
                                  color={0,0,127}));
    connect(mhotNor.y,ETSCon. mTanHotNor) annotation (Line(points={{-55,-18},{16,
            -18},{16,-24},{41,-24},{41,-24.1}},
                                      color={0,0,127}));
    connect(mColNor.y,ETSCon. mTanColNor) annotation (Line(points={{-55,-48},{16,
            -48},{16,-25.9},{41,-25.9}},
                                  color={0,0,127}));
    annotation (Icon(coordinateSystem(preserveAspectRatio=false), graphics={
          Ellipse(lineColor = {75,138,73},
                  fillColor={255,255,255},
                  fillPattern = FillPattern.Solid,
                  extent={{-98,-100},{98,98}}),
          Polygon(lineColor = {0,0,255},
                  fillColor = {75,138,73},
                  pattern = LinePattern.None,
                  fillPattern = FillPattern.Solid,
                  points={{-30,64},{70,4},{-30,-56},{-30,64}})}),  Diagram(coordinateSystem(preserveAspectRatio=false, extent={
              {-100,-140},{100,100}}),
          graphics={Line(points={{-22,22}}, color={28,108,200})}),
      experiment(StopTime=14400),
      __Dymola_Commands(
    file="modelica://Buildings/Resources/Scripts/Dymola/Applications/DHC/EnergyTransferStations/Control/HeatpumpController.mos"
          "Simulate and plot"),
           experiment(Tolerance=1e-6, StopTime=14400));
  end BufferTanksValidation;

  model ETSController "Validation of ETS controller."

    Control.ETSController
      ETSCon(THys=1)
                annotation (Placement(transformation(extent={{42,-34},{62,-16}})));
    Buildings.Controls.OBC.CDL.Continuous.Sources.Constant TTanhotTop(k=38 +
          273.15) "Load heat exchanger entering water temperature"
      annotation (Placement(transformation(extent={{0,36},{20,56}})));
    Modelica.Blocks.Sources.Constant TTanCooBot(k=8 + 273.15)
      "Cooling setpoint temperature"
      annotation (Placement(transformation(extent={{-14,-90},{6,-70}})));
    Modelica.Blocks.Sources.Constant THeaSet(k=35 + 273.15)
      "Heating set point temperature"
      annotation (Placement(transformation(extent={{-46,-12},{-26,8}})));
    Modelica.Blocks.Sources.Constant TTanCooTop(k=12 + 273.15)
      "Maximum heating set point temperature"
      annotation (Placement(transformation(extent={{12,-106},{32,-86}})));
    Modelica.Blocks.Sources.Constant TTanHotBot(k=40 + 273.15)
      "Minimum heating set point temperature"
      annotation (Placement(transformation(extent={{-24,8},{-4,28}})));
    Modelica.Blocks.Sources.Constant TCooSet(k=7 + 273.15)
      "Cooling setpoint temperature"
      annotation (Placement(transformation(extent={{-38,-74},{-18,-54}})));
    Modelica.Blocks.Sources.Constant mhotNor(k=0.2)
      "Heating set point temperature"
      annotation (Placement(transformation(extent={{-76,-28},{-56,-8}})));
    Modelica.Blocks.Sources.Constant mColNor(k=0.2)
      "Heating set point temperature"
      annotation (Placement(transformation(extent={{-76,-58},{-56,-38}})));
  equation
    connect(THeaSet.y, ETSCon.TSetHea) annotation (Line(points={{-25,-2},{18,-2},
            {18,-20.5},{41,-20.5}}, color={0,0,127}));
    connect(TTanhotTop.y, ETSCon.TTanHeaTop) annotation (Line(points={{22,46},{32,
            46},{32,-16.9},{41,-16.9}}, color={0,0,127}));
    connect(TTanHotBot.y, ETSCon.TTanHeaBot) annotation (Line(points={{-3,18},{24,
            18},{24,-18.7},{41,-18.7}}, color={0,0,127}));
    connect(ETSCon.TTanCooTop,TTanCooTop. y) annotation (Line(points={{41,-33.1},
            {38,-33.1},{38,-96},{33,-96}},
                                      color={0,0,127}));
    connect(TTanCooBot.y,ETSCon. TTanCooBot) annotation (Line(points={{7,-80},{28,
            -80},{28,-31.3},{41,-31.3}},
                                       color={0,0,127}));
    connect(TCooSet.y,ETSCon. TSetCoo) annotation (Line(points={{-17,-64},{20,-64},
            {20,-29.5},{41,-29.5}},
                                  color={0,0,127}));
    connect(mhotNor.y,ETSCon. mTanHotNor) annotation (Line(points={{-55,-18},{16,
            -18},{16,-24},{41,-24},{41,-24.1}},
                                      color={0,0,127}));
    connect(mColNor.y,ETSCon. mTanColNor) annotation (Line(points={{-55,-48},{16,
            -48},{16,-25.9},{41,-25.9}},
                                  color={0,0,127}));
    annotation (Icon(coordinateSystem(preserveAspectRatio=false), graphics={
          Ellipse(lineColor = {75,138,73},
                  fillColor={255,255,255},
                  fillPattern = FillPattern.Solid,
                  extent={{-98,-100},{98,98}}),
          Polygon(lineColor = {0,0,255},
                  fillColor = {75,138,73},
                  pattern = LinePattern.None,
                  fillPattern = FillPattern.Solid,
                  points={{-30,64},{70,4},{-30,-56},{-30,64}})}),  Diagram(coordinateSystem(preserveAspectRatio=false, extent={
              {-100,-140},{100,100}}),
          graphics={Line(points={{-22,22}}, color={28,108,200})}),
      experiment(StopTime=14400),
      __Dymola_Commands(
    file="modelica://Buildings/Resources/Scripts/Dymola/Applications/DHC/EnergyTransferStations/Control/HeatpumpController.mos"
          "Simulate and plot"),
           experiment(Tolerance=1e-6, StopTime=14400));
  end ETSController;

  model ChillerController "Example of the chiller controller block."
    package Medium = Buildings.Media.Water "Medium model";

    Control.ChillerOnOff       chiCon
      annotation (Placement(transformation(extent={{74,0},{94,20}})));

    Modelica.Blocks.Sources.BooleanPulse heaMod(width=50, period=1000)
      "Step control"
      annotation (Placement(transformation(extent={{0,60},{20,80}})));
    Modelica.Blocks.Sources.BooleanPulse cooMod(
      width=50,
      period=500,
      startTime=500)
      "Step control"
      annotation (Placement(transformation(extent={{0,30},{20,50}})));
    Modelica.Blocks.Sources.Constant TCooSet(k=7 + 273.15)
      "Cooling setpoint temperature"
      annotation (Placement(transformation(extent={{-44,70},{-24,90}})));
    Modelica.Blocks.Sources.Constant TCooSetMin(k=4 + 273.15)
      "Minimum cooling set point temperature"
      annotation (Placement(transformation(extent={{-40,0},{-20,20}})));
    Modelica.Blocks.Sources.Constant TConLvg(k=30 + 273.15)
      "Condenser leaving water temperature"
      annotation (Placement(transformation(extent={{0,-80},{20,-60}})));
    Modelica.Blocks.Sources.Constant THeaSet(k=40 + 273.15)
      "Heating set point temperature"
      annotation (Placement(transformation(extent={{-42,34},{-22,54}})));
    Modelica.Blocks.Sources.Constant TMinConEnt(k=25 + 273.15)
      "Minimum condenser entering temperature"
      annotation (Placement(transformation(extent={{-40,-40},{-20,-20}})));
    Modelica.Blocks.Sources.Constant TMaxEvaEnt(k=18 + 273.15)
      "Maximum Evaporator entering temperature."
      annotation (Placement(transformation(extent={{-40,-80},{-20,-60}})));
    Modelica.Blocks.Sources.Constant TEvaEnt(k=12 + 273.15)
      "Evaporator entering temperature"
      annotation (Placement(transformation(extent={{0,-40},{20,-20}})));
    Modelica.Blocks.Sources.Constant TConEnt(k=25 + 273.15)
      "Condenser entering temperature"
      annotation (Placement(transformation(extent={{0,-120},{20,-100}})));
  equation
    connect(chiCon.reqHea, heaMod.y) annotation (Line(points={{72.6,19},{70,19},{70,
            70},{21,70}}, color={255,0,255}));
    connect(chiCon.reqCoo, cooMod.y) annotation (Line(points={{72.6,15.4},{68,15.4},
            {68,40},{21,40}}, color={255,0,255}));
    connect(THeaSet.y, chiCon.TSetHea) annotation (Line(points={{-21,44},{-12,44},
            {-12,11},{73,11}}, color={0,0,127}));
    connect(TMinConEnt.y, chiCon.TMinConEnt) annotation (Line(points={{-19,-30},{-12,
            -30},{-12,7.2},{73,7.2}}, color={0,0,127}));
    connect(TMaxEvaEnt.y, chiCon.TMaxEvaEnt) annotation (Line(points={{-19,-70},{-6,
            -70},{-6,5.4},{73,5.4}}, color={0,0,127}));
    connect(TEvaEnt.y, chiCon.TEvaEnt) annotation (Line(points={{21,-30},{26,-30},
            {26,3.4},{73,3.4}}, color={0,0,127}));
    connect(TConEnt.y, chiCon.TConEnt) annotation (Line(points={{21,-110},{38,-110},
            {38,0.2},{73,0.2}}, color={0,0,127}));
    connect(chiCon.TSetCoo, TCooSet.y) annotation (Line(points={{73,13},{-6,13},{-6,
            80},{-23,80}}, color={0,0,127}));
    connect(TCooSetMin.y, chiCon.TSetCooMin) annotation (Line(points={{-19,10},{-16,
            10},{-16,8.8},{73,8.8}}, color={0,0,127}));
    connect(TConLvg.y, chiCon.TConLvg) annotation (Line(points={{21,-70},{32,-70},
            {32,1.6},{73,1.6}}, color={0,0,127}));
    annotation (Icon(coordinateSystem(preserveAspectRatio=false), graphics={
          Ellipse(lineColor = {75,138,73},
                  fillColor={255,255,255},
                  fillPattern = FillPattern.Solid,
                  extent={{-98,-100},{98,98}}),
          Polygon(lineColor = {0,0,255},
                  fillColor = {75,138,73},
                  pattern = LinePattern.None,
                  fillPattern = FillPattern.Solid,
                  points={{-30,64},{70,4},{-30,-56},{-30,64}})}),  Diagram(coordinateSystem(preserveAspectRatio=false, extent={
              {-100,-140},{100,100}}),
          graphics={Line(points={{-22,22}}, color={28,108,200})}),
      experiment(StopTime=1000, __Dymola_Algorithm="Dassl"),
      __Dymola_Commands(
    file="modelica://Buildings/Resources/Scripts/Dymola/Applications/DHC/EnergyTransferStations/Control/ChillerController.mos"
          "Simulate and plot"),
           experiment(Tolerance=1e-6, StopTime=2000),
  Documentation(info="<html>
<p>
This model validates the controller block
<a href=\"Buildings.Applications.DHC.EnergyTransferStations.Control.HeatPumpController\"> 
Buildings.Applications.DHC.EnergyTransferStations.Control.HeatPumpController</a>.
<p>

</html>",   revisions="<html>
<ul>
<li>
 <br/>
</li>
</ul>
</html>"));
  end ChillerController;
annotation (preferredView="info", Documentation(info="<html>
<p>
This package contains the validation of the energy transfer stations controllers.
</p>
</html>"));
end Validation;
