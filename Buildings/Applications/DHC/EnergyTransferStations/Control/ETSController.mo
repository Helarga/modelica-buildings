within Buildings.Applications.DHC.EnergyTransferStations.Control;
model ETSController
  "Overall controller of the 5thG substation "
  extends Modelica.Blocks.Icons.Block;

  parameter Modelica.SIunits.TemperatureDifference THys(min=0.1)=THys
    "Temperature hysteresis";
  parameter Modelica.Blocks.Types.SimpleController
    controllerType=Modelica.Blocks.Types.SimpleController.PI "Type of controller"
    annotation (Dialog(group="PID controller"));
  parameter Real k(final unit="1/K")=0.01
    "Gain of controller"
    annotation (Dialog(group="PID controller"));
  parameter Modelica.SIunits.Time Ti(min=0)=60
    "Time constant of integrator block"
    annotation (Dialog(group="PID controller",
      enable=controllerType==Modelica.Blocks.Types.SimpleController.PI
         or  controllerType==Modelica.Blocks.Types.SimpleController.PID));
  parameter Modelica.SIunits.Time Td(min=0) = 0.1
    "Time constant of derivative block"
    annotation (Dialog(group="PID controller",
      enable=controllerType==Modelica.Blocks.Types.SimpleController.PD
          or controllerType==Modelica.Blocks.Types.SimpleController.PID));
  parameter Real yMin = 0.01 "Minimum control output"
    annotation (Dialog(group="PID controller"));

  Buildings.Controls.OBC.CDL.Interfaces.RealInput TSetCoo(final unit="K", displayUnit="degC")
    "Setpoint for cooling supply water to space loads" annotation (Placement(transformation(extent={{-260,-160},{-220,
            -120}}), iconTransformation(extent={{-120,-60},{-100,-40}})));
  Buildings.Controls.OBC.CDL.Interfaces.RealInput TTanCooTop(final unit="K",displayUnit="degC")
    "Top temperature of cooling buffer tank"
    annotation (Placement(transformation(extent={{-260,-200},{-220,-160}}),
      iconTransformation(extent={{-120,-100},{-100,-80}})));
  Buildings.Controls.OBC.CDL.Interfaces.RealInput TTanCooBot(final unit="K",displayUnit="degC")
    "Bottom temperature of cooling buffer tank"
    annotation (Placement(transformation(extent={{-260,-230},{-220,-190}}),
      iconTransformation(extent={{-120,-80},{-100,-60}})));
  Buildings.Controls.OBC.CDL.Interfaces.RealInput TTanHeaTop(final unit="K",displayUnit="degC")
    "Top temperature of heating buffer tank"
    annotation (Placement(transformation(extent={{-260,170},{-220,210}}),
      iconTransformation(extent={{-120,80},{-100,100}})));
  Buildings.Controls.OBC.CDL.Interfaces.RealInput TTanHeaBot(final unit="K",displayUnit="degC")
    "Bottom temperature of heating buffer tank"
    annotation (Placement(transformation(extent={{-260,140},{-220,180}}),
      iconTransformation(extent={{-120,60},{-100,80}})));
  Buildings.Controls.OBC.CDL.Interfaces.RealInput TSetHea(final unit="K", displayUnit="degC")
    "Setpoint for heating supply water to space loads" annotation (Placement(transformation(extent={{-260,200},{-220,
            240}}), iconTransformation(extent={{-120,40},{-100,60}})));
  Buildings.Controls.OBC.CDL.Interfaces.RealInput mTanHotNor(final unit="1")
    "Normalized flow rate of hot buffer tank"
    annotation (Placement(transformation(extent={{-260,20},{-220,60}}),
      iconTransformation(extent={{-120,0},{-100,20}})));
  Buildings.Controls.OBC.CDL.Interfaces.RealInput mTanColNor(final unit="1")
    "Normalized flow rate of cold buffer tank"
    annotation (Placement(transformation(extent={{-260,-30},{-220,10}}),
      iconTransformation(extent={{-120,-20},{-100,0}})));
  Buildings.Controls.OBC.CDL.Interfaces.RealOutput pumConTanMin(final unit="1")
    "Condenser water supply pump control signal to assure minimum flow rate to the hot tank"
    annotation (Placement(transformation(extent={{220,60},{240,80}}),
        iconTransformation(extent={{100,20},{120,40}})));
  Buildings.Controls.OBC.CDL.Interfaces.RealOutput pumEvaTanMin(final unit="1")
    "Evaporator water supply pump control signal to assure minimum flow rate to the cold tank"
    annotation (Placement(transformation(extent={{220,-80},{240,-60}}),
        iconTransformation(extent={{100,0},{120,20}})));
  Buildings.Controls.OBC.CDL.Interfaces.RealOutput ValHeaPos "Hot side valve status(1:On, 0:Off)" annotation (
      Placement(transformation(extent={{220,120},{240,140}}), iconTransformation(extent={{100,-20},{120,0}})));
  Buildings.Controls.OBC.CDL.Interfaces.RealOutput ValCooPos "Cold side valve status(1:On, 0:Off)" annotation (
      Placement(transformation(extent={{220,-210},{240,-190}}), iconTransformation(extent={{100,-40},{120,-20}})));
  Buildings.Controls.OBC.CDL.Interfaces.BooleanOutput reqHea
    "True if heating is required from heat pump, false otherwise" annotation (Placement(transformation(extent={{220,
            200},{240,220}}), iconTransformation(extent={{100,80},{120,100}})));
  Buildings.Controls.OBC.CDL.Interfaces.BooleanOutput reqCoo
    "True if cooling is required from heat pump, false otherwise" annotation (Placement(transformation(extent={{220,
            -150},{240,-130}}), iconTransformation(extent={{100,-100},{120,-80}})));
  Buildings.Controls.OBC.CDL.Interfaces.BooleanOutput rejColFulLoa
    "true if cold side requires heat rejection with borefield and district heat exchanger"
    annotation (Placement(transformation(extent={{220,-170},{240,-150}}),
      iconTransformation(extent={{100,-82},{120,-62}})));
  Buildings.Controls.OBC.CDL.Interfaces.BooleanOutput rejHeaFulLoa
    "true if hot side requires heat rejection with borefield and district heat exchaner"
    annotation (Placement(transformation(extent={{220,182},{240,202}}),
      iconTransformation(extent={{100,-62},{120,-42}})));
  Buildings.Controls.OBC.CDL.Interfaces.BooleanOutput ValHea
    "Hot side valve status,true when rejection of part or full heating load is reuired" annotation (Placement(
        transformation(extent={{220,150},{240,170}}), iconTransformation(extent={{100,60},{120,80}})));
  Buildings.Controls.OBC.CDL.Interfaces.BooleanOutput ValCoo
    "Cold side valve status,true when rejection of part or full cooling load is reuired"
                                                                                        annotation (Placement(
        transformation(extent={{220,-190},{240,-170}}), iconTransformation(extent={{100,40},{120,60}})));

  Buildings.Controls.Continuous.LimPID conHeaTan(
    final k=k,
    final Ti=Ti,
    final Td=Td,
    reset=Buildings.Types.Reset.Parameter,
    y_reset=yMin,
    yMin=yMin,
    final controllerType=Modelica.Blocks.Types.SimpleController.P)
    "Heating water supply pump control"
    annotation (Placement(transformation(extent={{-40,60},{-20,80}})));
  Buildings.Controls.Continuous.LimPID conCooTan(
    final k=k,
    final Ti=Ti,
    final Td=Td,
    reset=Buildings.Types.Reset.Parameter,
    y_reset=yMin,
    yMin=yMin,
    reverseAction=true,
    final controllerType=Modelica.Blocks.Types.SimpleController.P)
    "Cooling water supply pump control"
    annotation (Placement(transformation(extent={{-40,10},{-20,30}})));
  Buildings.Controls.OBC.CDL.Continuous.Sources.Constant con4(k=0)
    annotation (Placement(transformation(extent={{60,80},{80,100}})));
  Buildings.Controls.OBC.CDL.Logical.Switch pumHeaCon
    "Heating water supply pump control"
    annotation (Placement(transformation(extent={{160,60},{180,80}})));
  Buildings.Controls.OBC.CDL.Logical.Switch pumCooCon
    "Cooling water supply pump control"
    annotation (Placement(transformation(extent={{160,-80},{180,-60}})));
  HotSideControllerUO conHotSid(THys=THys)
                                        "Hot side controller"
    annotation (Placement(transformation(extent={{-160,176},{-140,196}})));
  ColdSideControlleUO conColSid(THys=THys)
                                        "Cold side controller"
   annotation (Placement(
        transformation(extent={{-160,-210},{-140,-190}})));
  Buildings.Controls.OBC.CDL.Logical.Or or1
    annotation (Placement(transformation(extent={{-120,102},{-100,122}})));
  Buildings.Controls.OBC.CDL.Logical.Not not1
    annotation (Placement(transformation(extent={{-90,102},{-70,122}})));
  Buildings.Controls.OBC.CDL.Continuous.Sources.Constant mSetHotTan(k=0.2)
    "Miniumim setpoint value of the normalized flow charging the hot tank"
    annotation (Placement(transformation(extent={{-100,60},{-80,80}})));
  Buildings.Controls.OBC.CDL.Continuous.Sources.Constant mSetColTan(k=-0.2) "Miniumim setpoint value of the normalized flow charging the cold buffer tank,
   (negative value indicates the discharge flow direction)."
    annotation (Placement(transformation(extent={{-100,10},{-80,30}})));

  Buildings.Controls.OBC.CDL.Continuous.Sources.Constant           con1(k=0)
    annotation (Placement(transformation(extent={{62,-58},{82,-38}})));
equation
  connect(mSetColTan.y, conCooTan.u_s)
    annotation (Line(points={{-78,20},{-42,20}}, color={0,0,127}));
  connect(pumHeaCon.y,pumConTanMin)
    annotation (Line(points={{182,70},{230,70}}, color={0,0,127}));
  connect(pumCooCon.y,pumEvaTanMin)
    annotation (Line(points={{182,-70},{230,-70}}, color={0,0,127}));
  connect(TTanHeaTop, conHotSid.TTop) annotation (Line(points={{-240,190},{-200,
          190},{-200,191},{-161,191}}, color={0,0,127}));
  connect(TTanHeaBot, conHotSid.TBot) annotation (Line(points={{-240,160},{-180,
          160},{-180,181},{-161,181}}, color={0,0,127}));

  connect(TTanCooTop, conColSid.TTop) annotation (Line(points={{-240,-180},{-200,
          -180},{-200,-195},{-161,-195}}, color={0,0,127}));
  connect(TTanCooBot, conColSid.TBot) annotation (Line(points={{-240,-210},{-192,
          -210},{-192,-205},{-161,-205}}, color={0,0,127}));
  connect(conHotSid.reqHea, or1.u1) annotation (Line(points={{-139,195},{-130,195},
          {-130,112},{-122,112}}, color={255,0,255}));
  connect(conColSid.reqCoo, or1.u2) annotation (Line(points={{-139,-191},{-130,-191},
          {-130,104},{-122,104}}, color={255,0,255}));
  connect(or1.y, not1.u)
    annotation (Line(points={{-98,112},{-92,112}}, color={255,0,255}));
  connect(not1.y, conHeaTan.trigger) annotation (Line(points={{-68,112},{-60,112},
          {-60,50},{-38,50},{-38,58}},      color={255,0,255}));
  connect(not1.y, conCooTan.trigger) annotation (Line(points={{-68,112},{-60,112},
          {-60,0},{-38,0},{-38,8}},       color={255,0,255}));
  connect(mTanHotNor, conHeaTan.u_m)
    annotation (Line(points={{-240,40},{-30,40},{-30,58}}, color={0,0,127}));
  connect(mSetHotTan.y, conHeaTan.u_s)
    annotation (Line(points={{-78,70},{-42,70}}, color={0,0,127}));
  connect(conCooTan.u_m, mTanColNor) annotation (Line(points={{-30,8},{-30,-10},{-240,-10}},
                                         color={0,0,127}));
  connect(conHotSid.reqHea, reqHea)
    annotation (Line(points={{-139,195},{50,195},{50,210},{230,210}}, color={255,0,255}));
  connect(conColSid.reqCoo, reqCoo) annotation (Line(points={{-139,-191},{-130,-191},{-130,-180},{20,-180},{20,-140},
          {230,-140}}, color={255,0,255}));
  connect(conColSid.rejFulLoa, rejColFulLoa) annotation (Line(points={{-139,-194},
          {40,-194},{40,-160},{230,-160}}, color={255,0,255}));
  connect(conHotSid.rejFulLoa, rejHeaFulLoa)
    annotation (Line(points={{-139,192},{230,192}}, color={255,0,255}));
  connect(ValHea, conHotSid.valSta)
    annotation (Line(points={{230,160},{148,160},{148,186},{-139,186}}, color={255,0,255}));
  connect(conColSid.valSta, ValCoo)
    annotation (Line(points={{-139,-200},{100,-200},{100,-180},{230,-180}}, color={255,0,255}));
  connect(pumConTanMin,pumConTanMin)  annotation (Line(points={{230,70},{168,70},
          {168,70},{230,70}}, color={0,0,127}));
  connect(pumHeaCon.u1, con4.y) annotation (Line(points={{158,78},{122,78},{122,
          90},{82,90}}, color={0,0,127}));
  connect(pumHeaCon.u2, not1.y) annotation (Line(points={{158,70},{18,70},{18,112},
          {-68,112}},      color={255,0,255}));
  connect(not1.y, pumCooCon.u2) annotation (Line(points={{-68,112},{18,112},{18,
          -70},{158,-70}}, color={255,0,255}));
  connect(pumCooCon.u1, con1.y) annotation (Line(points={{158,-62},{120,-62},{
          120,-48},{84,-48}}, color={0,0,127}));
  connect(conHeaTan.y, pumHeaCon.u3) annotation (Line(points={{-19,70},{0,70},{
          0,62},{158,62}}, color={0,0,127}));
  connect(conCooTan.y, pumCooCon.u3) annotation (Line(points={{-19,20},{0,20},{
          0,-78},{158,-78}}, color={0,0,127}));
  connect(TSetCoo, conColSid.TSet) annotation (Line(points={{-240,-140},{-174,
          -140},{-174,-191},{-161,-191}}, color={0,0,127}));
  connect(TSetHea, conHotSid.TSet) annotation (Line(points={{-240,220},{-202,220},
          {-202,195},{-161,195}},      color={0,0,127}));
  connect(conHotSid.yVal, ValHeaPos) annotation (Line(points={{-139,180},{118,180},
          {118,130},{230,130}}, color={0,0,127}));
  connect(conColSid.yVal, ValCooPos) annotation (Line(points={{-139,-206},{120,-206},
          {120,-200},{230,-200}}, color={0,0,127}));
  annotation (Icon(coordinateSystem(preserveAspectRatio=false)), Diagram(
        coordinateSystem(preserveAspectRatio=false, extent={{-220,-220},{220,220}})),
        defaultComponentName="ETSCon",
        Documentation(info="<html>
<p>
The block generates the s       



<h4>Notice</h4>
<p>
<b>
This controller assures that both the evaporator and condenser pumps are controlled to maintain a minimum water flow rate
to the hot and cold buffer tanks based on two real input signals of the normalized water mass flow rate.
</b>
</p>
</P>
<p>
<b>
It is important to mention that water flow direction during discharging at the cold buffer tank is from bottom
to top which indicates a negative flow direction.
While in case of the hot buffer tank the discharging direction is from top to bottom which indicates a positive flow direction.
</b>
</p>
</html>", revisions="<html>
<ul>
<li>
November 25, 2019, by Hagar Elarga:<br/>
Added the info section. 
</li>
</ul>
</html>"));
end ETSController;
