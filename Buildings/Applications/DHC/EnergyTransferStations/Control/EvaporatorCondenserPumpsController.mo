within Buildings.Applications.DHC.EnergyTransferStations.Control;
model EvaporatorCondenserPumpsController
  "The control block of the condenser and the evaporator water pumps"
     extends Modelica.Blocks.Icons.Block;

  parameter Modelica.SIunits.MassFlowRate mCon_flow_nominal( final displayUnit="kg/s")
     "Condenser nominal water flow rate";
  parameter Modelica.SIunits.MassFlowRate mEva_flow_nominal(final displayUnit="kg/s")
     "Evaporator nominal water flow rate";

  Buildings.Controls.OBC.CDL.Interfaces.RealInput mPriHea(final displayUnit="kg/s")
    "Water mass flow rate at the primary hot side" annotation (Placement(
        transformation(extent={{-120,64},{-100,84}}), iconTransformation(extent=
           {{-116,76},{-100,92}})));
  Buildings.Controls.OBC.CDL.Interfaces.RealInput mPriCoo(final displayUnit="kg/s")
    "Water mass flow rate at the primary cold side" annotation (Placement(
        transformation(extent={{-120,-98},{-100,-78}}), iconTransformation(
          extent={{-116,-88},{-100,-72}})));
  Buildings.Controls.OBC.CDL.Interfaces.RealInput mSecHea(final displayUnit="kg/s")
    "Water mass flow rate at the secondary building hot side" annotation (
      Placement(transformation(extent={{-120,98},{-100,118}}),
        iconTransformation(extent={{-116,52},{-100,68}})));
  Buildings.Controls.OBC.CDL.Interfaces.RealInput mSecCoo(final displayUnit="kg/s")
    "Water mass flow rate at the secondary building cold side" annotation (
      Placement(transformation(extent={{-120,-74},{-100,-54}}),
        iconTransformation(extent={{-116,-64},{-100,-48}})));
  Buildings.Controls.OBC.CDL.Interfaces.RealInput conFloMin(final displayUnit="kg/s")
    "Minimum mass flow rate of the heatpump at the condenser set by the manufacturer"
    annotation (Placement(transformation(extent={{-120,130},{-100,150}}),
        iconTransformation(extent={{-116,32},{-100,48}})));
  Buildings.Controls.OBC.CDL.Interfaces.RealInput evaFloMin(final displayUnit="kg/s")
    "Minimum mass flow rate of the heatpump at the evaporator set by the manufacturer"
    annotation (Placement(transformation(extent={{-120,-54},{-100,-34}}),
        iconTransformation(extent={{-116,8},{-100,24}})));
  Buildings.Controls.OBC.CDL.Interfaces.RealInput heaTanMin(final displayUnit="kg/s")
    "Condenser water supply pump control signal to assure minimum flow rate to the hot tank."
    annotation (Placement(transformation(extent={{-122,160},{-102,180}}),
        iconTransformation(extent={{-116,-22},{-100,-6}})));
  Buildings.Controls.OBC.CDL.Interfaces.RealInput cooTanMin(final displayUnit="kg/s")
    "Evaporator water supply pump control signal to assure minimum flow rate to the cold tank"
    annotation (Placement(transformation(extent={{-120,-26},{-100,-6}}),
        iconTransformation(extent={{-116,-44},{-100,-28}})));
  Buildings.Controls.OBC.CDL.Interfaces.BooleanInput reqCoo
    "Cooling is required Boolean signal"
    annotation (Placement(transformation(extent={{-128,4},{-100,32}}),
        iconTransformation(extent={{-128,-112},{-100,-84}})));
  Buildings.Controls.OBC.CDL.Interfaces.BooleanInput reqHea
    "Heating is required Boolean signal"
    annotation (Placement(transformation(extent={{-128,26},{-100,54}}),
        iconTransformation(extent={{-128,86},{-100,114}})));
  Buildings.Controls.OBC.CDL.Interfaces.RealOutput yPumCon
    "Condenser pump speed outlet signal"  annotation (Placement(transformation(
          extent={{200,144},{232,176}}), iconTransformation(extent={{100,70},{120,
            90}})));
  Buildings.Controls.OBC.CDL.Interfaces.RealOutput yPumEva
    "Evaporator pump speed outlet signal" annotation (Placement(transformation(
          extent={{200,-96},{232,-64}}), iconTransformation(extent={{100,-90},{120,
            -70}})));
  Buildings.Controls.OBC.CDL.Continuous.Max      max1
    annotation (Placement(transformation(extent={{30,148},{50,128}})));

  Buildings.Controls.Continuous.LimPID pumCon(
    controllerType=Modelica.Blocks.Types.SimpleController.PI,
    yMin=0.1,
    reverseAction=false,
    reset=Buildings.Types.Reset.Parameter,
    y_reset=0.1,
    k=1,
    Ti(displayUnit="s") = 100) "Controller for controller pump speed"
    annotation (Placement(transformation(extent={{-6,106},{14,126}})));
  Buildings.Controls.OBC.CDL.Continuous.Sources.Constant shuOffSig(k=0)
  "HeatPump, condenser pump  and evaporator pump shut off signal =0"
   annotation (Placement(transformation(extent={{80,4},{100,24}})));
  Buildings.Controls.OBC.CDL.Continuous.Max max2
    annotation (Placement(transformation(extent={{78,100},{98,80}})));
  Buildings.Controls.OBC.CDL.Logical.Switch swi1
    annotation (Placement(transformation(extent={{120,22},{140,42}})));

  Buildings.Controls.OBC.CDL.Logical.Or or2
    annotation (Placement(transformation(extent={{-40,22},{-20,42}})));
  Buildings.Controls.Continuous.LimPID pumEva(
    controllerType=Modelica.Blocks.Types.SimpleController.PI,
    yMin=0.1,
    reverseAction=false,
    reset=Buildings.Types.Reset.Parameter,
    y_reset=0.1,
    k=1,
    Ti(displayUnit="s") = 100) "Controller for evaporator pump speed"
    annotation (Placement(transformation(extent={{4,-52},{24,-32}})));
  Buildings.Controls.OBC.CDL.Continuous.Max      max4
    annotation (Placement(transformation(extent={{44,-12},{64,-32}})));
  Modelica.Blocks.Sources.RealExpression mSecHeaNorReq(y=(mSecHea*1.1)/
        mCon_flow_nominal)
    "Required increment of 10% of the normalized secondary heating circuit  in flow rate."
    annotation (Placement(transformation(extent={{-80,120},{-60,142}})));
  Modelica.Blocks.Sources.RealExpression mPimHeaNor(y=mPriHea/mCon_flow_nominal)
    "Normalized flow rate at the primary heating circuit."
    annotation (Placement(transformation(extent={{-80,72},{-60,92}})));
  Modelica.Blocks.Sources.RealExpression mPrimCooNorAct(y=mPriCoo/
        mEva_flow_nominal)
    "Normalized flow rate at the primary cooling circuit. "
    annotation (Placement(transformation(extent={{-52,-78},{-32,-58}})));
  Modelica.Blocks.Sources.RealExpression mSecCooNor(y=(mSecCoo*1.1)/mEva_flow_nominal)
    "Required increment of 10% of the normalized secondary cooling circuit  in flow rate."
    annotation (Placement(transformation(extent={{-86,-40},{-66,-20}})));
  Modelica.Blocks.Sources.RealExpression minConFloNor(y=conFloMin/
        mCon_flow_nominal) "Normalized minimum flow rate of the condenser."
    annotation (Placement(transformation(extent={{-80,100},{-60,120}})));
  Modelica.Blocks.Sources.RealExpression minEvaFloNor(y=evaFloMin/
        mEva_flow_nominal) "Normalized minimum flow rate of the evaporator."
    annotation (Placement(transformation(extent={{-84,-60},{-64,-40}})));
  Buildings.Controls.OBC.CDL.Continuous.Max      max
    annotation (Placement(transformation(extent={{-40,126},{-20,106}})));
  Buildings.Controls.OBC.CDL.Continuous.Max      max3
    annotation (Placement(transformation(extent={{-48,-22},{-28,-42}})));
equation
  connect(yPumEva,yPumEva) annotation (Line(points={{216,-80},{216,-80}}, color={0,0,127}));
  connect(reqHea, or2.u1) annotation (Line(
      points={{-114,40},{-78,40},{-78,32},{-42,32}},
      color={255,0,255},
      pattern=LinePattern.Dot));
  connect(reqCoo, or2.u2) annotation (Line(points={{-114,18},{-78,18},{-78,24},{
          -42,24}},   color={255,0,255},
      pattern=LinePattern.Dot));
  connect(or2.y,pumCon. trigger) annotation (Line(
      points={{-18,32},{-4,32},{-4,104}},
      color={255,0,255},
      thickness=0.5));
  connect(shuOffSig.y, swi1.u3) annotation (Line(
      points={{102,14},{106,14},{106,24},{118,24}},
      color={0,0,127},
      pattern=LinePattern.Dash));
  connect(or2.y, swi1.u2) annotation (Line(points={{-18,32},{118,32}},
                color={255,0,255}));
  connect(max2.y, swi1.u1) annotation (Line(points={{100,90},{108,90},{108,40},{
          118,40}},  color={0,0,127}));
  connect(yPumCon,yPumCon) annotation (Line(points={{216,160},{216,160}},
                                                 color={0,0,127}));
  connect(or2.y,pumEva. trigger) annotation (Line(
      points={{-18,32},{-4,32},{-4,-62},{6,-62},{6,-54}},
      color={255,0,255},
      thickness=0.5));
  connect(swi1.y,yPumCon)  annotation (Line(points={{142,32},{158,32},{158,160},
          {216,160}},
        color={0,0,127}));
  connect(swi1.y,yPumEva)  annotation (Line(points={{142,32},{158,32},{158,-80},
          {216,-80}},
        color={0,0,127}));
  connect(mPimHeaNor.y, pumCon.u_m)
    annotation (Line(points={{-59,82},{4,82},{4,104}}, color={0,0,127}));
  connect(mPrimCooNorAct.y, pumEva.u_m) annotation (Line(points={{-31,-68},{14,-68},{14,-54}}, color={0,0,127}));
  connect(max1.y, max2.u2) annotation (Line(points={{52,138},{70,138},{70,96},{76,
          96}}, color={0,0,127}));
  connect(max4.y, max2.u1) annotation (Line(points={{66,-22},{70,-22},{70,84},{76,
          84}}, color={0,0,127}));
  connect(pumCon.u_s, max.y)
    annotation (Line(points={{-8,116},{-18,116}}, color={0,0,127}));
  connect(max.u1, minConFloNor.y)
    annotation (Line(points={{-42,110},{-59,110}}, color={0,0,127}));
  connect(max.u2, mSecHeaNorReq.y) annotation (Line(points={{-42,122},{-44,122},
          {-44,131},{-59,131}}, color={0,0,127}));
  connect(max1.u2, heaTanMin) annotation (Line(points={{28,144},{18,144},{18,170},
          {-112,170}}, color={0,0,127}));
  connect(pumCon.y, max1.u1) annotation (Line(points={{15,116},{18,116},{18,132},
          {28,132},{28,132}}, color={0,0,127}));
  connect(max4.u1, pumEva.y) annotation (Line(points={{42,-28},{38,-28},{38,-42},
          {25,-42}}, color={0,0,127}));
  connect(cooTanMin, max4.u2)
    annotation (Line(points={{-110,-16},{42,-16}}, color={0,0,127}));
  connect(pumEva.u_s, max3.y) annotation (Line(points={{2,-42},{-12,-42},{-12,-32},
          {-26,-32}}, color={0,0,127}));
  connect(max3.u2, mSecCooNor.y) annotation (Line(points={{-50,-26},{-58,-26},{-58,
          -30},{-65,-30}}, color={0,0,127}));
  connect(max3.u1, minEvaFloNor.y) annotation (Line(points={{-50,-38},{-56,-38},
          {-56,-50},{-63,-50}}, color={0,0,127}));
  annotation (defaultComponentName="pumCon",Icon(coordinateSystem(preserveAspectRatio=false, extent={{-100,
            -100},{100,100}})),                                  Diagram(
        coordinateSystem(preserveAspectRatio=false, extent={{-100,-100},{200,180}}),
        graphics={
        Rectangle(
          extent={{-100,180},{200,48}},
          lineColor={135,135,135},
          lineThickness=0.5,
          fillColor={223,223,223},
          fillPattern=FillPattern.Solid),
        Text(
          extent={{90,178},{194,170}},
          lineColor={0,0,0},
          lineThickness=0.5,
          fillColor={223,223,223},
          fillPattern=FillPattern.Solid,
          textStyle={TextStyle.Bold},
          textString="Condenser side water pump"),
        Rectangle(
          extent={{-100,0},{198,-102}},
          lineColor={255,0,255},
          pattern=LinePattern.Dot,
          fillColor={189,223,202},
          fillPattern=FillPattern.Solid),
        Text(
          extent={{92,-88},{196,-96}},
          lineColor={0,0,0},
          lineThickness=0.5,
          fillColor={223,223,223},
          fillPattern=FillPattern.Solid,
          textStyle={TextStyle.Bold},
          textString="Evaporator side water pump")}),
          Documentation(info="<html>
<p> 
The applied pumping configuration is variable flow primary-secondary systems. The primary
<code>pumEva</code> and <code>pumCon</code> pumps are interlocked and controlled to satisfiy 
the thermal requirments at the building(secondary) side considering the following
</p>
<ul>
<li>
The mass flow rate of the primary pumps is 10% higher than the secondary pumps to avoid 
the cross circulation through the hot and cold buffer tanks form the secondary side i.e. the return water 
from the secondary side is mixed with the supply water.

<p align=\"center\">
<img alt=\"Image BufferTankCrossCirculation\"
src=\"modelica://Buildings/Resources/Images/Applications/DHC/EnergyTransferStations/BufferTankCrossCirculation.png\"/>
</p>  
</li>
<li>
Maintain the heat pump flow rate between the minimum and maximum limit of the heat pump as advised by the manufacturer.
</li>
<li>
Maintain the hydraulic balance between the primary <code>pumEva</code>, <code>pumCon</code> 
, borefield pump <code>pumBor</code> and distrcit heat exchanger pump <code>pumDis</code> 
once the system is switched to reject heat to district network mode.
</li>
</ul>
<p>
The PI reverse action loop outputs the evaporator and condesner sides pumps rotating speed, taking
real inputs of primary and secondary flow rates. Whilest, the Boolean inputs <code>reqHea</code> and 
<code>reqCoo</code> are to turn on/off the pumps.
</p>

</html>", revisions="<html>
<ul>
<li>
 <br/>
</li>
</ul>
</html>"));
end EvaporatorCondenserPumpsController;
