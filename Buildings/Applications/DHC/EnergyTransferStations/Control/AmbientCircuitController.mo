within Buildings.Applications.DHC.EnergyTransferStations.Control;
model AmbientCircuitController
  "Generate control outputs for the ambient circuit model"
  extends Modelica.Blocks.Icons.Block;

  parameter Modelica.SIunits.MassFlowRate mGeo_flow_nominal
    "Nominal mass flow rate of geothermal exchanger";

  parameter Modelica.SIunits.MassFlowRate mHex_flow_nominal
    "Nominal mass flow rate of circulation pump";
  parameter Modelica.SIunits.TemperatureDifference dTGeo
    "Temperature difference in and out of borefield";
  parameter Modelica.SIunits.TemperatureDifference dTHex
    "Temperature difference in and out of substation heat exchanger";
  parameter Modelica.Blocks.Types.SimpleController
    controllerType=Modelica.Blocks.Types.SimpleController.PI "Type of controller"
    annotation (Dialog(group="PID controller"));
  parameter Real k(final unit="1/K")=0.1
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
  parameter Real yMin = 0.1 "Minimum control output"
    annotation (Dialog(group="PID controller"));

  Modelica.Blocks.Interfaces.RealInput TBorOut(final unit="K")
    "Water temperature at borfield outlet" annotation (Placement(transformation(
          extent={{-978,38},{-938,78}}), iconTransformation(extent={{-120,-110},
            {-100,-90}})));
  Modelica.Blocks.Interfaces.RealInput TAmbRetHed(final unit="K")
    "Ambient header return water temperature "
    annotation (Placement(transformation(extent={{-978,-212},{-938,-172}}), iconTransformation(extent={{-120,88},{
            -100,108}})));
  Modelica.Blocks.Interfaces.RealInput TAmbSupHed(final unit="K")
    "Ambient header supply water temperature " annotation (Placement(
        transformation(extent={{-978,-248},{-938,-208}}), iconTransformation(
          extent={{-120,-86},{-100,-66}})));
  Modelica.Blocks.Interfaces.BooleanInput valHea "Heating load side valve control"
    annotation (Placement(transformation(extent={{-978,218},{-938,258}}),
        iconTransformation(extent={{-120,36},{-100,56}})));
  Modelica.Blocks.Interfaces.BooleanInput valCoo "Cooling load side valve control"
    annotation (Placement(transformation(extent={{-978,172},{-938,212}}),
        iconTransformation(extent={{-120,-40},{-100,-20}})));
  Modelica.Blocks.Interfaces.BooleanInput reqHea "Heating loads are required"
                                                                   annotation (
      Placement(transformation(extent={{-978,138},{-938,178}}),
        iconTransformation(extent={{-120,62},{-100,82}})));
  Modelica.Blocks.Interfaces.BooleanInput reqCoo "Cooling loads are required "
                                                                  annotation (
      Placement(transformation(extent={{-978,106},{-938,146}}),
        iconTransformation(extent={{-120,-64},{-100,-44}})));
  Modelica.Blocks.Interfaces.BooleanInput rejCooFulLoa
    "true if hot side requires cold rejection with borefield and district heat exchaner"
    annotation (Placement(transformation(extent={{-978,-172},{-938,-132}}),
        iconTransformation(extent={{-120,-14},{-100,6}})));
  Modelica.Blocks.Interfaces.BooleanInput rejHeaFulLoa
    "true if hot side requires heat rejection with borefield and district heat exchaner"
    annotation (Placement(transformation(extent={{-978,-112},{-938,-72}}),
        iconTransformation(extent={{-120,10},{-100,30}})));
  Modelica.Blocks.Interfaces.IntegerOutput ModInd "Mode index" annotation (Placement(transformation(extent={{-498,
            -62},{-478,-42}}),
                         iconTransformation(extent={{100,-10},{120,10}})));
  Modelica.Blocks.Interfaces.RealOutput pumHexDis(final unit="1") "Heat exchanger district system pump control"
    annotation (Placement(transformation(extent={{-498,-190},{-478,-170}}),
                                                                          iconTransformation(extent={{100,-40},{120,
            -20}})));
  Modelica.Blocks.Interfaces.RealOutput pumBor(final unit="1") "Borefield pump control signal" annotation (
      Placement(transformation(extent={{-498,38},{-478,58}}),
                                                            iconTransformation(extent={{100,-90},{120,-70}})));

  Buildings.Controls.OBC.CDL.Logical.Or or3
    annotation (Placement(transformation(extent={{-758,106},{-738,126}})));
  Buildings.Controls.Continuous.LimPID hexPumCon(
    final k=k,
    final Ti=Ti,
    final Td=Td,
    reset=Buildings.Types.Reset.Parameter,
    y_reset=yMin,
    reverseAction=true,
    yMin=yMin,
    final controllerType=controllerType) "Heat exchanger pump control"
    annotation (Placement(transformation(extent={{-738,-182},{-718,-162}})));
  Buildings.Controls.OBC.CDL.Continuous.Add add4(k2=-1)
    annotation (Placement(transformation(extent={{-878,-232},{-858,-212}})));
  Buildings.Controls.OBC.CDL.Continuous.Abs abs3
    annotation (Placement(transformation(extent={{-838,-232},{-818,-212}})));
  Buildings.Controls.OBC.CDL.Continuous.Sources.Constant con3(k=dTHex)
    annotation (Placement(transformation(extent={{-798,-182},{-778,-162}})));
  Buildings.Controls.OBC.CDL.Continuous.Sources.Constant con4(k=0)
    annotation (Placement(transformation(extent={{-698,-122},{-678,-102}})));
  Buildings.Controls.OBC.CDL.Logical.Switch hexPumConOut "Hex pump control"
    annotation (Placement(transformation(extent={{-618,-190},{-598,-170}})));
  Buildings.Controls.OBC.CDL.Logical.Switch heaMod "Mode index"
    annotation (Placement(transformation(extent={{-596,-62},{-576,-42}})));
  Buildings.Controls.OBC.CDL.Continuous.Sources.Constant con2(k=1)
    annotation (Placement(transformation(extent={{-638,-34},{-618,-14}})));
  Buildings.Controls.OBC.CDL.Logical.Switch cooModInd "Cooling mode index"
    annotation (Placement(transformation(extent={{-636,-92},{-616,-72}})));
  Buildings.Controls.OBC.CDL.Continuous.Sources.Constant con5(k=-1)
    annotation (Placement(transformation(extent={{-698,-34},{-678,-14}})));
  Buildings.Controls.OBC.CDL.Conversions.RealToInteger reaToInt
    annotation (Placement(transformation(extent={{-540,-62},{-520,-42}})));
  Buildings.Controls.OBC.CDL.Logical.Or valOpe
    "True if hot or cold two way valve is open "
    annotation (Placement(transformation(extent={{-898,218},{-878,238}})));
  Buildings.Controls.OBC.CDL.Logical.Or3 runHex
    "Output true if the heat exchanger of the substation needs to run"
    annotation (Placement(transformation(extent={{-898,-142},{-878,-122}})));
  Buildings.Controls.OBC.CDL.Logical.Not noHex
    annotation (Placement(transformation(extent={{-778,-216},{-758,-196}})));
  Buildings.Controls.OBC.CDL.Logical.Not not1
    annotation (Placement(transformation(extent={{-858,160},{-838,180}})));
  Buildings.Controls.OBC.CDL.Logical.And runHexOnly
    "Output true if heating and cooling are not required while, one of the two way valves are open"
    annotation (Placement(transformation(extent={{-810,168},{-790,188}})));
  Buildings.Controls.OBC.CDL.Logical.Or3 runBorFie
    "Output true if borefield system pump must run"
    annotation (Placement(transformation(extent={{-678,98},{-658,118}})));
  Buildings.Controls.OBC.CDL.Continuous.Add add1(k2=-1)
    annotation (Placement(transformation(extent={{-764,6},{-744,26}})));
  Buildings.Controls.OBC.CDL.Continuous.Abs abs1
    annotation (Placement(transformation(extent={{-722,6},{-702,26}})));
  Buildings.Controls.OBC.CDL.Continuous.Sources.Constant con1(k=dTGeo)
    "Temperature difference between the borefield inlet and outlets "
    annotation (Placement(transformation(extent={{-698,48},{-678,68}})));
  Buildings.Controls.Continuous.LimPID geoPumCon(
    final Td=Td,
    reset=Buildings.Types.Reset.Parameter,
    reverseAction=true,
    y_reset=0,
    final k=1,
    yMin=0.5,
    final controllerType=controllerType,
    Ti(displayUnit="min") = 3600)
    "Geothermal pump control"
    annotation (Placement(transformation(extent={{-638,30},{-618,50}})));
  Buildings.Controls.OBC.CDL.Logical.Not not2
    annotation (Placement(transformation(extent={{-636,98},{-616,118}})));
  Buildings.Controls.OBC.CDL.Logical.Or runFulLoa
    "Output true if geothermal need to run at full load to reject heat"
    annotation (Placement(transformation(extent={{-898,-102},{-878,-82}})));
  Buildings.Controls.OBC.CDL.Logical.Switch modInd3 "Mode index"
    annotation (Placement(transformation(extent={{-578,38},{-558,58}})));
  Buildings.Controls.OBC.CDL.Logical.Switch runBorPum
    "Switch that enable borefield pump"
    annotation (Placement(transformation(extent={{-524,38},{-504,58}})));

  Buildings.Controls.OBC.CDL.Logical.Or heaOrCoo "True if heating or cooling loads are required"
    annotation (Placement(transformation(extent={{-898,138},{-878,158}})));
  Buildings.Controls.OBC.CDL.Logical.MultiAnd opeCoo(nu=2)
    "Operate to deliver cooling loads and reject heat from hot tank"
    annotation (Placement(transformation(extent={{-898,178},{-878,198}})));
  Buildings.Controls.OBC.CDL.Logical.MultiAnd opeHea(nu=2)
    "Operate to deliver heating loads and reject cold water from cold tank"
    annotation (Placement(transformation(extent={{-898,98},{-878,118}})));
  Modelica.Blocks.Interfaces.BooleanInput valHea1
                                                 "Heating load side valve control"
    annotation (Placement(transformation(extent={{-246,54},{-220,80}}),
        iconTransformation(extent={{-120,36},{-100,56}})));
  Modelica.Blocks.Interfaces.BooleanInput valCoo1
                                                 "Cooling load side valve control"
    annotation (Placement(transformation(extent={{-244,140},{-220,164}}),
        iconTransformation(extent={{-120,-40},{-100,-20}})));
  Modelica.Blocks.Interfaces.BooleanInput rejHeaFulLoa1
    "true if hot side requires heat rejection with borefield and district heat exchaner"
    annotation (Placement(transformation(extent={{-260,-138},{-220,-98}}),
        iconTransformation(extent={{-120,10},{-100,30}})));
  Modelica.Blocks.Interfaces.BooleanInput rejCooFulLoa1
    "true if hot side requires cold rejection with borefield and district heat exchaner"
    annotation (Placement(transformation(extent={{-260,-198},{-220,-158}}),
        iconTransformation(extent={{-120,-14},{-100,6}})));
  Modelica.Blocks.Interfaces.RealInput TAmbRetHed1(final unit="K")
    "Ambient header return water temperature "
    annotation (Placement(transformation(extent={{-260,-238},{-220,-198}}), iconTransformation(extent={{-120,88},{
            -100,108}})));
  Modelica.Blocks.Interfaces.RealInput THXOut(final unit="K")
    "District Heat Exchanger leaving temperature" annotation (Placement(
        transformation(extent={{-260,-274},{-220,-234}}), iconTransformation(
          extent={{-120,-86},{-100,-66}})));
  Modelica.Blocks.Interfaces.RealInput TBorOut1(final unit="K")
    "Water temperature at borfield outlet" annotation (Placement(transformation(
          extent={{-260,-60},{-220,-20}}),
                                         iconTransformation(extent={{-120,-110},
            {-100,-90}})));
  Buildings.Controls.OBC.CDL.Logical.Switch swi1
    annotation (Placement(transformation(extent={{-80,138},{-60,158}})));
  Buildings.Controls.OBC.CDL.Logical.Switch swi2
    annotation (Placement(transformation(extent={{-114,10},{-94,30}})));
  Buildings.Controls.OBC.CDL.Interfaces.RealInput yPumEva
    "Evaporator pump  speed inlet signal" annotation (Placement(transformation(
          extent={{-252,164},{-220,196}}), iconTransformation(extent={{100,68},
            {120,88}})));
  Buildings.Controls.OBC.CDL.Interfaces.BooleanInput           ReqHea
    "Heating is required Boolean signal"
    annotation (Placement(transformation(extent={{-246,106},{-218,134}}),
        iconTransformation(extent={{-128,76},{-100,104}})));
  Buildings.Controls.OBC.CDL.Interfaces.BooleanInput           ReqCoo
  "Cooling is required Boolean signal"
    annotation (Placement(transformation(extent={{-248,26},{-220,54}}),
        iconTransformation(extent={{-128,-104},{-100,-76}})));
  Modelica.Blocks.Logical.And and1
    annotation (Placement(transformation(extent={{-198,130},{-178,150}})));
  Buildings.Controls.OBC.CDL.Interfaces.RealInput yPumCon
    "Condenser pump  speed inlet signal" annotation (Placement(transformation(
          extent={{-252,76},{-220,108}}), iconTransformation(extent={{100,68},{
            120,88}})));
  Modelica.Blocks.Interfaces.RealOutput pumBor1(final unit="1")
                                                               "Borefield pump control signal" annotation (
      Placement(transformation(extent={{220,138},{240,158}}),
                                                            iconTransformation(extent={{100,-90},{120,-70}})));
  Modelica.Blocks.Logical.And and2
    annotation (Placement(transformation(extent={{-202,48},{-182,68}})));
  Buildings.Controls.OBC.CDL.Logical.Switch swi3
    annotation (Placement(transformation(extent={{-140,48},{-120,68}})));
equation
  connect(add4.y, abs3.u)
    annotation (Line(points={{-856,-222},{-840,-222}}, color={0,0,127}));
  connect(abs3.y,hexPumCon. u_m)
   annotation (Line(points={{-816,-222},{-728,-222},{-728,-184}},
                       color={0,0,127}));
  connect(con3.y,hexPumCon. u_s)
    annotation (Line(points={{-776,-172},{-740,-172}},
                                                     color={0,0,127}));
  connect(TAmbRetHed, add4.u1)
    annotation (Line(points={{-958,-192},{-918,-192},{-918,-216},{-880,-216}}, color={0,0,127}));
  connect(TAmbSupHed, add4.u2)
    annotation (Line(points={{-958,-228},{-880,-228}}, color={0,0,127}));
  connect(con2.y,heaMod. u1)
    annotation (Line(points={{-616,-24},{-608,-24},{-608,-44},{-598,-44}},
                                                  color={0,0,127}));
  connect(con5.y, cooModInd.u1) annotation (Line(points={{-676,-24},{-648,-24},{
          -648,-74},{-638,-74}},
                          color={0,0,127}));
  connect(con4.y, cooModInd.u3) annotation (Line(points={{-676,-112},{-658,-112},
          {-658,-90},{-638,-90}},
                             color={0,0,127}));
  connect(reaToInt.y, ModInd) annotation (Line(points={{-518,-52},{-488,-52}},
                                                                             color={255,127,0}));
  connect(noHex.y,hexPumCon. trigger) annotation (Line(points={{-756,-206},{-736,
          -206},{-736,-184}},                      color={255,0,255}));
  connect(hexPumConOut.u3, con4.y) annotation (Line(points={{-620,-188},{-658,-188},
          {-658,-112},{-676,-112}},
                                color={0,0,127}));
  connect(TAmbRetHed, add1.u2)
    annotation (Line(points={{-958,-192},{-858,-192},{-858,10},{-766,10}},
                                                                        color={0,0,127}));
  connect(TBorOut, add1.u1) annotation (Line(points={{-958,58},{-858,58},{-858,
          22},{-766,22}},color={0,0,127}));
  connect(add1.y, abs1.u)
    annotation (Line(points={{-742,16},{-724,16}},
                                                color={0,0,127}));
  connect(con1.y, geoPumCon.u_s)
    annotation (Line(points={{-676,58},{-652,58},{-652,40},{-640,40}},
                                                color={0,0,127}));
  connect(abs1.y, geoPumCon.u_m)
    annotation (Line(points={{-700,16},{-628,16},{-628,28}},
                                                       color={0,0,127}));
  connect(geoPumCon.y, modInd3.u3)
    annotation (Line(points={{-617,40},{-580,40}},
                                                 color={0,0,127}));
  connect(modInd3.u1, con2.y) annotation (Line(points={{-580,56},{-594,56},{-594,
          -24},{-616,-24}},color={0,0,127}));
  connect(hexPumConOut.u1, hexPumCon.y)
    annotation (Line(points={{-620,-172},{-717,-172}},
                                                  color={0,0,127}));
  connect(hexPumConOut.y, pumHexDis) annotation (Line(points={{-596,-180},{-488,
          -180}},                                                                     color={0,0,127}));
  connect(runBorPum.y,pumBor)  annotation (Line(points={{-502,48},{-488,48}},
                                                                            color={0,0,127}));
  connect(modInd3.y, runBorPum.u1) annotation (Line(points={{-556,48},{-544,48},
          {-544,56},{-526,56}},
                             color={0,0,127}));
  connect(con4.y, runBorPum.u3) annotation (Line(points={{-676,-112},{-658,-112},
          {-658,4},{-544,4},{-544,40},{-526,40}},
                                            color={0,0,127}));
  connect(valHea, valOpe.u1) annotation (Line(points={{-958,238},{-924,238},{
          -924,228},{-900,228}}, color={255,0,255}));
  connect(valCoo, valOpe.u2) annotation (Line(points={{-958,192},{-922,192},{
          -922,220},{-900,220}}, color={255,0,255}));
  connect(valOpe.y, runHexOnly.u1) annotation (Line(points={{-876,228},{-826,228},
          {-826,178},{-812,178}},      color={255,0,255}));
  connect(reqHea, heaOrCoo.u1)
    annotation (Line(points={{-958,158},{-916,158},{-916,148},{-900,148}}, color={255,0,255}));
  connect(reqCoo, heaOrCoo.u2)
    annotation (Line(points={{-958,126},{-916,126},{-916,140},{-900,140}}, color={255,0,255}));
  connect(valOpe.y, runBorPum.u2) annotation (Line(points={{-876,228},{-534,228},
          {-534,48},{-526,48}},
                              color={255,0,255}));
  connect(rejHeaFulLoa, runFulLoa.u1)
    annotation (Line(points={{-958,-92},{-900,-92}}, color={255,0,255}));
  connect(rejCooFulLoa, runFulLoa.u2) annotation (Line(points={{-958,-152},{
          -918,-152},{-918,-100},{-900,-100}},
                                             color={255,0,255}));
  connect(runHex.y, noHex.u) annotation (Line(points={{-876,-132},{-818,-132},{-818,
          -206},{-780,-206}},     color={255,0,255}));
  connect(rejCooFulLoa, runHex.u3) annotation (Line(points={{-958,-152},{-908,
          -152},{-908,-140},{-900,-140}}, color={255,0,255}));
  connect(rejHeaFulLoa, runHex.u2) annotation (Line(points={{-958,-92},{-924,
          -92},{-924,-132},{-900,-132}}, color={255,0,255}));
  connect(runHexOnly.u2, not1.y) annotation (Line(points={{-812,170},{-836,170}},
                                  color={255,0,255}));
  connect(heaOrCoo.y, not1.u)
    annotation (Line(points={{-876,148},{-870,148},{-870,170},{-860,170}}, color={255,0,255}));
  connect(valHea, opeCoo.u[1]) annotation (Line(points={{-958,238},{-912,238},{
          -912,191.5},{-900,191.5}}, color={255,0,255}));
  connect(reqCoo, opeCoo.u[2]) annotation (Line(points={{-958,126},{-912,126},{
          -912,184.5},{-900,184.5}}, color={255,0,255}));
  connect(valCoo, opeHea.u[1])
    annotation (Line(points={{-958,192},{-922,192},{-922,111.5},{-900,111.5}}, color={255,0,255}));
  connect(reqHea, opeHea.u[2])
    annotation (Line(points={{-958,158},{-926,158},{-926,104.5},{-900,104.5}}, color={255,0,255}));
  connect(opeHea.y, or3.u2) annotation (Line(points={{-876,108},{-760,108}},  color={255,0,255}));
  connect(opeCoo.y, or3.u1) annotation (Line(points={{-876,188},{-864,188},{
          -864,116},{-760,116}},                    color={255,0,255}));
  connect(runBorFie.y, not2.u)
    annotation (Line(points={{-656,108},{-638,108}},
                                                   color={255,0,255}));
  connect(runFulLoa.y, modInd3.u2) annotation (Line(points={{-876,-92},{-826,-92},
          {-826,-4},{-606,-4},{-606,48},{-580,48}},   color={255,0,255}));
  connect(runHexOnly.y, runHex.u1) annotation (Line(points={{-788,178},{-778,178},
          {-778,-112},{-912,-112},{-912,-124},{-900,-124}},color={255,0,255}));
  connect(runHex.y, hexPumConOut.u2) annotation (Line(points={{-876,-132},{-696,
          -132},{-696,-180},{-620,-180}},
                                      color={255,0,255}));
  connect(cooModInd.y, heaMod.u3) annotation (Line(points={{-614,-82},{-604,-82},
          {-604,-60},{-598,-60}},                 color={0,0,127}));
  connect(or3.y, runBorFie.u1) annotation (Line(points={{-736,116},{-680,116}},
                                                                             color={255,0,255}));
  connect(rejHeaFulLoa, runBorFie.u2) annotation (Line(points={{-958,-92},{-932,
          -92},{-932,94},{-704,94},{-704,108},{-680,108}},
                 color={255,0,255}));
  connect(rejCooFulLoa, runBorFie.u3) annotation (Line(points={{-958,-152},{
          -928,-152},{-928,90},{-700,90},{-700,100},{-680,100}},
                              color={255,0,255}));
  connect(not2.y, geoPumCon.trigger) annotation (Line(points={{-614,108},{-612,108},
          {-612,78},{-646,78},{-646,22},{-636,22},{-636,28}},
                    color={255,0,255}));
  connect(heaMod.y, reaToInt.u) annotation (Line(points={{-574,-52},{-542,-52}},
                                                                               color={0,0,127}));
  connect(rejHeaFulLoa, heaMod.u2) annotation (Line(points={{-958,-92},{-924,
          -92},{-924,-54},{-890,-54},{-890,-52},{-598,-52}},color={255,0,255}));
  connect(rejCooFulLoa, cooModInd.u2) annotation (Line(points={{-958,-152},{
          -738,-152},{-738,-82},{-638,-82}},
                                     color={255,0,255}));
  connect(yPumEva, swi1.u1) annotation (Line(points={{-236,180},{-160,180},{
          -160,156},{-82,156}}, color={0,0,127}));
  connect(swi1.u2, and1.y) annotation (Line(points={{-82,148},{-158,148},{-158,
          140},{-177,140}}, color={255,0,255}));
  connect(and1.u2, ReqHea) annotation (Line(points={{-200,132},{-206,132},{-206,
          120},{-232,120}}, color={255,0,255}));
  connect(valCoo1, and1.u1) annotation (Line(points={{-232,152},{-206,152},{
          -206,140},{-200,140}}, color={255,0,255}));
  connect(swi1.y, pumBor1)
    annotation (Line(points={{-58,148},{230,148}}, color={0,0,127}));
  connect(valHea1, and2.u1) annotation (Line(points={{-233,67},{-214,67},{-214,
          58},{-204,58}}, color={255,0,255}));
  connect(ReqCoo, and2.u2) annotation (Line(points={{-234,40},{-212,40},{-212,
          50},{-204,50}}, color={255,0,255}));
  connect(yPumCon, swi3.u1) annotation (Line(points={{-236,92},{-158,92},{-158,
          66},{-142,66}}, color={0,0,127}));
  connect(and2.y, swi3.u2)
    annotation (Line(points={{-181,58},{-142,58}}, color={255,0,255}));
  connect(swi3.y, swi1.u3) annotation (Line(points={{-118,58},{-110,58},{-110,
          140},{-82,140}}, color={0,0,127}));
  annotation (Diagram(
        coordinateSystem(                           extent={{-220,-260},{220,
            200}}, preserveAspectRatio=false),
        graphics={Text(
          extent={{-484,100},{-362,88}},
          lineColor={28,108,200},
          textString="PI with large time constant
because of long time constant
of borefield.
yMin=0.5 to stay turbulent")}), defaultComponentName="AmbCirCon",
 Documentation(info="<html>
<h4> Ambient circuit controller theory of operation </h4>
<p>
This block computes the output signals to turn on and off the borefield and heat exchanger district circuit pumps, and also
it computes the output integer <code>ModInd</code> which indicates the energy rejection index, i.e. heating or cooling energy is rejected.
</p>
<p>
The controller includes three operational modes:
</p>
<h4>Reject to borefield system only</h4>
<p>
The controller computes the real signal <code>borPum</code> to turn on and off the pump,
based on the boolean signals of the two way valve status <code>valHea</code> or <code>valCoo</code>.
The controller also modulates the pump <code>pumBor</code> speed, using a PI controller with
the setpoint temperature difference of <code>dTGeo</code> which is the temperature difference between the outlet water from the borefield <code>TBorOut</code>
and the return header water temperature <code>TAmbRetHed</code>.
<h4>Reject to both the district heat exchanger and borefield systems</h4>
The controller turns on heat exchanger district pump<code>pumHexDis</code> if one of the boolean signals  <code>rejHeaFulLoa</code>
reject full heating load or  <code>rejCooFulLoa</code> reject full cooling load is true. Accordingly, the PI controller modulates
the <code>pumHexDis</code> pump speed using the temperature difference between ambient supply header <code>TAmbSupHed</code> and ambient return header
 <code>TAmbRetHed</code> as the setpoint <code>dTHex</code>.
<h4>Reject to the district heat exchanger only</h4>
The controller tracks when heating and cooling are not required and one of the two way valves is open. i.e. when the
input boolean signal <code>reqHeaFulLoa</code>
and <code>rejCooFulLoa</code> are false and <code>valHea</code> or <code>valCoo</code> is true,
the controller computes the input signal to turn on the <code>pumHexDis</CODE> using the PI controller which
modulates the <code>pumHexDis</code> pump speed.
<p>
see <a href=\"Buildings.DistrictHeatingCooling.EnergyTransferStations.ETSControl.SubstationUO\">
Buildings.DistrictHeatingCooling.EnergyTransferStations.ETSControl.SubstationUO</a>
for more detailed description regarding the energy rejection control into borefield and district systems.
</p>
<h4> The energy rejection mode index </h4>
<p>
The controller computes the energy rejection <code>ModInd</code> to the borfield and district heat exchanger system
i.e.<code>ModInd</code> =1 heating load is rejected from the hot buffer tank, <code>ModInd</code> =-1 cooling load is rejected from the cold buffer tank.
</p>
<p>
The controller determines if both the heating is required <code>reqHea</code> and the cooling side two way valve is open <code>cooVal</code>, this indicates that rejection
occurs from the cold tank to first the borefield followed by the district heat exchanger, hence <code>ModInd</code>=1.
</p>
<p>
While, if both cooling is required <code>reqCoo</code> and the heating side two way valve  <code>HeaVal</code> is open, this indicates that rejection occurs from the hot tank
to first the borefield , followed by the district heat exchanger. The controller then computes <code>ModInd</code>=-1
</p>
</html>", revisions="<html>
<ul>
<li>
 <br/>
</li>
</ul>
</html>"),
    Icon(coordinateSystem(extent={{-100,-100},{100,100}}, initialScale=1)));
end AmbientCircuitController;
