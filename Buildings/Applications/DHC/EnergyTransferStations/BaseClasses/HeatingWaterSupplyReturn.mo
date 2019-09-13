within Buildings.Applications.DHC.EnergyTransferStations.BaseClasses;
model HeatingWaterSupplyReturn

  parameter Modelica.SIunits.Temperature THeaWatSup_nominal=314.15
    "Heating water supply temperature";
  parameter Modelica.SIunits.Temperature THeaWatRet_nominal=303.15
    "Return temperature";
  parameter Modelica.SIunits.Temperature THeaWatOut_nominal=263.15
    "Outside temperature";
  parameter Modelica.SIunits.Temperature THeaWatRoo_nominal=293.15
    "Nominal room temperature when supply heat";
  parameter Modelica.SIunits.TemperatureDifference dTOutHeaBal=5
    "Offset for heating curve";
  parameter Modelica.SIunits.TemperatureDifference dT_min=4
    "Minimum supply and return temperature difference";

  Modelica.Blocks.Interfaces.RealInput TOut "Outdoor drybulb temperature"
    annotation (Placement(transformation(extent={{-200,-20},{-160,20}}),
        iconTransformation(extent={{-140,-20},{-100,20}})));
  Modelica.Blocks.Interfaces.RealOutput TSup(final unit="K")
    "Heating supply water temperature"
    annotation (Placement(transformation(extent={{160,40},{180,60}}),
      iconTransformation(extent={{100,40},{120,60}})));
  Modelica.Blocks.Interfaces.RealOutput TRet(final unit="K")
    "Heating return water temperature" annotation (Placement(transformation(
          extent={{160,-70},{180,-50}}), iconTransformation(extent={{100,-60},{120,
            -40}})));

  Buildings.Controls.SetPoints.HotWaterTemperatureReset hotWatRes(
    m=1,
    TSup_nominal=THeaWatSup_nominal,
    TRet_nominal=THeaWatRet_nominal,
    TRoo_nominal=THeaWatRoo_nominal,
    TOut_nominal=THeaWatOut_nominal,
    dTOutHeaBal=dTOutHeaBal) "Heat supply and return water temperature reset"
    annotation (Placement(transformation(extent={{-120,-16},{-100,4}})));
  Modelica.Blocks.Logical.Switch switch1
    annotation (Placement(transformation(extent={{-60,20},{-40,40}})));
  Modelica.Blocks.Sources.Constant const(k=THeaWatSup_nominal)
    annotation (Placement(transformation(extent={{-120,60},{-100,80}})));
  Modelica.Blocks.Math.Add add(k2=-1)
    annotation (Placement(transformation(extent={{0,0},{20,20}})));
  Modelica.Blocks.Logical.Switch switch2
    annotation (Placement(transformation(extent={{-60,-70},{-40,-50}})));
  Modelica.Blocks.Sources.Constant const2(k=THeaWatRet_nominal)
    annotation (Placement(transformation(extent={{-120,-62},{-100,-42}})));
  Modelica.Blocks.Math.Add add1
    annotation (Placement(transformation(extent={{60,-40},{80,-20}})));
  Modelica.Blocks.Logical.Switch switch3
    annotation (Placement(transformation(extent={{120,40},{140,60}})));
  Modelica.Blocks.Sources.Constant const1(k=dT_min)
    "Minimim supply and return temperature difference"
    annotation (Placement(transformation(extent={{0,-40},{20,-20}})));

  Modelica.Blocks.Sources.Constant const3(k=THeaWatOut_nominal)
    annotation (Placement(transformation(extent={{-160,-60},{-140,-40}})));
  Modelica.Blocks.Sources.Constant const4(k=dT_min)
    annotation (Placement(transformation(extent={{-60,-20},{-40,0}})));
  Buildings.Controls.OBC.CDL.Continuous.Hysteresis hys(uLow=-0.1, uHigh=0.1)
    annotation (Placement(transformation(extent={{-110,20},{-90,40}})));
  Buildings.Controls.OBC.CDL.Continuous.Add add2(k1=-1)
    annotation (Placement(transformation(extent={{-144,20},{-124,40}})));
  Buildings.Controls.OBC.CDL.Continuous.Add add3(k1=-1)
    annotation (Placement(transformation(extent={{40,0},{60,20}})));
  Buildings.Controls.OBC.CDL.Continuous.Hysteresis hys1(uLow=-0.1, uHigh=0.1)
    annotation (Placement(transformation(extent={{70,0},{90,20}})));
equation
  connect(TOut, hotWatRes.TOut)
    annotation (Line(points={{-180,0},{-122,0}}, color={0,0,127}));
  connect(const.y, switch1.u1) annotation (Line(points={{-99,70},{-80,70},{-80,
          38},{-62,38}},
                     color={0,0,127}));
  connect(const2.y, switch2.u1) annotation (Line(points={{-99,-52},{-62,-52}},
                           color={0,0,127}));
  connect(hotWatRes.TSup, switch1.u3) annotation (Line(points={{-99,0},{-70,0},{
          -70,22},{-62,22}}, color={0,0,127}));
  connect(hotWatRes.TRet, switch2.u3) annotation (Line(points={{-99,-12},{-70,-12},
          {-70,-68},{-62,-68}}, color={0,0,127}));
  connect(switch1.y, add.u1) annotation (Line(points={{-39,30},{-20,30},{-20,16},
          {-2,16}}, color={0,0,127}));
  connect(switch2.y, add.u2) annotation (Line(points={{-39,-60},{-20,-60},{-20,4},
          {-2,4}}, color={0,0,127}));
  connect(switch1.y, switch3.u3) annotation (Line(points={{-39,30},{-20,30},{
          -20,42},{118,42}},
                        color={0,0,127}));
  connect(switch2.y, add1.u2) annotation (Line(points={{-39,-60},{40,-60},{40,-36},
          {58,-36}}, color={0,0,127}));
  connect(const1.y, add1.u1) annotation (Line(points={{21,-30},{40,-30},{40,-24},
          {58,-24}}, color={0,0,127}));
  connect(add1.y, switch3.u1) annotation (Line(points={{81,-30},{110,-30},{110,
          58},{118,58}},
                    color={0,0,127}));
  connect(switch3.y, TSup)
    annotation (Line(points={{141,50},{170,50}}, color={0,0,127}));
  connect(switch2.y, TRet)
    annotation (Line(points={{-39,-60},{170,-60}}, color={0,0,127}));
  connect(TOut, add2.u1) annotation (Line(points={{-180,0},{-154,0},{-154,36},{
          -146,36}}, color={0,0,127}));
  connect(const3.y, add2.u2) annotation (Line(points={{-139,-50},{-136,-50},{
          -136,-20},{-152,-20},{-152,24},{-146,24}}, color={0,0,127}));
  connect(add2.y, hys.u)
    annotation (Line(points={{-123,30},{-112,30}}, color={0,0,127}));
  connect(hys.y, switch1.u2)
    annotation (Line(points={{-89,30},{-62,30}}, color={255,0,255}));
  connect(hys.y, switch2.u2) annotation (Line(points={{-89,30},{-80,30},{-80,
          -60},{-62,-60}}, color={255,0,255}));
  connect(const4.y, add3.u2) annotation (Line(points={{-39,-10},{28,-10},{28,4},
          {38,4}}, color={0,0,127}));
  connect(add.y, add3.u1) annotation (Line(points={{21,10},{28,10},{28,16},{38,
          16}}, color={0,0,127}));
  connect(add3.y, hys1.u)
    annotation (Line(points={{61,10},{68,10}}, color={0,0,127}));
  connect(hys1.y, switch3.u2) annotation (Line(points={{91,10},{100,10},{100,50},
          {118,50}}, color={255,0,255}));
  annotation (Icon(coordinateSystem(preserveAspectRatio=false), graphics={
                                Rectangle(
        extent={{-100,-100},{100,100}},
        lineColor={0,0,127},
        fillColor={255,255,255},
        fillPattern=FillPattern.Solid),
        Line(points={{-80,76},{-80,-92}}, color={192,192,192}),
        Polygon(
          points={{-80,88},{-88,66},{-72,66},{-80,86},{-80,88}},
          lineColor={192,192,192},
          fillColor={192,192,192},
          fillPattern=FillPattern.Solid),
        Line(
          points={{-80,-82},{-58,-42},{-4,8},{60,32}},
          smooth=Smooth.Bezier),
        Line(
          points={{-80,-82},{-42,-38},{4,2},{60,32}},
          smooth=Smooth.Bezier),
        Line(
          points={{-80,-82},{60,32}}),
        Text(
          extent={{40,86},{90,36}},
          lineColor={0,0,127},
          textString="TSup"),
        Text(
          extent={{42,-30},{92,-80}},
          lineColor={0,0,127},
          textString="TRet"),
        Line(points={{-90,-82},{82,-82}}, color={192,192,192}),
        Polygon(
          points={{90,-82},{68,-74},{68,-90},{90,-82}},
          lineColor={192,192,192},
          fillColor={192,192,192},
          fillPattern=FillPattern.Solid),
                                        Text(
        extent={{-150,150},{150,110}},
        textString="%name",
        lineColor={0,0,255})}), Diagram(coordinateSystem(preserveAspectRatio=false,
          extent={{-160,-100},{160,100}})));
end HeatingWaterSupplyReturn;
