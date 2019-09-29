within Buildings.Fluid.HeatPumps.BaseClasses;
block DOE2Reversible
  "DOE2 method to compute the performance of the reversable heat pump"
  extends Modelica.Blocks.Icons.Block;

  parameter  Buildings.Fluid.Chillers.Data.ElectricEIR.Generic per
   "Performance data"
     annotation (choicesAllMatching = true,Placement(transformation(
         extent={{80,80},{100,100}})));
  parameter Real scaling_factor
   "Scaling factor for heat pump capacity";
  parameter Modelica.SIunits.HeatFlowRate QHea_flow= (-per.QEva_flow_nominal)*(1 + 1/(per.COP_nominal))*scaling_factor
   "Nominal heating capacity at the load side";
  parameter Modelica.SIunits.HeatFlowRate QCoo_flow = per.QEva_flow_nominal*scaling_factor
    "Nominal cooling capacity at the load side";
  parameter Modelica.SIunits.HeatFlowRate Q_flow_small = QHea_flow * 1E-9 *scaling_factor
   "Small value for heat flow rate or power, used to avoid division by zero";

   Modelica.Blocks.Interfaces.RealInput TLoaLvg(
    final unit="K",
    displayUnit="degC")
   "Load entering fluid temperature"
    annotation (Placement(transformation(extent={{-124,10},{-100,34}}),
        iconTransformation(extent={{-120,20},{-100,40}})));
  Modelica.Blocks.Interfaces.RealInput TSouEnt(
    final unit="K",
    displayUnit="degC")
   "Source entering fluid temperature"
    annotation (Placement(transformation(extent={{-124,-52},{-100,-28}}),
        iconTransformation(extent={{-120,-50},{-100,-30}})));
  Modelica.Blocks.Interfaces.RealInput mLoa_flow(final unit="kg/s")
    "Mass flow rate at load side" annotation (Placement(transformation(extent={{-124,38},
            {-100,62}}),          iconTransformation(extent={{-120,50},{-100,70}})));

  Modelica.Blocks.Interfaces.RealInput mSou_flow(final unit="kg/s")
    "Mass flow rate at source side" annotation (Placement(transformation(extent={{-124,
            -92},{-100,-68}}),       iconTransformation(extent={{-120,-90},{
            -100,-70}})));

  Modelica.Blocks.Interfaces.RealInput Q_flow_set(final unit="W")
    "Required heat to meet set point" annotation (Placement(transformation(
          extent={{-124,68},{-100,92}}), iconTransformation(extent={{-120,80},{-100,
            100}})));

  Modelica.Blocks.Interfaces.RealOutput QLoa_flow(final unit="W")
   "Load heat flow rate"
    annotation (Placement(transformation(extent={{100,50},{120,70}}),
        iconTransformation(extent={{100,50},{120,70}})));

  Modelica.Blocks.Interfaces.RealOutput QSou_flow(final unit="W")
   "Source heat flow rate"
    annotation (Placement(transformation(extent={{100,-30},{120,-10}}),
        iconTransformation(extent={{100,-30},{120,-10}})));

  Modelica.Blocks.Interfaces.RealOutput P(final unit="W")
   "Compressor power"
    annotation (Placement(transformation(extent={{100,10},{120,30}}),
        iconTransformation(extent={{100,10},{120,30}})));

  Modelica.Blocks.Interfaces.RealOutput COP(
   final min=0,
   final unit="1")
   "Coefficient of performance, assuming useful heat is at the load side (at Medium 1)"
    annotation (Placement(transformation(extent={{100,-70},{120,-50}}),
    iconTransformation(extent={{100,-70},{120,-50}})));
  Modelica.Blocks.Interfaces.IntegerInput uMod
   "Control input signal, cooling mode= -1, off=0, heating mode=+1"
    annotation (Placement(transformation(extent={{-124,-12},{-100,12}}),
        iconTransformation(extent={{-120,-12},{-100,8}})));
  Modelica.SIunits.HeatFlowRate Q_flow_ava
   "Heating (or cooling) capacity available";

protected
  Modelica.SIunits.Efficiency EIRFT(nominal=1)
   "Power input to load side capacity ratio function of temperature curve";
  Modelica.SIunits.Efficiency EIRFPLR(nominal=1)
   "Power input to load side capacity ratio function of part load ratio";

  Real CapFT(min=0,nominal=1)
   "Load side capacity factor function of temperature curve";
  Real PLR1(min=0, nominal=1, unit="1")
   "Part load ratio";
  Real PLR2(min=0, nominal=1, unit="1")
   "Part load ratio";
  Real CR(min=0, nominal=1, unit="1")
   "Cycling ratio";

  Modelica.SIunits.Conversions.NonSIunits.Temperature_degC TSouEnt_degC
   "Condenser entering water temperature in degC";
  Modelica.SIunits.Conversions.NonSIunits.Temperature_degC TLoaLvg_degC
   "Evaporator leaving water temperature in degC";

initial equation
  assert(QHea_flow > 0,
  "Parameter QHea_flow must be larger than zero.");
  assert(QCoo_flow < 0,
  "Parameter QCoo_flow must be lesser than zero.");
  assert(Q_flow_small > 0,
  "Parameter Q_flow_small must be larger than zero.");
  assert(per.PLRMinUnl >= per.PLRMin,
  "Parameter PLRMinUnl must be bigger or equal to PLRMin");
  assert(per.PLRMax > per.PLRMinUnl,
  "Parameter PLRMax must be bigger than PLRMinUnl");

equation
  TSouEnt_degC=Modelica.SIunits.Conversions.to_degC(TSouEnt);
  TLoaLvg_degC=Modelica.SIunits.Conversions.to_degC(TLoaLvg);

  CapFT = Buildings.Utilities.Math.Functions.smoothMax(
       x1 = 1E-7,
       x2 = Buildings.Utilities.Math.Functions.biquadratic(
         a =  per.capFunT,
         x1 = TSouEnt_degC,
         x2 = TLoaLvg_degC),
       deltaX = 1E-7);
  EIRFT = Buildings.Utilities.Math.Functions.biquadratic(
        a =  per.EIRFunT,
        x1 = TSouEnt_degC,
        x2 = TLoaLvg_degC);
  EIRFPLR = per.EIRFunPLR[1]+per.EIRFunPLR[2]*PLR2+per.EIRFunPLR[3]*PLR2^2;
  PLR1 = Buildings.Utilities.Math.Functions.smoothMax(
      x1 =  Q_flow_set/(Q_flow_ava + Q_flow_small),
      x2 =  per.PLRMax,
      deltaX =  per.PLRMax/100);
  PLR2 = Buildings.Utilities.Math.Functions.smoothMax(
      x1 =  per.PLRMinUnl,
      x2 =  PLR1,
      deltaX =  per.PLRMinUnl/100);
  CR = Buildings.Utilities.Math.Functions.smoothMin(
      x1 =  PLR1/per.PLRMin,
      x2 =  1,
      deltaX =  0.001);

 if (uMod==1) then

     Q_flow_ava = QHea_flow*CapFT*scaling_factor;
     P = (Q_flow_ava/per.COP_nominal)*EIRFT*EIRFPLR*CR*scaling_factor;

     QLoa_flow = Buildings.Utilities.Math.Functions.smoothMin(
        x1 = Q_flow_set,
        x2 = Q_flow_ava,
        deltaX = Q_flow_small/10);

     QSou_flow = -(QLoa_flow - P*per.etaMotor);
     COP =QLoa_flow/(P + Q_flow_small);

  elseif (uMod==-1) then

      Q_flow_ava = QCoo_flow*CapFT*scaling_factor;
      P = (-Q_flow_ava/per.COP_nominal)*EIRFT*EIRFPLR*CR*scaling_factor;
      QLoa_flow = Buildings.Utilities.Math.Functions.smoothMax(
         x1 = Q_flow_set,
         x2 = Q_flow_ava,
         deltaX = Q_flow_small/10);

      QSou_flow = -QLoa_flow + P*per.etaMotor;
      COP  =-QLoa_flow/(P + Q_flow_small);

  else
      Q_flow_ava = 0;
      QLoa_flow = 0;
      QSou_flow = 0;
      P    = 0;
      COP = 0;
  end if;

annotation (Icon(coordinateSystem(preserveAspectRatio=false)),
Diagram(coordinateSystem(preserveAspectRatio=false)),
defaultComponentName="doe2",
Documentation(info="<html>
<p>
Block that implements the DOE2 method for the reverse heat pump model
<a href=\"Buildings.Fluid.HeatPumps.DOE2Reversible\">
Buildings.Fluid.HeatPumps.DOE2Reversible</a>.
</p>
</html>",
revisions="<html>
<ul>
  <li>
September 27, 2019, by Hagar Elarga:<br/>
First implementation.
</li>
</ul>
</html>"));
end DOE2Reversible;
