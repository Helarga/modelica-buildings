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
  final parameter Modelica.SIunits.HeatFlowRate QHea_flow= -1*per.QEva_flow_nominal + P_nominal
   "Nominal heat flow at the condenser";
  final parameter Modelica.SIunits.HeatFlowRate QCoo_flow = per.QEva_flow_nominal*scaling_factor
   "Reference capacity";
  final parameter Modelica.SIunits.Power P_nominal = -per.QEva_flow_nominal/(per.COP_nominal)
   "Nominal power of the compressor";
  final parameter Modelica.SIunits.HeatFlowRate Q_flow_small = QHea_flow * 1E-9
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
  Modelica.SIunits.Efficiency EIRFT(nominal=1)
   "Power input to ccoling capacity ratio function of temperature curve";
  Modelica.SIunits.Efficiency EIRFPLR(nominal=1)
   "Power input to cooling capacity ratio function of part load ratio";

  Real CapFT(min=0,nominal=1)
   "Cooling capacity factor function of temperature curve";
  Real PLR1(min=0, nominal=1, unit="1")
   "Part load ratio";
  Real PLR2(min=0, nominal=1, unit="1")
   "Part load ratio";
  Real CR(min=0, nominal=1, unit="1")
   "Cycling ratio";
  Real PLR(min=0, nominal=1, unit="1")
     "Part load ratio";

  Modelica.SIunits.HeatFlowRate Q_flow_ava
   "Heat (or cooling) capacity available";

  Modelica.SIunits.Conversions.NonSIunits.Temperature_degC TSouEnt_degC
   "Condenser entering water temperature in degC";
  Modelica.SIunits.Conversions.NonSIunits.Temperature_degC TLoaLvg_degC
   "Evaporator leaving water temperature in degC";

initial equation
  assert(QHea_flow > 0,
  "Parameter QCon_flow_nominal must be larger than zero.");
  assert(QCoo_flow < 0,
  "Parameter QEva_heatflow_nominal must be lesser than zero.");
  assert(Q_flow_small > 0,
  "Parameter Q_flow_small must be larger than zero.");
  assert(per.PLRMinUnl >= per.PLRMin,
  "Parameter PLRMinUnl must be bigger or equal to PLRMin");
  assert(per.PLRMax > per.PLRMinUnl,
  "Parameter PLRMax must be bigger than PLRMinUnl");

equation
  TSouEnt_degC=Modelica.SIunits.Conversions.to_degC(TSouEnt);
  TLoaLvg_degC=Modelica.SIunits.Conversions.to_degC(TLoaLvg);

    CapFT  = Buildings.Utilities.Math.Functions.smoothMax(
     x1 = 1E-7,
     x2 = Buildings.Utilities.Math.Functions.biquadratic(
       a =  per.capFunT,
       x1 = TLoaLvg_degC,
       x2 = TSouEnt_degC),
       deltaX = 1E-7);

     EIRFT = Buildings.Utilities.Math.Functions.biquadratic(
       a =  per.EIRFunT,
       x1 = TLoaLvg_degC,
       x2 = TSouEnt_degC);

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

      Q_flow_ava = QHea_flow*CapFT;
      P = (Q_flow_ava/per.COP_nominal)*EIRFT*EIRFPLR*CR;
      QLoa_flow = Buildings.Utilities.Math.Functions.smoothMin(
                              x1 = Q_flow_set,
                              x2 = Q_flow_ava,
                          deltaX = Q_flow_small/10);

      QSou_flow = -(QLoa_flow - P*per.etaMotor);
      COP =QLoa_flow/(P + Q_flow_small);
      PLR = QLoa_flow / Q_flow_ava;

    elseif (uMod==-1) then
      Q_flow_ava = QCoo_flow*CapFT;

      P = (-Q_flow_ava/per.COP_nominal)*EIRFT*EIRFPLR*CR;

      QLoa_flow = Buildings.Utilities.Math.Functions.smoothMax(
                              x1 = Q_flow_set,
                              x2 = Q_flow_ava,
                          deltaX = Q_flow_small/10);

      QSou_flow = -QLoa_flow + P*per.etaMotor;

      COP  =-QLoa_flow/(P + Q_flow_small);
      PLR = QLoa_flow / Q_flow_ava;
  else
      Q_flow_ava = 0;
      QLoa_flow = 0;
      QSou_flow = 0;
      P    = 0;
      COP = 0;
      PLR = 0;
  end if;

  annotation (Icon(coordinateSystem(preserveAspectRatio=false)),
  Diagram(coordinateSystem(preserveAspectRatio=false)),
  defaultComponentName="doe2Rev",
  Documentation(info="<html>
  <p>
  The Block includes the description of the DOE2 method dedicated for<a href=\"Buildings.Fluid.HeatPumps.DOE2WaterToWater\">
  Buildings.Fluid.HeatPumps.DOE2WaterToWater</a>.
  </p>
  <p>
  The block uses three functions to predict the thermal capacity and power consumption through three operational modes executed by a control input integer signal uMod=1 heating mode, uMod=-1 cooling mode and uMod=0 shutoff.
  </p>
  <ol>
  <li>
  The first function is <code>CapFT</code> the capacity function of temperature bi-quadratic curve
  <p align=\"center\" style=\"font-style:italic;\">
  CapFT = capFunT<sub>1</sub>+ capFunT<sub>2</sub>T<sub>Eva,Lvg</sub>+
  capFunT<sub>3</sub>T<sup>2</sup><sub>Eva,Lvg</sub>+ capFunT<sub>4</sub>T<sub>Con,Ent</sub>+capFunT<sub>5</sub>T<sup>2</sup><sub>Con,Ent</sub>
  +capFunT<sub>6</sub>T<sub>Con,Ent</sub>T<sub>Eva,Lvg</sub>
  <p>
  where the performance curve coefficients from <i>capFunT<sub>1</sub> to capFunT<sub>6</sub> </i>
  are stored in the data record <code>per</code>.
  </p>
  </li>
  <li>
  The second function is <code>EIRFT</code> the electric input to capacity output ratio function of temperature bi-quadratic curve
  <p align=\"center\" style=\"font-style:italic;\">
  EIRFT = EIRFunT<sub>1</sub>+ EIRFunT<sub>2</sub>T<sub>Eva,Lvg</sub>+
  EIRFunT<sub>3</sub>T<sup>2</sup><sub>Eva,Lvg</sub>+ EIRFunT<sub>4</sub>T<sub>Con,Ent</sub>+EIRFunT<sub>5</sub>T<sup>2</sup><sub>Con,Ent</sub>
  +EIRFunT<sub>6</sub>T<sub>Con,Ent</sub>T<sub>Eva,Lvg</sub>
  <p> 
  where the performance curve coefficients from <i>EIRFunT<sub>1</sub> to EIRFunT<sub>6</sub> </i>
  are stored in the data record <code>per</code>.
  </p>
  </li>
  <li>
  The third performance function is <code>EIRFPLR</code> the electric input to capacity output ratio function of part load ratio bi-cubic curve
  <p align=\"center\" style=\"font-style:italic;\">
  EIRFPLR = EIRFunPLR<sub>1</sub>+ EIRFunPLR<sub>2</sub>PLR+EIRFunPLR<sub>3</sub>PLR<sup>2</sup>
  <p>
  where the performance curve coefficients from <i>EIRFunPLR<sub>1</sub> to EIRFunPLR<sub>3</sub> </i>
  are stored in the data record <code>per</code>.
  </p>
  </li>
  </ol>
  <p>
  The data record <code>per</code> is available at
  <a href=\"Buildings.Fluid.Chillers.Data.ElectricEIR\">
  Buildings.Fluid.Chillers.Data.ElectricEIR</a>.
  Additional performance curves can be developed using
  two available techniques Hydeman and Gillespie, (2002). The first technique is called the
  Least-squares Linear Regression method and is used when sufficient performance data exist
  to employ standard least-square linear regression techniques. The second technique is called
  Reference Curve Method and is used when insufficient performance data exist to apply linear
  regression techniques.
  </p>
  <p>
  The model has three tests on the part load ratio and the cycling ratio:
  </p>
  <ol>
  <li>
  The test<pre>
    PLR1 =min(QEva_flow_set/QEva_flow_ava, PLRMax);
  </pre>
  ensures that the heatpump capacity does not exceed the heatpump capacity specified
  by the parameter <code>PLRMax</code>.
  </li>
  <li>
  The test <pre>
    CR = min(PLR1/per.PRLMin, 1.0);
  </pre>
  computes a cycling ratio. This ratio expresses the fraction of time
  that a heatpump would run if it were to cycle because its load is smaller than the
  minimal load at which it can operate.
  Note that this model continuously operates even if the part load ratio is below the
  minimum part load ratio.
  Its leaving evaporator and condenser temperature can therefore be considered as an
  average temperature between the modes where the compressor is off and on.
  </li>
  <li>
  The test <pre>
    PLR2 = max(per.PLRMinUnl, PLR1);
  </pre>
  computes the part load ratio of the compressor.
  The assumption is that for a part load ratio below <code>per.PLRMinUnl</code>,
  the heatpump uses hot gas bypass to reduce the capacity, while the compressor
  power draw does not change.
  </li>
  </ol>
  <p>
  The electric power only contains the power for the compressor, but not any power
  for pumps or fans.
  </p>
  </html>",
  revisions="<html>
  <ul>
  <li>
June 24, 2019, by Hagar Elarga:<br/>
First implementation.
</li>
</ul>
</html>"));
end DOE2Reversible;
