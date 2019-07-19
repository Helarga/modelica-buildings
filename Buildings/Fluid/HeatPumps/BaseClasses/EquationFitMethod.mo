within Buildings.Fluid.HeatPumps.BaseClasses;
block EquationFitMethod "EquationFit method to predict heatpump performance"
  extends Modelica.Blocks.Icons.Block;

    parameter Data.EquationFitWaterToWater.Generic_EquationFit per
      "Performance data"
       annotation (choicesAllMatching = true,
                Placement(transformation(extent={{78,80},{98,100}})));
    final parameter Modelica.SIunits.HeatFlowRate   Q_flow_small = per.QCon_heatflow_nominal*1E-9
      "Small value for heat flow rate or power, used to avoid division by zero";

    Modelica.Blocks.Interfaces.RealInput TEvaSet(final unit="K", displayUnit="degC")
      "Set point for leaving chilled water temperature"
       annotation (Placement(
        transformation(extent={{-124,-112},{-100,-88}}), iconTransformation(
          extent={{-118,-108},{-100,-90}})));
    Modelica.Blocks.Interfaces.RealInput TConSet(final unit="K", displayUnit="degC")
      "Set point for leaving heating water temperature"
       annotation (Placement(
        transformation(extent={{-122,88},{-100,110}}), iconTransformation(
          extent={{-120,90},{-100,110}})));
    Modelica.Blocks.Interfaces.IntegerInput uMod
    "HeatPump control input signal, Heating mode= 1, Off=0, Cooling mode=-1"
       annotation (Placement(transformation(extent={{-124,
            -12},{-100,12}}),
        iconTransformation(extent={{-118,-10},{-100,8}})));
    Modelica.Blocks.Interfaces.RealInput TConLvg(final unit="K", displayUnit="degC")
      "Condenser leaving water temperature"
       annotation (Placement(transformation(extent={{-122,68},{-100,90}}), iconTransformation(extent={{-120,70},
            {-100,90}})));
    Modelica.Blocks.Interfaces.RealInput TConEnt(final unit="K", displayUnit="degC")
      "Condenser entering water temperature"
       annotation (Placement(transformation(extent={{-124,48},{-100,72}}),iconTransformation(extent={{-120,50},
            {-100,70}})));
    Modelica.Blocks.Interfaces.RealInput TEvaLvg(final unit="K", displayUnit="degC")
      "Evaporator leaving water temperature"
       annotation (Placement(transformation(extent={{-124,-72},{-100,-48}}), iconTransformation(extent={{-118,
            -70},{-100,-52}})));
    Modelica.Blocks.Interfaces.RealInput TEvaEnt(final unit="K", displayUnit="degC")
      "Evaporator entering water temperature"
       annotation (Placement(transformation(extent={{-124,-92},{-100,-68}}), iconTransformation(extent={{-118,
            -88},{-100,-70}})));
    Modelica.Blocks.Interfaces.RealInput m1_flow(final unit="kg/s")
      "Volume 1 massflow rate "
       annotation (Placement(transformation(extent={{-124,8},{-100,32}}),  iconTransformation(extent={{-120,30},
            {-100,50}})));
    Modelica.Blocks.Interfaces.RealInput m2_flow(final unit="kg/s")
      "Volume2 mass flow rate"
       annotation (Placement(transformation(extent={{-124,-32},{-100,-8}}),  iconTransformation(extent={{-118,
            -48},{-100,-30}})));
    Modelica.Blocks.Interfaces.RealInput QConFloSet(final unit="W")
       "Condenser setpoint heat flow rate"
        annotation (Placement(transformation(
          extent={{-124,28},{-100,52}}), iconTransformation(extent={{-120,10},{-100,
            30}})));
    Modelica.Blocks.Interfaces.RealInput QEvaFloSet(final unit="W")
      "Evaporator setpoint heat flow rate"
       annotation (Placement(transformation(
          extent={{-124,-52},{-100,-28}}), iconTransformation(extent={{-118,-30},
            {-100,-12}})));
    Modelica.Blocks.Interfaces.RealOutput QCon_flow(final unit="W")
    "Condenser heat flow rate "
       annotation (Placement(transformation(extent={{100,
            30},{120,50}}), iconTransformation(extent={{100,30},{120,50}})));
    Modelica.Blocks.Interfaces.RealOutput QEva_flow(final unit="W")
    "Evaporator heat flow rate "
       annotation (Placement(transformation(extent={{100,
            -48},{120,-28}}), iconTransformation(extent={{100,-50},{120,-30}})));
    Modelica.Blocks.Interfaces.RealOutput P(final unit="W")
      "Compressor power"
       annotation (Placement(transformation(extent={{100,-10},{120,10}}),iconTransformation(extent={{100,-10},
            {120,10}})));
    Modelica.SIunits.Efficiency HLR
      "Heating load ratio";
    Modelica.SIunits.Efficiency CLR
      "Cooling load ratio";
    Modelica.SIunits.Efficiency P_HD
      "Power Ratio in heating dominanat mode";
    Modelica.SIunits.Efficiency P_CD
      "Power Ratio in cooling dominant mode";
    Modelica.SIunits.HeatFlowRate QCon_flow_ava
      "Heating capacity available at the condender";
    Modelica.SIunits.HeatFlowRate QEva_flow_ava
      "Cooling capacity available at the Evaporator";

protected
    Real A1[5] "Thermal load ratio coefficients";
    Real x1[5] "Normalized inlet variables";
    Real A2[5] "Compressor power ratio coefficients";
    Real x2[5] "Normalized inlet variables";

initial equation
   assert(per.QCon_heatflow_nominal> 0,
   "Parameter QCon_heatflow_nominal must be larger than zero.");
   assert(per.QEva_heatflow_nominal< 0,
   "Parameter QEva_heatflow_nominal must be lesser than zero.");
   assert(Q_flow_small > 0,
   "Parameter Q_flow_small must be larger than zero.");

equation
    if (uMod==1) then

      A1=per.HLRC;
      x1={1,TConEnt/per.TRef,TEvaEnt/per.TRef,
      m1_flow/per.mCon_flow_nominal,m2_flow/per.mEva_flow_nominal};

      A2= per.P_HDC;
      x2={1,TConEnt/per.TRef,TEvaEnt/per.TRef,
      m1_flow/per.mCon_flow_nominal,m2_flow/per.mEva_flow_nominal};

      HLR  = sum( A1.*x1);
      CLR  = 0;
      P_HD = sum( A2.*x2);
      P_CD = 0;
      QCon_flow_ava= HLR *(per.QCon_heatflow_nominal);
      QEva_flow_ava = 0;

      QCon_flow = Buildings.Utilities.Math.Functions.smoothMin(
        x1=QConFloSet,
        x2=QCon_flow_ava,
        deltaX=Q_flow_small/10);

      P = P_HD * (per.PCon_nominal_HD);
      QEva_flow = -(QCon_flow - P);

    elseif (uMod==-1) then

      A1= per.CLRC;
      x1={1,TConEnt/per.TRef,TEvaEnt/per.TRef,
      m1_flow/per.mCon_flow_nominal,m2_flow/per.mEva_flow_nominal};

      A2= per.P_CDC;
      x2={1,TConEnt/per.TRef,TEvaEnt/per.TRef,
      m1_flow/per.mCon_flow_nominal,m2_flow/per.mEva_flow_nominal};

      HLR  = 0;
      CLR  = sum(A1.*x1);
      P_HD = 0;
      P_CD = sum(A2.*x2);
      QCon_flow_ava = 0;
      QEva_flow_ava = CLR* (per.QEva_heatflow_nominal);

      QEva_flow = Buildings.Utilities.Math.Functions.smoothMax(
        x1=QEvaFloSet,
        x2=QEva_flow_ava,
        deltaX=Q_flow_small/10);

      P = P_CD * (per.PEva_nominal_CD);
      QCon_flow = -QEva_flow + P;

    else

      A1={0,0,0,0,0};
      x1={0,0,0,0,0};

      A2={0,0,0,0,0};
      x2={0,0,0,0,0};

      HLR= 0;
      CLR=0;
      P_HD =0;
      P_CD = 0;
      P = 0;
      QCon_flow_ava = 0;
      QEva_flow_ava = 0;
      QCon_flow = 0;
      QEva_flow = 0;

    end if;

  annotation (Icon(coordinateSystem(preserveAspectRatio=false)),
              Diagram(coordinateSystem(preserveAspectRatio=false)),
defaultComponentName="equFit",
Documentation(info="<html>
  <p>
  The Block includes the description of the equation fit method dedicated for
  <a href=\"Buildings.Fluid.HeatPumps.EquationFitWaterToWater\">
  Buildings.Fluid.HeatPumps.EquationFitWaterToWater</a>.
  </p>
  <p>
  The block uses four functions to predict capacity and power consumption for heating mode
  and cooling mode:
  </p>
  <ul>
  <li>
  The heating mode when <code>uMod</code>=+1
  <p align=\"center\" style=\"font-style:italic;\">
  Q&#775;<sub>Con</sub>/Q&#775;<sub>Con,nominal</sub> = HLRC<sub>1</sub>+ HLRC<sub>2</sub> T<sub>Con,Ent</sub>/T<sub>Con,nominal</sub>+
  HLRC<sub>3</sub> T<sub>Eva,Ent</sub>/T<sub>Eva,nominal</sub>+ HLRC<sub>4</sub> V&#775;<sub>Con,Ent</sub>/V&#775;<sub>Con,nominal</sub>+
  + HLRC<sub>5</sub> V&#775;<sub>Eva,Ent</sub>/V&#775;<sub>Eva,nominal</sub>
  
  <p align=\"center\" style=\"font-style:italic;\">
  Power<sub>Con</sub>/Power<sub>Con,nominal</sub>= P_HDC<sub>1</sub>+ P_HDC<sub>2</sub> T<sub>Con,Ent</sub>/T<sub>Con,nominal</sub>+
  P_HDC<sub>3</sub>.T<sub>Eva,Ent</sub>/T<sub>Eva,nominal</sub>+ P_HDC<sub>4</sub> V&#775;<sub>Con,Ent</sub>/V&#775;<sub>Con,nominal</sub>+
  + P_HDC<sub>5</sub> V&#775;<sub>Eva,Ent</sub>/V&#775;<sub>Eva,nominal</sub>
  </li>
  </ul>
  <p>
  where the coefficients <i>HLRC<sub>1</sub> to HLRC<sub>5</sub> </i> and  <i>P_HDC<sub>1</sub> to P_HDC<sub>5</sub> </i>
  are stored in the data record <code>per</code> at <a href=\"Buildings.Fluid.HeatPumps.Data.EquationFitWaterToWater\">
  Buildings.Fluid.HeatPumps.Data.EquationFitWaterToWater</a>.
  </p>
  <ul>
  <li>
  The cooling mode when <code>uMod</code>=-1
  <p align=\"center\" style=\"font-style:italic;\">
  Q&#775;<sub>Eva</sub>/Q&#775;<sub>Eva,nominal</sub> = CLRC<sub>1</sub>+ CLRC<sub>2</sub> T<sub>Con,Ent</sub>/T<sub>Con,nominal</sub>+
  CLRC<sub>3</sub> T<sub>Eva,Ent</sub>/T<sub>Eva,nominal</sub>+ CLRC<sub>4</sub> V&#775;<sub>Con,Ent</sub>/V&#775;<sub>Con,nominal</sub>+
  + CLRC<sub>5</sub> V&#775;<sub>Eva,Ent</sub>/V&#775;<sub>Eva,nominal</sub>
  
  <p align=\"center\" style=\"font-style:italic;\">
   Power<sub>Eva</sub>/Power<sub>Eva,nominal</sub> = P_CDC<sub>1</sub>+ P_CDC<sub>2</sub>.T<sub>Con,Ent</sub>/T<sub>Con,nominal</sub>+
   P_CDC<sub>3</sub> T<sub>Eva,Ent</sub>/T<sub>Eva,nominal</sub>+ P_CDC<sub>4</sub> V&#775;<sub>Con,Ent</sub>/V&#775;<sub>Con,nominal</sub>+
   + P_CDC<sub>5</sub> V&#775;<sub>Eva,Ent</sub>/V&#775;<sub>Eva,nominal</sub>
  </li>
  </ul>
  <p>
  where the coefficients <i>CLRC<sub>1</sub> to CLRC<sub>5</sub> </i> and  <i>P_CDC<sub>1</sub> to P_CDC<sub>5</sub> </i>
  are stored in the data record <code>per</code> at <a href=\"Buildings.Fluid.HeatPumps.Data.EquationFitWaterToWater\">
  Buildings.Fluid.HeatPumps.Data.EquationFitWaterToWater</a>.
  </p>
  <p>
  For these four equations, the inlet conditions or variables are divided by the reference conditions.
  This formulation allows the coefficients to fall into smaller range of values. Moreover, the value of the coefficient
  indirectly represents the sensitivity of the output to that particular inlet variable.
  </p>
  </html>",
  revisions="<html>
  <ul>
  <li>
May 19, 2019, by Hagar Elarga:<br/>
First implementation.
</li>
</ul>
</html>"));
end EquationFitMethod;
