within Buildings.Applications.DHC.Examples.FifthGeneration.Unidirectional.Networks;
model UnidirectionalParallel2
  "Hydraulic network for unidirectional parallel DHC system"
  extends Applications.DHC.Networks.BaseClasses.PartialDistribution2Pipe(
      redeclare BaseClasses.ConnectionParallel2 con[nCon](dpCon_nominal=
          dpCon_nominal, dpDis_nominal=dpDis_nominal), redeclare model
      Model_pipDis = BaseClasses.PipeDistribution2 (dp_nominal=dpEnd_nominal));

  /* (
      final dh=dhEnd, final length=2*lEnd, final fac=facEnd))*/
    //  final lDis=lDis, final lCon=lCon, final dhDis=dhDis, final dhCon=dhCon, final fac=fac),
 /* parameter Modelica.SIunits.Length lDis[nCon]
    "Length of the distribution pipe before each connection (supply only, not counting return line)";
  parameter Modelica.SIunits.Length lCon[nCon]
    "Length of each connection pipe (supply only, not counting return line)";
  parameter Modelica.SIunits.Length lEnd
    "Length of the end of the distribution line (supply only, not counting return line)";
  parameter Modelica.SIunits.Length dhDis[nCon]
    "Hydraulic diameter of the distribution pipe before each connection";
  parameter Modelica.SIunits.Length dhCon[nCon]
    "Hydraulic diameter of each connection pipe";
  parameter Modelica.SIunits.Length dhEnd = dhDis[nCon]
    "Hydraulic diameter of the end of the distribution line";
  parameter Real facEnd
    "Factor to take into account resistance of bends etc., fac=dp_nominal/dpStraightPipe_nominal";
  parameter Real fac[nCon]
  "Factor to take into account resistance of bends etc., fac=dp_nominal/dpStraightPipe_nominal";*/
  parameter Modelica.SIunits.Pressure dpCon_nominal[nCon];
  parameter Modelica.SIunits.Pressure dpDis_nominal[nCon];
  parameter Modelica.SIunits.Pressure dpEnd_nominal;
end UnidirectionalParallel2;
