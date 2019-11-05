(* Mathematica Package         *)
(* Created by IntelliJ IDEA    *)

(* :Title: Projection     *)
(* :Context: Projection`  *)
(* :Author: kthierbach            *)
(* :Date: 07/10/15              *)

(* :Package Version: 1.0       *)
(* :Mathematica Version:       *)
(* :Copyright: (c) 2015 kthierbach *)
(* :Keywords:                  *)
(* :Discussion:                *)

BeginPackage["Eidomatica`Transformation`"]
Needs["Eidomatica`"]
Needs["Eidomatica`Utilities`"]

(* Exported symbols added here with SymbolName::usage *)
eGreatCircleDistance::usage = "eGreatCircleDistance[{longitudeS,latitudeS,radiusS},{longitudeE,latitudeE,radiusE}] computes the great circle distance a starting and an end point.
eGreatCircleDistance[{{longitude,latitude,radius}..}] computes the great circle distance between consecutive points in the given list."
ToSpherical::usage = "ToPolar[{{x,y}..}] transforms the given coordinates from the Mercator projection to polar coordinates."
  OriginalDimensions::usage = "OriginalDimension defines the dimensions of the original image, where the coordinates where computed from."
  LateralProjectionRange::usage = "LateralProjectionRange is the projection range of the Mercator projection."
  Radius::usage="Radius the radius of the resulting polar coordinate."
eComputeGeodesic::usage="eComputeGeodesic[p1:{latitude1,longitude1,radius1}, p2:{latitude2,longitude2,radius2}] returns the parameterized geodesic from p1 to p2."
ToCartesian::usage="ToCartesian[{{longitude,latitude,radius}..}] converts list of polar coordinates to cartesian coordinates.
ToCartesian[{longitud,latitude,radius}] converts a single polar point to cartesian coordinate system."
ToMercator::usage="ToMercator[{{longitude,latitude,radius}..}] converts list of polar coordinates to the respective positions on a mercator projection.
ToMercator[{longitude,latitude,radius}] converts a polar coordinate to the respective position on a mercator projection."
ToTransverseMercator::usage="ToTransverseMercator[{longitude, latitude, radius}] converts the given spherical point to the transerverse Mercator projection."

Bonne::usage="Bonne[{longitude,latitude}]"
InverseBonne::usage="InverseBonne[{x,y}]"
BonneRegionFunction::usage="BonneRegionFunction[{x,y,f}] returnes either True or False if the given point falls into the Bonne region with given OriginalDimensions, StandardParallel and CentralMeridian."
CentralMeridian::usage = "CentralMeridian is an option for all functions concerning Bonne projections. The option defines the central meridian of the projection."
StandardParallel::usage = "StandardParallel is an option for all functions concerning Bonne projections. The option defines the standard parallel of the projection."

Begin["`Private`"] (* Begin Private Context *)

(*returns a vector with {longitude,latitude,radius} with ranges {-Pi..Pi, -Pi/2..Pi/2, 0..}*)
Clear[ToSpherical,top]
Options[ToSpherical]={Radius->1,OriginalDimensions->{1000,1000},LateralProjectionRange->17Pi/18};
ToSpherical[track:{{{_?NumericQ,_?NumericQ},_?NumericQ}..},opts:OptionsPattern[]]:=Map[{ToSpherical[#[[1]],opts],#[[2]]}&,track]
ToSpherical[track:{{_?NumericQ,_?NumericQ}..},opts:OptionsPattern[]]:=Block[
  {tmp},
  tmp=Sequence@@Flatten@OptionValue[{OriginalDimensions,Radius,LateralProjectionRange}];
  Map[top[Sequence@@#,tmp]&,track]
]
ToSpherical[{x_?NumericQ,y_?NumericQ},opts:OptionsPattern[]]:=top[x,y,Sequence@@Flatten@OptionValue[{OriginalDimensions,Radius,LateralProjectionRange}]]
ToSpherical[{},opts:OptionsPattern[]]={}
top=Compile[{{x,_Real},{y,_Real},{xdim,_Real},{ydim,_Real},{radius,_Real},{lpr,_Real}},{Pi*(-1 + 2*x/xdim), ArcSin[Tanh[(y/ydim*2*Log[Tan[(lpr + Pi)/4]]) - Log[Tan[(lpr + Pi)/4]]]], radius}];
(*ToSpherical for 3D cartesian points*)
ToSpherical[list:{{_?NumericQ,_?NumericQ,_?NumericQ}..},opts:OptionsPattern[]]:=ToSpherical[#, opts]&/@list
ToSpherical[p:{x_?NumericQ,y_?NumericQ,z_?NumericQ},opts:OptionsPattern[]]:=top2[Sequence@@p]
top2=Compile[{{x, _Real}, {y, _Real}, {z, _Real}}, Module[
  {radius},
  radius=Norm[{x,y,z}];
  {ArcTan[x,y], ArcCos[z/radius]-Pi/2, radius}
]]

(*Lateral projection range is implemented in none of these functions*)
Clear[ToMercator,tom];
Options[ToMercator]:=Options[ToSpherical];
ToMercator[{},opts:OptionsPattern[]]:={}
ToMercator[line:{{_?NumericQ,_?NumericQ,_?NumericQ}..},opts:OptionsPattern[]]:=ToMercator[#, opts]&/@line
ToMercator[{longitude_,latitude_,radius_},opts:OptionsPattern[]]:=tom[longitude,latitude,Sequence@@Flatten@OptionValue[{OriginalDimensions,LateralProjectionRange}]]
tom=Compile[{{longitude,_Real},{latitude,_Real},{xdim,_Real},{ydim,_Real},{lpr,_Real}},{(longitude + Pi)/(2 Pi)*xdim, (ArcTanh[Sin[latitude]] + Log[Tan[(lpr + Pi)/4]])/(2*Log[Tan[(lpr + Pi)/4]])*ydim}]

Clear[ToTransverseMercator, ttm];
Options[ToTransverseMercator]:=Options[ToSpherical];
ToTransverseMercator[list : {{_?NumericQ, _?NumericQ, _?NumericQ} ..},opts:OptionsPattern[]] := ToTransverseMercator[#, opts] & /@ list
ToTransverseMercator[p:{longitude_, latitude_, radius_}, opts:OptionsPattern[]] := ttm[Sequence@@p, Sequence@@Flatten@OptionValue[{OriginalDimensions,LateralProjectionRange}]];
ttm=Compile[{{longitude, _Real}, {latitude, _Real}, {radius, _Real}, {xdim,_Real},{ydim,_Real},{lpr,_Real}}, {(ArcTanh[Cos[latitude] Sin[longitude]]+Pi)/(2Pi)*xdim, (ArcTan[Cos[longitude], Tan[latitude]]+Pi)/(2Pi)*ydim}]


Clear[ToCartesian, tcart]
ToCartesian[{}]:={}
ToCartesian[track:{{_?NumericQ,_?NumericQ,_?NumericQ}..}]:=Map[ToCartesian[#]&,track]
ToCartesian[coord:{longitude_?NumericQ,latitude_?NumericQ,radius_?NumericQ}]:=tcart[Sawtooth[longitude-Pi,2Pi,Pi], latitude, radius]
tcart=Compile[{{longitude,_Real},{latitude,_Real},{radius,_Real}}, {-radius Cos[latitude] Cos[longitude], -radius Cos[latitude] Sin[longitude], -radius Sin[latitude]}]

Clear[Bonne]
Options[Bonne]={CentralMeridian->0,StandardParallel->Pi/256,OriginalDimensions->{1000,1000}};
Bonne[{longitude_,latitude_},opts:OptionsPattern[]]:=Block[
  {longitude0,latitude0,rho,e},
  {longitude0,latitude0}=OptionValue[{CentralMeridian,StandardParallel}];
  rho[latitude1_]:=Cot[latitude0]+latitude0-latitude1;
  e[longitude1_,latitude1_]:=((longitude1-longitude0)*Cos[latitude1])/rho[latitude1];
  {rho[latitude]*Sin[e[longitude,latitude]],Cot[latitude0]-rho[latitude]*Cos[e[longitude,latitude]]}
]

(*returns {longitude,latitude,radius}*)
Clear[InverseBonne]
Options[InverseBonne]:=Union@Join[Options[Bonne],{Radius->100}];
InverseBonne[{x_,y_},opts:OptionsPattern[]]:=Block[
  {centralMeridian,standardParallel,rho,latitude},
  {centralMeridian,standardParallel}=OptionValue[{CentralMeridian,StandardParallel}];
  rho=Sign[standardParallel]*Sqrt[x^2+(Cot[standardParallel]-y)^2];
  latitude=Cot[standardParallel]+standardParallel-rho;
  N@{centralMeridian+rho*ArcTan[(Cot[standardParallel]-y),x]/Cos[latitude],latitude,OptionValue[Radius]}
]

Clear[BonneRegionFunction];
Options[BonneRegionFunction]:=Options[Bonne];
BonneRegionFunction[{x_,y_,f_},opts:OptionsPattern[]]:=Block[
  {dims,tmp},
  dims=OptionValue[OriginalDimensions];
  tmp=InverseBonne[{x/dims[[1]]*2Pi-Pi,y/dims[[2]]*2Pi-Pi},opts];
  Abs[tmp[[2]]]<=Pi/2&&Abs[tmp[[1]]]<=Pi
]

(*
computes great circle distance for a sphere of radius 1 (radius is omitted), latitude has to be in the range of 0..Pi and longitude in the range from 0..2Pi
distance is computed for latitude between -Pi/2..Pi/2 (latitude-Pi/2)
*)
Clear[eGreatCircleDistance,gcd]
eGreatCircleDistance[{longitudeS_,latitudeS_,radiusS_}, list:{{_?NumericQ,_?NumericQ,_?NumericQ}..}]:=Map[gcd[longitudeS,latitudeS,radiusS,Sequence@@#]&,list]
eGreatCircleDistance[list:{{_?NumericQ,_?NumericQ,_?NumericQ}..}]:=MapThread[eGreatCircleDistance[#1,#2]&,{Drop[list,1],Drop[list,-1]}]
eGreatCircleDistance[{longitudeS_,latitudeS_,radiusS_},{longitudeE_,latitudeE_,radiusE_}]:=gcd[longitudeS,latitudeS,radiusS,longitudeE,latitudeE,radiusE]
gcd=Compile[{{longitudeS,_Real},{latitudeS,_Real},{radiusS,_Real},{longitudeE,_Real},{latitudeE,_Real},{radiusE,_Real}},radiusS*2ArcSin[Sqrt[Haversine[latitudeE-latitudeS]+Cos[latitudeS]*Cos[latitudeE]*Haversine[longitudeE-longitudeS]]]];

Clear[eComputeGeodesic, geodesic];
eComputeGeodesic::same="The given points from=`1` and to=`2` are identical!";
eComputeGeodesic[from:{_?NumericQ, _?NumericQ, _?NumericQ}, to:{_?NumericQ, _?NumericQ, _?NumericQ}] := geodesic[from, to, None]
eComputeGeodesic[from:{_?NumericQ, _?NumericQ, _?NumericQ}, to:{_?NumericQ, _?NumericQ, _?NumericQ}, distance_?NumericQ] := geodesic[from, to, distance]
eComputeGeodesic[from:{_?NumericQ, _?NumericQ, _?NumericQ}, azimuth_?NumericQ, distance_?NumericQ] := geodesic[from, 2 Pi - (azimuth - Pi/2), distance]
eComputeGeodesic[from:{_?NumericQ, _?NumericQ, _?NumericQ}, azimuth_?NumericQ] := geodesic[from, 2 Pi - (azimuth - Pi/2), None]
geodesic[p1:{lambda1_, theta1_, r1_}, p2:{lambda2_, theta2_, r2_}, distance_] := Block[
  {f, lambda12, alpha1, alpha2, sigma12, alpha0, sigma01, sigma02, lambda01, lambda0, sign, position, pos, correction},
  (*lambda=longitude, theta=latitude*)
  If[p1===p2,
    Message[eComputeGeodesic::same,p1,p2];
    Return[BSplineFunction[{p1,p2},SplineDegree->1]]
  ];
  f=Compile[{{delta, _Real}}, Piecewise[{{Sign[-delta] Pi + Sign[delta] Mod[Abs@delta, Pi], Abs@delta > Pi}, {delta, Abs@delta <= Pi}}]];
  lambda12 = f[lambda2 - lambda1];
  alpha1 = ArcTan[Cos[theta1] * Tan[theta2] - Sin[theta1] * Cos[lambda12], Sin[lambda12]];
  alpha2 = ArcTan[-Cos[theta2] * Tan[theta1] + Sin[theta2] * Cos[lambda12], Sin[lambda12]];
  sigma12 = ArcCos[Sin[theta1] Sin[theta2] + Cos[theta1] Cos[theta2] Cos[ lambda12]];
  alpha0 = ArcTan[Sqrt[Cos[alpha1]^2 + Sin[alpha1]^2 * Sin[theta1]^2], Sin[alpha1] Cos[theta1]];
  sigma01 = Piecewise[{{0, alpha1 == Pi / 2 && theta1 == 0}, {ArcTan[Cos[alpha1], Tan[theta1]], True}}];
  sigma02 = Piecewise[{{0, alpha2 == Pi / 2 && theta2 == 0}, {ArcTan[Cos[alpha2], Tan[theta2]], True}}];
  lambda01 = ArcTan[Cos[sigma01], Sin[alpha0] Sin[sigma01]];
  lambda0 = f[lambda1 - lambda01];
  position = Compile[{{sigma,_Real}}, {ArcTan[Cos[sigma], Sin[alpha0] Sin[sigma]] + lambda0, ArcTan[Sqrt[Cos[sigma]^2 + Sin[alpha0]^2 * Sin[sigma]^2], Cos[alpha0] Sin[sigma]], r1}];
  correction = Compile[{{longitude, _Real}, {latitude, _Real}, {radius, _Real}}, {Sawtooth[longitude, 2 Pi, Pi], Sawtooth[latitude, Pi, Pi/2], radius}];
  If[distance === None,
    BSplineFunction[Table[correction[Sequence@@position[f[sigma]]], {sigma, sigma01, sigma01 + sigma12, sigma12 / 500}], SplineDegree -> 1],
    correction[Sequence@@position[f[sigma01+distance]]]
  ]
]
geodesic[p1:{lambda1_, theta1_, r1_}, azimuth_?NumericQ, distance_]:=Block[
  {sigma01, alpha0, lambda01, lambda0, f, position, correction},
  sigma01 = Piecewise[{{0, azimuth == Pi / 2 && theta1 == 0}, {ArcTan[Cos[azimuth], Tan[theta1]], True}}];
  alpha0 = ArcTan[Sqrt[Cos[azimuth]^2 + Sin[azimuth]^2 * Sin[theta1]^2], Sin[azimuth] Cos[theta1]];
  lambda01 = ArcTan[Cos[sigma01], Sin[alpha0] Sin[sigma01]];
  lambda0 = f[lambda1 - lambda01];
  f=Compile[{{delta, _Real}}, Piecewise[{{Sign[-delta] Pi + Sign[delta] Mod[Abs@delta, Pi], Abs@delta > Pi}, {delta, Abs@delta <= Pi}}]];
  position = Compile[{{sigma,_Real}}, {ArcTan[Cos[sigma], Sin[alpha0] Sin[sigma]] + lambda0, ArcTan[Sqrt[Cos[sigma]^2 + Sin[alpha0]^2 * Sin[sigma]^2], Cos[alpha0] Sin[sigma]], r1}];
  (*correction = Compile[{{longitude, _Real}, {latitude, _Real}, {radius, _Real}}, {-2 ArcTan[Cot[(longitude - Pi)/2]], -ArcTan[Cot[(latitude - Pi/2)]], radius}];*)
  correction = Compile[{{longitude, _Real}, {latitude, _Real}, {radius, _Real}}, {Sawtooth[longitude, 2 Pi, Pi], Sawtooth[latitude, Pi, Pi/2], radius}];
  If[distance===None,
    BSplineFunction[Table[correction[Sequence@@position[f[sigma01+distance]]], {distance, 0, 2Pi, 2Pi/500}], SplineDegree -> 1],
    correction[Sequence@@position[f[sigma01+distance]]]
  ]
]

End[] (* End Private Context *)

EndPackage[]