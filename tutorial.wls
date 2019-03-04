#!/usr/bin/env wolframscript

(* When it comes to weather radar data that is open to the public the gold standard is the US 
NEXRAD system consisting of about 160 stations in the US and abroad equipped with 8 m parabolic 
dishes that rotate in azimuth. They also scan a couple of different elevations to give complete 
coverage of the skies in a number of operational modes which are collectively called "volume 
coverage patterns" (VCP). A complete volume scan takes some minutes resulting in a ray pattern 
along which different meteorologically relevant signals are measured like the reflectivity, 
radial Doppler velocity etc.
Reflectivity is relevant since it correlates well with precipitation, the frequency of NEXRAD 
radars is around 3 GHz, pretty close to the frequencies used in commercial microwave ovens so 
the radiation strikes a good tradeoff between scattering and penetration properties such that 
weather can be observed to several hundreds of kilometers from one single station. 
In fact against a clear sky these radars are capable of seeing a golf ball at 100 km distance! 
But our interest is to understand more of the weather, getting capable of digging into the 
wealth of data ourselves and start to become data explorers.
The NEXRAD information is streamed and stored in the Amazon cloud and is open to the public 
courtesy of NOAA's National Weather Service and the US government. *)

(* import an example dataset *)
datasets = 
  Import["./cfrad.20190101_001553.696_to_20190101_002119.084_KCXX_Surveillance_SUR.nc", "NetCDF"];

ds = Dataset[
  AssociationThread[datasets, 
   Import["./cfrad.20190101_001553.696_to_20190101_002119.084_KCXX_Surveillance_SUR.nc", {"Datasets", 
     datasets}]]]

(* plot a map of where the radar station location *)
GeoPosition[{ds["latitude"], ds["longitude"], ds["altitude"]}] // GeoGraphics

(* define a couple of tables to translate measurement table indices to end of rays, ray number, range helper tables etc. *)
rayEndRange = 
  Normal[ds[
     "ray_start_range"]] + (Normal[ds["ray_n_gates"]] - 1) Normal[
     ds["ray_gate_spacing"]];
rayNumber = 
  Table[i, {i, Length[ds["ray_start_range"]]}, {j, 
     ds["ray_n_gates", i]}]  // Flatten;
     
rangeAll = 
  MapThread[
   Range[#1, #2, #3] & , {Normal[ds["ray_start_range"]], rayEndRange, 
    Normal[ds["ray_gate_spacing"]]}];
rangeAllFlat = Flatten[rangeAll];

(* calculate geolocations of points belonging to a ray *)
eastProj = 
  Normal[ ds["azimuth", Sin[# Degree] &]] Normal[ 
    ds["elevation", Cos[# Degree] &]];
northProj = 
  Normal[ ds["azimuth", Cos[# Degree] &]] Normal[ 
    ds["elevation", Cos[# Degree] &]];
upProj = Normal[ ds["elevation", Sin[# Degree] &]];
NexradPos[r_, e_, n_, u_] := 
 GeoPosition[
  GeoPositionENU[r {e, n, u}, 
   GeoPosition[{ds["latitude"], ds["longitude"], ds["altitude"]}]]]

(* take the first ray in the dataset as an example and plot its points on a map *)
rayExample = 
 NexradPos[#, eastProj[[1]], northProj[[1]], upProj[[1]]] & /@ 
  rangeAll[[1, ;;]]
  
GeoListPlot[rayExample]

(* have look at the histogram of all reflectivities in the scanned volume *)
Histogram[ds["REF"], PlotRange -> All]

(* select only the top reflectivity voxels and plot a histogram of their geographic distribution *)
refSelected = Position[Normal[ds["REF"]], _?(# > -10 &)] // Flatten;

posREFHigh = 
 NexradPos[rangeAllFlat[[#]], eastProj[[rayNumber[[#]] ]], 
    northProj[[ rayNumber[[#]] ]], upProj[[rayNumber[[#]] ]]] & /@ 
  refSelected[[ ;; ]]
  
GeoHistogram[posREFHigh]
