# WISE
This shows how to use WISE data to make faceted plots containing all observations for Young Stellar Objects and making Lomb Scargle plots and Folded Light Curves for objects with more than 30 observations.

## data folder
This contains a limited set of the WISE data with columns `seq` for the unique object identifier, `w1mpro` and `w2mpro`, or the measurement of brightness of an object, the `mjd`, when the observation was taken. You should be able to run this script on any file with columns with these names.

## facet_plots
This is a folder where all the images of the facet plots will be stored.

## lomb_plots
This is a folder where all the images of the Lomb Scargle plots for objects with more than 30 observations.

## folded_curve_plots
This is a folder where all the images of the Folded Light Curve plots (using information from the Lomb Scargle plots) for objects with more than 30 observations.
