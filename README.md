# Planetary Health Index

Planetary Health Index (PHI) descibes the state of a region at a given time using three sets of features: biosphere, atmosphere, and sociosphere.

## Novelity

- treat eurostat data not as a list of observations, but as a datacube, e.g. feature array with values for every combiunation of region and time
- upscaling of coarser NUTS level, e.g. country GDP to NUTS3

## Issues

- regular data cube assumes no NA. This depends on the selected variables.

## Notes

## processing depends on question to be answered
- trend is signal and not noise in analyzing long term co-evolution
- extensive features like population cound must be added up during regional aggregtion while intensive features like GDP per capita must be averaged