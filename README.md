# EvaluatingVDBA

## Project Description
Bio-logging accelerometers are frequently used to estimate energetic output (metabolic cost) of animal movement as the vectorial sum of all axes (VDBA). While VDBA has conclusively been found to correlate temporally with metabolic rate within individuals, how VDBA scales between individuals of differing body mass and across species of different sizes, is inconclusive. A recent publication on a single species [The scaling of motion: dynamic body acceleration declines with body mass in black-tailed prairie dogs](https://link.springer.com/article/10.1186/s40317-026-00497-7) found a negative (though highly variable) relationship between VDBA and body mass. In this extended analysis, we combine data from multiple species across a greater body mass range.

![Graphical Abstract](Manuscript/Figures/GraphicalAbstract.png)

## Method
- XX species datasets of raw tri-axial accelerometer data where locomotion periods are known (either provided with labels or inferred using thresholds).
- Formatted to standard data structure.
- Generated dynamic VDBA by removing static acceleration (where static was calculated as a rolling average of 1 second)
- Separated locomotion instances
- Calculated the mean acceleration for a stride window approporiate to each species (e.g., kangaroo hop 2-3 times per second, to get 5-10 strides, use ~3 seconds)
- Summarised mean VDBA per stride window and then calculated mean and deviation for each individual
- Mass was calculculated as either an average (of the species or from the specific study where available) or per individual (where that data was available).

## Acknowledgements
Project was conceptualised by Chris Clemente. Data collected from various publically available sources as well as unpublished data personally provided by Jasmin Annett and Chris Clemente. Analysis conducted by Oakleigh Wilson (me). Conceptual assistance from Pasha van Bijlert and Pranav Minasandra.

## Prior attempts / Legacy code
This project was under active exploration for multiple years and included several analyses that were not deemed fruitful. For example, one version of the analysis we tried was isolating only locomotion events from the datasets and then contrasting this with acceleration as measured from simulation and motion tracking. However, while interesting, this was not pursued. Data, code, and results for this attempt is retained in the repository for legacy purposes.


