# **Identification of reindeer fine-scale foraging behaviour using tri-axial accelerometer data**

** author: "Heidi Rautiainen, with acknowledgements to Måns Karlsson for hidden Markov model"**

[https://doi.org/10.5061/dryad.8sf7m0cs7](https://doi.org/10.5061/dryad.8sf7m0cs7)

###### Manuscript: [https://doi.org/10.1186/s40462-022-00339-0](https://doi.org/10.1186/s40462-022-00339-0)

Description of data handling, accelerometer data, and behavioural data from video recordings.

## Description of files provided

All in .csv format (","-separated columns)

* Filename: "acceleration.csv" (for step 1)
* Filename: "annotations.csv" (for step 1)
* Filename: "df4_2s_allIDs.csv" (for step 2)

#### Filename: *"acceleration.csv"*

**Description:** Accelerometer data in three dimensions (x,y,z), animal id, and timestamp using axy-4 loggers from technosmart. Used for step 1 "Data handling_oneID".

\**Column names:  **

* "TagID": Accelerometer ID
* "Timestamp": H:M:S:OS
* "X"
* "Y"
* "Z"

#### Filename: *"annotations.csv"*

**Description:** Behavioural data from video recordings (labelled and exported csv from BORIS software). Used for step 1 "Data handling_oneID".

**N/A's:** default columns from BORIS output, where no notes has been made (e.g. "Description", Behavorial.category").

\**Column names:    **

* "Observation.id": Chosen observation ID
* "Observation.date": Date of observations
* "Description": N/A
* "Subject": Animal ID (named by accelerometer ID)
* "Behavior": Observered behaviour
* "Behavioral.category": Behavioural category
* "Modifiers": Observered behaviour
* "Behavior.type": STATE
* "Start..s.": Starttime of observered behaviour in seconds
* "Stop..s.": Stoptime of observered behaviour in seconds
* "Duration..s.": Duration of observered behaviour in seconds

#### Filename: *"df4\_2s\_allIDs.csv"*

**Description:** Example dataset of processed data and variables ued (following steps in "Data handling_oneID") from 19 individuals. Used for step 2 "Training-and-validation-HMM".

\**Description for column names **given for acceleration (X, Y and Z), static acceleration (stX, stY, stZ) and dynamic acceleration (dyX, dyY, dyZ):

* "samp": Window number (segment)
* "mrot_": Median (of rotated values) for each axis in each window
* "drot_": Third quantile (Q3) subtracted by the first quantile (Q1) for each axis
* "meanrot_": Mean value for each axis in each window
* "minrot_": Minimum value for each axis in each window
* "maxrot_": Maximum value for each axis in each window
* "sdrot_": Standard deviation for each axis in each window
* "mroll"/"mpitch":  Median for roll and pitch in each window
* "droll"/"dpitch":  Third quantile (Q3) subtracted by the first quantile (Q1) for roll and pitch in each window
* "ID":  Animal ID = Accelerometer ID
* "Timestamp":  Y-M-D H:M.S
* "Modifiers":  Behaviour

## Description of data handling

### Step 1: Data handling\_oneID

**Files**: "annotations.csv" and "acceleration.csv"

Example data given for one individual for data processing.

##### a.  Examples of initial data handling

* Basic data handling

##### b.  Merging of acceleration and behaviour

* Fix date and time format
* Add behaviours (or modifiers) to acceleration file
* Data merging ends (acceleration for each start-stop of behaviour)

##### c.  Filtering, computation of variables and rotation matrices

* Filtering using running mean
* Raw acceleration in bits (X, Y, Z)
* Static acceleration (stX, stY, stZ)
* Dynamic acceleration (dX, dY, dZ)
* Pitch (rotation around Y axis)
* Roll (rotation around X axis)
* Rotation matrix around X to account for rotation around the neck

##### d.  Segmentation of data to two second windows with the most common behavior

* 2, 3 or 5 second windows with most common behavior
* Example only for using 2 sec window

### Step 2: Training-and-validation-HMM

**File**: "df4_2s_allIDs.csv"

Example code given for the final model (hidden Markov model) for 19 individuals. Random forests, Support vector machines or forward feature selection not included here (see packages used in manuscript). Example data is given for a subset of 200 observations for each individual for demonstration to enable fast processing. 

## Code and software

Behaviours labelled from video recordings using BORIS Version 7.9.8. Data handling, model training and validation performed in R (R version 4.0.3 and RStudio version 1.3.1093). Note: Although not part of this manuscript, predictions on new individuals done using Anaconda [https://www.anaconda.com/products/distribution](https://www.anaconda.com/products/distribution) to enable fast processing of data. 
