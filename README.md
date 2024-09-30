# RSV_Flu_interaction
This repository contains the data and code used to analyze RSV and Flu interaction in the United States.

## Data

* FluView Data: accesed July 8, 2021
source: https://gis.cdc.gov/grasp/fluview/fluportaldashboard.html
* RSV data were requested from CDC’s National Respiratory and Enteric Virus Surveillance System (NREVSS) in 2021. These RSV surveillance data are currently available at https://healthdata.gov/dataset/Respiratory-Syncytial-Virus-Laboratory-Data-NREVSS/7zgq-bp9w/data.![image](https://github.com/user-attachments/assets/c8a9b999-420c-4a5e-b045-dc426a56429f)

* Sequence data was obtained from GISAID.
* See also [here](Genetic_analysis/README.md).

## Prerequisites
* R, and the R packages listed [here](./src/libraries.R).


## Instructions
* In each folder:
    - In the root directory, start with sepecific order.
    - This figure.R contains all figures used in manuscript.


## Additional Details

* BEAST was used for the analysis displayed in Figure 2. For details, see [here](Genetic_analysis/README.md).
* Time series analysis was conducted in [here](Data_prep/).
* Parameter estimation for statistical model was conducted in R on a cluster due to computational time considerations. For conveneience, this step can be by-passed and all the plots can be built from pre-computed R objects (located in `model/results`). See [here](model/README.md) for additional details.
