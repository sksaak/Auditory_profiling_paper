# "A flexible data-driven audiological patient stratification method for deriving auditory profiles"

### Samira K. Saak, David Huelsmeier, Birger Kollmeier, Mareike Buhl
---------------------------------------------------------------------------------------------

## ABSTRACT
For characterizing the complexity of hearing deficits, it is important to consider different aspects of auditory functioning in addition to the audiogram. For this purpose, extensive test batteries have been developed aiming to cover all relevant aspects as defined by experts or model assumptions. However, as the assessment time of physicians is limited, such test batteries are often not used in clinical practice. Instead, fewer measures are used, which vary across clinics.

This study aimed at proposing a flexible data-driven approach for characterizing distinct patient groups (patient stratification into auditory profiles) based on one prototypical database (N = 595) containing audiogram data, loudness scaling, speech tests, and anamnesis questions. To further maintain the applicability of the auditory profiles in clinical routine, we built random forest classification models based on a reduced set of audiological measures which are often available in clinics. Different parameterizations regarding binarization strategy, cross-validation procedure, and evaluation metric were compared to determine the optimum classification model. 

Our data-driven approach, involving model-based clustering, resulted in a set of 13 patient groups, which serve as auditory profiles. The 13 auditory profiles separate patients within certain ranges across audiological measures and are audiologically plausible. Both a normal hearing profile and profiles with varying extents of hearing impairments are defined. Further, a random forest classification model with a combination of a one-vs.-all and one-vs.-one binarization strategy, 10-fold cross-validation, and the kappa evaluation metric was determined as the optimal model. With the selected model, patients can be classified into 12 of the 13 auditory profiles with adequate precision (mean across profiles = 0.9) and sensitivity (mean across profiles = 0.84). 

The proposed approach, consequently, allows generating of audiologically plausible and interpretable, data-driven clinical auditory profiles, providing an efficient way of characterizing hearing deficits, while maintaining clinical applicability. The method should by design be applicable to all audiological data sets from clinics or research, and in addition be flexible to summarize information across databases by means of profiles, as well as to expand the approach toward aided measurements, fitting parameters, and further information from databases.

----------------------------------------------------------------------------------------------
#### Link to the paper: https://www.frontiersin.org/journals/neurology/articles/10.3389/fneur.2022.959582/full#B23 
----------------------------------------------------------------------------------------------
## The code for reproducing the analysis can be found in the folders:  

## Robust_learning
Estimating the number of profiles via bootstrapping

## Generating_profiles
Generating the profiles using the learned parameters from the robust learning step

## Classification_models
Model training and evaluation (Random forests) with different parameterization (CV,evaluation metric, binarization strategy)

#### Data availability
According to the data usage agreement of the authors, the datasets analyzed in this study can only be shared upon motivated request.
