---
title: "LLE: Case II"
author: "Jason Orender"
date: "2022-11-06"
output: 
  html_document: 
    keep_md: yes
---


## Introduction

This document was created by running the "Case_2_markdown.Rmd" file in RStudio.
It utilizes the script files "functions.R" and "rectify.R" within the "Code"
directory in this repository, which are used to define the functions that are
called below.  This document will create all of the synthetic data, fit the data
to a model, and then display the results.  Anyone interested is invited to clone
or fork the repository and change the settings for the purposes of
self-demonstration.  The verbiage here reflects the text in the paper, but in
some cases figures or examples that are present here were cut out of that
document in order to stay within a ten page limit.

## Generate the Data

The data generation script is in the file named "build_case_2_data.R".  It will
generate nine data sets: 1) dset, 2) dset_train, 3) dset_test, 4) dset_m5,
5) dset_m5_train, 6) dset_m5_test, 7) dset_m510, 8) dset_m510_train, and
9) dset_m510_test, .  The "dset" objects are consolidated data sets, and
"dset_train" and "dset_test" objects are mutually exclusive subsets of the "dset"
objects.  The "m5" and "m510" qualifiers indicate that the data is missing curve
#5 (m5 = "minus 5") or missing curves #5 and #10 (m510 = "minus 5 and 10"). This
document can be run as a notebook, block by block, or it can also be knitted
into an html document if desired.

The "relevant" data is taken from curves 5, 8, 9, 10, 13, 25, and 30.  They are
marked "RELEVANT" in the chart title.  The plots generated from the next code
block show 40 different curves, only 7 of which contain relevant data.  The
others are added to obfuscate the data and make the task of picking out the
relevant features and time steps harder.  The red markers on the curves show
the time steps for which all of the criteria for an event are met.  Each time
step generates an example which includes the data from the ten time steps before.

The data generation script will produce a plot of the entire data set for each
feature over each time step.  There are a large number of these plots, but the
process for generation and the explanation of the results is located below.

![](Case_2_markdown_files/figure-html/generate_data-1.png)<!-- -->![](Case_2_markdown_files/figure-html/generate_data-2.png)<!-- -->![](Case_2_markdown_files/figure-html/generate_data-3.png)<!-- -->![](Case_2_markdown_files/figure-html/generate_data-4.png)<!-- -->![](Case_2_markdown_files/figure-html/generate_data-5.png)<!-- -->![](Case_2_markdown_files/figure-html/generate_data-6.png)<!-- -->![](Case_2_markdown_files/figure-html/generate_data-7.png)<!-- -->![](Case_2_markdown_files/figure-html/generate_data-8.png)<!-- -->![](Case_2_markdown_files/figure-html/generate_data-9.png)<!-- -->![](Case_2_markdown_files/figure-html/generate_data-10.png)<!-- -->![](Case_2_markdown_files/figure-html/generate_data-11.png)<!-- -->![](Case_2_markdown_files/figure-html/generate_data-12.png)<!-- -->![](Case_2_markdown_files/figure-html/generate_data-13.png)<!-- -->![](Case_2_markdown_files/figure-html/generate_data-14.png)<!-- -->![](Case_2_markdown_files/figure-html/generate_data-15.png)<!-- -->![](Case_2_markdown_files/figure-html/generate_data-16.png)<!-- -->![](Case_2_markdown_files/figure-html/generate_data-17.png)<!-- -->![](Case_2_markdown_files/figure-html/generate_data-18.png)<!-- -->![](Case_2_markdown_files/figure-html/generate_data-19.png)<!-- -->![](Case_2_markdown_files/figure-html/generate_data-20.png)<!-- -->![](Case_2_markdown_files/figure-html/generate_data-21.png)<!-- -->![](Case_2_markdown_files/figure-html/generate_data-22.png)<!-- -->![](Case_2_markdown_files/figure-html/generate_data-23.png)<!-- -->![](Case_2_markdown_files/figure-html/generate_data-24.png)<!-- -->![](Case_2_markdown_files/figure-html/generate_data-25.png)<!-- -->![](Case_2_markdown_files/figure-html/generate_data-26.png)<!-- -->![](Case_2_markdown_files/figure-html/generate_data-27.png)<!-- -->![](Case_2_markdown_files/figure-html/generate_data-28.png)<!-- -->![](Case_2_markdown_files/figure-html/generate_data-29.png)<!-- -->![](Case_2_markdown_files/figure-html/generate_data-30.png)<!-- -->![](Case_2_markdown_files/figure-html/generate_data-31.png)<!-- -->![](Case_2_markdown_files/figure-html/generate_data-32.png)<!-- -->![](Case_2_markdown_files/figure-html/generate_data-33.png)<!-- -->![](Case_2_markdown_files/figure-html/generate_data-34.png)<!-- -->![](Case_2_markdown_files/figure-html/generate_data-35.png)<!-- -->![](Case_2_markdown_files/figure-html/generate_data-36.png)<!-- -->![](Case_2_markdown_files/figure-html/generate_data-37.png)<!-- -->![](Case_2_markdown_files/figure-html/generate_data-38.png)<!-- -->![](Case_2_markdown_files/figure-html/generate_data-39.png)<!-- -->![](Case_2_markdown_files/figure-html/generate_data-40.png)<!-- -->

## The data generation process

The following process is common to all of the synthetic data sets.

The simulated data was generated with the following requirements in mind:

1.	The data needed to be time-varying.  Each independent variable had a
    continuous stream of data that varied over time.  This was necessary to
    generate realistic scenarios that show the potential for analyzing lagged
    longitudinal data.
2.	The data needed to be range bound.  This reflected the desire to model
    realistic physical attributes, which tend to neither rise nor fall without
    end.
3.	The data needed to be complex.  In order to provide a convincing test, it
    needed to have no easily discernable pattern.
4.	The data needed to be cyclic.  Since the idea was to simulate sensor data or
    physical processes, and enough interactions needed to be generated in order
    to create a robust data set, some sort of cyclic process was necessary.

To meet these requirements, a sine function was chosen as the basic building
block for the simulated data.  Some additional details:

1.	When necessary, the Mersenne-Twister pseudorandom algorithm was used to
    generate pseudorandom numbers.  This is the default algorithm when
    generating pseudorandom numbers via the base R package.
2.	To generate complexity, multiple sine waves of varying frequency and
    amplitude were combined via superposition.
3.	The data was generated first, and then particular data streams were chosen
    and designated as the “relevant” features.
    
The procedure (the code is in the "build_case_1_data.R" script file):

1.	Generate a large number of sine waves with five possible randomly chosen
    frequencies per cycle.  Something like this was generated:  
    ![Curve Generation Step #1](../../Papers/Graphics/curve_generation_1.png)
2.  Superpose several of the curves generated in step 1.  Something like the
    curve next curve was generated by this step:  
    ![Curve Generation Step #2](../../Papers/Graphics/curve_generation_2.png)
3.	To create a diversity of values a new range was selected using a
    pseudorandom normal distribution centered on zero, standard deviation of
    0.05, and multiplied by 100. This produced a large enough diversity of
    magnitudes to demonstrate the range agnostic quality of the method.
4.	A version of the waves was shifted by a pre-determined amount to simulate
    lag.  The next graphic shows an example of this. Using lagged data simulates
    sensor data with readings that are time-late by an unknown amount, or an
    effect that has an unknown delay time between the change in reading and the
    occurrence of an event.   
    ![Curve Generation Step #3](../../Papers/Graphics/curve_generation_3.png)
5.	Assemble enough curves to have a large selection of potentially “relevant”
    and non-relevant curves.  Fig. 9 shows how the relevant curves are
    interpreted to generate events.  Note that the shifted curves determine when
    the event occurs, while the unshifted curves are matched with the time step
    in which they occurred to compose the data.  
    ![Curve Generation logical AND illustration](../../Papers/Graphics/curve_logical_AND.png)
6.	Each time step is interpreted as a separate example, and if all relevant
    curves meet the selection criteria during that time step, it is labeled as a
    “True” example, and otherwise is labeled as “False”.  The data for each
    example is taken from the unshifted curves.
7.	The data is next flattened such that a row contains the data from the
    current time step as well as the ten time steps previous (this is adjustable
    in the script).  The data from time steps zero (the current time step)
    through nine (the data nine time steps in the past) on the row for one
    example, for instance, will be the data for time steps one through ten on
    the next chronological example.  The next graphic illustrates:
    ![Curve Generation flatten](../../Papers/Graphics/curve_flatten.png)
8.	The final step is to randomize the rows to prevent any relationship between
    time steps from influencing the final outcome of the experiment.  There is
    no indication in the flattened data features where an example belongs
    chronologically with respect to the other examples.
9.	Once the data set has been created, it is split into training and testing
    sets with 70% of the data used for training and 30% of the data used for
    testing.

The procedure described generated a data set that included several relevant
features combined via a logical AND, is representative of what might be observed
with real sensor measurements and is complex enough to prevent any latent
patterns within the data from influencing the results.  The last example will
also illustrate what happens when a logical OR is introduced.

A data set with relevant features combined via a logical OR or a combination of
logical OR and logical AND will be generated in an identical way up to step 6.
Step 6 will be modified accordingly to generate the desired relationships among
the relevant variables.



## Plot the Results

The second case is a variation on the first case, but shows the degradation of
answer fidelity when not all relevant variables are present. The following specific characteristics apply:

1.	7 relevant variables are intermixed with 33 non-relevant variables, all 7 of which must be within their relevant ranges to trigger an event.
2.	Each variable is tracked over 10 previous time-steps as well as the current time step (time step zero) and as a consequence generates 11 features per variable, for a total of 440 features per example in the flattened data.
3.	There is a single time-step/feature relevant for each variable.  As a result, the ratio of non-relevant to relevant features is approximately 63:1.  The time step features are meant to describe the delayed effect once the variable enters its critical range.
4.	There are 3437 examples in the training set, 530 of which are positive examples.
5.	There are 1474 examples in the test set, 232 of which are positive examples.  All test set examples are unseen until tested against the trained model and should be considered novel data.



```
## Detecting groups...
```

```
##   Done.
```

```
## Fitting Models...
```

```
## Loading required package: rlang
```

```
## Warning: package 'rlang' was built under R version 4.2.1
```

```
## Loading required package: gglasso
```

```
## Warning: package 'gglasso' was built under R version 4.2.1
```

```
## Loading required package: randomForest
```

```
## Warning: package 'randomForest' was built under R version 4.2.1
```

```
## randomForest 4.7-1.1
```

```
## Type rfNews() to see new features/changes/bug fixes.
```

```
## 
## Attaching package: 'randomForest'
```

```
## The following object is masked from 'package:dplyr':
## 
##     combine
```

```
## Loading required package: openssl
```

```
## Warning: package 'openssl' was built under R version 4.2.1
```

```
## Linking to: OpenSSL 1.1.1k  25 Mar 2021
```

```
##   Done.
```
There is very little difference between the plots of the training and test set
performance below and those of Case I with the exception of the absent variable
#5.  The low magnitude of the coefficient was a clue that variable #5, while
relevant, was not of great importance to the final solution.  This is because
all but a handful of examples can be fully explained by the other relevant
variables.  However, if a more important relevant variable is not in the
training set, there is a considerably greater effect.  


```
## Plotting transformed data examples...
```

```
## Loaded glmnet 4.1-4
```

```
## 
## Attaching package: 'gridExtra'
```

```
## The following object is masked from 'package:randomForest':
## 
##     combine
```

```
## The following object is masked from 'package:dplyr':
## 
##     combine
```

![](Case_2_markdown_files/figure-html/transformed_data_m5_plots-1.png)<!-- -->![](Case_2_markdown_files/figure-html/transformed_data_m5_plots-2.png)<!-- -->

```
##   Done.
```


```
## Plotting un-transformed data examples...
```

![](Case_2_markdown_files/figure-html/untransformed_data_m5_plots-1.png)<!-- -->![](Case_2_markdown_files/figure-html/untransformed_data_m5_plots-2.png)<!-- -->

```
##   Done.
```

Using the same data set as the previous case, but with relevant variable #5
deleted from the data set before training yields the coefficient bar plot below.

![](Case_2_markdown_files/figure-html/transformed_data_m5_barplot-1.png)<!-- -->

![](Case_2_markdown_files/figure-html/untransformed_data_m5_barplot-1.png)<!-- -->

With both variable #5 and variable #10 missing, the plots below show that the
performance suffers and that the coefficients are far less clear about which
variables and time steps are the most relevant.  


```
## Plotting transformed data examples...
```

![](Case_2_markdown_files/figure-html/transformed_data_m510_plots-1.png)<!-- -->![](Case_2_markdown_files/figure-html/transformed_data_m510_plots-2.png)<!-- -->

```
##   Done.
```

With both variable #5 and variable #10 missing, the plots below show that the
performance suffers and that the coefficients are far less clear about which
variables and time steps are the most relevant. 


```
## Plotting un-transformed data examples...
```

![](Case_2_markdown_files/figure-html/untransformed_data_m510_plots-1.png)<!-- -->![](Case_2_markdown_files/figure-html/untransformed_data_m510_plots-2.png)<!-- -->

```
##   Done.
```

The coefficients when there is data missing are far less sparse and not nearly
as interpretable as the case I plots, but it does show that the correct
variables and time steps were still captured, even if they did not stand out
from all the other spurious returns on non-relevant variables by virtue of
coefficient size.  Because of this, the model which used the transformed data
still outperforms the one that was fitted with the original continuous data
by a large margin.

![](Case_2_markdown_files/figure-html/transformed_data_m510_barplot-1.png)<!-- -->

![](Case_2_markdown_files/figure-html/untransformed_data_m510_barplot-1.png)<!-- -->

The more missing data there is, the greater the level of noise in the final
result.  While this is inconvenient, it is also a reliable indicator regarding
whether there might be an under- or un-represented causative factor in the data,
perhaps leading an investigator to continue searching for new features to
achieve a sparse return similar to Fig. 13.
