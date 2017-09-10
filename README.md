
# Disparity Project
This repository contains work done while an intern in the Etzioni Group at Fred Hutchinson Cancer Research Center. 
The goal of this project is to explain whether the uptake of surgery and radiation had different efficacies in black versus white men 
with prostate cancer, and, in more general terms, to determine the effect of different uptake and different efficacy of a treatment 
on disparity statistics in disease mortality.

## Model
Our model for mortality is a continuous time Markov chain, where a person can transfer between states of healthy, cancerous and dead 
(in that order only), with baseline age and race specific rates (calculated from people who did not receive treatments). Additionally,
Each person is given a "c" value or a cutoff date. If a patient is diagnosed after their cutoff, they receive a treatment which 
increases mean survival by a constant multiplier, decreasing hazard by the inverse of that constant (a hazard ratio). We used uptake 
data and, assuming a monotonically increasing uptake, made a cdf of cutoff dates (with some at infinity), and thus created a treatment
distribution for black and white men. Using known parameters of age-specific incidence rate, survival rate past past diagnosis, uptake 
of treatment based on year of diagnosis, each calculated for black and white men separately, we optimized our treatment efficacy parameter
with a poisson likelihood (where a poisson distribution would give the number of deaths each year, and finding the likelihood of our 
observed data). 

## Results
Through numerous iterations of our model (originally age independent incidence, optimizing for years before 1900, optimizing for ages 
above 85, etc.), it is clear that our model indicates a different treatment effect for surgery/radiation between black and white men, 
with white men getting a significantly better efficacy. Using a likelihood ratio test between our model of different efficacies and one
optimized for the same treatment efficacy in both races, the p value attained was indistinguishable from 0 by RStudio. 

## Limitations and Next Steps
Limitations of our model include the introduction of PSA screening in the 1990s. Our model is prepared to include only one treatment, and
thus our question is truly "was there a different treatment effect of surgery and radiation among black and white men with prostate cancer
ASSUMING PSA screening had no effect on mortality." A further step in this model would be to include a second or third or nth treatment 
possibility so that it could be more applicable to real situations for real diseases. 

Furthermore, though our model fits mortality relatively well for individual birth cohorts between 1905 and 1965, for men under age 80, 
it fits poorly for ages over 80, and for entire population statistics such as age-adjusted mortality and various disparity statistics. 
For men over age 80, there is a slight age effect on treatment uptake in real data. Men past that age begin to decrease in treatment 
reception, so, when our optimization algorithm included those men, we were assuming that a certain percent were getting treatments, but 
the true percent was much lower, and our treatment efficacy was thus artificially lowered, so we had to exclude them. Our model only 
allowed for increasing probability of receiving treatment throughout time, independent of age. Because our model fails to fit men over 
age 80, who get a large increase in mortality, these errors compound and cause poor fits to population level data. Thus, another possible 
improvement would be to change the cutoff date approach in order to allow a date/age where people refuse treatment as well. 

Another potential reason our model fits population level data poorly is because of certain trends in prostate cancer mortality, such as a
seemingly constant increase in age-adjusted mortality rates before the mid 1990's, which may be due to artifact (more deaths being attributed
to prostate cancer due to inflated diagnosis of cases in the PSA screening area) or some other reason, but our model cannot fit this increase 
and thus has to try to fit a treatment during an era in which mortality increased for a couple years. One possible next step would be to find
a different cancer or non-reversible disease with clearer trends and a clear singular treatment introduction, which would fit the scope of 
our model better.


## File List
### Data Files
2000-Standard-Pop.csv: 2000 U.S. standard population data\
SEERout1980-86.csv: Case level data for survival\
agemort.csv: Data for death year and birth year specific mortality with modeled values as well\
incidence-by-age.csv: Data for age-specific incidence rates\
long-age-specific-mortality.csv: Data for creating agemort.csv, death year and age mortality rates\
mortality-age-adjusted.csv: Age adjusted population mortality data\
mortality-crude.csv: Not age adjusted population mortality data\
treatment-type-race.csv: Data for rates of receiving different treatments\
### Data Manipulation
2000-std-pop.R: Making a dataframe with standard population data\
creating-population-data.R: Turning agemort from age-specific to age-adjusted population mortality\
crude-vs-ageadjusted-exploration.R: Seeing how looking at crude versus age adjusted rates distorts population data\
defining-simulation-functions.R: Defining functions which create our analytic model of mortality\
real-world-age-specific-mortality.R: Creating a dataframe of values which we can compare our model to\
real-world-parameter-dataframe.R: Creating a dataframe which contains incidence and survival values for each race and age group\
real-world-uptake-trends.R: Creating uptake distributions for black and white men over time\
selectedyears-optimizing-treatment-effect-likelihood.R: Optimizing treatment effect parameter with a poisson likelihood method
### Reports
disparityreport-withoutagedependentincidence.Rmd: Old report before model was updated\
disparity-report.Rmd: Most current report (in progress) 
