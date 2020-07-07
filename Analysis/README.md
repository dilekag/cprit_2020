# Analysis Plan: General Overview
- Calculate expected lung cancer frequencies for each TX county
  - Calculate SIR’s for each county 
  - Plot SIR’s over time using INLA modeling 
    - Redo for each histologic type
- Calculate expected COVID-19 mortality rates* for each TX county
  - Calculate SIR’s for each county 
  - Plot SIR’s over time using INLA modeling 
- Calculate exceedance probabilities for the different counties 
- Determine whether there’s an association between the SIR’s of the counties for COVID-19 and for lung cancer (2017)
- Real lung cancer rates for Texas over time across counties 
  - Cases by histologic type
- Create a model using COVID-19 mortality rate as the dependent variable and prevalence of cancer patients to see if it’s equivalent to 0 
  - Repeat and consider some additional covariates such as rurality, etc.
  - Repeat using cancer as the dependent variable 
- Bivariate map comparing COVID-19 cases/ 100,000 people as well as lung cancer/ 100,000 people 
  - Same map but COVID-19 mortality rate 
- Attempt using some spatial span/ clustering R packages to see if there are certain areas/ clusters where any of these rates/ covariates seem to play a particularly large role

*There’s no literature suggesting that having cancer increases the likelihood of contracting cancer although there is literature suggesting that it increases the likelihood of passing away*
