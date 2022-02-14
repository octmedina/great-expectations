Octavio and Luis Met on Friday 3 December 2021

# Discussed: 

Non-representative samples are a fact of life. Our goal is to explore the array of statistical techniques that allow to overcome data quality concerns of **representativeness** (including non-response bias) and **item non-response**. Idea is to use the _Indice de Confianza del Consumidor_ (ICC) as a test case. It is a telephone survey, with a lot of quality issues (non-response, etc). The question is whether we can improve this - and we want to find a case in which it matters.

We consider two avenues to improvement:

- **Address item non-response to improve sample size**: many items in the survey have missings. This reduces the effective sample size, and thus generate imprecise. However, we can use other items to address this, since they have a lot of correlation: Item Response theory, factor analysis, or missing data imputation. Key caveat: this should only work if non-response results from low effect.
    - Empirically explore what is the 'best' way to adjust the missing data? Does imputation or composite indicatores improve the predictive power?
- **Address representativeness**: use auxiliary information (from EPA, Census) as a frame to calibrate the sample:
  - Find an area (e.g. province) where representativeness is really an issue, and find an application that is interesting (more on this below).
  - Explore the array of techniques available (Raking, MRP, etc), and check which one works better. 
  - We also want to explore which variables matter for representativeness. We are also interested in using **political variables** (past vote or ideology), a unique feature of ICC. The key is here: should political variables be a criterion for the representativeness of expectation surveys? If we show that by post-stratifying by ideology we improve the estimates, then they should. 

We also discussed to create something interactive to show our results to the nation. A map of economic sentiment, for example. 

We agreed on starting working after Christmas and allocate time to that time. We can then define a schedule for the project. 


# To-do's:

- Create a github repo. 
- Start playing with EPA and ICC for 2019, and test models:
    - Document correlation between items -- potential for filling missing
    - Explore different versions of MRP.
    - Explore missing data patterns, both over time and across items
    - Document covariates of interest and select post-strat variables: demographics, politics, etc. Document which one are available. 
- Gather code and packages on MRP:
    - https://bookdown.org/jl5522/MRP-case-studies/
    - http://joshuamccrain.com/index.php/mrp-in-r/
    - dgo: Dynamic Estimation of Group-Level Opinion -Dynamic Group level IRT: https://mran.revolutionanalytics.com/snapshot/2020-04-25/web/packages/dgo/index.html based on paper https://www.cambridge.org/core/journals/political-analysis/article/abs/dynamic-estimation-of-latent-opinion-using-a-hierarchical-grouplevel-irt-model/6EBDF2FC39CEEEE1108CEC799FC9B7BC 
    - AutoMrp : https://lucasleemann.files.wordpress.com/2020/07/automrp-r2pa.pdf https://cran.r-project.org/web/packages/autoMrP/vignettes/autoMrP_vignette.pdf
    - https://github.com/joekroese/tidymrp/
- Download **papers** that apply MRP to inspire ourselves: 
    - https://www.ipw.unibe.ch/unibe/portal/fak_wiso/c_dep_sowi/inst_pw/content/e39849/e49015/e952151/e953053/e965657/MrP_LWrevision1_ger.pdf
    - http://chriswarshaw.com/papers/mrp.pdf

- Find a **benchmark** against which to assess different estimates: this is what we will use to convince people that our adjusted estimates are 'better' than direct estimation. 
  - **Basic benchmark**: compare 'direct' estimate (the mean or that unit), with the adjusted  estimates, and show that the time series is smoother. Alternatively, try some form of out of sample validation.  
  - Think of an **application** that is interesting: do our estimates allow to uncover something we did not know?
    - **Descriptive**: Does it allow to see new things? Examples
      - a. link downsized expectations with impact of the crisis (foreclosure, construction boom, housing prices, etc);
      - b. price of housing has increased since 2011 only in big city areas; but people get the feeling through the national news which reflect big cities. To what extent is their perception of house prices influenced by local market conditions, or by national?
      - c. The effect of **confinamiento** --geographically located-- on the ability to pay bills, or other measures of economic insecurity.
      - d. Anything related to Empty Spain (how is economic sentiment there, are they sad or what?) vs. creative class urban areas.
    - **Measurement power: Basic**: Predict some variable for which we know the true value. Does our adjusted estimate allow to reduce the error at predicting something for which we know the true value? Example: unemployment rate.
    - **Predictive power: More sophisticated**: most variables of interest in ICC are not available outside (there is no 'true' consumer index). do our adjusted estimates allow to predict something related to sentiment that is available at the local/province levels (unemployment, prices, house prices etc). For example:
      - Predict unemployment rates using perception of number of persons unemployed?
      - Predict matriculaciones based on people
      - Predict casas que se compra, el precio de la vivienda.
      - Predict spending for different groups, using outside benchmark of Encuesta de Presupuestos familiares.
      - Predict poverty or income inequality 


######
######
######Meeting February 13 ####

- Comentamos los resultados de las regresiones que había hecho OJM para las principales variables (precios de la vivienda; expectativa inflación, situación económica del hogar y situación del mercado laboral ). En algunos casos 
- El enfoque del documento sería caracterizar bajo qué condiciones se puede utilizar una encuesta no representativa como la ICC, y cuando puede ayudar usar información auxiliar para corregir ese sesgo. Es necesario que ocurran dos cosas:
  1. Que las variables auxiliares X influyan en la variable dependiente. Esto dependerá típicamente del item que estemos viendo y de las variables auxiliares. Mientras que los demográficos poco efecto, nosotros esperamos que las variable política tengan efecto y queremos mostrar que es así.
  2. Que exista un imbalance (o variación a lo largo del tiempo) en las variables auxiliares, por no respuesta o undersampling.

En ese caso es posible corregir el sesgo mediante la poestratificación (especialmente si los efectos son no linear o no varían por provincia) o el raking (si el e)
  
Lo que queremos documentar es a) cuando existe un problema de representatividad que se pueda corregir y b) la mejor forma de corregir. 

Para la próxima semana: 
- OJM se encargará de ahondar en la parte 1 (ver el efecto de la ideología o el voto, de la situación laboral, y documentar como varía por personas). 
- LMG se encargará de documentar 2. -caracterizar el imbalance entre variables en el ICC y la EPA, y también su variación a lo largo del tiempo. 

