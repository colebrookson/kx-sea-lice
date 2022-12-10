# Pseudocode

## Fit model to get $r$ and $b$

First step is to get the data in the form we want to fit this model. In the dataset, each row of the data contain: 

1. Spawners ($N$) in year $t$
2. Recruits in year $t+2$
3. Area location
4. Population identifier
5. Survival (i.e. $\textrm{ln}\frac{R_{i,t+2}}{N_{i,t}}$)
6. Removal by fisheries in year 


##### Get the small data pieces to fit the model 

```
pre_farm_df <- subset full df to pre-farm years

non_affected_areas <- subset full df to non-affected areas in the affected years

```

##### Fit model to assess $r$ and $b$
```
  null_model = lme4::lmer(log_survival ~ S:population_name + 
                            (1|year_fac/area),
                          data = df)
```

Once the model is fit, we now have $r$, productivity at low-density, and $b$ the density-dependence parameter. 
