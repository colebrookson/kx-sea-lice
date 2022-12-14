# Pseudocode

## Fit model to get parameters to simulate across

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
  null_model <- lme4::lmer(log_survival ~ S:population_name + 
                            (1|year_fac/area),
                          data = df)
```

Once the model is fit, we now have $r$, productivity at low-density, and $b$ the density-dependence parameter. 

##### Fit model to assess $\theta_t$

Since we can, we'll use the years of the dataset after salmon farms were introduced, to fit yearly values for $\theta_t$, in populations **not** exposed to salmon farms

```
# subset data to only years > salmon farm introduction, and populations that aren't exposed 
re_t <- lme4::lmer(log_survival ~ (1|year), data = df)
```

This should give a yearly estimate for $\theta_t$. 

**We now have values for $r$, $b$, and $\theta_t$.**

## Data on Sea Lice numbers 



## Simulation from Fit Parameters

We now care about within year. 

```
for i in years 
  assign (r, b, theta_t, and number of lice, select c from range 0-1)
  simulate ???
  # fit model 
    alt_model = lme4::lmer(log_survival ~ S:population_name +
                           lice + (1|year_fac/area),
                         data = df)
  # redraw c?
end 
```




### Questions

How am I dealing with populations and areas - just as they are in the data? 

The ln(R/S) is coming from all values I've already estimated or chosen yes? 

Simulate population trajectories by keeping this value (from above point) and continuing on in the sim?




Draw from residual distribution around fitted model predictions for each spawner values 

area within year, you just draw from the distribution

three loops 

LRT - 

Do it up to 1

One challenge is that models fail to fit -- if random effect draws fail 

if the proportion of farm affected is really low, possibly think about it then 

