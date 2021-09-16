# Additional modelling code for Rmarkdown

### LIST

**Manyglms for model comparisons**
  
  - prob_life_spp (redone)

- prob_ocean_spp (redone)

- prob_lat_spp (redone)

- prob_life_clusters_many (redone)

- prob_ocean_clusters_many (redone)

- prob_life_trait_many

- prob_ocean_trait_many


**LASSO glm1path models for significance testing**
  
  - prob_life_cluster_LASSO (redone)

- prob_ocean_cluster_LASSO (redone)

- prob_life_trait_LASSO

- prob_ocean_trait_LASSO

- prob_lat_trait_LASSO

### READ MODELS BACK IN

Species and environment matrix models - manyglm.

```{r READ IN SPP MODELS}
#/Users/tashhardy/Documents/GitHub/albacore-diet-global/outputs_figures/traitglms/prob_models

prob_life_spp = readRDS(here("outputs_figures/traitglms/prob_models/prob_life_spp.rds")) 
prob_ocean_spp = readRDS(here("outputs_figures/traitglms/prob_models/prob_ocean_spp.rds"))
prob_lat_spp = readRDS(here("outputs_figures/traitglms/prob_models/prob_lat_spp.rds"))

```

Species, cluster number and environment models - manyglms + LASSOs.

```{r READ IN CLUSTER MODELS}

#Manyglms
prob_life_clusters_many = readRDS(here("outputs_figures/traitglms/prob_models/prob_life_clusters_many.rds")) 
prob_ocean_clusters_many = readRDS(here("outputs_figures/traitglms/prob_models/prob_ocean_clusters_many.rds"))
prob_lat_clusters_many = readRDS(here("outputs_figures/traitglms/prob_models/prob_lat_clusters_many.rds"))

#LASSO glms
prob_life_cluster_LASSO = readRDS(here("outputs_figures/traitglms/prob_models/prob_life_cluster_LASSO.rds"))
prob_ocean_cluster_LASSO = readRDS(here("outputs_figures/traitglms/prob_models/prob_ocean_cluster_LASSO.rds"))
prob_lat_cluster_LASSO = readRDS(here("outputs_figures/traitglms/prob_models/prob_lat_cluster_LASSO.rds"))

```

Species, traits and environment models - manyglms + LASSOs.

```{r READ IN TRAIT MODELS}

# Manyglms
prob_life_trait_many = readRDS(here("outputs_figures/traitglms/prob_models/prob_life_trait_many.rds")) 
prob_ocean_trait_many = readRDS(here("outputs_figures/traitglms/prob_models/prob_ocean_trait_many.rds"))
prob_lat_trait_many = readRDS(here("outputs_figures/traitglms/prob_models/prob_lat_trait_many.rds"))

#LASSO glms
prob_life_trait_LASSO = readRDS(here("outputs_figures/traitglms/prob_models/prob_life_trait_LASSO.rds"))
prob_ocean_trait_LASSO = readRDS(here("outputs_figures/traitglms/prob_models/prob_ocean_trait_LASSO.rds"))
prob_lat_trait_LASSO = readRDS(here("outputs_figures/traitglms/prob_models/prob_lat_trait_LASSO.rds"))

```

Interaction models

```{r READ IN INTERACTION MODELS}
#Manyglm
prob_inter_trait_many = readRDS(here("outputs_figures/traitglms/prob_models/prob_inter_trait_many.rds"))
prob_inter_trait_many2 = readRDS(here("outputs_figures/traitglms/prob_models/prob_inter_trait_many2.rds"))

#LASSO glms
prob_inter_trait_LASSO = readRDS(here("outputs_figures/traitglms/prob_models/prob_inter_trait_LASSO.rds"))
prob_inter_trait_LASSO2 = readRDS(here("outputs_figures/traitglms/prob_models/prob_inter_trait_LASSO2.rds"))

```
