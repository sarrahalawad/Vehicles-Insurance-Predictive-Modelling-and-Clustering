
# ------------------------------------------------------------------------------
# Generate Vehicle Effect GLM Tweedie Model
# ------------------------------------------------------------------------------
library(tweedie)
library(statmod)

## Updating train-val-test data
vehicle_feats <- c("construction_start","fuel_type","HP","schadenaufwand")
train_data_groups <- train_data[, which(names(train_data) %in% vehicle_feats)]
val_data_groups <- validation_data[, which(names(validation_data) %in% vehicle_feats )]
test_data_groups <- test_data[, which(names(test_data) %in% vehicle_feats)]

#### Looking at the Interactions between vehicle features to define the significant interactions

# Adding interaction between fuel_type & construction_start
glm_model1 <- glm(schadenaufwand ~ fuel_type + HP + construction_start + 
                    fuel_type:construction_start, 
                  data = train_data_groups, weights = train_data$jahreseinheiten,  
                  family = tweedie(var.power = 1.5, link.power = 0))
fit1_summary <- summary(glm_model1)
coef_fit1 <- fit1_summary$coefficients
# Filter for significant predictors (p < 0.05)
significant_coef1 <- coef_fit1[coef_fit1[, 4] < 0.05, ]
# Print significant predictors
print(significant_coef1)


# Adding interaction between fuel_type & HP
glm_model2 <- glm(schadenaufwand ~ fuel_type + HP + construction_start + fuel_type:HP, 
                  data = train_data_groups, weights = train_data$jahreseinheiten, 
                  family = tweedie(var.power = 1.5, link.power = 0))
fit2_summary <- summary(glm_model2)
coef_fit2 <- fit2_summary$coefficients
# Filter for significant predictors (p < 0.05)
significant_coef2 <- coef_fit2[coef_fit2[, 4] < 0.05, ]
# Print significant predictors
print(significant_coef2)



## Adding interaction between HP & construction_start
glm_model3 <- glm(schadenaufwand ~ fuel_type + HP + construction_start + 
                    HP:construction_start, 
                  data = train_data_groups, weights = train_data$jahreseinheiten,  
                  family = tweedie(var.power = 1.5, link.power = 0))
fit3_summary <- summary(glm_model3)
coef_fit3 <- fit3_summary$coefficients
# Filter for significant predictors (p < 0.05)
significant_coef3 <- coef_fit3[coef_fit3[, 4] < 0.05, ]
# Print significant predictors
print(significant_coef3)


### Tweedie Model: vehicle features (including interaction terms) and insurance data features (except the GDV Groupus "typklasse_vk")
train_data_vf <- train_data
val_data_vf <- validation_data
test_data_vf <- test_data

## For Combined interaction 
train_data_vf$isBenzin <- ifelse(train_data_vf$fuel_type == "Benzin", 1, 0)
train_data_vf$isDiesel <- ifelse(train_data_vf$fuel_type == "Diesel", 1, 0)
train_data_vf$isDiesel_Elektro <- ifelse(train_data_vf$fuel_type == "Diesel/Elektro", 1, 0)
train_data_vf$isElektro <- ifelse(train_data_vf$fuel_type == "Elektro", 1, 0)
train_data_vf$isOthers <- ifelse(train_data_vf$fuel_type == "others", 1, 0)

val_data_vf$isBenzin <- ifelse(val_data_vf$fuel_type == "Benzin", 1, 0)
val_data_vf$isDiesel <- ifelse(val_data_vf$fuel_type == "Diesel", 1, 0)
val_data_vf$isDiesel_Elektro <- ifelse(val_data_vf$fuel_type == "Diesel/Elektro", 1, 0)
val_data_vf$isElektro <- ifelse(val_data_vf$fuel_type == "Elektro", 1, 0)
val_data_vf$isOthers <- ifelse(val_data_vf$fuel_type == "others", 1, 0)

test_data_vf$isBenzin <- ifelse(test_data_vf$fuel_type == "Benzin", 1, 0)
test_data_vf$isDiesel <- ifelse(test_data_vf$fuel_type == "Diesel", 1, 0)
test_data_vf$isDiesel_Elektro <- ifelse(test_data_vf$fuel_type == "Diesel/Elektro", 1, 0)
test_data_vf$isElektro <- ifelse(test_data_vf$fuel_type == "Elektro", 1, 0)
test_data_vf$isOthers <- ifelse(test_data_vf$fuel_type == "others", 1, 0)




# Choosing the index param with the lieklihood profile 
options(warn = -1)
par(mar = c(5.1, 4.1, 4.1, 2.1))
tweedie_profile_g <- tweedie.profile(schadenaufwand ~ fuel_type + HP + construction_start 
                                    + construction_start * isBenzin
                                    + construction_start * isDiesel
                                    + construction_start * isDiesel_Elektro
                                    + HP * isDiesel
                                    + HP * isDiesel_Elektro
                                    + HP * isElektro
                                    + HP * isOthers
                                    + HP:construction_start 
                                    + abstellplatz + abw_halter + bundesland + diff_nutzeralter_kasko 
                                    + fahrzeugalter + fahrzeugalter_erwerb + freie_werkstattwahl + gap_deckung 
                                    + jahresfahrleistung + kennzeichentyp + laufzeit + nutzergruppe + nutzungsart
                                    + rabattschutz + regionalklasse_vk + schutzbrief + sf_klasse + statistikjahr                     
                                    + soziodaten_arbeitslosenquote + soziodaten_bevoelkerungsdichte  
                                    + soziodaten_pkw_je_einwohnerzahl + tarifgruppe + tarifvariante                   
                                    + wohngebaeude + zahlungsart + zahlungsweise + VK + TK,
                                    data = train_data_vf, weights = train_data_vf$jahreseinheiten,
                                    do.ci=TRUE, do.plot=TRUE, link.power = 0)
summary(tweedie_profile_g)
plot(tweedie_profile_g)
plot(tweedie_profile_g, xlab = "Index Parameter" , ylab = "Profile Log-Likelihood")

tweedie_profile_g$xi.max #---> 1.508163
tweedie_profile_g
options(warn = 0)



## Fitting the GLM Model Combined interactions
glm_modelc <- glm(schadenaufwand ~ fuel_type + HP + construction_start 
                   + construction_start * isBenzin
                   + construction_start * isDiesel
                   + construction_start * isDiesel_Elektro
                   + HP * isDiesel
                   + HP * isDiesel_Elektro
                   + HP * isElektro
                   + HP * isOthers
                   + HP:construction_start 
                   + abstellplatz + abw_halter + bundesland + diff_nutzeralter_kasko 
                   + fahrzeugalter + fahrzeugalter_erwerb + freie_werkstattwahl + gap_deckung 
                   + jahresfahrleistung + kennzeichentyp + laufzeit + nutzergruppe + nutzungsart
                   + rabattschutz + regionalklasse_vk + schutzbrief + sf_klasse + statistikjahr                     
                   + soziodaten_arbeitslosenquote + soziodaten_bevoelkerungsdichte  
                   + soziodaten_pkw_je_einwohnerzahl + tarifgruppe + tarifvariante                   
                   + wohngebaeude + zahlungsart + zahlungsweise + VK + TK,
                   data = train_data_vf, weights = train_data_vf$jahreseinheiten,
                  family = tweedie(var.power = tweedie_profile_g$xi.max, link.power = 0))


### Vehicle effect
coefs <- coef(glm_modelc)
baseline <- coefs["(Intercept)"]


significant_coefs_vf <- coefs[coefs[,4]< 0.05, ]
sorted_significant_coefs_vf <- significant_coefs_vf[order(significant_coefs_vf[, 4]), ]
print(sorted_significant_coefs_vf)

sorted_estimate_coefs_vf <- round(coefs[order(-(abs(coefs[, 1]))), ],4)
print(sorted_estimate_coefs_vf)





## Main Effects
HP_effect <- coefs["HP"] * train_data_vf$HP
construction_start_effect <- coefs["construction_start"] * train_data_vf$construction_start

fuel_type_effect <- sapply(1:nrow(train_data_vf), function(i) {
  level <- train_data_vf$fuel_type[i]
  
  # Handling the reference level 
  if (level == "Benzin") {
    return(0)
  }
  
  coef_name <- paste0("fuel_type", level)
  
  # Return the coefficient if it exists, otherwise return 0
  if (coef_name %in% names(coefs)) {
    return(coefs[coef_name])
  } else {
    return(0)
  }
})


## Interaction Effect

interaction_effect_construction_start_isBenzin <- coefs["construction_start:isBenzin"] * train_data_vf$construction_start * train_data_vf$isBenzin
interaction_effect_construction_start_isDiesel <- coefs["construction_start:isDiesel"] * train_data_vf$construction_start * train_data_vf$isDiesel
interaction_effect_construction_start_isDiesel_Elektro <- coefs["construction_start:isDiesel_Elektro"] * train_data_vf$construction_start * train_data_vf$isDiesel_Elektro

interaction_effect_HP_isDiesel <- coefs["HP:isDiesel"] * train_data_vf$HP * train_data_vf$isDiesel
interaction_effect_HP_isDiesel_Elektro <- coefs["HP:isDiesel_Elektro"] * train_data_vf$HP * train_data_vf$isDiesel_Elektro
interaction_effect_HP_isElektro <- coefs["HP:isElektro"] * train_data_vf$HP * train_data_vf$isElektro
interaction_effect_HP_isOthers <- coefs["HP:isOthers"] * train_data_vf$HP * train_data_vf$isOthers

interaction_effect_HP_construction_start<- coefs["HP:construction_start"] * train_data_vf$HP * train_data_vf$construction_start




## Vehicle Effect Equation

vehicle_effect <- baseline+fuel_type_effect + HP_effect + construction_start_effect +interaction_effect_construction_start_isBenzin+
  interaction_effect_construction_start_isDiesel + interaction_effect_construction_start_isDiesel_Elektro+
  interaction_effect_HP_isDiesel+interaction_effect_HP_isDiesel_Elektro+
  interaction_effect_HP_isElektro+interaction_effect_HP_isOthers

train_data_vf$vehicle_effect <- vehicle_effect
train_data$vehicle_effect <- vehicle_effect
## Exploring Vehicle effect
hist(train_data_vf$vehicle_effect)
range(train_data_vf$vehicle_effect)
summary(train_data_vf$vehicle_effect)

### Generating vehicle effect for the validation and test data 

# For Validation Data 
## Main Effects
val_HP_effect <- coefs["HP"] * val_data_vf$HP
val_construction_start_effect <- coefs["construction_start"] * val_data_vf$construction_start

val_fuel_type_effect <- sapply(1:nrow(val_data_vf), function(i) {
  level <- val_data_vf$fuel_type[i]
  
  # Handling the reference level ("Benzin" is the reference level)
  if (level == "Benzin") {
    return(0)
  }
  
  coef_name <- paste0("fuel_type", level)
  
  if (coef_name %in% names(coefs)) {
    return(coefs[coef_name])
  } else {
    return(0)
  }
})


## Interaction Effect

val_interaction_effect_construction_start_isBenzin <- coefs["construction_start:isBenzin"] * val_data_vf$construction_start * val_data_vf$isBenzin
val_interaction_effect_construction_start_isDiesel <- coefs["construction_start:isDiesel"] * val_data_vf$construction_start * val_data_vf$isDiesel
val_interaction_effect_construction_start_isDiesel_Elektro <- coefs["construction_start:isDiesel_Elektro"] * val_data_vf$construction_start * val_data_vf$isDiesel_Elektro

val_interaction_effect_HP_isDiesel <- coefs["HP:isDiesel"] * val_data_vf$HP * val_data_vf$isDiesel
val_interaction_effect_HP_isDiesel_Elektro <- coefs["HP:isDiesel_Elektro"] * val_data_vf$HP * val_data_vf$isDiesel_Elektro
val_interaction_effect_HP_isElektro <- coefs["HP:isElektro"] * val_data_vf$HP * val_data_vf$isElektro
val_interaction_effect_HP_isOthers <- coefs["HP:isOthers"] * val_data_vf$HP * val_data_vf$isOthers

val_interaction_effect_HP_construction_start<- coefs["HP:construction_start"] * val_data_vf$HP * val_data_vf$construction_start






vehicle_effect_val <- baseline+val_fuel_type_effect + val_HP_effect + val_construction_start_effect +val_interaction_effect_construction_start_isBenzin+
  val_interaction_effect_construction_start_isDiesel + val_interaction_effect_construction_start_isDiesel_Elektro+
  val_interaction_effect_HP_isDiesel+val_interaction_effect_HP_isDiesel_Elektro+
  val_interaction_effect_HP_isElektro+val_interaction_effect_HP_isOthers

val_data_vf$vehicle_effect <- vehicle_effect_val
validation_data$vehicle_effect <- vehicle_effect_val





# For Test Data 
## Main Effects
test_HP_effect <- coefs["HP"] * test_data_vf$HP
test_construction_start_effect <- coefs["construction_start"] * test_data_vf$construction_start

test_fuel_type_effect <- sapply(1:nrow(test_data_vf), function(i) {
  level <- test_data_vf$fuel_type[i]
  
  if (level == "Benzin") {
    return(0)
  }
  
  coef_name <- paste0("fuel_type", level)
  
  if (coef_name %in% names(coefs)) {
    return(coefs[coef_name])
  } else {
    return(0)
  }
})


## Interaction Effect

test_interaction_effect_construction_start_isBenzin <- coefs["construction_start:isBenzin"] * test_data_vf$construction_start * test_data_vf$isBenzin
test_interaction_effect_construction_start_isDiesel <- coefs["construction_start:isDiesel"] * test_data_vf$construction_start * test_data_vf$isDiesel
test_interaction_effect_construction_start_isDiesel_Elektro <- coefs["construction_start:isDiesel_Elektro"] * test_data_vf$construction_start * test_data_vf$isDiesel_Elektro

test_interaction_effect_HP_isDiesel <- coefs["HP:isDiesel"] * test_data_vf$HP * test_data_vf$isDiesel
test_interaction_effect_HP_isDiesel_Elektro <- coefs["HP:isDiesel_Elektro"] * test_data_vf$HP * test_data_vf$isDiesel_Elektro
test_interaction_effect_HP_isElektro <- coefs["HP:isElektro"] * test_data_vf$HP * test_data_vf$isElektro
test_interaction_effect_HP_isOthers <- coefs["HP:isOthers"] * test_data_vf$HP * test_data_vf$isOthers

test_interaction_effect_HP_construction_start<- coefs["HP:construction_start"] * test_data_vf$HP * test_data_vf$construction_start






vehicle_effect_test <- baseline+ test_fuel_type_effect + test_HP_effect + test_construction_start_effect +test_interaction_effect_construction_start_isBenzin+
  test_interaction_effect_construction_start_isDiesel + test_interaction_effect_construction_start_isDiesel_Elektro+
  test_interaction_effect_HP_isDiesel+test_interaction_effect_HP_isDiesel_Elektro+
  test_interaction_effect_HP_isElektro+test_interaction_effect_HP_isOthers

test_data_vf$vehicle_effect <- vehicle_effect_test
test_data$vehicle_effect <- vehicle_effect_test



# ------------------------------------------------------------------------------
# Creating Groups: Cluster Vehicle Effect 
# ------------------------------------------------------------------------------



# Ensure a unique identifier for each dataset
train_data_vf <- train_data_vf %>% mutate(id = row_number(), dataset = "train")
val_data_vf <- val_data_vf %>% mutate(id = row_number() + nrow(train_data_vf), dataset = "validation")
test_data_vf <- test_data_vf %>% mutate(id = row_number() + nrow(train_data_vf) + nrow(val_data_vf), dataset = "test")

  
  
# Combine the vehicle effects with the identifier
all_vehicle_effects <- bind_rows(
  select(train_data_vf, id, vehicle_effect),
  select(val_data_vf, id, vehicle_effect),
  select(test_data_vf, id, vehicle_effect)
)

## Quick EDA
all_vehicle_effects
summary(all_vehicle_effects$vehicle_effect)
ggplot(all_vehicle_effects, aes(x = vehicle_effect)) +
  geom_histogram(fill = "#D3D3D3", color = "black",bins=30) +
  theme_minimal()

## Choosing k for k-means
library(cluster)

data_for_kmeans <- all_vehicle_effects$vehicle_effect

# Compute and plot wss for k = 1 to k = 50 with 100 different initializations
wss_multiple <- numeric(50)
for (k in 1:50) {
  set.seed(123)
  kmeans_result <- kmeans(data_for_kmeans, centers=k, nstart=100) # 100 initializations
  wss_multiple[k] <- kmeans_result$tot.withinss
}

# Plot the curve for elbow point 
plot(1:50, wss_multiple, type="b", xlab="Number of Clusters", ylab="Within Groups Sum of Squares")

set.seed(123)
kmeans_groups <- kmeans(data_for_kmeans, centers=10, nstart=100)
all_vehicle_effects$cluster <- kmeans_groups$cluster
kmeans_groups
dist_mat <- dist(data_for_kmeans)
silhouette_widths <- silhouette(kmeans_groups$cluster, dist(data_for_kmeans))

clusters_kmeans<- kmeans_groups$cluster


library(dplyr)
train_data_vf <- left_join(train_data_vf, select(all_vehicle_effects, id, cluster), by = "id")
val_data_vf <- left_join(val_data_vf, select(all_vehicle_effects, id, cluster), by = "id")
test_data_vf <- left_join(test_data_vf, select(all_vehicle_effects, id, cluster), by = "id")

combined_vf_groups <- rbind(train_data_vf, val_data_vf, test_data_vf)
colnames(combined_vf_groups)[colnames(combined_vf_groups) == "cluster"] <- "vf_group"

## Re-ordering the VF Groups based on the mean of the vehicle effect
vf_group_means <- combined_vf_groups %>% 
  group_by(vf_group) %>% 
  summarise(mean_value = mean(vehicle_effect)) %>% 
  arrange(mean_value) %>% 
  mutate(vf_groups_ordered = row_number())

# Join back to the original data frame
combined_vf_groups <- combined_vf_groups %>%
  left_join(select(vf_group_means, vf_group, vf_groups_ordered), by = "vf_group")

## VF Groups Centers

get_mode <- function(x) {
  tbl <- table(x)
  max_freq <- max(tbl)
  mode_value <- names(tbl[tbl == max_freq])
  return(mode_value)
}

vf_groups_centers <- combined_vf_groups %>%
  group_by(vf_groups_ordered) %>%
  summarise(
    size = n(),
    weighted_mean_claims = round(weighted.mean(schadenaufwand,jahreseinheiten),2),
    mean_VehicleEffect = round(mean(vehicle_effect),2),
    mean_HP = round(mean(HP),2),
    mean_construction_start = ceiling(mean(construction_start)),
    mode_fuel_type = get_mode(fuel_type)
  )
    
vf_groups_centers <- data.frame(vf_groups_centers)

# ------------------------------------------------------------------------------
# Evaluating against GDV Groups 
# ------------------------------------------------------------------------------
library(tweedie)
library(statmod)

combined_vf_groups$vf_groups_ordered <- factor(combined_vf_groups$vf_groups_ordered, levels = sort(unique(combined_vf_groups$vf_groups_ordered)), ordered = TRUE)

m2_feats <-  c("abstellplatz", "soziodaten_arbeitslosenquote" , "soziodaten_bevoelkerungsdichte" , "soziodaten_pkw_je_einwohnerzahl"                   
                   , "abw_halter"     , "bundesland"          , "diff_nutzeralter_kasko"         
                   , "fahrzeugalter"  , "fahrzeugalter_erwerb", "freie_werkstattwahl"            
                   , "gap_deckung"    , "jahresfahrleistung"  , "kennzeichentyp"                 
                   , "laufzeit"       , "nutzergruppe"        , "nutzungsart"                    
                   , "rabattschutz"   , "regionalklasse_vk"   , "TK" ,"VK"                
                   , "schutzbrief"    , "sf_klasse"           , "statistikjahr"                  
                   , "tarifgruppe"    , "tarifvariante"       , "vf_groups_ordered"                   
                   , "wohngebaeude"   , "zahlungsart"         , "zahlungsweise"    
                   , "schadenaufwand"  )


train_data_vf_groups_m2 <- combined_vf_groups[combined_vf_groups$dataset== "train", which(names(combined_vf_groups) %in% m2_feats)]
val_data_vf_groups_m2 <- combined_vf_groups[combined_vf_groups$dataset== "validation",which(names(combined_vf_groups) %in% m2_feats)]
test_data_vf_groups_m2 <- combined_vf_groups[combined_vf_groups$dataset== "test",which(names(combined_vf_groups) %in% m2_feats)]
  
options(warn = -1)
par(mar = c(5.1, 4.1, 4.1, 2.1))
tweedie_profile_m2 <- tweedie.profile(schadenaufwand ~ .,    data = train_data_vf_groups_m2, 
                                     weights = train_data_vf$jahreseinheiten,
                                     do.ci=TRUE, do.plot=TRUE, link.power = 0)
summary(tweedie_profile_m2)
plot(tweedie_profile_m2)
plot(tweedie_profile_m2, xlab = "Index Parameter" , ylab = "Profile Log-Likelihood")

tweedie_profile_m2$xi.max #---> 1.508163
tweedie_profile_m2
options(warn = 0)


fit_tweedie_m2 <- glm(schadenaufwand ~ ., family = tweedie(var.power = tweedie_profile_m2$xi.max , link.power = 0),
                   data = train_data_vf_groups_m2, weights = train_data_vf$jahreseinheiten)

preds_m2_train <- exp(predict(fit_tweedie_m2, newdata = train_data_vf_groups_m2))
preds_m2_val <- exp(predict(fit_tweedie_m2, newdata = val_data_vf_groups_m2))
preds_m2_test <- exp(predict(fit_tweedie_m2, newdata = test_data_vf_groups_m2))

train_m2_mse <- mean((preds_m2_train - train_data_vf_groups_m2$schadenaufwand)^2) 
val_m2_mse <- mean((preds_m2_val - val_data_vf_groups_m2$schadenaufwand)^2) 
test_m2_mse <- mean((preds_m2_test - test_data_vf_groups_m2$schadenaufwand)^2) 


train_m2_wmse <- calc_wmse(train_data_vf_groups_m2$schadenaufwand, preds_m2_train, combined_vf_groups[combined_vf_groups$dataset== "train", "jahreseinheiten"]) #488994.8
val_m2_wmse <- calc_wmse(val_data_vf_groups_m2$schadenaufwand, preds_m2_val,combined_vf_groups[combined_vf_groups$dataset== "validation", "jahreseinheiten"] ) #521613.4
test_m2_wmse <- calc_wmse(test_data_vf_groups_m2$schadenaufwand, preds_m2_test, combined_vf_groups[combined_vf_groups$dataset== "test", "jahreseinheiten"]) #511115.3



### M3 All F + Vehicle Features + GDV Groups = Complete model

m3_feats <-  c("abstellplatz", "soziodaten_arbeitslosenquote" , "soziodaten_bevoelkerungsdichte" , "soziodaten_pkw_je_einwohnerzahl"                   
               , "abw_halter"     , "bundesland"          , "diff_nutzeralter_kasko"         
               , "fahrzeugalter"  , "fahrzeugalter_erwerb", "freie_werkstattwahl"            
               , "gap_deckung"    , "jahresfahrleistung"  , "kennzeichentyp"                 
               , "laufzeit"       , "nutzergruppe"        , "nutzungsart"                    
               , "rabattschutz"   , "regionalklasse_vk"   , "TK" ,"VK"                
               , "schutzbrief"    , "sf_klasse"           , "statistikjahr"                  
               , "tarifgruppe"    , "tarifvariante"       , "typklasse_vk"                   
               , "wohngebaeude"   , "zahlungsart"         , "zahlungsweise" 
               , "fuel_type"      ,"HP"                   , "construction_start"    
               , "schadenaufwand"  )



train_data_m3 <- combined_vf_groups[combined_vf_groups$dataset== "train", which(names(combined_vf_groups) %in% m3_feats)]
val_data_m3 <- combined_vf_groups[combined_vf_groups$dataset== "validation",which(names(combined_vf_groups) %in% m3_feats)]
test_data_m3 <- combined_vf_groups[combined_vf_groups$dataset== "test",which(names(combined_vf_groups) %in% m3_feats)]

options(warn = -1)
par(mar = c(5.1, 4.1, 4.1, 2.1))
tweedie_profile_m3 <- tweedie.profile(schadenaufwand ~ .,    data = train_data_m3, 
                                      weights = train_data_vf$jahreseinheiten,
                                      do.ci=TRUE, do.plot=TRUE, link.power = 0)
summary(tweedie_profile_m3)
plot(tweedie_profile_m3)
plot(tweedie_profile_m3, xlab = "Index Parameter" , ylab = "Profile Log-Likelihood")

tweedie_profile_m3$xi.max #---> 1.502041
tweedie_profile_m3
options(warn = 0)


fit_tweedie_m3 <- glm(schadenaufwand ~ ., family = tweedie(var.power = tweedie_profile_m3$xi.max , link.power = 0),
                      data = train_data_m3, weights = train_data_vf$jahreseinheiten)

preds_m3_train <- exp(predict(fit_tweedie_m3, newdata = train_data_m3))
preds_m3_val <- exp(predict(fit_tweedie_m3, newdata = val_data_m3))
preds_m3_test <- exp(predict(fit_tweedie_m3, newdata = test_data_m3))


train_m3_mse <- mean((preds_m3_train - train_data_m3$schadenaufwand)^2) #
val_m3_mse <- mean((preds_m3_val - val_data_m3$schadenaufwand)^2) #
test_m3_mse <- mean((preds_m3_test - test_data_m3$schadenaufwand)^2) #



train_m3_wmse <- calc_wmse(train_data_vf_groups_m3$schadenaufwand, preds_m3_train, combined_vf_groups[combined_vf_groups$dataset== "train", "jahreseinheiten"]) #488166.3
val_m3_wmse <- calc_wmse(val_data_vf_groups_m3$schadenaufwand, preds_m3_val,combined_vf_groups[combined_vf_groups$dataset== "validation", "jahreseinheiten"] ) #520745.3
test_m3_wmse <- calc_wmse(test_data_vf_groups_m3$schadenaufwand, preds_m3_test, combined_vf_groups[combined_vf_groups$dataset== "test", "jahreseinheiten"]) #510160.1



