library(dbscan)
library(cluster)
library(ggplot2)
library(dplyr)



clust_data <- combined_vf_groups[,c("fuel_type", "HP", "construction_start")]
clust_data$fuel_type <- factor(clust_data$fuel_type)
clust_data <- unique(clust_data)

## Plot Before Clustering 
ggplot(clust_data, aes(x=construction_start, y=HP)) +
  geom_point()

ggplot(clust_data, aes(x=construction_start, y=HP,color=fuel_type)) +
  geom_point() +
  labs(color="Fuel Type") +
  theme_minimal()


## Distance Matrix 
gower_dist <- daisy(clust_data, metric = "gower")

## Huertsitc
MinPts <- 2* 3 - 1 # hueristic
kdist_db_all <- kNNdistplot(gower_dist, k = MinPts) 
abline(h=0.04, col="red", lwd=2)  
eps <- 0.04

db <- dbscan::dbscan(gower_dist, eps = eps, minPts = MinPts) 
sil_db <- silhouette(db$cluster, gower_dist)
summary(sil_db)
avg.silwidth <- mean(sil_db[, 3], na.rm = TRUE)



### Joining with the data 
clust_data$cluster <- db$cluster
clust_data$cluster <- as.factor(clust_data$cluster)

ggplot(clust_data, aes(x=construction_start, y=HP,color=cluster)) +
  geom_point() +
  labs(color="cluster")



data_WithDBSCAN_Groups <- merge(combined_vf_groups, clust_data, by = c('HP', 'construction_start', 'fuel_type'), all.x = TRUE)


## Re ordering the clusters based on the mean value 
# Calculate the mean value for each cluster
cluster_means <- data_WithDBSCAN_Groups %>%
  group_by(cluster) %>%
  summarise(mean_value = weighted.mean(schadenaufwand, jahreseinheiten)) %>%
  arrange(mean_value) %>%
  mutate(ordered_cluster = row_number())

# Join back to the original data to get the ordered cluster
data_WithDBSCAN_Groups <- data_WithDBSCAN_Groups %>%
  left_join(cluster_means, by = "cluster") %>%
  select(-mean_value)


#### centers


get_mode <- function(x) {
  tbl <- table(x)
  max_freq <- max(tbl)
  mode_value <- names(tbl[tbl == max_freq])
  return(mode_value)
}

db_groups_centers <- data_WithDBSCAN_Groups %>%
  group_by(ordered_cluster) %>%
  summarise(
    size = n(),
    old_cluster = get_mode(cluster),
    weighted_mean_claims = round(weighted.mean(schadenaufwand,jahreseinheiten),2),
    mean_HP = round(mean(HP),2),
    mean_construction_start = ceiling(mean(construction_start)),
    mode_fuel_type = get_mode(fuel_type)
  )

db_groups_centers <- data.frame(db_groups_centers)


## Plotting 

## For plotting 
mapping <- c('0' = 1, '7' = 2, '5' = 3, '1' = 4, '3' = 5, '8' = 6, '4' = 7, '2' = 8, '6' = 9)
clust_data$cluster <- as.character(clust_data$cluster)
clust_data$ord_cluster <- mapping[clust_data$cluster]
clust_data$ord_cluster <- as.factor(clust_data$ord_cluster)

# Custom color palette
color_palette <- c(
  "#D55E00", # dark orange
  "#E69F00", # orange
  "#F0E442", # yellow
  "#009E73", # teal
  "#56B4E9", # sky blue
  "#0072B2", # blue
  "#CC79A7", # pink
  "#999999", # grey
  "#6F6F6F"  # dark grey
)

# First plot with clustering
p1 <- ggplot(clust_data, aes(x = construction_start, y = HP, color = factor(ord_cluster))) +
  geom_point() +
  scale_color_manual(values = color_palette) +
  labs(color = "Clusters") +
  theme_minimal()

# Second plot without clustering
p2 <- ggplot(clust_data, aes(x = construction_start, y = HP)) +
  geom_point() +
  theme_minimal()

# Desired dimensions (width and height) for the plots
plot_width <- 6  # in inches
plot_height <- 8  # in inches

# Save the first plot
ggsave("VD_after.pdf", plot = p1, width = plot_width, height = plot_height, dpi = 300)

# Save the second plot
ggsave("VD_before.pdf", plot = p2, width = plot_width, height = plot_height, dpi = 300)


###################### Evaluation #################################

                   
                                  
data_WithDBSCAN_Groups$ordered_cluster <- factor(data_WithDBSCAN_Groups$ordered_cluster, levels = sort(unique(data_WithDBSCAN_Groups$ordered_cluster)), ordered = TRUE)


m4_feats <-  c("abstellplatz", "soziodaten_arbeitslosenquote" , "soziodaten_bevoelkerungsdichte" , "soziodaten_pkw_je_einwohnerzahl"                   
               , "abw_halter"     , "bundesland"          , "diff_nutzeralter_kasko"         
               , "fahrzeugalter"  , "fahrzeugalter_erwerb", "freie_werkstattwahl"            
               , "gap_deckung"    , "jahresfahrleistung"  , "kennzeichentyp"                 
               , "laufzeit"       , "nutzergruppe"        , "nutzungsart"                    
               , "rabattschutz"   , "regionalklasse_vk"   , "TK" ,"VK"                
               , "schutzbrief"    , "sf_klasse"           , "statistikjahr"                  
               , "tarifgruppe"    , "tarifvariante"       , "ordered_cluster"                   
               , "wohngebaeude"   , "zahlungsart"         , "zahlungsweise"
               , "schadenaufwand" )



train_data_m4 <- data_WithDBSCAN_Groups[data_WithDBSCAN_Groups$dataset== "train", which(names(data_WithDBSCAN_Groups) %in% m4_feats)]
val_data_m4 <- data_WithDBSCAN_Groups[data_WithDBSCAN_Groups$dataset== "validation",which(names(data_WithDBSCAN_Groups) %in% m4_feats)]
test_data_m4 <- data_WithDBSCAN_Groups[data_WithDBSCAN_Groups$dataset== "test",which(names(data_WithDBSCAN_Groups) %in% m4_feats)]

options(warn = -1)
par(mar = c(5.1, 4.1, 4.1, 2.1))
tweedie_profile_m4 <- tweedie.profile(schadenaufwand ~ .,    data = train_data_m4, 
                                      weights = train_data_vf$jahreseinheiten,
                                      do.ci=TRUE, do.plot=TRUE, link.power = 0)
summary(tweedie_profile_m4)
plot(tweedie_profile_m4)
plot(tweedie_profile_m4, xlab = "Index Parameter" , ylab = "Profile Log-Likelihood")

tweedie_profile_m4$xi.max #---> 1.508163
tweedie_profile_m4
options(warn = 0)


fit_tweedie_m4 <- glm(schadenaufwand ~ ., family = tweedie(var.power = tweedie_profile_m4$xi.max , link.power = 0),
                      data = train_data_m4, weights = train_data_vf$jahreseinheiten)

preds_m4_train <- exp(predict(fit_tweedie_m4, newdata = train_data_m4))
preds_m4_val <- exp(predict(fit_tweedie_m4, newdata = val_data_m4))
preds_m4_test <- exp(predict(fit_tweedie_m4, newdata = test_data_m4))

train_m4_mse <- mean((preds_m4_train - train_data_m4$schadenaufwand)^2) 
val_m4_mse <- mean((preds_m4_val - val_data_m4$schadenaufwand)^2) 
test_m4_mse <- mean((preds_m4_test - test_data_m4$schadenaufwand)^2) 


train_m4_wmse <- calc_wmse(train_data_m4$schadenaufwand, preds_m4_train, data_WithDBSCAN_Groups[data_WithDBSCAN_Groups$dataset== "train", "jahreseinheiten"]) #490840.9
val_m4_wmse <- calc_wmse(val_data_m4$schadenaufwand, preds_m4_val, data_WithDBSCAN_Groups[data_WithDBSCAN_Groups$dataset== "validation", "jahreseinheiten"]) #523957.8
test_m4_wmse <- calc_wmse(test_data_m4$schadenaufwand, preds_m4_test, data_WithDBSCAN_Groups[data_WithDBSCAN_Groups$dataset== "test", "jahreseinheiten"]) #513182.2


