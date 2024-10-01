library(ggplot2)
library(dplyr)
library(tidyverse)

# ------------------------------------------------------------------------------
# Errors Comparison
# ------------------------------------------------------------------------------

groups_error_df <- data.frame(
  Model = c("GDV", "VFG", "GDV+VehFeat", "DBG"),
  Training_MSE = c(tweedie_train_mse, train_m2_mse, train_m3_mse,train_m4_mse),
  Validation_MSE = c(tweedie_val_mse, val_m2_mse, val_m3_mse,val_m4_mse),
  Test_MSE = c(tweedie_test_mse, test_m2_mse, test_m3_mse,test_m4_mse)
)

groups_weighted_error_df <- data.frame(
  Model = c("GDV", "VFG", "GDV+VehFeat", "DBG"),
  Training_WMSE = c(tweedie_train_wmse, train_m2_wmse, train_m3_wmse,train_m4_wmse),
  Validation_WMSE = c(tweedie_val_wmse, val_m2_wmse, val_m3_wmse,val_m4_wmse),
  Test_WMSE = c(tweedie_test_wmse, test_m2_wmse, test_m3_wmse,test_m4_wmse)
)

# View the data frame
print(groups_error_df)
print(groups_weighted_error_df)


########

#### Preparing data for the comparision plots (Prediction on test data)
test_preds_m1 <- exp(predict(fit_tweedie, newdata = test_data_models))
test_preds_m2 <- exp(predict(fit_tweedie_m2, newdata = test_data_vf_groups_m2))
test_preds_m3 <- exp(predict(fit_tweedie_m3, newdata = test_data_m3))
test_preds_m4 <- exp(predict(fit_tweedie_m4, newdata = test_data_m4))


test_data_vf_groups$preds_m1 <- test_preds_m1
test_data_vf_groups$preds_m2 <- test_preds_m2
test_data_vf_groups$preds_m3 <- test_preds_m3
test_data_vf_groups$preds_m4 <- test_preds_m4

test_data_WithDBSCAN_Groups <- data_WithDBSCAN_Groups[data_WithDBSCAN_Groups$dataset=="test",]
test_data_WithDBSCAN_Groups$preds <- test_preds_m4

# ------------------------------------------------------------------------------
# Comparison Graphs
# ------------------------------------------------------------------------------

## p_GDV 
p_GDV <- test_data_vf_groups %>%
  group_by(typklasse_vk) %>%
  summarise(
    weighted_avg_claims_true = round(weighted.mean(schadenaufwand, jahreseinheiten), 2),
    weighted_avg_claims_pred_m1 = round(weighted.mean(preds_m1, jahreseinheiten), 2),
    count = n() # Add a count of observations
  )

p_GDV <- data.frame(p_GDV)

# Reshape data for ggplot
p_GDV_long <- p_GDV %>%
  gather(key = "Type", value = "Weighted_Mean", -typklasse_vk, -count)

# Determine the scaling factor for the secondary axis
scaling_factor <- max(p_GDV_long$Weighted_Mean) / max(p_GDV$count)

# Plot with secondary axis for counts
ggplot(p_GDV_long, aes(x = typklasse_vk)) +
  geom_line(aes(y = Weighted_Mean, color = Type, group = Type)) +
  geom_point(aes(y = Weighted_Mean, color = Type)) +
  geom_bar(aes(y = count * scaling_factor, group = 1), stat = "identity", alpha = 0.3) +
  scale_y_continuous(
    "Weighted Mean",
    sec.axis = sec_axis(~ . / scaling_factor, name = "Count of Observations")
  ) +
  scale_color_hue(labels = c("Predicted Claims (Model 1)", "Observed Claims")) +
  labs(color = "Type") +
  xlab("GDV Groups") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



## p_VFG
# Calculate the number of observations in each cluster
p_VFG <- test_data_vf_groups %>%
  group_by(vf_groups_ordered) %>%
  summarise(
    weighted_avg_claims_true = round(weighted.mean(schadenaufwand, jahreseinheiten), 2),
    weighted_avg_claims_pred_m2 = round(weighted.mean(preds_m2, jahreseinheiten), 2),
    count = n() 
  )

p_VFG <- data.frame(p_VFG)

# Reshape data for ggplot
p_VFG_long <- p_VFG %>%
  gather(key = "Type", value = "Weighted_Mean", -vf_groups_ordered, -count)

# Determine the scaling factor for the secondary axis
scaling_factor <- max(p_VFG_long$Weighted_Mean) / max(p_VFG$count)

# Plot with secondary axis for counts
ggplot(p_VFG_long, aes(x = vf_groups_ordered)) +
  geom_line(aes(y = Weighted_Mean, color = Type, group = Type)) +
  geom_point(aes(y = Weighted_Mean, color = Type)) +
  geom_bar(aes(y = count * scaling_factor, group = 1), stat = "identity", alpha = 0.3) +
  scale_y_continuous(
    "Weighted Mean",
    sec.axis = sec_axis(~ . / scaling_factor, name = "Count of Observations")
  ) +
  scale_color_hue(labels = c("Predicted Claims (Model 2)", "Observed Claims")) +
  labs(color = "Type") +
  xlab("Vehicel Effect Groups") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#############



## P_DBG
# Calculate the number of observations in each cluster
p_DBG <- test_data_WithDBSCAN_Groups %>%
  group_by(ordered_cluster) %>%
  summarise(
    weighted_avg_claims_true = round(weighted.mean(schadenaufwand, jahreseinheiten), 2),
    weighted_avg_claims_pred = round(weighted.mean(preds, jahreseinheiten), 2),
    count = n() # Add a count of observations
  )

p_DBG <- data.frame(p_DBG)

# Reshape data for ggplot
p_DBG_long <- p_DBG %>%
  gather(key = "Type", value = "Weighted_Mean", -ordered_cluster, -count)

# Determine the scaling factor for the secondary axis
scaling_factor <- max(p_DBG_long$Weighted_Mean) / max(p_DBG$count)

# Plot with secondary axis for counts
ggplot(p_DBG_long, aes(x = ordered_cluster)) +
  geom_line(aes(y = Weighted_Mean, color = Type, group = Type)) +
  geom_point(aes(y = Weighted_Mean, color = Type)) +
  geom_bar(aes(y = count * scaling_factor, group = 1), stat = "identity", alpha = 0.3) +
  scale_y_continuous(
    "Weighted Mean",
    sec.axis = sec_axis(~ . / scaling_factor, name = "Count of Observations")
  ) +
  scale_color_hue(labels = c("Predicted Claims (Model 3)", "Observed Claims")) +
  labs(color = "Type") +
  xlab("DBSCAN Clusters") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



###################### M1 & M4 (GDV Groups Models)

# Calculate the mean residual for each group
res_stats_GDV <- test_data_vf_groups %>%
  group_by(typklasse_vk) %>%
  summarise(mean_residual = weighted.mean(res_test_m1, jahreseinheiten))

res_stats_GDV_comb <- test_data_vf_groups %>%
  group_by(typklasse_vk) %>%
  summarise(mean_residual = weighted.mean(res_test_m3, jahreseinheiten))

### Combining Residuals per GDV groups in M1 and M3 (M4 for the thesis writing)
res_stats_GDV$source <- "Residuals in Model 1"
res_stats_GDV_comb$source <- "Residuals in Model 4"
res_stats_GD <- rbind(res_stats_GDV, res_stats_GDV_comb)

# Calculate the count of observations for the groups
count_data <- test_data_vf_groups %>%
  group_by(typklasse_vk) %>%
  summarise(count = n()) %>%
  ungroup()

# Determine the scaling factor for the secondary axis
scaling_factor <- max(res_stats_GD$mean_residual) / max(count_data$count)

# Plot with secondary axis for counts
ggplot(res_stats_GD, aes(x = typklasse_vk, y = mean_residual, color = source, group = source)) +
  geom_line() +  
  geom_point() +
  geom_bar(data = count_data, aes(y = count * scaling_factor, x = typklasse_vk), stat = "identity", alpha = 0.3, inherit.aes = FALSE) +
  scale_y_continuous(
    "Mean Residual",
    sec.axis = sec_axis(~ . / scaling_factor, name = "Count of Observations")
  ) +
  xlab("GDV Groups") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
