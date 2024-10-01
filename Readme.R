#### READ ME #####

Data_PreProcessing.R: 
  Is the file containing the R code for the uploading and merging the datasets. 
  It includes all the preProcessing steps made on the data.
  
Models.R:
  Is the file containing the R code for implementing the Supervised Learning Models.
  It includes the implementations for the Tweedie GLM, Decision Tree, Random Forest, and XGBoost models_feats
  It also includes the codes for the comparisons of the resulting models on the test data.
  
Groups_1_VehicleEffect.R:
  Is the file containing the R code for creating the new vehicle groups using the vehicle effects.
  It includes the formation of the vehicle effect formula and the generation of the vehicle effect values.
  It also includes the clustering of the vehicle effect values using K-means.
  
Groups_2_DBSCAN.R:
  Is the file containing the R code for creating the new vehicle group using the vehicle features(fuel type, construction_start, and hp)  
  It includes the formation of the groups with the implementation of the DBSCAN algorithm.
  
Groups_Comparision.R:
  Is the file containing the R code for the comparisons of the new groups with the GDV groups.

Webscrapping.R
  Is the file containing the R code for the webscarpping of additional features to be merged with data and used in the modelling and the clustering.
