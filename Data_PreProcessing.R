
## Data Upload and Cleaning
# Insurance Data
dat_all <- read.csv("Dat_all.csv")  

# Missing Values:
cols_with_na <- colnames(dat_all)[colSums(is.na(dat_all)) > 0]

print(cols_with_na)
sum(is.na(dat_all)) # 111,576
(sum(is.na(dat_all))/nrow(dat_all))*100  # = 7.83 %

dat_all$diff_nutzeralter_kasko

# Dropping Missing values
dat <- dat[complete.cases(dat_all), ] #  Dropping NA's

# External Vehicle Features
auto_ample <- read.csv("webscr_data_AutoAmple")
auto_ample <- auto_ample[,c("hsn", "tsn", "Kraftstoff", "HP", "construction_start")]  # Relevant Columns Only

## Merging Data
merged <- merge(dat, auto_ample, by = c("hsn", "tsn"), all.x = TRUE)

# Removing rows with no external vehicle features info
merged <- merged[complete.cases(merged), ] 

# Number of entries removed = 720 
nrow(dat) - nrow(merged)

# Number of unique vehicles removed
library(dplyr)
removed_vehicles <- anti_join(dat, merged, by = c("hsn", "tsn"))

removed_vehicles <- removed_vehicles %>% mutate(hsn_tsn_comb = paste(hsn, tsn, sep = "-"))
removed_vehicles_unique <- distinct(removed_vehicles, hsn_tsn_comb)
#print(removed_vehicles_unique)
nrow(removed_vehicles_unique)

# ------------------------------------------------------------------------------
# Data Pre-Processing Section
# ------------------------------------------------------------------------------


### Modifying Vehicle Age ###

nrow(merged[merged$fahrzeugalter_erwerb > merged$fahrzeugalter,])

## They are 2 cases for vehicle age at purchase > vehicle age:

## Case 1:  cases vehicle age at purchase > vehicle age by just 1 year ---> set vehicle age at purchase equal to vehicle age.
case1 <- merged[ merged$fahrzeugalter_erwerb == merged$fahrzeugalter +1 ,]
nrow(case1)
merged$fahrzeugalter_erwerb[merged$fahrzeugalter_erwerb == merged$fahrzeugalter +1] <- merged$fahrzeugalter[merged$fahrzeugalter_erwerb == merged$fahrzeugalter +1]


## Case 2:  vehicle age at purchase > vehicle age and vehicle age at purchase = 30 ---> set vehicle age at purchase to NA.
case2 <- merged[merged$fahrzeugalter_erwerb == 30 & merged$fahrzeugalter_erwerb > merged$fahrzeugalter,]
nrow(case2)
merged$fahrzeugalter_erwerb[merged$fahrzeugalter_erwerb == 30 & merged$fahrzeugalter_erwerb > merged$fahrzeugalter] <- NA


## Imputation for NAs resulting from case 2
  # Calculate the age difference for available data
age_difference <- merged$fahrzeugalter - merged$fahrzeugalter_erwerb

  # Calculate the average age difference
average_difference <- mean(age_difference, na.rm = TRUE)

  # Fill in the missing values using the average difference
merged$fahrzeugalter_erwerb[is.na(merged$fahrzeugalter_erwerb)] <- merged$fahrzeugalter[is.na(merged$fahrzeugalter_erwerb)] - average_difference

  # Check
sum(is.na(merged))


### Deckungsart (deductable of policy holder) Splitting into 2 features VK and TK ###
library(dplyr)
merged$deckungsart <- gsub("\\s", "", merged$deckungsart) # Removing all spaces in the naming of the category 
merged$deckungsart <- factor(merged$deckungsart)
merged <- merged %>%
  mutate(VK = gsub("^VK\\s*(\\d+).*", "\\1", deckungsart),
         VK = ifelse(grepl("VK/",VK), "0", VK),
         VK = ifelse(grepl("VKoSB", VK), "oSB", VK),
         TK = gsub(".*TK(\\S+)", "\\1", deckungsart))

# New Features into factors
merged$VK <- factor(merged$VK)  
merged$TK <- factor(merged$TK) 
#Deleting Old Feature 
merged <- merged[, -which(names(merged) =="deckungsart")]


### Changing Categorical Varibles into Factors ###

colnames(merged)[colnames(merged) == 'Kraftstoff'] <- 'fuel_type'


# Remove non-ASCII characters
merged$bundesland <- sapply(merged$bundesland, function(x) iconv(x, "UTF-8", "ASCII", sub=""))


### Modifying the Fuel Type name (was Kraftstoff)
table(merged$Kraftstoff)

# Renaming the Fuel Type 
colnames(merged)[colnames(merged) == 'Kraftstoff'] <- 'fuel_type'
table(merged$fuel_type)

# Fuel Types with few observations are put in "others" group.
others <- c("Wasserstoff/Elektro", "Wankel",  "Zweitakt" , "Benzin/Alkohol")
merged$fuel_type[merged$fuel_type %in% others] <- "others"

## Nominal Fcators
nominal <- c("regionalklasse_vk","tarifgruppe","abstellplatz","wohngebaeude", "hsn","tsn", 
             "zahlungsweise","gap_deckung", "rabattschutz", "zahlungsart", "schutzbrief",
             "tarifvariante", "kennzeichentyp", "nutzungsart", "nutzergruppe", "freie_werkstattwahl","bundesland",
             "abw_halter", "fuel_type")


# Loop through the nominal columns changing them into factors with the highest frequency category as a refence category
for (var in nominal) {
  # Convert to factor
  merged[[var]] <- as.factor(merged[[var]])
  
  # Identify the most frequent level
  freq_table <- table(merged[[var]])
  most_frequent <- as.character(attributes(which.max(freq_table))$names)
  
  # Reorder levels to set the most frequent as reference
  merged[[var]] <- relevel(merged[[var]], ref = most_frequent)
}


## Ordinal Factors 
merged$typklasse_vk <- factor(merged$typklasse_vk, levels = sort(unique(merged$typklasse_vk)), ordered = TRUE)
merged$sf_klasse <- factor(merged$sf_klasse, levels = sort(unique(merged$sf_klasse)), ordered = TRUE)
merged$VK <- ordered(merged$VK, levels = c("0", "150", "300", "500", "1000", "2500", "5000", "oSB"))
merged$TK <- ordered(merged$TK, levels = c("150","300","500","1000","2500","5000","oSB"))
merged$jahresfahrleistung <- ordered(merged$jahresfahrleistung, levels = c( "unb","3000","6000","10000","12000",
                                                                                          "15000","20000","25000","30000" ,"35000" ,">35000"))

## Integers
merged$statistikjahr <- as.integer(merged$statistikjahr)


write.csv(merged, "Merged_PreProcessed")



# ------------------------------------------------------------------------------
# Data Splits (Train-Validation-Test)
# ------------------------------------------------------------------------------

library(caret)

set.seed(123)
index_train_val <- createDataPartition(merged$schadenaufwand, p = 0.6, list = FALSE)
index_val_test <- createDataPartition(merged[-index_train_val, ]$schadenaufwand, p = 0.5, list = FALSE)

#Split the data based on the indices
train_data <- merged[index_train_val, ]
validation_data <- merged[index_val_test, ]
test_data <- merged[-index_train_val, ][-index_val_test, ]

write.csv(train_data, "train_data")
write.csv(validation_data, "validation_data")
write.csv(test_data, "test_data")

