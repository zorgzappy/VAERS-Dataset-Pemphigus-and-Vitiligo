Data2020 <-read.csv("2020VAERSDATA.csv")
Vax2020 <-read.csv("2020VAERSVAX.csv")
Data2021 <-read.csv("2021VAERSDATA.csv")
Vax2021 <-read.csv("2021VAERSVAX.csv")
Data2022 <-read.csv("2022VAERSDATA.csv")
Vax2022 <-read.csv("2022VAERSVAX.csv")
Data2023 <-read.csv("2023VAERSDATA.csv")
Vax2023 <-read.csv("2023VAERSVAX.csv")

# Install and load the dplyr package if you haven't already
# install.packages("dplyr")
library(dplyr)
install.packages("stringr")
library(stringr)


# Replace 'desired_symptom' with the symptom you're interested in
desired_symptom <- "emphi"
covid <- "COVID19"
covidboost <- "COVID19-2"
psor <- "pustular"


# Subset the dataset to include only rows with the desired symptom
#2020
Pemph2020 <- Data2020 %>%
  filter(str_detect(SYMPTOM_TEXT, desired_symptom))
unique_values <- unique(Pemph2020$VAERS_ID)
PemphVax2020 <- Vax2020 %>%
  filter(VAERS_ID %in% unique_values)


#2021
Pemph2021 <- Data2021 %>%
  filter(str_detect(SYMPTOM_TEXT, desired_symptom))
unique_values21 <- unique(Pemph2021$VAERS_ID)
PemphVax2021 <- Vax2021 %>% #Amount of pempigus/pemphigoid wiith AE from any vaccine
  filter(VAERS_ID %in% unique_values21)
PemphVaxCovid2021 <- PemphVax2021 %>% # Amount of Pemphigus/Pemphigoid with AE from Covid Vaccine
  filter(str_detect(VAX_TYPE, covid))
Covid2021 <- Vax2021 %>% #amount of people with AE from Covid Vaccine
  filter(str_detect(VAX_TYPE, covid))
#test <- Vax2021 %>% 
#  filter(str_detect(VAX_TYPE, covidboost))
CovidVaccineNames21 <- unique(Covid2021$VAX_MANU)
unique_values21Data <- unique(PemphVaxCovid2021$VAERS_ID)
PemphVaxCovidData2021 <-  Data2021 %>% #Creates the subset from the DATA file with people who have pemph from AE of COVID
  filter(VAERS_ID %in% unique_values21Data)
PemphVaxCovidData2021 <- PemphVaxCovidData2021[PemphVaxCovidData2021$SYMPTOM_TEXT != "", ]
  
psoriasis2021 <- Data2021 %>%
  filter(str_detect(SYMPTOM_TEXT, psor))


#2022
Pemph2022 <- Data2022 %>%
  filter(str_detect(SYMPTOM_TEXT, desired_symptom))
unique_values22 <- unique(Pemph2022$VAERS_ID)
PemphVax2022 <- Vax2022 %>%
  filter(VAERS_ID %in% unique_values22)
PemphVaxCovid2022 <- PemphVax2022 %>%
  filter(str_detect(VAX_TYPE, covid))
Covid2022 <- Vax2022 %>%
  filter(str_detect(VAX_TYPE, covid))
unique_values22Data <- unique(PemphVaxCovid2022$VAERS_ID)
PemphVaxCovidData2022 <-  Data2022 %>% #Creates the subset from the DATA file with people who have pemph from AE of COVID
  filter(VAERS_ID %in% unique_values22Data)


#2023
Pemph2023 <- Data2023 %>%
  filter(str_detect(SYMPTOM_TEXT, desired_symptom))
unique_values23 <- unique(Pemph2023$VAERS_ID)
PemphVax2023 <- Vax2023 %>%
  filter(VAERS_ID %in% unique_values23)
PemphVaxCovid2023 <- PemphVax2023 %>%
  filter(str_detect(VAX_TYPE, covid))
Covid2023 <- Vax2023 %>%
  filter(str_detect(VAX_TYPE, covid))
unique_values23Data <- unique(PemphVaxCovid2023$VAERS_ID)
PemphVaxCovidData2023 <-  Data2023 %>% #Creates the subset from the DATA file with people who have pemph from AE of COVID
  filter(VAERS_ID %in% unique_values23Data)

combined_df <- rbind(PemphVaxCovidData2021, PemphVaxCovidData2022)
combined_df <- rbind(combined_df, PemphVaxCovidData2023) 
combinedVAERSunique <- unique(combined_df$VAERS_ID)

allCovidVAX <- rbind(Covid2021, Covid2022, Covid2023)
allData <- rbind(Data2021, Data2022, Data2023)
uniqueAllCovid <- unique(allCovidVAX$VAERS_ID)
allCovidDATA <- allData %>%
  filter(VAERS_ID %in% uniqueAllCovid)
allCovidDATA <- allCovidDATA %>%
  filter(!(VAERS_ID %in% combinedVAERSunique))
allCovidVAX <- allCovidVAX %>%
  filter(!(VAERS_ID %in% combinedVAERSunique))
mean(allCovidDATA$AGE_YRS, na.rm = TRUE)
max(allCovidDATA$AGE_YRS, na.rm = TRUE)
df_ordered <- allCovidDATA[order(-allCovidDATA$AGE_YRS),]
head(df_ordered$AGE_YRS, 100)
percentage_male <- (sum(allCovidDATA$SEX == "M", na.rm = TRUE) / sum(!is.na(allCovidDATA$SEX)))
men <- sum(allCovidDATA$SEX == "M")
female <- sum(allCovidDATA$SEX == "F")
testing <- unique(allCovidDATA$SEX)
UNd <- sum(allCovidDATA$SEX == "U")
tot <- men + female
menper <- men/tot
total_na_age <- sum(is.na(allCovidDATA$AGE_YRS))

avgOnsetNumDays <- sum(allCovidDATA$NUMDAYS, na.rm = TRUE) / sum(!is.na(allCovidDATA$NUMDAYS))
total <- sum(allCovidDATA$NUMDAYS, na.rm = TRUE)
total_na_NUMDAYS <- sum(is.na(allCovidDATA$NUMDAYS))

numdied <- sum(allCovidDATA$DIED == "Y", na.rm = TRUE)
na <- sum(is.na(allCovidDATA$ER_ED_VISIT))
numERVIS <- sum(allCovidDATA$ER_VISIT == "Y", na.rm = TRUE)
numHospitalized <- sum(allCovidDATA$HOSPITAL == "Y")
avgamountspenthosp <- mean(allCovidDATA$HOSPDAYS, na.rm = TRUE)
testerino <- unique(allCovidVAX$VAX_TYPE)

#save.image("PemphStudy.RData")
#load("PemphStudy.RData")
#PemphVaxCovidData2021 <- PemphVaxCovidData2021[PemphVaxCovidData2021$SYMPTOM_TEXT != "", ]
#PemphVaxCovidData2022 <- PemphVaxCovidData2022[PemphVaxCovidData2022$SYMPTOM_TEXT != "", ]
#PemphVaxCovidData2023 <- PemphVaxCovidData2023[PemphVaxCovidData2023$SYMPTOM_TEXT != "", ]
num <- nrow(PemphVaxCovidData2021)
vulgaris <- combined_df %>%
  filter(str_detect(SYMPTOM_TEXT, "igus v") | str_detect(SYMPTOM_TEXT, "igus V") | str_detect(SYMPTOM_TEXT, "aris"))
vulgarisuni <- unique(vulgaris$VAERS_ID)
gore <- rbind(Covid2021, Covid2022, Covid2023)
vulgarisVAX <- gore %>%
  filter(VAERS_ID %in% vulgarisuni)
combined_dfTEST <- combined_df
combined_dfTEST <- combined_dfTEST %>%
  filter(!(VAERS_ID %in% vulgarisuni))
foliaceous <- combined_df %>%
  filter(str_detect(SYMPTOM_TEXT, "oliac"))
foliUNIQUE <- unique(foliaceous$VAERS_ID)
foliVAX <- gore %>%
  filter(VAERS_ID %in% foliUNIQUE)
combined_dfTEST <- combined_dfTEST %>% #do this after vegetans shit is done
  filter(!(VAERS_ID %in% foliUNIQUE))
pemphigoidBullous <- combined_dfTEST %>% #REMOVE RANDOM ONES
  filter(str_detect(SYMPTOM_TEXT, "bull") | str_detect(SYMPTOM_TEXT, "Bull") | str_detect(SYMPTOM_TEXT, "bulb") | str_detect(SYMPTOM_TEXT, "Bulb") | str_detect(SYMPTOM_TEXT, "lous"))

pemphigoidBullousVAX <- pemphigoidBullousVAX %>%
  filter(VAERS_ID != 1294298, VAX_LOT != "025L20A")
FuckYou <- combined_df %>%
  filter(str_detect(SYMPTOM_TEXT, "hailey"))
allCovidDATA <- rbind(allCovidDATA, FuckYou)
FuckYouVAX <- PemphVaxCovid2022 %>%
  filter(VAERS_ID == 2268564)
pemphigoidBullousUnique <- unique(pemphigoidBullous$VAERS_ID)
pemphigoidBullousVAX <- gore %>%
  filter(VAERS_ID %in% pemphigoidBullousUnique)
allCovidVAX <- rbind(allCovidVAX, FuckYouVAX)
combined_dfTEST <- combined_dfTEST %>% #do this after vegetans shit is done
  filter(!(VAERS_ID %in% pemphigoidBullousUnique))
mucousmembranePemphigoid <- combined_dfTEST %>%
  filter(str_detect(SYMPTOM_TEXT, "ucous") | str_detect(SYMPTOM_TEXT, "tricial") | str_detect(SYMPTOM_TEXT, "embrane"))
mucousmembranePemphigoidUNIQUE <- unique(mucousmembranePemphigoid$VAERS_ID)
mucousmembranePemphigoidVAX <- gore %>%
  filter(VAERS_ID %in% mucousmembranePemphigoidUNIQUE) 
combined_dfTEST <- combined_dfTEST %>% #do this after vegetans shit is done
  filter(!(VAERS_ID %in% mucousmembranePemphigoidUNIQUE))

bollous <- combined_dfTEST %>%
  filter(str_detect(SYMPTOM_TEXT, "Bollus"))
bollousVAX <- pemphigoidBullousVAX %>%
  filter(VAERS_ID == 2262965)

pemphigoidBullous <- rbind(pemphigoidBullous, bollous)

istherFuck <- allCovidDATA %>%
  filter(VAERS_ID == 2268564)

TESTINGTEST <- allCovidDATA %>%
  filter(VAERS_ID == 1028885)

pemphigusUnspecified <- combined_dfTEST %>%
  filter(str_detect(SYMPTOM_TEXT, "igus"))
pemphigusUnspecifiedUnique <- unique(pemphigusUnspecified$VAERS_ID)
pemphigusUnspecifiedVax <- gore %>%
  filter(VAERS_ID %in% pemphigusUnspecifiedUnique)
combined_dfTEST <- combined_dfTEST %>% #do this after vegetans shit is done
  filter(!(VAERS_ID %in% pemphigusUnspecifiedUnique))

pemphigoidGest <- combined_dfTEST %>%
  filter(str_detect(SYMPTOM_TEXT, "Gest") | str_detect(SYMPTOM_TEXT, "gest"))
pemphigoidGestUnique <- unique(pemphigoidGest$VAERS_ID)
pemphigusGestVax <- gore %>%
  filter(VAERS_ID %in% pemphigoidGestUnique)
combined_dfTEST <- combined_dfTEST %>% #do this after vegetans shit is done
  filter(!(VAERS_ID %in% pemphigoidGestUnique))
combined_dfTEST <- combined_dfTEST %>% #do this after vegetans shit is done
  filter(!(VAERS_ID %in% bollous$VAERS_ID[1]))

pemphigoidUnspecified <- combined_dfTEST
pemphigoidUnspecifiedUnique <- unique(pemphigoidUnspecified$VAERS_ID)
pemphigoidUnspecifiedVax <- gore %>%
  filter(VAERS_ID %in% pemphigoidUnspecifiedUnique)


# for the last data part
ALLPEMPH <- vulgaris
ALLPEMPH <- rbind(ALLPEMPH, foliaceous)
ALLPEMPHVAX <- vulgarisVAX
ALLPEMPHVAX <- rbind(ALLPEMPHVAX, foliVAX)
ALLPEMPHIGOID <- pemphigoidBullous
ALLPEMPHIGOIDVAX <- pemphigoidBullousVAX
ALLPEMPHIGOID <- rbind(ALLPEMPHIGOID, mucousmembranePemphigoid)
ALLPEMPHIGOIDVAX <- rbind(ALLPEMPHIGOIDVAX, mucousmembranePemphigoidVAX)
ALLPEMPHIGOID <- rbind(ALLPEMPHIGOID, bollous)
ALLPEMPHIGOIDVAX <- rbind(ALLPEMPHIGOIDVAX, bollousVAX)
ALLPEMPH <- rbind(ALLPEMPH, pemphigusUnspecified)
ALLPEMPHVAX <- rbind(ALLPEMPHVAX, pemphigusUnspecifiedVax)
ALLPEMPHIGOID <- rbind(ALLPEMPHIGOID, pemphigoidGest, pemphigoidUnspecified)
ALLPEMPHIGOIDVAX <- rbind(ALLPEMPHIGOIDVAX, pemphigusGestVax, pemphigoidUnspecifiedVax)


library(dplyr)

uniuni <- allCovidDATA$VAERS_ID
uniqueCovidVaxALL <- allCovidVAX %>%
  filter(VAERS_ID %in% uniuni) %>%
  group_by(VAERS_ID) %>%
  filter(VAX_DOSE_SERIES == max(VAX_DOSE_SERIES[!is.na(VAX_DOSE_SERIES)])) %>%
  ungroup()

myasthenia <- allCovidDATA %>%
  filter(str_detect(SYMPTOM_TEXT, "myasthenia") | str_detect(SYMPTOM_TEXT, "Myasthenia"))

methodTwo <- allCovidVAX %>%
  filter(VAERS_ID %in% uniuni) %>%
  arrange(VAERS_ID, desc(VAX_DOSE_SERIES)) %>%
  group_by(VAERS_ID) %>%
  slice_head(n = 1) %>%
  ungroup()

rm(methodTwo)

uniquetest <- unique(allCovidVAX$VAERS_ID)

ALLPEMPHage <- mean(ALLPEMPH$AGE_YRS, na.rm = TRUE)
sum(is.na(ALLPEMPH$AGE_YRS))
min(ALLPEMPH$AGE_YRS, na.rm = TRUE)
max(ALLPEMPH$AGE_YRS, na.rm = TRUE)
pemphMen <- sum(ALLPEMPH$SEX == "M")
pemphWomen <- sum(ALLPEMPH$SEX == "F")
mean(ALLPEMPH$NUMDAYS, na.rm = TRUE)
sum(is.na(ALLPEMPH$NUMDAYS))
sum(ALLPEMPH$HOSPITAL == "Y")
mean(ALLPEMPH$HOSPDAYS, na.rm = TRUE)
table(ALLPEMPHVAX$VAX_DOSE_SERIES)

mean(ALLPEMPHIGOID$AGE_YRS, na.rm = TRUE)
sum(is.na(ALLPEMPHIGOID$AGE_YRS))
min(ALLPEMPHIGOID$AGE_YRS, na.rm = TRUE)
max(ALLPEMPHIGOID$AGE_YRS, na.rm = TRUE)
pemphIGOIDMen <- sum(ALLPEMPHIGOID$SEX == "M")
pemphIGOIDWomen <- sum(ALLPEMPHIGOID$SEX == "F")
mean(ALLPEMPHIGOID$NUMDAYS, na.rm = TRUE)
sum(is.na(ALLPEMPHIGOID$NUMDAYS))
sum(ALLPEMPHIGOID$DIED == "Y")
sum(ALLPEMPHIGOID$ER_VISIT == "Y")
sum(ALLPEMPHIGOID$HOSPITAL == "Y")
mean(ALLPEMPHIGOID$HOSPDAYS, na.rm = TRUE)
sum(is.na(ALLPEMPH$HOSPDAYS))
sum(is.na(ALLPEMPHIGOID$HOSPDAYS))
table(ALLPEMPHIGOIDVAX$VAX_DOSE_SERIES)
