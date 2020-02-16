### In this problem set, you will tidy up an IAT dataset 
### The original data is available at https://osf.io/szwuf/, but it comes as an SPSS .sav file
### I've included trimmed down .csv version of 2019's data in this repository for you to work with

# loading libraries  ---------------------------------------------
library(tidyverse)

# reading in IAT data  ---------------------------------------------
# use a tidyverse function to read in the included IAT_2019.csv file 
tbl <- read_csv("IAT.csv")

# Removing unnecessary rows and columns  ---------------------------------------------
# This data frame only contains 21 of the 454 available variables, but it's still too much

# use tidyverse functions so that only the following variables are included: 'session_id',"genderidentity","raceomb_002","D_biep.White_Good_all","Mn_RT_all_3467",
#       "edu_14","politicalid_7","STATE","att_7","tblacks_0to10","twhites_0to10","labels"
tbl_clean <- select(tbl,session_id,gender,raceomb_002,D_biep.White_Good_all,Mn_RT_all_3467,edu_14,
                    politicalid_7,STATE,att_7,tblacks_0to10,twhites_0to10,labels)
tbl_clean


### next, clean up the rows 
###our primary dependent variable is D_biep.White_Good_all, but some subjects don't have any data...
#Remove the rows with missing D_biep.White_Good_all entries 
tbl_clean %>%
  filter(is.na(D_biep.White_Good_all)) #view NA rows

tbl_clean <- tbl_clean %>% drop_na(D_biep.White_Good_all) #remove cases with NA for this variable

#rerun line 25 to ensure new df has no missing cases for D_biep.White_Good_all

# Renaming varialbles  ---------------------------------------------

# next rename variables with more intuitive, short labels 
# here are some suggestions (along with variable info)
# id : session_id (subject number)
# gender : genderidentity (gender 1 "Male" 2 "Female" 3 "Trans male/Trans man" 4 "Trans female/Trans woman" 5 "Genderqueer/Gender nonconforming" 6 "A different identity") 
# race : raceomb_002 (race: 1 "American Indian" 2 "East Asian" 3 "South Asian" 4 "Hawaiian Pacifica Islander" 5 "black Africian American" 6 "white" 7 "other" 8 "multiracial")
# bias :D_biep.White_Good_all (overall IAT score)
# rt : Mn_RT_all_3467 (overall reaction time)
# edu : edu_14 (education: 1 "elementary" 2 "junior high" 3 "some high school" 4 "HS grad" 5 "some college" 6 "associate's" 7 "bachelor's" 8 "some grad" 9 "MA" 10 "JD" 11 "MD" 12 "PHD" 13 "other advanced" 14 "MBA")
# pol : politicalid_7 (political identification: 1 "strongly conservative 7 "strongly liberal)
# state : STATE
# att : att_7 (race attitude 1 "strongly prefer AA" 7 "strongly prefer white")
# temp_b : tblacks_0to10 (temperature feelings black 1 "extremely cold" 10 "extremly warm")
# temp_w : twhites_0to10 (temperature feelings black 1 "extremely cold" 10 "extremly warm")

tbl_clean <- rename(tbl_clean, ID = session_id, race =raceomb_002, white_bias =D_biep.White_Good_all, RT =Mn_RT_all_3467, education =edu_14,
                    pol_id =politicalid_7, state =STATE, attitude =att_7, temp_b =tblacks_0to10, temp_w =twhites_0to10) 
                    #10vars #gender & labels are fine as is

#  missing values  ---------------------------------------------  

summary(tbl_clean)
# some of our variables have missing values that aren't properly coded as missing  
# recode missing values in gender and state

#look at all distinct values for gender
vals_gender <- tbl_clean %>%
  distinct(gender) #only weird value is "NA"
 
tbl_clean$gender <- na_if(tbl_clean$gender, "NA")
is.na(tbl_clean$gender) #recode "NA" values as missing

tbl_clean %>%
  filter(is.na(gender)) #look at NAs for gender variable
--------------------------------------
#look at all distinct values for gender
vals_state <- tbl_clean %>%
  distinct(state) #only weird value is "NA"

tbl_clean$state <- na_if(tbl_clean$state, "NA")
is.na(tbl_clean$state) #recode "NA" values as missing

tbl_clean %>%
  filter(is.na(state)) #look at NAs for state variable


# changing variable types  ---------------------------------------------  
# next, convert id and all variables that are character types to factors
# try to convert all variables at once using tidyverse functions
tbl_clean[, sapply(tbl_clean, class) == 'character'] #get variables which are character

factorVars <- c('ID', 'gender','state') #create object with variables we want to convert to factors
tbl_clean_fac <- mutate_at(tbl_clean, factorVars, ~factor(.))
tbl_clean_fac[, sapply(tbl_clean_fac, class) == 'factor'] #check that your variables now come up as factors

# recoding variables  ---------------------------------------------  
# participants were instructed to select all the gender idenities that apply to them
# this results in a lot of combinations!
# this pipeline tabulates the number of participants who endorse different gender identities. 
#get the number of participants who endorse different gender identities: 

tbl_clean #use my none-factor tbl, bc can't use factors for group_by function
gender_count <- tbl_clean %>% group_by(gender) %>% tally()

# sort the output and then use indexing to print the 3 most common response (not inlcuding missing values)
gender_count <- gender_count %>% arrange(desc(n))
gender_count <- na.omit(gender_count)
gender_count[1:3,] #print first 3 rows

# create a new variable that recodes gender to have 4 levels: the 3 most common responses and the others collapsed together
# you can use the key provided on line 31 to understand the levels
# check out recode documentation to see if there's a trick for setting defaults values for unspecified rows
# *note that this excercise in data recoding doesn't reflect the instructors' views on gender identities...
tbl_clean$gender4 <- recode(tbl_clean$gender, "[2]" = "1", "[1]" = "2", "[5]" = "3", .default = "other", .missing = NULL)

# Now take a look at how highest obtained education is coded (key on line 35)
edu_count <- tbl_clean %>% group_by(education) %>% tally() 

#create a new variable that recodes education into: no highscool, some highschool, highschool graduate, some college, postsecondary degree, masters (MA & MBA), advanced degree
#remember that the recode function isn't always the best solution for numeric variables
tbl_clean$edu7 <- recode(tbl_clean$education, '1' = 'no HS','2' = 'no HS', '3' = 'some HS', '4' = 'HS grad',
                         '5' = 'some college', '6' = 'PS degree', '7' = 'PS degree', '8' = 'PS degree', '9' = 'masters', 
                         '14' = 'masters', '10' = 'advanced', '11' = 'advanced', '12' = 'advanced', '13' = 'advanced')

# mutating variables ---------------------------------------------  
# rewrite the above recoding steps so that they both occur within a single call of the mutate function
tbl_clean <- ...
  
# filtering and math ---------------------------------------------  

# using filtering, calculate and print the mean bias score for:
# white men
white_men <- tbl_clean %>%
  filter(race == 6, gender4 == '2')
mean(white_men$white_bias) # = 0.3798384

# white women
white_women <- tbl_clean %>%
  filter(race == 6, gender4 == '1')
mean(white_women$white_bias) # = 0.3270798

# advanced degree holders who are men
adv_men <- tbl_clean %>%
  filter(edu7 == 'advanced', gender4 == '2')
mean(adv_men$white_bias) # = 0.318439

# high school graduates who are men
HS_men <- tbl_clean %>%
  filter(edu7 == 'HS grad', gender4 == '2')
mean(HS_men$white_bias) # = 0.3228535




