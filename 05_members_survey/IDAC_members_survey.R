# Project: IDAC Members survey
# Author: Sebastian Palmas

# PROFILE ----
rm(list=ls())

USERNAME    <- Sys.getenv("USERNAME")
USERPROFILE <- Sys.getenv("USERPROFILE")
USER        <- Sys.getenv("USER")

#file paths for each user of the repository
if (USERNAME == "palma"){
  projectFolder  <- file.path(file.path(Sys.getenv("USERPROFILE"), "OneDrive - UNICEF/IDAC/IDAC members survey")) #Output files
  repoFolder  <- file.path(file.path(Sys.getenv("USERPROFILE"), "code/Dem-Analytics/16_IDAC_members_survey")) #repository files
  rawdataFolder <- file.path(file.path(Sys.getenv("USERPROFILE"), "OneDrive - UNICEF/Migration and Displacement/Data/"))  #raw data folder
} 

# confirm that the main directory is correct
# check if the folders exist
stopifnot(dir.exists(projectFolder))
stopifnot(dir.exists(repoFolder))

# PACKAGES ----

library(dplyr) # filter tibble left_join select mutate pull
library(forcats)
library(ggplot2) # fortify
library(tidyr) 

# LOAD DATA ----
responses <- readxl::read_xlsx(file.path(projectFolder, "International Data Alliance for Children on the Move (IDAC) Members Feedback Survey (1-19).xlsx"),
                              sheet="Sheet1")

# DELETE TEST DATA ----
responses <- responses |> filter(`Full name: ` != "Jebilson Raja Joslin")

# 4 Country ----
continent <- tribble(~Continent, ~Responses,
                     "America", 4,
                     "Africa",6,
                     "Asia", 1,
                     "Europe",6,
                     "Oceania",1)

g1 <- ggplot(continent, aes(x=Continent, y=Responses))+
  geom_bar(stat = 'identity', fill = "#4B0082")+
  theme_minimal()+
  xlab("")
print(g1)

# 6 Joining ----
table(responses$`Is your organization a member of the International Data Alliance for Children on the Move (IDAC)? If not, would you consider joining? `)/18

# 7 Challenges ----
challenges <- data.table::tstrsplit(responses$`Please describe any challenges that you/your organization have encountered in collecting data and evidence on migrant and displaced children. (Multiple Selection)`, ";") |> 
  unlist() |> na.omit() 
challenges[challenges == "falta de priorización de la temática para la producción de las estadísticas migratorias"] <- "Lack of political will or prioritization (e.g., low government interest or support for data collection)"
challenges[challenges == "Testing to ensure that this feature to input text is working"] <- NA


challenges <- challenges |> table() |> as.data.frame() |> 
  mutate(challenges = gsub("\\s*\\([^\\)]+\\)", "", challenges))

#Adding results from free text responses
challenges$Freq[challenges$challenges == "Lack of political will or prioritization"] = challenges$Freq[challenges$challenges == "Lack of political will or prioritization"] + 1
challenges$Freq[challenges$challenges == "Limited access to affected populations"] = challenges$Freq[challenges$challenges == "Limited access to affected populations"] + 2

challenges <- challenges |> mutate(challenges = fct_reorder(challenges, Freq))


g7 <- ggplot(challenges, aes(x=challenges, y=Freq))+
  geom_bar(stat = 'identity', fill = "#4B0082")+
  theme_minimal()+
  xlab("")+
  coord_flip()
print(g7)

# 9 Challenges addressed ----
table(responses$`Have any of the challenges selected above been addressed? `)/18

# 11 Projects ----
table(responses$`Have you/your organization initiated, participated in or encountered successful examples of data collection efforts to address the data gaps on children on the move?`)/18  

# 12 Project details ----
responses$`If answered yes to above question, please provide details (Optional)`

# 13 data on policy ----
responses$`Please provide any examples in your organization of instances when data/evidence have been used to influence/inform policy change on migrants or displaced children (Open text)`


# 14 IDAC impact: data availability ----
table(responses$`Improvements in the availability of disaggregated data by age and sex on migrant and displaced children?`)/18
responses$`If yes, please specify how. (Open text)`


# 16 IDAC impact: approach to----
approach <- data.table::tstrsplit(responses$`Changes or improvements to your organization’s approach to:`,
                      ";") |> unlist() |> na.omit()

approach[approach == "awareness of the interest at an international level for this type of data"] <- "Awareness for this type of data"

approach <- approach |> table() |> as.data.frame() |>   mutate(approach = fct_reorder(approach, Freq))

g16 <- ggplot(approach, aes(x=approach, y=Freq))+
  geom_bar(stat = 'identity', fill = "#4B0082")+
  theme_minimal()+
  xlab("")+
  coord_flip()
print(g16)

# 18 IDAC impact: influence policymaking----
table(responses$`Improvements in your organization’s ability to influence policymaking using data and evidence on migrant and displaced children?`)/18

# 20 IDAC impact: new or strengtened evidence----
table(responses$`New or strengthened evidence-based policies or programming that benefit migrant and displaced children?`)/18

# 22 IDAC further support----
responses$`How can IDAC further support your organization to improve data and evidence and related policymaking/advocacy work on behalf of children on the move? (Open text)`

# 23 IDAC products receive----
table(responses$`Did you receive these documents via email? If not, would you like to receive these in the future?`)/18


# 24 IDAC products use----
table(responses$`Please provide your feedback on these publications and indicate to what extent your organization has used them to improve data work or advocate for better data for migrant and displaced children. `)/18

# 25 IDAC future ----
responses$`Which areas or topics would you like to see IDAC focus on in future publications and guideline documents related to data for migrant and displaced children on the move?`

# 26 Pledges ----
pledges <- data.table::tstrsplit(responses$`How many of the pledge’s five actions has your country implemented? (Multiple selection)`,
                                  ";") |> unlist() |> na.omit() |> 
  table() |> as.data.frame() 

pledges$Var1 <- factor(pledges$Var1, levels = rev(pledges$Var1))

g26 <- ggplot(pledges, aes(x=Var1, y=Freq))+
  geom_bar(stat = 'identity', fill = "#4B0082")+ 
  theme_minimal()+
  xlab("") + 
  coord_flip()
print(g26)

# 27 Pledges examples ----
responses$`Can you provide examples of adjustments, if any, made along the lines of the 5 actions? (Open text)`


# 28 Pledges obstacles ----
responses$`What obstacles have you faced in implementing the pledge, and how have you overcome them? (Open text)`




# 29  ----
responses$`In your view/in your context, what are the most critical areas regarding data availability or gaps on migrant and displaced children? (Open text)`

# 30 IDAC better support ----

support <- data.table::tstrsplit(responses$`In which areas can IDAC’s network better support your organization (e.g., data gaps)? (Multiple selection)`,
                                  ";") |> unlist() |> na.omit()

support <- support |> table() |> as.data.frame() |>   mutate(support = fct_reorder(support, Freq))

g30 <- ggplot(support, aes(x=support, y=Freq))+
  geom_bar(stat = 'identity', fill = "#4B0082")+
  theme_minimal()+
  xlab("")+
  coord_flip()
print(g30)
