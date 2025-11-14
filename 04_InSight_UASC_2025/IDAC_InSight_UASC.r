# Project: IDAC InSIGHT Unaccompanied and Separated Children Data Section
# Script: Main and only script

rm(list = ls())

# Profile ----
# set working directories and all directories 
USERNAME    <- Sys.getenv("USERNAME")
USERPROFILE <- Sys.getenv("USERPROFILE")
USER        <- Sys.getenv("USER")

#file paths for each user of the repository
if (USERNAME == "palma"){
  projectFolder  <- file.path(file.path(Sys.getenv("USERPROFILE"), "OneDrive - UNICEF/IDAC/UASC InSight/output")) #Output files
  repoFolder  <- file.path(file.path(Sys.getenv("USERPROFILE"), "code/IDAC/04_InSight_UASC_2025/")) #repository files
  rawdataFolder <- file.path(file.path(Sys.getenv("USERPROFILE"), "OneDrive - UNICEF/Migration and Displacement/Data/"))  #raw data folder
} 


# check if the folders exist
stopifnot(dir.exists(projectFolder))
stopifnot(dir.exists(repoFolder))
stopifnot(dir.exists(rawdataFolder))

# Packages ----
library(dplyr)
library(eurostat)
library(ggplot2)
library(ggpubr)
library(readxl)
library(scales)
library(stringr)
library(tidyr)
library(ggmosaic)

# Helpers ----
addUnits <- function(n) {
  labels <- ifelse(n < 1000, n,  # less than thousands
                   ifelse(n < 1e6, paste0(round(n/1e3), 'k'),  # in thousands
                          ifelse(n < 1e9, paste0(round(n/1e6,1), 'M'),  # in millions
                                 ifelse(n < 1e12, paste0(round(n/1e9,1), 'B'), # in billions
                                        ifelse(n < 1e15, paste0(round(n/1e12), 'T'), # in trillions
                                               'too big!'
                                        )))))
  return(labels)
}

# Download and save Eurostat data----
# last downloaded: 2025-06-03
# #Asylum applicants by type, citizenship, age and sex - annual aggregated data
# migr_asyappctza <- get_eurostat("migr_asyappctza", time_format = "num", stringsAsFactors = TRUE)
# write.table(migr_asyappctza, file = file.path(rawdataFolder, "Eurostat/migr_asyappctza.csv"), row.names = F, sep = ",")
# 
# #Asylum applicants considered to be unaccompanied minors by citizenship, age and sex - annual data
# migr_asyunaa <- get_eurostat("migr_asyunaa", time_format = "num", stringsAsFactors = TRUE)
# write.table(migr_asyunaa, file = file.path(rawdataFolder, "Eurostat/migr_asyunaa.csv"), row.names = F, sep = ",")
# 
# #Unaccompanied minor asylum applicants by type, citizenship, age and sex - monthly data
# migr_asyumactm <- get_eurostat("migr_asyumactm", time_format = "num", stringsAsFactors = TRUE)
# write.table(migr_asyumactm, file = file.path(rawdataFolder, "Eurostat/migr_asyumactm.csv"), row.names = F, sep = ",")
# 
# #	Decisions granting temporary protection by citizenship, age and sex - annual aggregated data
# migr_asytpfa <- get_eurostat("migr_asytpfa", time_format = "num", stringsAsFactors = TRUE)
# write.table(migr_asytpfa, file = file.path(rawdataFolder, "Eurostat/migr_asytpfa.csv"), row.names = F, sep = ",")
# 
# #	Decisions granting temporary protection by citizenship, age and sex - quarterly data
# migr_asytpfq <- get_eurostat("migr_asytpfq", time_format = "num", stringsAsFactors = TRUE)
# write.table(migr_asytpfq, file = file.path(rawdataFolder, "Eurostat/migr_asytpfq.csv"), row.names = F, sep = ",")
# 
# #	Beneficiaries of temporary protection at the end of the month by citizenship, age and sex - monthly data
# migr_asytpsm <- get_eurostat("migr_asytpsm", time_format = "num", stringsAsFactors = TRUE)
# write.table(migr_asytpsm, file = file.path(rawdataFolder, "Eurostat/migr_asytpsm.csv"), row.names = F, sep = ",")

EU27 <- c("AT", "BE", "BG", "CY", "CZ", "DE", "DK", "EE", "EL", "ES", "FI",
          "FR", "HR", "HU", "IE", "IT", "LT", "LU", "LV", "MT", "NL", 
          "PL", "PT", "RO", "SE", "SI", "SK")

# Load data ----
load(file.path(rawdataFolder,"UNICEF/country_metadata/country_metadata.Rdata")) 

## Eurostat----
migr_asyappctza <- read.csv(file.path(rawdataFolder, "Eurostat/migr_asyappctza.csv"))
migr_asyunaa <- read.csv(file.path(rawdataFolder, "Eurostat/migr_asyunaa.csv"))
migr_asytpfa <- read.csv(file.path(rawdataFolder, "Eurostat/migr_asytpfa.csv"))
migr_asytpfq <- read.csv(file.path(rawdataFolder, "Eurostat/migr_asytpfq.csv"))
migr_asytpsm <- read.csv(file.path(rawdataFolder, "Eurostat/migr_asytpsm.csv"))


age_join <- tibble(age=c("Y16-17", "Y14-15", "Y_LT14","UNK"),
                   Age=factor(x = c("16-17", "14-15","0-14", "Unknown"),
                              levels=c("16-17", "14-15","0-14", "Unknown")))

migr_asyunaa_sum <- migr_asyunaa |> 
  filter(sex =='T',
         age %in% c("UNK", "Y_LT14", "Y14-15", "Y16-17"),
         citizen == "EXT_EU27_2020",
         geo %in% EU27) |> 
  group_by(TIME_PERIOD, age) |> 
  summarise(values = sum(values), .groups = "drop") |> 
  left_join(age_join, by='age')


## Mexico ----
# Source: https://portales.segob.gob.mx/es//PoliticaMigratoria/Boletines_Estadisticos
mex <- readxl::read_xlsx(path = file.path(rawdataFolder, "Mexico/Compilacion_Cuadro_3_1_5_eventos_de_ninos.xlsx"), sheet = "Sheet1")
#total events (adults, children), compiled manually from xlsx tables, 3.1
mex.totalevents <- tribble(~year, ~events,
                                     2014, 127149,
                                     2015, 198141,
                                     2016, 186216,
                                     2017, 93846,
                                     2018, 131445,
                                     2019, 182940,
                                     2020, 82379,
                                     2021, 309692,
                                     2022, 441409,
                                     2023, 778907,
                                     2024, 1234698)

## USA ----
sbo_19_22 <- read.csv(file.path(rawdataFolder, "US CBP/sbo-encounters-fy19-fy22.csv"))
sbo_21_24 <- read.csv(file.path(rawdataFolder, "US CBP/sbo-encounters-fy21-fy24.csv")) |> 
  filter(Fiscal.Year != "2024 (FYTD)",
         Month.Grouping == "FYTD") |> 
  mutate(Fiscal.Year = as.numeric(Fiscal.Year))
sbo_19_24 <- bind_rows(sbo_19_22, sbo_21_24) |> 
  distinct()

## UK ----
# Source: https://www.gov.uk/government/statistical-data-sets/immigration-system-statistics-data-tables#asylum-applications-decisions-and-resettlement
#Notes: UK considers some people between 18-29 years old as UASC
uk <- read_xlsx(file.path(rawdataFolder, "UK/asylum-claims-datasets-mar-2025.xlsx"),
                range="Data_Asy_D01!A2:J78883") |> 
  filter( Year %in% seq(2014,2024,1))

uk_applications <- uk |>  pull(Claims) |> sum()
uk_children_applications <- uk |> filter(Age=="Under 18") |> pull(Claims) |> sum()
uk_children_applications_male <- uk |> filter(Age=="Under 18", Sex == "Male") |> pull(Claims) |> sum()
uk_accompanied_applications <- sum(uk |> filter(Age=="Under 18", UASC=="Non-UASC") |> pull(Claims))
uk_accompanied_applications_male <- uk |> filter(Age=="Under 18", Sex == "Male",UASC=="Non-UASC") |> pull(Claims) |> sum()
uk_UASC_applications <- sum(uk |> filter(Age=="Under 18", UASC=="UASC") |> pull(Claims))
uk_UASC_applications_male <- uk |> filter(Age=="Under 18", Sex == "Male",UASC=="UASC") |> pull(Claims) |> sum()

print("-----Children asylum applicants in the UK between 2014 and 2023----")
print(paste0("Total applicants: ", addUnits(uk_applications) ))
print(paste0("Children: ", addUnits(uk_children_applications) ))
print(paste0("UASC: ", addUnits(uk_UASC_applications)))
print(paste0("Percentage UASC among total: ", round(100*(uk_UASC_applications/uk_applications)), "%"))
print(paste0("Percentage UASC among children: ", round(100*(uk_UASC_applications/uk_children_applications)), "%"))
print(paste0("Male among all children: ", round(100*uk_children_applications_male/uk_children_applications), "%"))
print(paste0("Male among accompanied children: ", round(100*uk_accompanied_applications_male/uk_accompanied_applications), "%"))
print(paste0("Male among UASC: ", round(100*uk_UASC_applications_male/uk_UASC_applications), "%"))


# Q2 ----
## EU ----
EU.as <- migr_asyappctza |> filter(TIME_PERIOD >=2014, TIME_PERIOD <=2024, citizen == "TOTAL", geo %in% EU27, age=="TOTAL", sex=="T", asyl_app =="ASY_APP") |> pull(values) |> sum()
EU.as.children <- migr_asyappctza |> filter(TIME_PERIOD >=2014, TIME_PERIOD <=2024, citizen == "TOTAL", geo %in% EU27, age=="Y_LT18", sex=="T", asyl_app =="ASY_APP") |> pull(values) |> sum()
EU.as.UASC <- migr_asyunaa |> filter(TIME_PERIOD >=2014, TIME_PERIOD <=2024, citizen == "TOTAL", geo %in% EU27, age=="TOTAL", sex=="T") |> pull(values) |> sum()
EU.as.UASC.annual <- migr_asyunaa |> filter(citizen == "TOTAL", geo %in% EU27, sex=="T") |> group_by(TIME_PERIOD, age) |> summarise(values = sum(values))

print(paste0("Total asylum applicants in the EU between 2014 and 2024: ", addUnits(EU.as) ))
print(paste0("Children asylum applicants in the EU between 2014 and 2024: ", addUnits(EU.as.children) ))
print(paste0("UASC asylum applicants in the EU between 2014 and 2024: ", addUnits(EU.as.UASC) ))
print(paste0("UASC asylum applicants among all applicants: ", round(100*EU.as.UASC/EU.as), "%. 1 in ", round(1/(EU.as.UASC/EU.as))))
print(paste0("UASC asylum applicants among children: ", round(100*EU.as.UASC/EU.as.children), "%. 1 in ", round(1/(EU.as.UASC/EU.as.children))))

## USA ----
encounters <-  sum(sbo_19_24$Encounter.Count) 
encounters.UASC <-  sbo_19_24 |> filter(Demographic == "UC / Single Minors" ) |> pull(Encounter.Count) |> sum()
print(paste0("Total encounters 2019 and 2024 Fiscal years: ", addUnits(encounters) ))
print(paste0("UASC encounters 2019 and 2024 Fiscal years: ", addUnits(encounters.UASC) ))
print(paste0("UASC encounters among total: ", round(100*encounters.UASC/encounters), "%. 1 in ", round(1/(encounters.UASC/encounters))))

annual_summary <- sbo_19_24 |> 
  filter(Demographic == "UC / Single Minors" ) |>
  group_by(Fiscal.Year) |> 
  summarise(Encounter.Count = sum(Encounter.Count))

## Mexico----
mex.total <- mex.totalevents |> pull(events) |> sum()
mex.children <- mex |> pull(Children) |> sum()
mex.UASC <- mex |> filter(Status == "Unaccompanied") |>  pull(Children) |> sum()
print(paste0("Events of irregular situation in Mexico between 2014 and 2024: ", addUnits(mex.total) ))
print(paste0("Events of children in irregular situation in Mexico between 2014 and 2024: ", addUnits(mex.children) ))
print(paste0("UASC events of irregular situation in Mexico between 2014 and 2024: ", addUnits(mex.UASC) ))
print(paste0("UASC events among total: ", round(100*mex.UASC/(mex.total)), "%. 1 in ", round(1/(mex.UASC/mex.total))))
print(paste0("UASC events among children: ", round(100*mex.UASC/(mex.children)), "%. 1 in ", round(1/(mex.UASC/mex.children))))

# FIG 0 ----
#percentage of countries that have data on ref and AS UASC
#done in power point
# https://unicef-my.sharepoint.com/:p:/g/personal/spalmas_unicef_org/Ec_xcewtAllNqlFargD5FfoB4xwRJJ4iGl7YIXi2sEIFYQ?e=GdsC9r

# FIG 1 UASC among cchildren ----

### EU ----
q1.eu.1 <- migr_asyunaa |> 
  filter(sex =='T',
         age == "TOTAL",
         citizen == "EXT_EU27_2020",
         geo %in% EU27) |> 
  group_by(TIME_PERIOD) |> 
  summarise(UASC = sum(values), .groups = "drop")

q1.eu.2 <- migr_asyappctza |> 
  filter(sex =='T',
         age == "Y_LT18",
         citizen == "EXT_EU27_2020",
         geo %in% EU27,
         asyl_app == "ASY_APP") |>  #ASY_APP: Asylum applicant, NASY_APP: First time applicant
  group_by(TIME_PERIOD) |> 
  summarise(Children = sum(values), .groups = "drop") |> 
  select(TIME_PERIOD, Children)
q1.eu <- left_join(q1.eu.1, q1.eu.2, by = "TIME_PERIOD") |> 
  filter(TIME_PERIOD > 2013) |> 
  mutate(UASC_perc = UASC/Children) |> 
  pivot_longer(cols = c(UASC, Children), names_to = "demographic") |> 
  mutate(value = value/1000,
         value_label = "")

q1.eu$value_label[q1.eu$demographic == "UASC"] = paste0(round(100*q1.eu$UASC_perc[q1.eu$demographic == "UASC"]), "%")
q1.eu <- q1.eu |> arrange(demographic)

q1.eu.g <- q1.eu|> 
  ggplot(mapping = aes(x=TIME_PERIOD, y=value, fill=demographic))+
  geom_bar(stat = 'identity', show.legend = FALSE, position = "identity")+
  geom_text(aes(label=value_label), vjust = -0.5, fontface='bold' , show.legend = FALSE)+
  scale_fill_manual(values=c("#B5E2FA",  "#0FA3B1"))+
  scale_color_manual(values=c("#B5E2FA",  "#0FA3B1"))+
  xlab("Year") + ylab("Children\n(in thousands)")+ 
  labs(fill='demographic', title = "European Union", subtitle = "Asylum applications", caption = "Source: Eurostat, 2025.\n(online codes: migr_asyunaa and migr_asyappctza)")+
  scale_x_continuous(breaks=seq(2014,2024,1))+
  theme_minimal()+
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        legend.title = element_blank(),
        legend.position = "none")
print(q1.eu.g)

### UK ----
q1.uk <- uk |> 
  filter(Age == "Under 18") |> 
  group_by(Year) |> 
  summarise(Children = sum(Claims),
            UASC = sum(Claims * (UASC == "UASC"))) |> 
  mutate(UASC_perc=UASC/Children) |> 
  pivot_longer(cols=c(UASC, Children), names_to = "demographic") |> 
  mutate(value=value/1000,
         value_label = "")

q1.uk$value_label[q1.uk$demographic == "UASC"] = paste0(round(100*q1.uk$UASC_perc[q1.uk$demographic == "UASC"]), "%")
q1.uk <- q1.uk |> arrange(demographic)

q1.uk.g <- q1.uk|> 
  ggplot(mapping = aes(x=Year, y=value, fill=demographic))+
  geom_bar(stat = 'identity', show.legend = FALSE)+
  geom_text(aes(label=value_label), vjust = -0.5, fontface='bold' , show.legend = FALSE)+
  scale_fill_manual(values=c("#B5E2FA",  "#0FA3B1"))+
  scale_color_manual(values=c("#B5E2FA",  "#0FA3B1"))+
  xlab("Year") + ylab("Children\n(in thousands)")+
  labs(fill='demographic', title = "United Kingdom", subtitle = "Asylum claims", caption = "Source: Home Office, 2025")+
  scale_x_continuous(breaks=seq(2013,2024,1))+
  theme_minimal()+
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        legend.title = element_blank(),
        legend.position = "none")
print(q1.uk.g)

### Mex ----
q1.mex <- mex |> 
  group_by(Year,Status) |> 
  summarise(values = sum(Children), .groups = 'drop') |> 
  pivot_wider(names_from = Status,values_from = values) |> 
  mutate(Children = Accompanied + Unaccompanied,
         UASC=Unaccompanied,
         UASC_perc=UASC/Children) |> 
  left_join(mex.totalevents, by=c("Year"='year')) |> 
  select(Year, UASC=Unaccompanied, Children,UASC_perc,total=events) |> 
  pivot_longer(cols=c(UASC, Children), names_to = "demographic") |> 
  mutate(value=value/1000,
         value_label = "")
q1.mex$value_label[q1.mex$demographic == "UASC"] = paste0(round(100*q1.mex$UASC_perc[q1.mex$demographic == "UASC"]), "%")
q1.mex <- q1.mex |> arrange(demographic)

q1.mex.g <- q1.mex|> 
  ggplot(mapping = aes(x=Year, y=value, fill=demographic))+
  geom_bar(stat = 'identity')+
  geom_text(aes(label=value_label), vjust = -0.5, fontface='bold' , show.legend = FALSE)+
  scale_fill_manual(values=c("#B5E2FA",  "#0FA3B1"))+
  scale_color_manual(values=c("#B5E2FA",  "#0FA3B1"))+
  xlab("Year") + ylab("Encounters\n(in thousands)")+
  labs(fill='demographic', title = "Mexico", subtitle = "Encounters in an irregular situation", caption = "Source: Secretaria de Gobernacion, 2025")+
  scale_x_continuous(breaks=seq(2013,2024,1))+
  scale_y_continuous(limits = c(0,155), breaks = seq(0,150,50))+
  theme_minimal()+
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        legend.title = element_blank())

common.legend <- get_legend(q1.mex.g)
q1.mex.g <- q1.mex.g + theme(legend.position = "none")

### USA ----
q1.us <- sbo_19_24 |>
  filter(Demographic == "UC / Single Minors") |> 
  group_by(Fiscal.Year) |> 
  summarise(UASC = sum(Encounter.Count)/1000) 
q1.us.g <- q1.us|> 
  ggplot(mapping = aes(x=Fiscal.Year, y=UASC))+
  geom_bar(stat = 'identity', fill="#0FA3B1")+
  scale_x_continuous(breaks = seq(2019, 2024, 1))+
  scale_y_continuous(limits = c(0,155), breaks = seq(0,150,50))+
  xlab("Fiscal Year") + ylab("Encounters\n(in thousands)")+
  labs(fill='demographic', title = "United States", subtitle = "Encounters at the Southwest land border.(Total number of children not available)", caption = "Source: CBP, 2025")+
  theme_minimal()+
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        legend.position = "none",
        legend.title = element_blank())

ggarrange(q1.eu.g, q1.uk.g, q1.mex.g, q1.us.g,
          ncol = 2,nrow=2,
          #labels = c('European Union', 
          #           'United Kingdom',
          #           "Mexico",
          #           "USA"),
          font.label = list(size = 16, color = "black", face = "bold", family = NULL),
          label.x = 0,
          hjust = -0.1,
          legend.grob = common.legend,
          legend = 'bottom')
ggsave(file.path(projectFolder, "Fig1.pdf"), width = 25, height = 20, units = 'cm')

# Fig 2 Gender and age ----
## EU Gender and age ----
q5.eu <- migr_asyunaa |> 
  filter(age %in% c("Y_LT14", "Y14-15", "Y16-17"),
         sex %in% c("F", "M"),
         TIME_PERIOD > 2013,
         citizen == "EXT_EU27_2020",
         geo %in% EU27) |>
  mutate(Sex=as.character(sex),
         Age=as.character(age)) |> 
  group_by(Sex, Age) |> 
  summarise(values=sum(values, na.rm = T), .groups = 'drop') |> 
  mutate(perc = values/sum(values)) |> 
  arrange(Sex, Age)

data1 <- as.table(matrix(c(q5.eu$perc[3], q5.eu$perc[1], q5.eu$perc[2],
                           q5.eu$perc[6], q5.eu$perc[4], q5.eu$perc[5]), 3, 2))
dimnames(data1) <- list(Age = c("Ages\n0 to 13", "Ages\n14 to 15", "Ages\n16 to 17"),
                        Sex = c("Female", "Male"))

perc_label <- label_percent(accuracy = 1)(c(q5.eu$perc[3], q5.eu$perc[1], q5.eu$perc[2], q5.eu$perc[6], q5.eu$perc[4], q5.eu$perc[5]))

p0 <- ggplot(data1 %>% as_tibble()) +
  geom_mosaic(aes(weight = n, x = product(Sex), fill=Age)) + 
  # move x-axis to top
  scale_x_productlist(position = 'top') +
  scale_fill_manual(values=c("#F9F7F3", "#B5E2FA", "#0FA3B1")) +
  theme_minimal() +
  theme(legend.position = 'none', axis.title = element_blank())
p <- p0 + geom_text(
    # extract rectangle centers, add labels
    data = 
      # extract rectangle centers, add strings for labels
      ## 'layer_data(p, 1)' extracts data frame with data from 1st layer of p
      layer_data(p0, 1) %>% 
      select(xmin:ymax) %>% 
      mutate(m.x = (xmin + xmax)/2, m.y = (ymin + ymax)/2) %>% 
      select(m.x, m.y)  %>% 
      mutate(string = perc_label),
    # set label locations to centers, set labels to strings
    aes(x = m.x, y = m.y, label = string)
  ) +
  labs(title = "European Union", subtitle = "Asylum applications", caption = "Source: Eurostat.\n(online codes: migr_asyunaa and migr_asyappctza)")
  

print(paste0("EU, Total UASC 2014-2024: ", sum(q5.eu$values)))


#
EU.as.age.UASC <- migr_asyunaa |> filter(TIME_PERIOD >=2014, TIME_PERIOD <= 2024, citizen == "TOTAL", geo %in% EU27, sex=="T", age %in% c("Y14-15", "Y16-17","Y_LT14" )) |> group_by(age) |> summarise(UASC=sum(values), .groups = 'drop')
EU.as.age.children <- migr_asyappctza |> filter(TIME_PERIOD >=2014, TIME_PERIOD <= 2024, citizen == "TOTAL", geo %in% EU27, asyl_app =="ASY_APP", sex=="T", age %in% c("Y14-17","Y_LT14")) |> group_by(age) |> summarise(children=sum(values), .groups = 'drop')

#correct numbers
EU.as.age.accompanied <- EU.as.age.children
EU.as.age.accompanied$accompanied <- EU.as.age.accompanied$children
EU.as.age.accompanied$accompanied[1] <- EU.as.age.accompanied$children[1] - sum(EU.as.age.UASC$UASC[1:2])
EU.as.age.accompanied$accompanied[2] <- EU.as.age.accompanied$children[2] - EU.as.age.UASC$UASC[3]

EU.as.age.UASC |> mutate(UASC=round(100 * UASC / sum(UASC)))
EU.as.age.accompanied |> mutate(accompanied = round(100*accompanied/sum(accompanied)))

## Mex Gender and age ----
q5.mex <- mex |> 
  filter(Year >= 2014, Status=='Unaccompanied') |> 
  group_by(Sex, Age) |> 
  summarise(values = sum(Children), .groups = 'drop') |> 
  mutate(perc = values/sum(values))

data3 <- as.table(matrix(q5.mex$values, 2, 2))
dimnames(data3) <- list(Age = c("Ages\n0 to 11", "Ages\n12 to 17"),
                        Sex = c("Female", "Male"))
perc_label <- label_percent(accuracy = 1)(q5.mex$perc)

p2_0 <- ggplot(data3 %>% as_tibble()) +
  geom_mosaic(aes(weight = n, x = product(Sex), fill=Age)) + 
  # move x-axis to top
  scale_x_productlist(position = 'top') +
  scale_fill_manual(values=c("#B5E2FA", "#0FA3B1"))+
  theme_minimal() +
  theme(legend.position = 'none', axis.title = element_blank())
p2 <- p2_0 + geom_text(
    # extract rectangle centers, add labels
    data = 
      # extract rectangle centers, add strings for labels
      ## 'layer_data(p, 1)' extracts data frame with data from 1st layer of p
      layer_data(p2_0, 1) %>% 
      select(xmin:ymax) %>% 
      mutate(m.x = (xmin + xmax)/2, m.y =  (ymin + ymax)/2) %>% 
      select(m.x, m.y)  %>% 
      mutate(string = perc_label),
    # set label locations to centers, set labels to strings
    aes(x = m.x, y = m.y, label = string))+
  labs(title = "Mexico", subtitle = "Encounters in an irregular situation", caption = "Source: Secretaria de Gobernacion")

print(paste0("Mex, Total UASC in 2014-2024: ", sum(q5.mex$values)))

ggarrange(p,p2, nrow = 1)
ggsave(file.path(projectFolder, "Fig2.pdf"),width = 25, height = 9, units = 'cm')


# Q4 Gender ----
EU.as.2014.2023.sex <- migr_asyappctza |> filter(TIME_PERIOD >=2014, TIME_PERIOD <=2023, citizen == "TOTAL", geo=="EU27_2020", age=="TOTAL", asyl_app =="ASY_APP", sex!="T") |> group_by(sex) |> summarise(total=sum(values), .groups = 'drop') |> mutate(total=round(100*total/sum(total)))
EU.as.2014.2023.sex.children <- migr_asyappctza |> filter(TIME_PERIOD >=2014, TIME_PERIOD <=2023, citizen == "TOTAL", geo=="EU27_2020", age=="Y_LT18", asyl_app =="ASY_APP", sex!="T") |> group_by(sex) |> summarise(children=sum(values), .groups = 'drop')|> mutate(children=round(100*children/sum(children)))
EU.as.2014.2023.sex.UASC <- migr_asyunaa |> filter(TIME_PERIOD >=2014, TIME_PERIOD <=2023, citizen == "TOTAL", geo=="EU27_2020", age=="TOTAL", sex!="T") |> group_by(sex) |> summarise(UASC=sum(values), .groups = 'drop')|> mutate(UASC=round(100*UASC/sum(UASC)))
print(full_join(EU.as.2014.2023.sex,EU.as.2014.2023.sex.children, by="sex") |> full_join(EU.as.2014.2023.sex.UASC, by="sex"))

## Mexico ----
print(paste0("% boys among all children in irregular situation in 2023: ", round(100*61236/(61236+52306))))
print(paste0("% boys among UASC in irregular situation in 2023: ", round(100*(4257+312)/(4257+1856+312+307))))

# Data InSight 2  ----
ukr <- readxl::read_xlsx(file.path(rawdataFolder, "Eurostat/Temporary_protection_for_persons_fleeing_Ukraine_monthly_statistics_August_2025.xlsx"),
                         range = "UAM Figure 4!T4:U30")
ukr$...1[ukr$...1 == "Netherlands*"] <- "Netherlands"
ukr <- ukr |> 
  filter(...1 != "Switzerland") |> 
  mutate(share = as.numeric(`Share of unaccompanied minors in the total number of minors granted temporary protection from March 2022 to June 2025 (%)`) )
ukr$country <- factor(ukr$...1, levels = ukr$...1)
           
g <- ggplot(ukr, aes(x = country, y = share)) + 
  geom_bar(stat = "identity", fill = "blue") +
  labs(title = "Share of unaccompanied minors in the total number of minors\ngranted temporary protection between March 2022 and June 2025",
       subtitle = "%",
       caption = "Source: Eurostat, 2025\nonline codes: migr_asytpfm, migr_asytpfq, migr_asyumtpfm, migr_asyumtpfq") +
  xlab("") + ylab("") + 
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 70, hjust = 1),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank())

print(g)
ggsave(file.path(projectFolder, "FigDataInsight2 .pdf"), width = 25, height = 18, units = 'cm')


# Box Ukraine  ----
migr_asytpsm$region <- NA
migr_asytpsm$region[migr_asytpsm$geo %in% c("IS", "NO", "LI", "CH")] <- "EFTA"
migr_asytpsm$region[migr_asytpsm$geo %in% eu_countries$code] <- "EU27_2020"

# Beneficiaries of temporary protection at the end of the month by citizenship, age and sex - monthly data
ukr_beneficiaries_2024_june <- migr_asytpsm |> 
  filter(age %in% c("Y_LT18"),
         sex ==  "T",
         citizen == "UA",
         TIME_PERIOD > 2024.4,
         TIME_PERIOD < 2024.45)

ukr_beneficiaries_2024_june_sum <- ukr_beneficiaries_2024_june |> 
  group_by(region, age) |> 
  summarise(values = sum(values), .groups='drop')

#Total in temporary protection by age and region  in June 2025
ukr_beneficiaries_by_age_inJune2025 <- migr_asytpsm |> 
  filter(!is.na(region),
         sex ==  "T",
         citizen == "UA",
         TIME_PERIOD > 2025.4,
         TIME_PERIOD < 2025.45) |> 
  group_by(region, age) |> 
  summarise(values = sum(values), .groups='drop')

#	Decisions granting temporary protection by citizenship, age and sex - quarterly data
ukr_decisions <- migr_asytpfq |> filter(geo %in% c("EU27_2020", "IS", "NO", "LI", "CH"),
                              age %in% c("TOTAL", "Y_LT18", "Y14-17", "Y_LT14"),
                              citizen == "UA",
                              sex=="T")
ukr_decisions$geo[ukr_decisions$geo %in% c("IS", "NO", "LI", "CH")] <- "EFTA"
ukr_decisions_summayr <- ukr_decisions |> 
  group_by(geo, age) |> 
  summarise(OBS_VALUE = sum(OBS_VALUE), .groups='drop')




#18 EU countries + EFTA (Switzerland + Iceland, Malta, Norway, Liechtenstein)

temp_prot_UKR_UASC <- migr_asytpfa |> 
  filter(citizen == "UA", sex=="T", age %in% c("Y14-17", "Y_LT14")) |> 
  group_by(age, geo) |> 
  summarise(values = sum(values), .groups = 'drop')

#	Decisions granting temporary protection by citizenship, age and sex - annual aggregated data
temp_prot_UKR <- migr_asytpfa |> 
  filter(age %in% c("Y14-17", "Y_LT14"), citizen == "UA", sex=="T", TIME_PERIOD %in% c(2022, 2023), geo %in% temp_prot_UKR_UASC) |> 
  group_by(age) |> 
  summarise(values = sum(values), .groups = 'drop')

print(paste0("Number of Ukrainan children granted Temp. Prot. EU 18 + EFTA (2022-2023): ", sum(temp_prot_UKR_UASC$values)))
print(paste0("Y_LT14: ", temp_prot_UKR$values[1]))
print(paste0("Y14-17: ", temp_prot_UKR$values[2]))
print(paste0("Number of Ukrainan UASC granted Temp. Prot. EU 18 + EFTA (2022-2023): ", sum(temp_prot_UKR_UASC$values)))
print(paste0("Y_LT14: ", temp_prot_UKR_UASC$values[1]))
print(paste0("Y14-17: ", temp_prot_UKR_UASC$values[2]))
print(paste0("% of UASC among UKR children: ", round(100*14210/501205)))


# FIG EU Annual UASC Asylum applicants Age ----
g <- ggplot(migr_asyunaa_sum, aes(x=TIME_PERIOD, y=values, fill=Age))+
  geom_bar(position = 'stack', stat="identity")+
  theme_minimal()+
  scale_x_continuous(name = "", breaks = c(2012:2024))+
  scale_y_continuous(name ="Children (thousands)", limits=c(0,100000), breaks = seq(0, 100000, 25000), labels = c(0, 25, 50, 75, 100))+
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.text.x = element_text(hjust = 1, vjust = 0.5, angle = 90))+
  scale_fill_manual(values=c("#271033", "#388697", "#FFE882","#FA8334"))

print(g)

ggsave(filename = file.path(projectFolder, "UNAA_EU_annual.pdf"), 
       plot = g,
       width=12, height=6, units='cm')



# FIG EU Annual UASC Asylum applicants Sex ----
sex_join <- tibble(sex=c("F", "M", "UNK"),
                   Sex=factor(x = c("Female", "Male", "Unknown"),
                              levels=c("Female", "Male", "Unknown")))

migr_asyunaa_sum <- migr_asyunaa |> 
  filter(sex %in% c('F', 'M', 'UNK')) |> 
  filter(age %in% c("UNK", "Y_LT14", "Y14-15", "Y16-17")) |> 
  filter(citizen == "EXT_EU27_2020") |> 
  filter(geo == "EU27_2020") |> 
  left_join(sex_join, by='sex')

g <- ggplot(migr_asyunaa_sum, aes(x=TIME_PERIOD, y=values, fill=Sex))+
  geom_bar(position = 'stack', stat="identity")+
  theme_minimal()+
  scale_x_continuous(name = "", breaks = c(2012:2023))+
  scale_y_continuous(name ="Children (thousands)", limits=c(0,100000), breaks = seq(0, 100000, 25000), labels = c(0, 25, 50, 75, 100))+
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.text.x = element_text(hjust = 1, vjust = 0.5, angle = 90))+
  scale_fill_manual(values=c("#271033", "#388697", "#FFE882","#FA8334"))

print(g)

ggsave(filename = file.path(projectFolder, "UNAA_EU_annual_sex.pdf"), 
       plot = g,
       width=12, height=6, units='cm')



# FIG EU Annual % UASC Asylum applicants ----


migr_asyunaa_sum <- migr_asyunaa |> 
  filter(sex =='T') |> 
  filter(age == "TOTAL") |> 
  filter(citizen == "EXT_EU27_2020") |> 
  filter(geo == "EU27_2020") |> 
  rename(UNAA = values)

#Total and minor applicants
migr_asyappctza_sum <- migr_asyappctza |> 
  filter(sex =='T') |> 
  filter(citizen == "EXT_EU27_2020") |> 
  filter(geo == "EU27_2020") |> 
  filter(asyl_app == "ASY_APP") |> 
  group_by(TIME_PERIOD) |> 
  summarise(
    Y_LT18=sum(values[age=="Y_LT18"]),
    TOTAL=sum(values[age=="TOTAL"]))
  
migr_asyunaa_sum <- migr_asyunaa_sum |>
  left_join(migr_asyappctza_sum, by="TIME_PERIOD") |> 
  mutate(perc_UNAA = UNAA/Y_LT18)

g <- ggplot(migr_asyunaa_sum, aes(x=TIME_PERIOD, y=perc_UNAA, fill=age))+
  geom_line(stat="identity")+
  ylim(c(0,0.5))+
  ggtitle("Percentage of asylum-seeking UASC among children")
print(g)
ggsave(filename = file.path(projectFolder, "perc_UNAA_EU.pdf"), 
       plot = g,
       width=10, height=6, units='cm')




# FIG EU Monthly UASC Asylum applicants ----
#It is only from 2018

#search_results <- search_eurostat("sylum applicants considered to be unaccompanied minors by citizenship, age and sex - annual data",
#                                  type = "dataset")


#Downloading data from Eurostat

migr_asyumactm_sum <- migr_asyumactm |> 
  filter(sex =='T') |> 
  filter(age %in% c("UNK", "Y_LT14", "Y14-17")) |> 
  filter(citizen == "EXT_EU27_2020") |> 
  group_by(age, TIME_PERIOD) |> 
  summarise(values=sum(values))

g <- ggplot(migr_asyumactm_sum, aes(x=TIME_PERIOD, y=values, fill=age))+
  geom_bar(position = 'stack', stat="identity")+
  ggtitle("EU asylum applicants considered to be unaccompanied minors by age")
ggsave(filename = file.path(projectFolder, "UNAA_EU_monthly.pdf"), 
       plot = g,
       width=12, height=6, units='cm')

# FIG EU UASC decisions ----
#Downloading data from Eurostat
migr_asyumdcfq <- get_eurostat("migr_asyumdcfq", time_format = "num", stringsAsFactors = TRUE)
saveRDS(object = migr_asyumdcfq, file = file.path(rawdataFolder, "Eurostat/migr_asyumdcfq.Rdata"))


migr_asyumdcfq_sum <- migr_asyumdcfq |> 
  filter(sex =='T') |> 
  filter(age %in% c("Y_LT18")) |> 
  filter(citizen == "EXT_EU27_2020") |> 
  filter(decision %in% c("GENCONV", "HUMSTAT", "SUB_PROT", "REJECTED")) |> 
  group_by(TIME_PERIOD, decision) |> 
  summarise(values=sum(values, na.rm=T))

g <- ggplot(migr_asyumdcfq_sum, aes(x=TIME_PERIOD, y=values, fill=decision))+
  geom_bar(position = 'stack', stat="identity")+
  ggtitle("Decisions on asylum-seeking UASC in EU")
ggsave(filename = file.path(projectFolder, "UNAA_EU_decisions.pdf"), 
       plot = g,
       width=12, height=6, units='cm')


# FIG US CBP ----
sbo_19_22 <- read.csv(file.path(rawdataFolder, "US CBP/sbo-encounters-fy19-fy22.csv"))

sbo_21_24 <- read.csv(file.path(rawdataFolder, "US CBP/sbo-encounters-fy21-fy24-feb.csv"))
sbo_21_24$Fiscal.Year[sbo_21_24$Fiscal.Year=="2024 (FYTD)"] <- "2024"
sbo_21_24$Fiscal.Year <- as.numeric(sbo_21_24$Fiscal.Year)

month.order <- tibble("Month..abbv." = c("JAN", "FEB", "MAR", "APR", "MAY", "JUN", "JUL", "AUG", "SEP", "OCT", "NOV", "DEC"),
                      "Month.order" = seq(1,12,1))

sbo_19_24 <- bind_rows(sbo_19_22, sbo_21_24) |> 
  filter(Month.Grouping == "FYTD") |>   #some are "remaining" Don't know what that means
  distinct()

#Convert FY to calendar year
#US Gov FY runs from 1-Oct to 30-Sep, The identification of a fiscal year is the calendar year in which it ends
#October to December are in the previous calendar year
#sbo_19_24$Year <- sbo_19_24$Fiscal.Year
#sbo_19_24$Year[sbo_19_24$Month..abbv. %in% c("OCT", "NOV", "DEC")] <- sbo_19_24$Year[sbo_19_24$Month..abbv. %in% c("OCT", "NOV", "DEC")]-1

#summarise
sbo_19_24_sum <- sbo_19_24 |> 
  filter(Demographic %in% c("UC / Single Minors")) |> 
  group_by(Fiscal.Year, `Month..abbv.`,Demographic) |> 
  summarise(count = sum(`Encounter.Count`)) 

sbo_19_24_sum$Month..abbv. <- factor(sbo_19_24_sum$Month..abbv.,
                                     labels = c( "OCT", "NOV", "DEC","JAN", "FEB", "MAR", "APR", "MAY", "JUN", "JUL", "AUG", "SEP"))

g <- ggplot(sbo_19_24_sum, aes(x=`Month..abbv.`, y=count, group=Fiscal.Year, color=factor(Fiscal.Year)))+
  geom_line()
print(g)

# FIG Mexico ----
mex <- readxl::read_xlsx(path = file.path(rawdataFolder, "Mexico/Compilacion_Cuadro_3_1_5_eventos_de_ninos.xlsx"), sheet = "Sheet1")
mex$Age <- factor(x = mex$Age, levels=c("12 to 17", "0 to 11"))
mex$Sex <- factor(x = mex$Sex, levels=c("Female", "Male"))


mex_sum_1 <- mex |> filter(Status == "Unaccompanied") |>
  group_by(Year, Sex) |> 
  summarise(Children = sum(Children), .groups='drop')

mex_sum_2 <- mex |> filter(Status == "Unaccompanied") |>
  group_by(Year, Age) |> 
  summarise(Children = sum(Children), .groups='drop')


g1 <- ggplot(mex_sum_1, aes(x=Year, y=Children, fill=Sex))+
  geom_bar(position = "Stack", stat="identity")+
  theme_minimal()+
  scale_x_continuous(name = "", breaks = c(2014:2023))+
  scale_y_continuous(name ="Children (thousands)", limits=c(0,21000), breaks = seq(0, 20000, 5000), labels = c(0, 5, 10, 15, 20))+
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.text.x = element_text(hjust = 1, vjust = 0.5, angle = 90))+
  scale_fill_manual(values=c( "#388697", "#FFE882","#FA8334"))
g2 <- ggplot(mex_sum_2, aes(x=Year, y=Children, fill=Age))+
  geom_bar(position = "Stack", stat="identity")+
  theme_minimal()+
  scale_x_continuous(name = "", breaks = c(2014:2023))+
  scale_y_continuous(name ="Children (thousands)", limits=c(0,21000), breaks = seq(0, 20000, 5000), labels = c(0, 5, 10, 15, 20))+
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.text.x = element_text(hjust = 1, vjust = 0.5, angle = 90))+
  scale_fill_manual(values=c( "#388697", "#FFE882","#FA8334"))

ggarrange(g1, g2, ncol = 2, labels=c("A","B")) |> 
  ggexport(filename=file.path(projectFolder, "MEX_Events.pdf"))
# Fig X Nationality ----
## EU ----
eu.nationality <- migr_asyunaa |>
  filter(geo == "EU27_2020", age == "TOTAL", sex=="T", !(citizen %in% c("TOTAL", "EU27_2020", "EXT_EU27_2020"))) |> 
  mutate(citizen=as.character(citizen))

#Fixing codes for countries to iso2
unique(eu.nationality$citizen) [!(unique(eu.nationality$citizen) %in% country_metadata$iso2)]
eu.nationality$citizen[eu.nationality$citizen == "EL"] <- "GR"
eu.nationality$citizen[eu.nationality$citizen == "UK"] <- "GB"
eu.nationality$citizen[eu.nationality$citizen == "UK_OCT"] <- "GB"

eu.nationality <- eu.nationality |> 
  left_join(country_metadata |> select(iso2, Location) , by = c("citizen"="iso2"))
eu.nationality$Location[eu.nationality$citizen == "RNC"] <- "Recognized non-citizen"
eu.nationality$Location[eu.nationality$citizen == "STLS"] <- "Stateless"
eu.nationality$Location[eu.nationality$citizen == "UNK"] <- "Unknown"

#Making an 'other' category to simplify visualization
eu.nationality$Location[!(eu.nationality$citizen %in% c("SY", "AF", "SO", "ER", "PK"))] <- "Other"
eu.nationality$Location <- factor(eu.nationality$Location, levels = c("Afghanistan",
                                                                      "Syrian Arab Republic",
                                                                      "Somalia",
                                                                      "Eritrea",
                                                                      "Pakistan",
                                                                      'Other'))

eu.nationality <- eu.nationality |>
  group_by(Location, TIME_PERIOD) |> 
  summarise(values = sum(values), .groups='drop') |> 
  rename(Citizenship = Location)

q7.eu <- ggplot(eu.nationality, aes(x=TIME_PERIOD, y=values, fill=Citizenship))+
  geom_bar(stat = 'identity', position = position_stack())+
  theme_minimal()+
  scale_x_continuous(breaks = seq(2012, 2023, 1))+
  scale_y_continuous(breaks = seq(0, 100000, 25000), limits = c(0, 100000))+
  scale_fill_manual(values = c("#1CABE2", "#FAF3DD", "#C8D5B9", "#E77728", "#EF3E36", "grey"))+
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank())+
  xlab("Year") + ylab("Asylum applications of unaccompanied minors")
print(q7.eu)

## USA ----
us.nationality <- sbo_19_24 |> 
  filter(Demographic == "UC / Single Minors") |> 
  group_by(Fiscal.Year, Citizenship.Grouping) |> 
  summarise(Encounter.Count = sum(Encounter.Count), .groups = 'drop') |> 
  rename(Citizenship = Citizenship.Grouping)
us.nationality$Citizenship <- factor(us.nationality$Citizenship, levels=c("Guatemala","Honduras","Mexico","El Salvador","Other"))

#to check for percentages of all applications
eu.nationality |> filter(TIME_PERIOD>2013) |> group_by(Citizenship) |> summarise(value = sum(values)) |> mutate(perc=value/sum(value))


q7.us <- ggplot(us.nationality, aes(x=Fiscal.Year, y=Encounter.Count, fill=Citizenship))+
  geom_bar(stat = 'identity', position = position_stack())+
  theme_minimal()+
  scale_x_continuous(breaks=seq(2019, 2023, 1))+
  #scale_y_continuous(breaks=seq(0, 100000, 25000), limits = c(0, 100000))+
  scale_fill_manual(values=c("#1CABE2", "#FAF3DD", "#C8D5B9", "#E77728", "grey"))+
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank())+
  xlab("Fiscal year") + ylab("Border encounters of unaccompanied minors")
print(q7.us)

##  UK ----
#to check for percentages of all applications
uk.children.perc <- uk |> 
  group_by(Nationality) |> 
  summarise(Children = sum(Applications), .groups = 'drop') |> 
  mutate(Children_perc=round(100*Children/sum(Children))) 

uk.uasc.perc <- uk |> 
  filter(UASC == "UASC")  |> 
  group_by(Nationality) |> 
  summarise(UASC = sum(Applications), .groups = 'drop') |> 
  mutate(UASC_perc=round(100*UASC/sum(UASC))) |> 
  arrange(desc(UASC)) |> 
  left_join(uk.children.perc, by="Nationality")

g.uk.scatter <- ggplot(uk.uasc.perc, aes(x=Children_perc, y=UASC_perc))+
  geom_point()+
  geom_text(mapping = aes(label = `Nationality`))+
  geom_abline(intercept = 0, slope =1)
print(g.uk.scatter)


uk.nationality.data <- uk
uk.nationality.data$Nationality[!(uk.nationality.data$Nationality %in% c("Afghanistan", "Eritrea", "Iran", "Sudan", "Albania"))] <- "Other"

uk.nationality <- uk.nationality.data |> 
  filter(UASC == "UASC") |> 
  group_by(Year, Nationality) |> 
  summarise(Applications = sum(Applications), .groups = 'drop')
uk.nationality$Citizenship <- factor(uk.nationality$Nationality, levels = c("Afghanistan", "Eritrea", "Iran", "Sudan", "Albania",
                                                                            'Other'))

q7.uk <- ggplot(uk.nationality, aes(x=Year, y=Applications, fill=Citizenship))+
  geom_bar(stat = 'identity', position = position_stack())+
  theme_minimal()+
  scale_x_continuous(breaks=seq(2014, 2023, 1))+
  #scale_y_continuous(breaks=seq(0, 100000, 25000), limits = c(0, 100000))+
  scale_fill_manual(values=c("#1CABE2", "#FAF3DD", "#C8D5B9", "#E77728", "#EF3E36", "grey"))+
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank())+
  xlab("Year") + ylab("Asylum applications of unaccompanied minors")
print(q7.uk)

ggarrange(q7.eu,q7.uk, q7.us, nrow = 3, labels=c('EU', "UK", "USA"), label.x=0.25)
ggsave(file.path(projectFolder, "Q7.pdf"), width = 18, height = 30, units = 'cm')


