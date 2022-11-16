setwd('C:/Users/dosullivan1/IdeaProjects/data_visualisations/MSFD')

library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)
library(stringr)

pom_report_net_data = read_excel('./ReportNetFiles2022/ReportNetFile_Oct2022_V31.xlsx')
overall_status = read_excel("C:/Users/dosullivan1/IdeaProjects/data_visualisations/MSFD/2018 Article 8 9 10 last reported data/Art8_GES/OverallStatus.xlsx")


new_ecosystem_features = data.frame(old_feature = rep('EcosystemFoodWeb', 5),
                                    new_feature = c('CharaChem', 'CharaPhyHydro', 'EcosysCoastal', 'EcosysOceanic', 'EcosysShelf')
)

old_new_mappings = data.frame(GEScomponent_2015 = c(rep('D1', 19), rep('D10', 2), 'D3', 'D6/D1'),	
                              feature_2015 = c(rep('Birds', 5), rep('Mammals', 4), 'Reptiles', rep('Fish', 5), 
                                               rep('Cephalopods', 2), rep('Water column habitats', 2), rep('Seabed habitats', 2), 
                                               'Fish', 'Seabed habitats'),
                              GEScomponent_2022 = c(rep('D1.1', 5), rep('D1.2', 4), 'D1.3', rep('D1.4', 5), 
                                                    rep('D1.5', 2), rep('D1.6', 2), rep('D10', 2), 
                                                    'D3', 'D6'),	
                              feature_2022 = c('BirdsBenthicFeeding', 'BirdsGrazing', 'BirdsPelagicFeeding', 'BirdsSurfaceFeeding', 'BirdsWading',
                                               'MamCetacBaleenWhales', 'MamCetacDeepDiving', 'MamCetacSmall', 'MamSeals',
                                               'RepTurtles',
                                               'FishCoastal', 'FishDeepSea', 'FishDemersalShelf', 'FishPelagicShelf', 'PresEnvBycatch',
                                               'CephaCoastShelf', 'CephaDeepSea',
                                               'HabPelBHT', 'HabPelOther',
                                               'PresEnvLitterMicro', 'PresEnvLitter',
                                               'FishCommercial', 'PresPhyDisturbSeabed')
)


# Data Cleaning on POMs 2022 data
poms_2022 =
  pom_report_net_data%>%
  mutate(GEScomponent = strsplit(as.character(GEScomponent), "; ")) %>% 
  unnest(GEScomponent) %>%
  mutate(GEScomponent = strsplit(as.character(GEScomponent), ";")) %>% 
  unnest(GEScomponent) %>%
  mutate(GEScomponent = strsplit(as.character(GEScomponent), ", ")) %>% 
  unnest(GEScomponent) %>%
  mutate(GEScomponent = ifelse(GEScomponent==8, 'D8', ifelse(GEScomponent==10, 'D10', 
                                                             ifelse(GEScomponent==1, 'D1',
                                                                    ifelse(GEScomponent==4,'D4',
                                                                           ifelse(GEScomponent==6,'D6',
                                                                                  ifelse(str_starts(GEScomponent, 'D1\\.'), 'D1', 
                                                                                         GEScomponent))))))
  ) %>% 
  unnest(GEScomponent) %>%
  mutate(MeasureOldCode = strsplit(as.character(MeasureOldCode), ";")) %>% 
  mutate(Feature = strsplit(as.character(Feature), "; ")) %>% 
  unnest(Feature) %>%
  mutate(Feature = strsplit(as.character(Feature), ";")) %>% 
  unnest(Feature) %>% 
  unnest(MeasureOldCode) %>%
  select(MeasureCode, MeasureOldCode, Feature, GEScomponent, UpdateType) %>%
  unique()


###########################################################
## Gap Analysis
## Count the number of measures in 2015 Update Type
count_2015_measures = 
  overall_status %>%
  mutate(GEScomponent = ifelse(str_starts(GEScomponent, 'D1\\.'), 'D1', 
                               GEScomponent)) %>%
  left_join(poms_2022, by = c("GEScomponent"="GEScomponent")) %>%
  filter(UpdateType %in% c('Measure same as in 2015 PoM', '2015 measure that was not reported electronically',
                           'Measure modified since 2015 PoM', 'Measure withdrawn')) %>%
  group_by(GEScomponent) %>%
  summarise(no_measures_2015 = n_distinct(MeasureOldCode, na.rm=TRUE))

## Count how 2022 measures
count_2022_measures = 
  overall_status %>%
  mutate(GEScomponent = ifelse(str_starts(GEScomponent, 'D1\\.'), 'D1', 
                               GEScomponent)) %>%
  left_join(poms_2022, by = c("GEScomponent"="GEScomponent")) %>%
  filter(!UpdateType %in% c("Measure withdrawn")) %>%
  group_by(GEScomponent) %>%
  summarise(no_measures_2022 = n_distinct(MeasureCode, na.rm=TRUE))

## Count new measures in 2022
new_2022_measures = 
  overall_status %>%
  mutate(GEScomponent = ifelse(str_starts(GEScomponent, 'D1\\.'), 'D1', 
                               GEScomponent)) %>%
  left_join(poms_2022, by = c("GEScomponent"="GEScomponent")) %>%
  filter(UpdateType %in% c("Measure new in 2021 PoM")) %>%
  group_by(GEScomponent) %>%
  summarise(new_measures_2022 = n_distinct(MeasureCode, na.rm=TRUE))

# Count of modified measures in 2022
modified_2022_measures = 
  overall_status %>%
  mutate(GEScomponent = ifelse(str_starts(GEScomponent, 'D1\\.'), 'D1', 
                               GEScomponent)) %>%
  left_join(poms_2022, by = c("GEScomponent"="GEScomponent")) %>%
  filter(UpdateType %in% c("Measure modified since 2015 PoM")) %>%
  group_by(GEScomponent) %>%
  summarise(modified_measures_2022 = n_distinct(MeasureCode, na.rm=TRUE))


# Join all together and write to CSV
count_2015_measures %>%
  left_join(count_2022_measures, by=c("GEScomponent"="GEScomponent")) %>%
  left_join(new_2022_measures, by=c("GEScomponent"="GEScomponent")) %>%
  left_join(modified_2022_measures, by=c("GEScomponent"="GEScomponent")) %>%
  write.csv(., file = './GeneratedDataFiles/gap_analysis_v3.csv')
