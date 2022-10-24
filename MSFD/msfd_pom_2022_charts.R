# MSFD Programme of Measures 2022 Charts
# Date: 11/10/2022
# Created by: Denise O'Sullivan
# Code to create the charts used in the POMs 2022 report

setwd('C:/Users/dosullivan1/IdeaProjects/data_visualisations/MSFD')

library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)
library(stringr)
library(ggpubr)
library(tidyverse)
library(treemap)
library(waffle)
library(packcircles)

################### ReportNetData ##################################
pom_report_net_data = read_excel('ReportNetFile_Oct2022_V13.xlsx')
descriptors_list = c('D1', 'D2', 'D3', 'D4', 'D5', 'D6', 'D7', 'D8', 'D9', 'D10', 'D11')
names(descriptors_list) = c('D1', 'D2', 'D3', 'D4', 'D5', 'D6', 'D7', 'D8', 'D9', 'D10', 'D11')

### Data Cleaning ####
heatmap_data = 
  pom_report_net_data %>%
  filter(ImplementationStatus != 'Measure withdrawn') %>%
  mutate(MeasurePurpose = gsub("\\s*\\([^\\)]+\\)","",as.character(MeasurePurpose))) %>%
  mutate(MeasurePurpose = strsplit(as.character(MeasurePurpose), "; ")) %>%
  unnest(MeasurePurpose) %>%
  mutate(ResponsibleCompetentAuthority = strsplit(as.character(ResponsibleCompetentAuthority), "; ")) %>% 
  unnest(ResponsibleCompetentAuthority) %>%
  mutate(ResponsibleCompetentAuthority = ifelse(ResponsibleCompetentAuthority=='Department of the Environment Climate, and Communications', 
                                                'Department of the Environment, Climate and Communications',
                                                ResponsibleCompetentAuthority)) %>% 
  mutate(PoliciesConventions = strsplit(as.character(PoliciesConventions), "; ")) %>% 
  unnest(PoliciesConventions) %>%
  mutate(PoliciesConventions = strsplit(as.character(PoliciesConventions), ";")) %>% 
  unnest(PoliciesConventions) %>%
  mutate(GEScomponent = strsplit(as.character(GEScomponent), "; ")) %>% 
  unnest(GEScomponent) %>%
  mutate(GEScomponent = strsplit(as.character(GEScomponent), ";")) %>% 
  unnest(GEScomponent) %>%
  mutate(GEScomponent = strsplit(as.character(GEScomponent), ", ")) %>% 
  unnest(GEScomponent) %>%
  mutate(GEScomponent = strsplit(as.character(GEScomponent), "/")) %>% 
  unnest(GEScomponent) %>%
  mutate(GEScomponent = ifelse(GEScomponent==8, 'D8', ifelse(GEScomponent==10, 'D10', 
                                                             ifelse(GEScomponent==1, 'D1',
                                                                    ifelse(GEScomponent==4,'D4',
                                                                           ifelse(GEScomponent==6,'D6',
                                                                                  ifelse(str_starts(GEScomponent, 'D1\\.'), 'D1', 
                                                                                         GEScomponent)))))
  )
  ) %>% 
  unnest(GEScomponent) %>%
  mutate(GEScomponent = factor(descriptors_list[GEScomponent], ordered=TRUE, levels = rev(descriptors_list))) %>%
  mutate(CoordinationLevel = strsplit(as.character(CoordinationLevel), "; ")) %>% 
  unnest(CoordinationLevel) %>%
  mutate(CoordinationLevel = strsplit(as.character(CoordinationLevel), ";")) %>% 
  unnest(CoordinationLevel) %>%
  mutate(RelevantKTMs = strsplit(as.character(RelevantKTMs), "; ")) %>% 
  unnest(RelevantKTMs) %>%
  mutate(RelevantKTMs = strsplit(as.character(RelevantKTMs), ";")) %>% 
  unnest(RelevantKTMs) %>%
  mutate(RelevantKTMs = ifelse(RelevantKTMs=='ALL MSFD CODES', RelevantKTMs,
                               strsplit(as.character(RelevantKTMs), " "))) %>% 
  unnest(RelevantKTMs) %>%
  mutate(Feature = strsplit(as.character(Feature), "; ")) %>% 
  unnest(Feature) %>%
  mutate(Feature = strsplit(as.character(Feature), ";")) %>% 
  unnest(Feature) %>% 
  mutate(PolicyNational = strsplit(as.character(PolicyNational), "; ")) %>% 
  unnest(PolicyNational) %>%
  mutate(PolicyNational = strsplit(as.character(PolicyNational), ";")) %>% 
  unnest(PolicyNational) %>%
  select(MeasureCode, GEScomponent, CoordinationLevel, PoliciesConventions, MeasurePurpose, ResponsibleCompetentAuthority,
         DPSiR, ImplementationStatus, UpdateType, RelevantKTMs, Feature, `Type of measure`, PolicyNational)



#### BUBBLE CHART - POLICY vs DESCRIPTOR ####
heatmap_data %>%
  group_by(PoliciesConventions, GEScomponent) %>%
  summarise(n = n_distinct(MeasureCode)) %>%
  ggballoonplot(x='GEScomponent', y='PoliciesConventions', 
                size='n',
                fill='GEScomponent') +
  labs(size='Number of Measures',
       y='Policy',
       x='Descriptor',
       title = 'Number of Measures per Descriptor and Policy') +
  scale_fill_manual(values=rev(c('#FF0000',
                                 '#12AD5E', # biodiversity
                                 '#CECD3C', # NI Species
                                 '#39BFBE', # commercial shellfish
                                 '#61B5E5', # food webs
                                 '#70923D', # eutrophication
                                 '#f78308',#rgb(0.9648438, 0.5195312, 0.3164062),#, # seafloor integrity
                                 '#585995', # hydrographical conditions
                                 '#5b5e5d', # contaminants
                                 '#c21f42', # contaminants in seafood
                                 '#176552', # marine litter
                                 '#1A78B6' # underwater noise
  ))) +
  scale_size_continuous(breaks=c(0,5,10,15,20,25,30),
                        range=c(1,12.5),
                        labels=c('0', '1 - 5', '6 - 10', '11 - 15', '16 - 20', '21 - 25', '26 - 30')
                        ) +
  scale_x_discrete(position='top',limits = rev(levels(heatmap_data$GEScomponent))) +
  scale_y_discrete(limits = rev(sort(unique(heatmap_data$PoliciesConventions)))) +
  theme_minimal() +
  theme(plot.title = element_text(hjust=0.5)) +
  guides(fill="none")
ggsave('POM_charts/policy_descriptor_balloon.jpg')

#### TREEMAP - DPSiR and Descriptor ####
heatmap_data %>%
  group_by(DPSiR, GEScomponent) %>%
  summarise(n = as.double(n_distinct(MeasureCode))) %>%
  treemap(
    index = c("GEScomponent", "DPSiR"),
    vSize = "n",
    vColor = "GEScomponent",
    type='categorical',
    palette = rev(c('#12AD5E', # biodiversity
                    '#CECD3C', # NI Species
                    '#39BFBE', # commercial shellfish
                    '#61B5E5', # food webs
                    '#70923D', # eutrophication
                    '#f78308',#rgb(0.9648438, 0.5195312, 0.3164062),#, # seafloor integrity
                    '#585995', # hydrographical conditions
                    '#5b5e5d', # contaminants
                    '#c21f42', # contaminants in seafood
                    '#176552', # marine litter
                    '#1A78B6' # underwater noise
    )),
    title='Number of measures for each Descriptor and DPSiR',
    position.legend='bottom',
    reverse.legend=TRUE,
    title.legend='Descriptor'
  )
ggsave('POM_charts/pom_dpsir_descriptor_treemap.jpg')  


#### CIRCULAR - number of descriptors####
heatmap_data %>%  
  group_by(GEScomponent) %>%
  summarise(n = n_distinct(MeasureCode)) %>%
  drop_na() %>%
  ggplot() +
  geom_bar(aes(x=GEScomponent,
               y=n,
               fill=GEScomponent),
           stat='identity') +
  scale_x_discrete(expand = c(0, 0)) +
  labs(x='Descriptor',
       y='# of measures',
       title='Number of measures per descriptor') +
  geom_text(aes(x=reorder(GEScomponent, n), y=n-3.5, label = n), 
            colour='white', fontface='bold') +
  scale_fill_manual(values=rev(c('#12AD5E', # biodiversity
                                 '#CECD3C', # NI Species
                                 '#39BFBE', # commercial shellfish
                                 '#61B5E5', # food webs
                                 '#70923D', # eutrophication
                                 '#f78308',#rgb(0.9648438, 0.5195312, 0.3164062),#, # seafloor integrity
                                 '#585995', # hydrographical conditions
                                 '#5b5e5d', # contaminants
                                 '#c21f42', # contaminants in seafood
                                 '#176552', # marine litter
                                 '#1A78B6' # underwater noise
  ))) +
  scale_x_discrete(limits = rev(levels(heatmap_data$GEScomponent))) +
  theme_minimal() +
  theme(axis.title = element_blank(),
        axis.text.y = element_blank(),
        legend.position='none',
        plot.title = element_text(hjust=0.5),
        axis.text.x = element_text(face='bold', size=12)) +
  coord_polar()
ggsave('POM_charts/pom_descriptor_count_circular.jpg')

#### LOLLIPOP - Implementation Status ####
heatmap_data %>%  
  group_by(ImplementationStatus) %>%
  summarise(n = n_distinct(MeasureCode)) %>%
  drop_na() %>%
  ggdotchart(x = "ImplementationStatus", 
             y = "n",
             size=3,
             sorting = "descending",
             add = "segments",
             rotate = TRUE,
             dot.size = 9,
             label = "n", 
             color='#145C9E',
             font.label = list(color = "white", size = 10, vjust = 0.5, face='bold'),
             ggtheme = theme_pubr(),
             title='Implementation Status of Measures',
             ylab = '# of measures',
             xlab = ''
  ) +
  scale_y_continuous(expand=c(0,1), limits=c(0,120)) +
  theme(plot.title = element_text(hjust=0.5))
ggsave('POM_charts/pom_implementation_status.jpg') 

#### BUBBLE CHART - Measure Update Type ####
# heatmap_data %>%
#   group_by(UpdateType) %>%
#   summarise(n = n_distinct(MeasureCode)) %>%
#   ggballoonplot(y='UpdateType', x=c(0)*length(unique(heatmap_data$UpdateType)),
#                 size='n', fill='#39BFBE') +
#   labs(size='',
#        x='',
#        y='') +
#   scale_size_continuous(breaks=c(10,20,30,40,50,60),
#                         range=c(10,50)) +
#   scale_x_continuous(limits=c(-0.00001, 0.00001), 
#                      breaks=c(-0.00001, 0, 0.00001),
#                      expand=c(0,0)) +
#   geom_text(aes(x=0, y=UpdateType, label = n), 
#             colour='white', fontface='bold') +
#   geom_text(aes(x=0.0000018, y=UpdateType, label = UpdateType), 
#             colour='grey35', fontface='bold', hjust=0) +
#   theme_void() +
#   guides(fill="none",
#          size="none")
# ggsave('POM_charts/pom_update_type.jpg')
data_circle = 
  heatmap_data %>%
    group_by(UpdateType) %>%
    summarise(n = n_distinct(MeasureCode))
packing = circleProgressiveLayout(data_circle$n, sizetype='area')
# We can add these packing information to the initial data frame
data_circle <- cbind(data_circle, packing)

# The next step is to go from one center + a radius to the coordinates of a circle that
# is drawn by a multitude of straight lines.
dat.gg <- circleLayoutVertices(packing, npoints=50)

# Make the plot
ggplot() + 
  geom_polygon(data = dat.gg, aes(x, y, group = id, fill=as.factor(id)), colour = "white") +
  geom_text(data = data_circle, size=5.75, aes(x, y, label = n), 
            colour='white', fontface='bold') +
  # geom_text(data = data_circle, size=5.75, aes(x, y-0.35, label = paste('(',V1, ')', sep='')), 
  #           colour='white', fontface='bold') +
  scale_size_continuous(range = c(1,2)) +
  scale_fill_brewer(palette='RdBu',
                    labels=c("2015 measure that was not reported electronically",
                             "Measure modified since 2015 PoM",
                             "Measure new in 2021 PoM",
                             "Measure same as in 2015 PoM")) +
  theme_void() + 
  labs(fill="") +
  theme(legend.position="top") +
  coord_equal()
ggsave('POM_charts/pom_update_type.jpg')

#### WAFFLE CHART ####
waffle_data = 
  pom_report_net_data %>%
  mutate(DPSiR = ifelse(is.na(DPSiR), 'None', DPSiR)) %>%
  group_by(DPSiR) %>%
  summarise(n = n_distinct(MeasureCode)) %>%
  mutate(prop = (n/sum(n) * 100)) %>%
  mutate(pct_text = paste(DPSiR, ' (', as.character(round(prop, 0)), '%)',
                          sep=''))

waffle_vector = round(waffle_data$prop/2, 0)
names(waffle_vector) = waffle_data$pct_text

waffle(waffle_vector, rows = 5, legend_pos='top',
       colors=c('#145C9E',
                '#00A6A6',
                '#F49F0A',
                'grey70',
                '#0B5563'
       )
)
ggsave('POM_charts/pom_dpsir_waffle.jpg')

#### TREEMAP - Measure Type ####
heatmap_data %>%
  group_by(`Type of measure`) %>%
  summarise(n = n_distinct(MeasureCode)) %>%
  mutate(prop = paste(`Type of measure`, '\n',
                      '(', as.character(round(n/sum(n) * 100, 0)), '%)',
                      sep='')
  ) %>%
  treemap(
    index = c("prop"),
    vSize = "n",
    vColor = "Type of measure",
    palette = rev(c('#D8315B',
                    '#0A2463',
                    '#3E92CC',
                    '#D8315B',
                    '#FAC748',
                    '#F4D8CD'
    )),
    title='Percentage of measures assigned to each Measure Type',
    position.legend='none',
    reverse.legend=TRUE,
    title.legend='Descriptor'
  )
ggsave('POM_charts/measure_type_percentage.jpg')

#### BARCHART - Relevant KTMs ####
heatmap_data %>%
  group_by(RelevantKTMs) %>%
  summarise(n = n_distinct(MeasureCode)) %>%
  mutate(group = case_when(grepl("MSFD", RelevantKTMs) ~ "MSFD",
                           grepl("WFD", RelevantKTMs) ~"WFD")) %>%
  ggplot(aes(x=RelevantKTMs,
             y=n,
             fill=group)) + 
  geom_bar(stat="identity") +
  labs(title="Number of measures associated with Relevant KTMs",
       y = '# of measures',
       fill='')+
  coord_flip() +
  scale_y_continuous(expand = c(0, 0), limits=c(0, 65)) +
  scale_x_discrete(limits=rev(sort(unique(heatmap_data$RelevantKTMs)))) +
  scale_fill_manual(values=c('#145C9E',
                             '#00A6A6',
                             'grey50')
  ) +
  geom_text(aes(x=RelevantKTMs, y=n-0.5, label=n),
            fontface='bold',
            colour='white') +
  theme_minimal() +
  theme(legend.position='top',
        plot.title=element_text(hjust=0.5))
ggsave('POM_charts/relevant_ktms_bar.jpg')

#### Heatmap of GES vs Measure Purpose ####
heatmap_data %>%
  group_by(MeasurePurpose, GEScomponent) %>%
  summarise(n = n_distinct(MeasureCode)) %>%
  ggplot() +
  geom_raster(aes(x=GEScomponent, 
                  y=MeasurePurpose, 
                  fill=n),
              hjust=0,
              vjust=0.01) +
  scale_x_discrete(position = "top", expand = c(0, 0), 
                   limits=rev(levels(heatmap_data$GEScomponent))) +
  scale_y_discrete(expand = c(0, 0), 
                   limits=rev(sort(unique(heatmap_data$MeasurePurpose)))) +
  scale_fill_viridis_c(option = "magma", direction=-1) +
  # scale_color_gradient(low = "blue", high = "red") +
  labs(fill='# of measures') +
  theme_minimal() +
  theme(legend.position = 'top',
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_text(angle = 90),
        axis.text.y = element_text(vjust=1.8),
        panel.grid.major = element_line(colour = 'darkgrey'),
        panel.border = element_rect(colour = 'darkgrey', fill=NA),
        panel.ontop = TRUE)
ggsave('POM_charts/pom_measure_purpose_vs_ges.jpg')

#### BUBBLE - Feature vs Descriptor ####
heatmap_data %>%
  group_by(Feature, GEScomponent) %>%
  summarise(n = n_distinct(MeasureCode)) %>%
  ggballoonplot(y='Feature', x='GEScomponent', 
                size='n',
                fill='GEScomponent') +
  labs(size='Number of Measures',
       x='Descriptor',
       y='Feature') +
  scale_fill_manual(values=rev(c('#12AD5E', # biodiversity
                                 '#CECD3C', # NI Species
                                 '#39BFBE', # commercial shellfish
                                 '#61B5E5', # food webs
                                 '#70923D', # eutrophication
                                 '#f78308',#rgb(0.9648438, 0.5195312, 0.3164062),#, # seafloor integrity
                                 '#585995', # hydrographical conditions
                                 '#5b5e5d', # contaminants
                                 '#c21f42', # contaminants in seafood
                                 '#176552', # marine litter
                                 '#1A78B6' # underwater noise
  ))) +
  scale_size_continuous(breaks=c(20,40,60),
                        range=c(1,6),
                        labels=c('0-20', '21-40', '41-60')) +
  scale_x_discrete(position='top',
                   limits = rev(levels(heatmap_data$GEScomponent))) +
  theme_minimal() +
  guides(fill="none")
ggsave('POM_charts/descriptor_feature_bubble.jpg')

#### National Policies ####
heatmap_data %>%
  filter(!is.na(PolicyNational)) %>%
  group_by(PolicyNational, GEScomponent) %>%
  summarise(n = n_distinct(MeasureCode)) %>%
  ggballoonplot(y='PolicyNational', x='GEScomponent', 
                size='n',
                fill='GEScomponent') +
  labs(size='# of measures',
       x='Descriptor',
       y='Feature') +
  scale_fill_manual(values=rev(c('#12AD5E', # biodiversity
                                 '#CECD3C', # NI Species
                                 '#39BFBE', # commercial shellfish
                                 '#61B5E5', # food webs
                                 '#70923D', # eutrophication
                                 '#f78308',#rgb(0.9648438, 0.5195312, 0.3164062),#, # seafloor integrity
                                 '#585995', # hydrographical conditions
                                 '#5b5e5d', # contaminants
                                 '#c21f42', # contaminants in seafood
                                 '#176552', # marine litter
                                 '#1A78B6' # underwater noise
  )))  +
  scale_size_continuous(breaks=c(2,5,10),
                        range=c(1,10)) +
  scale_x_discrete(position='top',
                   limits = rev(levels(heatmap_data$GEScomponent))) +
  scale_y_discrete(limits = rev(sort(unique(heatmap_data$PolicyNational)))) +
  theme_minimal() +
  guides(fill="none")
ggsave('POM_charts/national_policies_bubble.jpg')
