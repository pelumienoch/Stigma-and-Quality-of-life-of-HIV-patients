pacman::p_load(
  rio,          # File import
  here,         # File locator
  skimr,        # get overview of data
  tidyverse,    # data management + ggplot2 graphics, 
  gtsummary,    # summary statistics and tests
  forcats,
  rstatix,      # statistics
  corrr,        # correlation analayis for numeric variables
  janitor,      # adding totals and percents to tables
  flextable     # converting tables to HTML
)



stigma <- import(here("Stigma Dataset2.xlsx"))
 skim(stigma)

 ### i want to quickly create a table for the sociodemographic variables
sociodemo <- stigma %>%
  select(Age:Income_range) %>%
  tbl_summary()
  
as.data.frame(sociodemo)
sociodemo <- as.data.frame(sociodemo)

sociodemo <- flextable(sociodemo) %>%
  save_as_docx(sociodemo, path = "Sociodemographic factors.docx")

### i want to quickly create a table for the clinical variables

clinic <- stigma %>%
  select(HowlonghaveyoubeenawareofyourHIVstatus:Presentviralload) %>%
  tbl_summary()

as.data.frame(clinic)
clinic <- as.data.frame(clinic)

clinic <- flextable(clinic) %>%
  save_as_docx(clinic, path = "clinic factors.docx")


pacman::p_load(
  rio,          # File import
  here,         # File locator
  skimr,        # get overview of data
  tidyverse,    # data management + ggplot2 graphics, 
  gtsummary,    # summary statistics and tests
  forcats,
  rstatix,      # statistics
  corrr,        # correlation analayis for numeric variables
  janitor,      # adding totals and percents to tables
  flextable,     # converting tables to HTML
  apyramid,     # a package dedicated to creating age pyramids
  stringr,
  dplyr
)


stigma$Age <- as.numeric(stigma$Age)
stigma$Viralloadafter6monthsofcART <- as.numeric(stigma$Viralloadafter6monthsofcART)
stigma$Presentviralload <- as.numeric(stigma$Presentviralload)
stigma$Age_group <- as.factor(stigma$Age_group)
str(stigma$Age_group)


stigma <- stigma %>%
  mutate(Age_group = recode(Age_group,
                           "20-29 ye" = "20-29 years",
                           "30-39 ye" = "30-39 years",
                           "40-49 ye" = "40-49 years",
                           "50-59 ye" = "50-59 years",
                           "60-69 ye" = "60-69 years",
                           "70-79 ye" = "70-79 years"))

#### I want to do a sophisticated age-sex pyramid, showing proportion, colour and extra aesthetics
apyramid::age_pyramid(
  data = stigma,
  age_group = "Age_group",
  split_by = "Sex",
  proportional = TRUE,              # show percents, not counts
  show_midpoint = FALSE,            # remove bar mid-point line
  #pal = c("orange", "purple")      # can specify alt. colors here (but not labels)
)+                 
  
  # additional ggplot commands
  theme_minimal()+                               # simplfy background
  scale_fill_manual(                             # specify colors AND labels
    values = c("orange", "purple"),              
    labels = c("m" = "Male", "f" = "Female"))+
  labs(y = "Percent of all cases",              # note x and y labs are switched
       x = "Age categories",                          
       fill = "Sex", 
       #caption = "My data source and caption here",
       title = "Age-sex pyramid of the participants",
       subtitle = "PERCEIVED STIGMA AND THE HEALTH-RELATED QUALITY OF LIFE AMONG 
PEOPLE LIVING WITH HIV")+
  theme(
    legend.position = "bottom",                          # legend to bottom
    axis.text = element_text(size = 10, face = "bold"),  # fonts/sizes
    axis.title = element_text(size = 12, face = "bold"))


#### Summary statstics of the health related quality of life, WHOQOL_BREF
WHOQOL <- stigma %>% 
  get_summary_stats(
    Physical,Psychological,Level_of_independence,Social_relationship,Environmental,Spiritual,  # columns to calculate for
    type = "common")                    # summary stats to return
WHOQOLL  <- flextable(WHOQOL) %>%
  save_as_docx(WHOQOL, path = " Descriptive of WHOQOL.docx")


#### Summary statstics of the BERGER HSS-40, 
BERGER <- stigma %>% 
  get_summary_stats(
    HSS_40_Overall,HSS_40_personalized_stigma,HSS_40_disclosure,HSS_40_Negative_self_image,HSS_40_public_attitudes,  # columns to calculate for
    type = "common")                    # summary stats to return

BERGER  <- flextable(BERGER) %>%
  save_as_docx(BERGER, path = " Descriptive of BERGER HSS-40.docx")


### I want to create a boxplot of multiple columns OF THE WHOBREF


library(ggplot2)

# Assuming your data is in a data frame called `stigma`

pacman::p_load(tidyr)
# Melt the data into a long format
stigma_HIV <- tidyr::pivot_longer(stigma, cols = c(Physical,Psychological,Level_of_independence,Social_relationship,
                                                   Environmental,Spiritual), names_to = "Domain", values_to = "Score") #%>%
  # switch order of levels and also relabel them
 # mutate(labels=c("DOM1"= "Physical health","DOM2" = "Psychological","DOM3" = "Social Relationship","DOM4" = "Environmental"))

# Create the boxplot with jitter points and statistical summaries
ggplot(stigma_HIV, aes(x = Domain, y = Score)) +
  geom_boxplot(fill = c("red", "darkgreen", "skyblue", "purple", "yellow", "blue")) +
  geom_jitter(alpha = 0.8, size = 0.5) +  # Add jitter points for individual data points
  stat_summary(fun = mean, geom = "point", shape = 18, size = 4, color = "black") +  # Add mean points
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2) +  # Add confidence intervals
  labs(x = "Domain", y = "Score")


### I want to create a boxplot of multiple columns OF THE BERGER

# Assuming your data is in a data frame called `stigma`

# Melt the data into a long format
BERGER_LONG <- tidyr::pivot_longer(stigma, cols = c(HSS_40_Overall,HSS_40_personalized_stigma,HSS_40_disclosure,HSS_40_Negative_self_image,HSS_40_public_attitudes), 
                                   names_to = "Berger HSS-40", values_to = "Score") #%>% 

BERGER_LONG <- BERGER_LONG %>%
  mutate(`Berger HSS-40` = recode(`Berger HSS-40`, "HSS_40_personalized_stigma"= "Personalized stigma","HSS_40_disclosure" = "Disclosure","HSS_40_Negative_self_image" = "Negative self-image",
"HSS_40_public_attitudes" = "public_attitudes", "HSS_40_Overall" = "Overall")) %>%
  mutate(`Berger HSS-40` = fct_relevel(`Berger HSS-40`, "Personalized stigma", "Disclosure", "Negative self-image",
                                       "public_attitudes", "Overall"))

# Create the boxplot with jitter points and statistical summaries
ggplot(BERGER_LONG, aes(x = `Berger HSS-40`, y = Score)) +
  geom_boxplot(fill = c("skyblue", "purple", "yellow", "blue", "red")) +
  geom_jitter(alpha = 0.8, size = 0.5) +  # Add jitter points for individual data points
  stat_summary(fun = mean, geom = "point", shape = 18, size = 4, color = "black") +  # Add mean points
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2) +  # Add confidence intervals
  labs(x = "Berger HSS-40", y = "Score")

### I want to perform a correlation matrix

stigma_corr <- stigma %>%
  select(HSS_40_Overall:Spiritual) %>%
  correlate()      # create correlation table (using default pearson)

### Another method
SCORE <- stigma %>%
  select(HSS_40_Overall:Spiritual)

d1 <- correlate(SCORE, quiet = TRUE, diagonal = " ", method = "spearman")%>%
  shave()  
d2 <- as.data.frame(d1)

d2 <- flextable(d2)%>%
  save_as_docx(d2, path = "correlation table.docx")


skim(SCORE)


### Another method
SCORE3 <- stigma %>%
  select(Viralloadafter6monthsofcART:HSS_40_public_attitudes)

d3 <- correlate(SCORE3, quiet = TRUE, diagonal = " ", method = "spearman")%>%
  shave()  
d4 <- as.data.frame(d3)

d4 <- flextable(d4)%>%
  save_as_docx(d4, path = "correlation viral load and stigma.docx") 