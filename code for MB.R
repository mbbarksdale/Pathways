## PACKAGES
library(tidyverse)
library(readxl)
library(ggpubr)
library(pals)
library(rlang)

## DATA CLEANING 

pathways <- read_excel("~/Documents/Pathways/Expanded_Primary_Codes_Table.xlsx")

#relevel in decreasing order 
pathways$PrimaryCode_1 = factor(pathways$PrimaryCode_1, levels = c("None",
                                                           "Life or family event",
                                                           "Ocean-science themed camps or classes",
                                                           "General interest",
                                                           "Pop culture",
                                                           "Aquariums or science centers",
                                                           "Specific training (e.g., SCUBA, snorkeling)",
                                                           "Teacher or mentor",
                                                           "Internship or volunteer opportunity",
                                                           "Desire to make a significant impact",
                                                           "Job prospects",
                                                           "Undergraduate research experience",
                                                           "Entry-level job",
                                                           "Sense or promise of discovery or wonder",
                                                           "Specific interest",
                                                           "Talk, lecture, or article by field expert",
                                                           "Other",
                                                           "Desire to live and/or work near the ocean",
                                                           "Current events or news",
                                                           "Career fair or aptitude evaluation",
                                                           "High school research experience",
                                                           "Family legacy",
                                                           "Family pressure",
                                                           "Love of and/or exposure to the outdoors"))

pathways$race = factor(pathways$race, levels = c("White/Caucasian",
                                                 "Non-White/Caucasian",
                                                 "Prefer not to respond"))

#pathways$ethnicity = factor(pathways$ethnicity, levels = c("No",
                                                           #"Yes",
                                                           #"Prefer not to respond",
                                                           #"NA"))
#relevel so progression left to right makes sense
pathways$years_employed = factor(pathways$years_employed, levels = c("5 or fewer",
                                                                     "6-10",
                                                                     "11-15",
                                                                     "16-20",
                                                                     "Greater than 20"))
#relevel so progression left to right makes sense
pathways$highest_degree = factor(pathways$highest_degree, levels = c("Some college but no degree",
                                                                     "Associate degree",
                                                                     "University Advanced Diploma",
                                                                     "Bachelor's degree",
                                                                     "Master of Professional Science",
                                                                     "Master's degree",
                                                                     "Doctoral (PhD) degree",
                                                                     "J.D."))
#mutate into agegroups
pathways$age = as.numeric(pathways$age)
pathways = pathways %>%
  mutate(agegroup = case_when(age <= 30 & age > 20 ~ "21-30",
                              age <= 40 & age > 30 ~ "31-40",
                              age <= 50 & age > 40 ~ "41-50",
                              age <= 60 & age > 50 ~ "51-60",
                              age <= 70 & age > 60 ~ "61-70",
                              age <= 80 & age > 70 ~ "71-80"))

## OVERALL LOLLIPOP PLOTS
# just first primary codes
pathways %>%
  filter(!is.na(PrimaryCode_1)) %>%
  group_by(PrimaryCode_1) %>%
  summarize(count = n(),
            percent = count/503,
            roundpercent = round(percent*100, digits = 2)) %>%
  ggplot(aes(reorder(PrimaryCode_1, count), y = count)) +
  geom_point(size = 10, color = "maroon") +
  geom_segment(aes(x = PrimaryCode_1, xend = PrimaryCode_1, y = 0, yend = count), size = 3, color = "hotpink") +
  geom_text(aes(label = paste0(roundpercent, "%")),
            size = 7, nudge_y = 12, show.legend = FALSE) +
  coord_flip() + 
  labs(y = "Number of Experiences",
       x = "Main Code") + 
  theme_classic() +
  theme(axis.title = element_text(size = 20, face = "bold"),
        axis.text = element_text(size = 18),
  )

# all primary codes
pathways %>%
  filter(!is.na(all_primary)) %>%
  group_by(all_primary) %>%
  summarize(count = n(),
            percent = count/649,
            roundpercent = round(percent*100, digits = 2)) %>%
  ggplot(aes(reorder(all_primary, count), y = count)) +
  geom_point(size = 10, color = "maroon") +
  geom_segment(aes(x = all_primary, xend = all_primary, y = 0, yend = count), size = 3, color = "hotpink") +
  geom_text(aes(label = paste0(roundpercent, "%")),
            size = 7, nudge_y = 12, show.legend = FALSE) +
  coord_flip() + 
  labs(y = "Number of Experiences",
       x = "Main Code") + 
  theme_classic() +
  theme(axis.title = element_text(size = 20, face = "bold"),
        axis.text = element_text(size = 18),
  )

# socioeconomic status
# make subset of data
socioecon_dat = pathways %>%
  filter(main_code == "Socioeconomic status") %>% #writing out what the subcodes stand for
  mutate(sub_codech = case_when(sub_code == "A" ~ "High cost for educational programs",
                                sub_code == "B" ~ "Insufficient income to pay for education",
                                sub_code == "C" ~ "Unpaid and/or low-paying jobs",
                                sub_code == "D" ~ "High cost of equipment",
                                sub_code == "E" ~ "High cost of living",
                                sub_code == "F" ~ "Low income background",
                                sub_code == "G" ~ "Accessibility of research funding",
                                sub_code == "H" ~ "Loans, usually student",
                                sub_code == "I" ~ "Pay/stipend for graduate school",
                                sub_code == "J" ~ "Cost of transportation",
                                sub_code == "K" ~ "Unaffordable health insurance",
                                is.na(sub_code) ~ "No subcode described"))
#nrow(socioecon_dat) #120 - so i know the denominator

#plot
socioecon_dat %>%
  group_by(sub_codech) %>%
  summarize(count = n(),
            percent = count/120,
            roundpercent = round(percent*100, digits = 2)) %>%
  ggplot(aes(reorder(sub_codech, count), y = count)) +
  geom_point(size = 10, color = "maroon") +
  geom_segment(aes(x = sub_codech, xend = sub_codech, y = 0, yend = count), size = 3, color = "hotpink") +
  geom_text(aes(label = paste0(roundpercent, "%")),
            size = 7, nudge_y = 10, show.legend = FALSE) +
  coord_flip() +
  labs(y = "Number of Experiences",
       x = "Socioeconomic status subcode",
       #title = "Socioeconomic Status Breakdown"
  ) +
  theme_classic() + 
  theme(axis.title = element_text(size = 20, face = "bold"),
        axis.text = element_text(size = 18),
  )

#lack of support
#make subset of data
mentorshipdat = pathways %>% #writing out what subcodes stand for
  filter(main_code == "Lack of support (mentorship, peers, family)") %>%
  mutate(sub_codech = case_when(sub_code == "A" ~ "Not having a mentor",
                                sub_code == "B" ~ "Mentor/family not supportive of ocean sciences",
                                sub_code == "C" ~ "First generation student",
                                sub_code == "D" ~ "External push towards high-paying fields",
                                sub_code == "E" ~ "Treated poorly by mentors",
                                sub_code == "F" ~ "Dissuasive comments from mentors/family",
                                sub_code == "G" ~ "No family support",
                                sub_code == "H" ~ "Cultural values not supportive of ocean science",
                                sub_code == "I" ~ "Having a mentor but they did not provide guidance",
                                is.na(sub_code) ~ "No subcode described"))

#nrow(mentorshipdat) #75 - so i know the denominator

#plot
mentorshipdat %>%
  group_by(sub_codech) %>%
  summarize(count = n(),
            percent = count/75,
            roundpercent = round(percent*100, digits = 2)) %>%
  ggplot(aes(reorder(sub_codech, count), y = count)) +
  geom_point(size = 10, color = "maroon") +
  geom_segment(aes(x = sub_codech, xend = sub_codech, y = 0, yend = count), size = 3, color = "hotpink") +
  geom_text(aes(label = paste0(roundpercent, "%")),
            size = 7, nudge_y = 10, show.legend = FALSE) +
  coord_flip() +
  labs(y = "Number of Experiences",
       x = "Socioeconomic status subcode",
       #title = "Lack of Support Breakdown"
  ) +
  theme_classic() + 
  theme(axis.title = element_text(size = 20, face = "bold"),
        axis.text = element_text(size = 18),
  )

#observed barriers
#make subset of data
observeddat = pathways %>%
  filter(main_code == "Observed Barrier") %>% #also writiting out what subcodes stand for
  mutate(sub_codech = case_when(sub_code == "Ethnicity" ~ "Ethnicity",
                                sub_code == "Skill building challenges and competition" ~ "Skill building challenges and competition",
                                sub_code == "Socioeconomic Status I" ~ "Pay/stipend for graduate school",
                                sub_code == "Socioeconomic Status E" ~ "High cost of living",
                                sub_code == "Socioeconomic Status C" ~ "Unpaid and/or low-paying jobs",
                                sub_code == "Socioeconomic Status" ~ "Socioeconomic Status General",
                                sub_code == "Gender" ~ "Gender",
                                sub_code == "Psychological Barrier" ~ "Psychological Barrier"))
# nrow(observeddat) #14 - so i know the denominator

#plot
observeddat %>%
  group_by(sub_codech) %>%
  summarize(count = n(),
            percent = count/14,
            roundpercent = round(percent*100, digits = 2)) %>%
  ggplot(aes(reorder(sub_codech, count), y = count)) +
  geom_point(size = 10, color = "maroon") +
  geom_segment(aes(x = sub_codech, xend = sub_codech, y = 0, yend = count), size = 3, color = "hotpink") +
  geom_text(aes(label = paste0(roundpercent, "%")),
            size = 7, nudge_y = 7, show.legend = FALSE) +
  coord_flip() +
  labs(y = "Number of Experiences",
       x = "Observed Barrier Subcode",
       #title = "Observed Barrier Breakdown"
  ) +
  theme_classic() + 
  theme(axis.title = element_text(size = 20, face = "bold"),
        axis.text = element_text(size = 18),
  )


## MAKING OTHER GROUPS
#overall
pathways2 = pathways %>%
  group_by(main_code) %>%
  summarize(count = n(),
            percent = count/476,
            roundpercent = round(percent*100, digits = 2)) %>%
  mutate(code_simple = case_when(roundpercent < 5 ~ "Other", #anything less than 5% is now "Other"
                                 TRUE ~ main_code))  #this means cases when roundpercent is not less than 5 just keep it the same

#joining new labels (code_simple) into original dataset
pathways = left_join(pathways, pathways2[, c("main_code", "code_simple")],
                     by = c("main_code"),
                     multiple = "any") 

## relevel so other is at the end 
pathways$code_simple = factor(pathways$code_simple, 
                              levels = c("Socioeconomic status",
                                         "Lack of support (mentorship, peers, family)",
                                         "Skill building challenges and competition",
                                         "Knowledge of jobs and career paths",
                                         "Gender",
                                         "Access to full time jobs",
                                         "Other"))
## repeat above for other code groupings ... (ie for me socioeconomic, lack of support, and observed barrier subcodes)


## PLOTTING BARRIERS BY DEMOGRAPHICS (ex: age group)

#calculate num in each age group
agegroupcount <- pathways %>%
  group_by(age) %>%
  summarise(n = n()) %>%
  mutate(label = paste0("N = ", n))

#my colors - based on alphabet2 in `pals`
mypal <- c("#AA0DFE", "#3283FE", "#85660D",  "#1C8356","#C4451C", "#FE00FA", "#1CFFCE", "#565656", "#F7E1A0")

#plot
ggplot(pathways, aes(x = agegroup, fill = code_simple)) +
  geom_bar(stat = "count", position = "fill") +
  labs(fill = "Barriers",
       x = "Age Demographic",
       y = "Percent of Responses") +
  scale_fill_manual(values = mypal) +
  scale_y_continuous(labels = scales::percent) +
  geom_text(data = agegroupcount,
            aes(x = agegroup, y = 1.00, label = label),
            size = 8,
            inherit.aes = FALSE,
            vjust = -1) +
  theme_classic() +
  theme(axis.title = element_text(size = 20, face = "bold"),
        axis.text = element_text(size = 18),
        legend.text = element_text(size = 18),
        legend.title = element_text(size = 20))

## FUNCTION: to quickly make the above figure for different demographics & differnt dfs (ie for me socioeconomic, lack of support, and observed barrier subcodes))
## arguments are `condition` (the dataset to use) & `demographic` (agegroup, highest degree, gender, etc)

demographicsplots = function(condition, demographic){
  
  #color palette
  mypal <- c("#AA0DFE", "#3283FE", "#85660D",  "#1C8356","#C4451C", "#FE00FA", "#1CFFCE", "#565656", "#F7E1A0")
  
  
  if(condition == "barriers") { #overall
    
    dat = pathways
    
    #calculation for labels
    counts <- dat %>%
      group_by({{demographic}}) %>%
      summarise(n = n()) %>%
      mutate(label = paste0("N = ", n))
    
    #figure
    ggplot(dat, aes(x = {{demographic}}, fill = code_simple)) +
      geom_bar(stat = "count", position = "fill") +
      labs(fill = "Codes",
           title = "All Barriers",
           x = as_label(enquo(demographic)),
           y = "Percent of Responses") + 
      scale_fill_manual(values = mypal) +
      scale_y_continuous(labels = scales::percent) +
      geom_text(data = counts, 
                aes(x = .data[[rlang::as_name(enquo(demographic))]], 
                    y = 1.00, 
                    label = label),
                size = 8,
                inherit.aes = FALSE,
                vjust = -1) +
      theme_classic() +
      theme(axis.title = element_text(size = 20, face = "bold"),
            axis.text = element_text(size = 18),
            legend.text = element_text(size = 18),
            legend.title = element_text(size = 20),
            title = element_text(size = 20, face = "bold"),
            axis.text.x = element_text(angle = 90, vjust = .5))
    
  } else {
    if(condition == "socioecon_dat"){ #socioeconomic status
      
      dat = socioecon_dat
      
      #calculation for labels
      counts <- dat %>%
        group_by({{demographic}}) %>%
        summarise(n = n()) %>%
        mutate(label = paste0("N = ", n))
      
      ggplot(dat, aes(x = {{demographic}}, fill = code_simple)) +
        geom_bar(stat = "count", position = "fill") +
        labs(fill = "Codes",
             title = "Socioeconomic Subcodes",
             x = as_label(enquo(demographic)),
             y = "Percent of Responses") + 
        scale_fill_manual(values = mypal) +
        scale_y_continuous(labels = scales::percent) +
        geom_text(data = counts, 
                  aes(x = .data[[rlang::as_name(enquo(demographic))]], 
                      y = 1.00, 
                      label = label),
                  size = 8,
                  inherit.aes = FALSE,
                  vjust = -1) +
        theme_classic() +
        theme(axis.title = element_text(size = 20, face = "bold"),
              axis.text = element_text(size = 18),
              legend.text = element_text(size = 18),
              legend.title = element_text(size = 20),
              title = element_text(size = 20, face = "bold"),
              axis.text.x = element_text(angle = 90, vjust = .5))
      
    } else {
      if(condition == "mentorshipdat") { #lack of support
        
        dat = mentorshipdat
        
        #calculation for labels
        counts <- dat %>%
          group_by({{demographic}}) %>%
          summarise(n = n()) %>%
          mutate(label = paste0("N = ", n))
        
        ggplot(dat, aes(x = {{demographic}}, fill = code_simple)) +
          geom_bar(stat = "count", position = "fill") +
          labs(fill = "Codes",
               title = "Lack of Support Subcodes",
               x = as_label(enquo(demographic)),
               y = "Percent of Responses") + 
          scale_fill_manual(values = mypal) +
          scale_y_continuous(labels = scales::percent) +
          geom_text(data = counts, 
                    aes(x = .data[[rlang::as_name(enquo(demographic))]], 
                        y = 1.00, 
                        label = label),
                    size = 8,
                    inherit.aes = FALSE,
                    vjust = -1) +
          theme_classic() +
          theme(axis.title = element_text(size = 20, face = "bold"),
                axis.text = element_text(size = 18),
                legend.text = element_text(size = 18),
                legend.title = element_text(size = 20),
                title = element_text(size = 20, face = "bold"),
                axis.text.x = element_text(angle = 90, vjust = .5))
        
      } else { #observed barriers
        
        dat = observeddat
        
        #calculation for labels
        counts <- dat %>%
          group_by({{demographic}}) %>%
          summarise(n = n()) %>%
          mutate(label = paste0("N = ", n))
        
        ggplot(dat, aes(x = {{demographic}}, fill = sub_codech)) +
          geom_bar(stat = "count", position = "fill") +
          labs(fill = "Codes",
               title = "Observed Barriers",
               x = as_label(enquo(demographic)),
               y = "Percent of Responses") + 
          scale_fill_manual(values = mypal) +
          scale_y_continuous(labels = scales::percent) +
          geom_text(data = counts, 
                    aes(x = .data[[rlang::as_name(enquo(demographic))]], 
                        y = 1.00, 
                        label = label),
                    size = 8,
                    inherit.aes = FALSE,
                    vjust = -1) +
          theme_classic() +
          theme(axis.title = element_text(size = 20, face = "bold"),
                axis.text = element_text(size = 18),
                title = element_text(size = 20, face = "bold"),
                legend.text = element_text(size = 18),
                legend.title = element_text(size = 20),
                axis.text.x = element_text(angle = 90, vjust = .5))
        
      }
    }
  }
  
}

## USING THE FUNCTION
#"pathways" is the name of my df, agegroup is the column name in pathways
demographicsplots("pathways", agegroup)
demographicsplots("socioecon_dat", agegroup)
demographicsplots("mentorshipdat", agegroup)
demographicsplots("observeddat", agegroup)

#replicate for a different demographic
demographicsplots("socioecon_dat", race)
demographicsplots("mentorshipdat", race)
demographicsplots("observeddat", race)



