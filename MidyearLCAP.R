

library(tidyverse)
library(readxl)
library(here)
library(googlesheets4)


exported.file <- "211024_Graves ESD_21 LCAP MRR Data.xlsx"

ets.file <- read_xlsx(here("data",exported.file))

data <- ets.file %>%
    mutate(descrip2 =  str_remove(Description,"21-22 LCAP: Goal 1 - Measuring/Reporting - " )) %>%
    separate(descrip2, sep = " - ", into = c("Number", "Type")) %>% 
    select(-Description,-SectionPosition) %>%
    pivot_wider(names_from = Type, values_from = FieldData)


advice <- data %>%
    mutate(`MidYear Advice` = case_when(str_detect(str_to_lower(Metric), " 1 a| 1a") ~ "Likely can report" ,
                                        str_detect(str_to_lower(Metric), "fully credentialed") ~ "Likely can report", 
                                        str_detect(str_to_lower(Metric), " 1 b| 1b") ~ "Likely can report" ,
                                        str_detect(str_to_lower(Metric), "instructional materials") ~ "Likely can report", 
                                        str_detect(str_to_lower(Metric), " 1 c| 1c") ~ "Likely can report" ,
                                        str_detect(str_to_lower(Metric), "school facilities") ~ "Likely can report", 
                                        str_detect(str_to_lower(Metric), " 2 a| 2a") ~ "Likely can report" ,
                                        str_detect(str_to_lower(Metric), " state academic standards") ~ "Likely can report" ,
                                        str_detect(str_to_lower(Metric), " 2 b| 2b") ~ "Likely can report" ,
                                        str_detect(str_to_lower(Metric), " eld standards") ~ "Likely can report" ,
                                        str_detect(str_to_lower(Metric), " 3 a| 3a") ~ "Likely can report" ,
                                        str_detect(str_to_lower(Metric), " family partnerships") ~ "Likely can report" ,
                                        str_detect(str_to_lower(Metric), " 4 a| 4a") ~ "Cannot report" ,
                                        str_detect(str_to_lower(Metric), " caaspp") ~ "Cannot report" ,
                                        str_detect(str_to_lower(Metric), " 4 b| 4b") ~ "Cannot report" ,
                                        str_detect(str_to_lower(Metric), " elpac") ~ "Cannot report" ,
                                        str_detect(str_to_lower(Metric), " 5 a| 5a") ~ "Likely can report" ,
                                        str_detect(str_to_lower(Metric), " attendance") ~ "Likely can report" ,
                                        str_detect(str_to_lower(Metric), " 5 b| 5b") ~ "Likely can report" ,
                                        str_detect(str_to_lower(Metric), " chronic absenteeism") ~ "Likely can report" ,
                                        str_detect(str_to_lower(Metric), " 5 c| 5c") ~ "Likely can report" ,
                                        str_detect(str_to_lower(Metric), " middle school drop") ~ "Likely can report" ,
                                        str_detect(str_to_lower(Metric), " 6 a| 6a") ~ "Likely can report" ,
                                        str_detect(str_to_lower(Metric), "suspension|expulsion") ~ "Likely can report" ,
                                        str_detect(str_to_lower(Metric), " 6 c| 6c") ~ "Unlikely to report" ,
                                        str_detect(str_to_lower(Metric), "school climate") ~ "Unlikely to report" ,
                                        str_detect(str_to_lower(Metric), " 7 a| 7a") ~ "Likely can report" ,
                                        str_detect(str_to_lower(Metric), "broad course") ~ "Likely can report" ,
                                        
                                        ),
           `Data Source` = case_when(str_detect(str_to_lower(Metric), " 1 a| 1a") ~ "CDE will release a new data file in November;  historically done with internal school records",
                                     str_detect(str_to_lower(Metric), "fully credentialed") ~ "CDE will release a new data file in November;  historically done with internal school records",
                                     str_detect(str_to_lower(Metric), " 1 b| 1b") ~ "SARC" ,
                                     str_detect(str_to_lower(Metric), "instructional materials") ~ "SARC", 
                                     str_detect(str_to_lower(Metric), " 1 c| 1c") ~ "SARC" ,
                                     str_detect(str_to_lower(Metric), "school facilities") ~ "SARC", 
                                     str_detect(str_to_lower(Metric), " 2 a| 2a") ~ "Locally determined, possibly Class schedules" ,
                                     str_detect(str_to_lower(Metric), " state academic standards") ~ "Locally determined, possibly Class schedules" ,
                                     str_detect(str_to_lower(Metric), " 2 b| 2b") ~ "Likely can report" ,
                                     str_detect(str_to_lower(Metric), " eld standards") ~ "Likely can report" ,
                                     str_detect(str_to_lower(Metric), " 3 a| 3a") ~ "IEP records, Parent Conference records, Parent Meeting records" ,
                                     str_detect(str_to_lower(Metric), " family partnerships") ~ "IEP records, Parent Conference records, Parent Meeting records" ,
                                     str_detect(str_to_lower(Metric), " 4 a| 4a") ~ "CAASPP End of Year testing - CERS or CAASPP-ELPAC.org" ,
                                     str_detect(str_to_lower(Metric), " caaspp") ~ "CAASPP End of Year testing - CERS or CAASPP-ELPAC.org" ,
                                     str_detect(str_to_lower(Metric), " 4 b| 4b") ~ "ELPAC End of Year testing - CERS or CAASPP-ELPAC.org" ,
                                     str_detect(str_to_lower(Metric), " elpac") ~ "ELPAC End of Year testing - CERS or CAASPP-ELPAC.org" ,
                                     str_detect(str_to_lower(Metric), " 5 a| 5a") ~ "SIS" ,
                                     str_detect(str_to_lower(Metric), " attendance") ~ "SIS" ,
                                     str_detect(str_to_lower(Metric), " 5 b| 5b") ~ "SIS and Dashboard reports" ,
                                     str_detect(str_to_lower(Metric), " chronic absenteeism") ~ "SIS and Dashboard reports" ,
                                     str_detect(str_to_lower(Metric), " 5 c| 5c") ~ "SIS" ,
                                     str_detect(str_to_lower(Metric), " middle school drop") ~ "SIS" ,
                                     str_detect(str_to_lower(Metric), " 6 a| 6a") ~ "SIS or SWIS,  Dashboard reports" ,
                                     str_detect(str_to_lower(Metric), "suspension|expulsion") ~ "SIS or SWIS,  Dashboard reports" ,
                                     str_detect(str_to_lower(Metric), " 6 c| 6c") ~ "Locally determined, often survey done at end of the year" ,
                                     str_detect(str_to_lower(Metric), "School Climate") ~ "Locally determined, often survey done at end of the year" ,
                                     str_detect(str_to_lower(Metric), " 7 a| 7a") ~ "Locally determined, possibly Class schedules" ,
                                     str_detect(str_to_lower(Metric), "broad course") ~ "Locally determined, possibly Class schedules" ,
                                     
                                     
           )
           
           )


ss <- "https://docs.google.com/spreadsheets/d/1WjFBvLxwhfbmJdCattvM3hd-Hak71N2EEd6pzIBmK3M/edit#gid=1341274390"

leas <- unique(advice$`District Name`)

for (i in leas) {
 
    write_sheet(advice, ss, sheet = i)
 
}


