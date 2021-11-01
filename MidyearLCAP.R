

library(tidyverse)
library(readxl)
library(here)
library(googlesheets4)


exported.file <- "211029_Monterey COE_MRR Summary.xlsx"

ets.file <- read_xlsx(here("data",exported.file))

data <- ets.file %>%
    mutate(descrip2 =  str_remove(Description,"21-22 LCAP: " )) %>%
    separate(descrip2, sep = " - ", into = c("Goal","Trash","Number", "Type")) %>% 
    select(-Description,-Section, - Trash) %>%
    pivot_wider(names_from = Type, values_from = FieldData)

# Work from here on Monday

advice <- data %>%
    mutate(`MidYear Advice` = case_when(str_detect(str_to_lower(Metric), " 1 a| 1a") ~ "Likely can report" ,
                                        str_detect(str_to_lower(Metric), "credential") ~ "Likely can report", 
                                        str_detect(str_to_lower(Metric), " 1 b| 1b") ~ "Likely can report" ,
                                        str_detect(str_to_lower(Metric), "instructional materials") ~ "Likely can report", 
                                        str_detect(str_to_lower(Metric), " 1 c| 1c") ~ "Likely can report" ,
                                        str_detect(str_to_lower(Metric), "school facilities") ~ "Likely can report", 
                                        str_detect(str_to_lower(Metric), " 2 a| 2a") ~ "Likely can report" ,
                                        str_detect(str_to_lower(Metric), "state academic standards") ~ "Likely can report" ,
                                        str_detect(str_to_lower(Metric), " 2 b| 2b") ~ "Likely can report" ,
                                        str_detect(str_to_lower(Metric), "eld standards") ~ "Likely can report" ,
                                        str_detect(str_to_lower(Metric), " 3 a| 3a") ~ "Likely can report" ,
                                        str_detect(str_to_lower(Metric), " family partnerships|input") ~ "Likely can report" ,
                                        str_detect(str_to_lower(Metric), " 3 b| 3b") ~ "Likely can report" ,
                                        str_detect(str_to_lower(Metric), " participation") ~ "Likely can report" ,
                                        str_detect(str_to_lower(Metric), " 3 c| 3c") ~ "Likely can report" ,
                                        str_detect(str_to_lower(Metric), " participation") ~ "Likely can report" ,
                                        str_detect(str_to_lower(Metric), " 4 a| 4a") ~ "Cannot report" ,
                                        str_detect(str_to_lower(Metric), "caaspp|sbac") ~ "Cannot report" ,
                                        str_detect(str_to_lower(Metric), " 4 b| 4b") ~ "Cannot report" ,
                                        str_detect(str_to_lower(Metric), "elpac") ~ "Cannot report" ,
                                        str_detect(str_to_lower(Metric), " 4 c| 4c") ~ "Cannot report" ,
                                        str_detect(str_to_lower(Metric), " eap") ~ "Cannot report" ,
                                        str_detect(str_to_lower(Metric), " 4 d| 4d") ~ "Can report on last year's testing" ,
                                        str_detect(str_to_lower(Metric), "make progress") ~ "Can report on last year's testing" ,
                                        str_detect(str_to_lower(Metric), " 4 e| 4e") ~ "Can report based on last year's testing" ,
                                        str_detect(str_to_lower(Metric), "reclass") ~ "Can report based on last year's testing" ,
                                        # str_detect(str_to_lower(Metric), " 4 f| 4f") ~ "Cannot report" ,
                                        # str_detect(str_to_lower(Metric), "elpac") ~ "Cannot report" ,
                                        # str_detect(str_to_lower(Metric), " 4 g| 4g") ~ "Cannot report" ,
                                        # str_detect(str_to_lower(Metric), "elpac") ~ "Cannot report" ,
                                        str_detect(str_to_lower(Metric), " 5 a| 5a") ~ "Likely can report" ,
                                        str_detect(str_to_lower(Metric), "attendance") ~ "Likely can report" ,
                                        str_detect(str_to_lower(Metric), " 5 b| 5b") ~ "Likely can report" ,
                                        str_detect(str_to_lower(Metric), "chronic absenteeism") ~ "Likely can report" ,
                                        str_detect(str_to_lower(Metric), " 5 c| 5c") ~ "Likely can report" ,
                                        str_detect(str_to_lower(Metric), "middle school drop|dropout|drop out") ~ "Likely can report" ,
                                      str_detect(str_to_lower(Metric), " 5 d| 5d") ~ "Likely can report" ,
                                      str_detect(str_to_lower(Metric), "high school drop") ~ "Likely can report" ,  
                                      str_detect(str_to_lower(Metric), " 5 e| 5e") ~ "Cannot report" ,
                                      str_detect(str_to_lower(Metric), "graduation") ~ "Cannot report" ,
                                        str_detect(str_to_lower(Metric), " 6 a| 6a|6 b|6b") ~ "Likely can report" ,
                                        str_detect(str_to_lower(Metric), "suspension|expulsion") ~ "Likely can report" ,
                                        str_detect(str_to_lower(Metric), " 6 c| 6c") ~ "Unlikely to report" ,
                                        str_detect(str_to_lower(Metric), "school climate") ~ "Unlikely to report" ,
                                        str_detect(str_to_lower(Metric), " 7 a| 7a") ~ "Likely can report" ,
                                        str_detect(str_to_lower(Metric), "broad course|programs and services") ~ "Likely can report" ,
                                        str_detect(str_to_lower(Metric), " 7 b| 7b|7 c|7c") ~ "Likely can report" ,
                                      str_detect(str_to_lower(Metric), "priority 8") ~ "Likely can report" ,
                                      str_detect(str_to_lower(Metric), "renaissance|star ") ~ "Likely can report" ,
                                      
              
                                        
                                        ),
           `Possible Data Source` = case_when(str_detect(str_to_lower(Metric), " 1 a| 1a") ~ "CDE will release a new data file in November;  historically done with internal school records",
                                     str_detect(str_to_lower(Metric), "credential") ~ "CDE will release a new data file in November;  historically done with internal school records",
                                     str_detect(str_to_lower(Metric), " 1 b| 1b") ~ "SARC" ,
                                     str_detect(str_to_lower(Metric), "instructional materials") ~ "SARC", 
                                     str_detect(str_to_lower(Metric), " 1 c| 1c") ~ "SARC" ,
                                     str_detect(str_to_lower(Metric), "school facilities") ~ "SARC", 
                                     str_detect(str_to_lower(Metric), " 2 a| 2a") ~ "Locally determined, possibly Class schedules" ,
                                     str_detect(str_to_lower(Metric), "state academic standards") ~ "Locally determined, possibly Class schedules" ,
                                     str_detect(str_to_lower(Metric), " 2 b| 2b") ~ "Locally determined, possibly Class schedules" ,
                                     str_detect(str_to_lower(Metric), "eld standards") ~ "Locally determined, possibly Class schedules" ,
                                     str_detect(str_to_lower(Metric), " 3 a| 3a") ~ "Locally determined, possibly IEP records, Parent Conference records, Parent Meeting records" ,
                                     str_detect(str_to_lower(Metric), " family partnerships|input") ~ "Locally determined, possibly IEP records, Parent Conference records, Parent Meeting records" ,
                                     str_detect(str_to_lower(Metric), " 3 b| 3b") ~ "Locally determined, possibly sign-in sheets" ,
                                     str_detect(str_to_lower(Metric), " participation") ~ "Likely can report" ,
                                     str_detect(str_to_lower(Metric), " 3 c| 3c") ~ "Locally determined, possibly sign-in sheets" ,
                                     str_detect(str_to_lower(Metric), " participation") ~ "Likely can report" ,
                                     str_detect(str_to_lower(Metric), " 4 a| 4a") ~ "CAASPP End of Year testing - CERS or CAASPP-ELPAC.org" ,
                                     str_detect(str_to_lower(Metric), "caaspp|sbac") ~ "CAASPP End of Year testing - CERS or CAASPP-ELPAC.org" ,
                                     str_detect(str_to_lower(Metric), " 4 b| 4b") ~ "ELPAC End of Year testing - CERS or CAASPP-ELPAC.org" ,
                                     str_detect(str_to_lower(Metric), "elpac") ~ "ELPAC End of Year testing - CERS or CAASPP-ELPAC.org" ,
                                     str_detect(str_to_lower(Metric), " 4 c| 4c") ~ "CERS, CAASPP_ELPAC.org or perhaps College Board" ,
                                     str_detect(str_to_lower(Metric), " eap") ~ "CERS, CAASPP_ELPAC.org or perhaps College Board" ,
                                     str_detect(str_to_lower(Metric), " 4 d| 4d") ~ "CERS or CAASPP_ELPAC.org" ,
                                     str_detect(str_to_lower(Metric), "make progress") ~ "CERS or CAASPP_ELPAC.org" ,
                                     str_detect(str_to_lower(Metric), " 4 e| 4e") ~ "SIS" ,
                                     str_detect(str_to_lower(Metric), "reclass") ~ "SIS" ,
                                     # str_detect(str_to_lower(Metric), " 4 f| 4f") ~ "Cannot report" ,
                                     # str_detect(str_to_lower(Metric), "elpac") ~ "Cannot report" ,
                                     # str_detect(str_to_lower(Metric), " 4 g| 4g") ~ "Cannot report" ,
                                     # str_detect(str_to_lower(Metric), "elpac") ~ "Cannot report" ,
                                      str_detect(str_to_lower(Metric), " 5 a| 5a") ~ "SIS" ,
                                     str_detect(str_to_lower(Metric), "attendance") ~ "SIS" ,
                                     str_detect(str_to_lower(Metric), " 5 b| 5b") ~ "SIS and Dashboard reports" ,
                                     str_detect(str_to_lower(Metric), "chronic absenteeism") ~ "SIS and Dashboard reports" ,
                                     str_detect(str_to_lower(Metric), " 5 c| 5c") ~ "SIS" ,
                                     str_detect(str_to_lower(Metric), "middle school drop|dropout|drop out") ~ "SIS" ,
                                     str_detect(str_to_lower(Metric), " 5 d| 5d") ~ "SIS" ,
                                     str_detect(str_to_lower(Metric), "high school drop") ~ "SIS" ,
                                     
                                     str_detect(str_to_lower(Metric), " 5 e| 5e") ~ "SIS, Dashboard reports" ,
                                     str_detect(str_to_lower(Metric), "graduation") ~ "SIS, Dashboard reports" ,
                                     
                                     str_detect(str_to_lower(Metric), " 6 a| 6a|6 b|6b") ~ "SIS or SWIS,  Dashboard reports" ,
                                     str_detect(str_to_lower(Metric), "suspension|expulsion") ~ "SIS or SWIS,  Dashboard reports" ,
                                     str_detect(str_to_lower(Metric), " 6 c| 6c") ~ "Locally determined, often survey done at end of the year" ,
                                     str_detect(str_to_lower(Metric), "School Climate") ~ "Locally determined, often survey done at end of the year" ,
                                     str_detect(str_to_lower(Metric), " 7 a| 7a") ~ "Locally determined, possibly Class schedules" ,
                                     str_detect(str_to_lower(Metric), "broad course|programs and services") ~ "Locally determined, possibly Class schedules" ,
                                     str_detect(str_to_lower(Metric), " 7 b| 7b|7 c|7c") ~ "Locally determined, possibly Class schedules" ,
                                     str_detect(str_to_lower(Metric), "priority 8") ~ "Locally determined" ,
                                     str_detect(str_to_lower(Metric), "renaissance|star ") ~ "Proprietary System" ,
                                     
                                     
           )
           
           )


ss <- "https://docs.google.com/spreadsheets/d/1WjFBvLxwhfbmJdCattvM3hd-Hak71N2EEd6pzIBmK3M/edit#gid=1341274390"

leas <- unique(advice$`District Name`)

for (i in leas) {
 
    advice %>%
        filter(`District Name` == i) %>%
    write_sheet(ss, sheet = i)
 
}


# When a district put multiple indicators into a single metric, this tool will usually not work (e.g. Washington)
# When districts put indicators in a seperate appendix this will not work (e.g. San Ardo)

