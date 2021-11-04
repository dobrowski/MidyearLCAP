

library(tidyverse)
library(readxl)
library(here)
library(googlesheets4)


exported.file <- "211102_Monterey COE_MRR Summary.xlsx"

ets.file <- read_xlsx(here("data",exported.file))

data <- ets.file %>%
    mutate(FieldData = str_replace_all(FieldData, "<br/>", "\n")) %>% 
    mutate(descrip2 =  str_remove(Description,"21-22 LCAP: " )) %>%
    separate(descrip2, sep = " - ", into = c("Goal","Trash","Number", "Type")) %>% 
    select(-Description,-Section, - Trash) %>%
    pivot_wider(names_from = Type, values_from = FieldData) 



advice <- data %>%
    mutate(`MidYear Advice` = case_when(str_detect(str_to_lower(Metric), " 1 a| 1a") ~ "Likely can report" ,
                                        str_detect(str_to_lower(Metric), "credential| appropriately assigned|misass") ~ "Likely can report", 
                                        str_detect(str_to_lower(Metric), " 1 b| 1b") ~ "Likely can report" ,
                                        str_detect(str_to_lower(Metric), "materials") ~ "Likely can report", 
                                        str_detect(str_to_lower(Metric), " 1 c| 1c") ~ "Likely can report" ,
                                        str_detect(str_to_lower(Metric), "facilities|good repair| fit") ~ "Likely can report", 
                                        str_detect(str_to_lower(Metric), " 2 a| 2a") ~ "Likely can report" ,
                                        str_detect(str_to_lower(Metric), "state academic standards|performance standards|state standards") ~ "Likely can report" ,
                                        str_detect(str_to_lower(Metric), " 2 b| 2b") ~ "Likely can report" ,
                                        str_detect(str_to_lower(Metric), "eld standards") ~ "Likely can report" ,
                                        str_detect(str_to_lower(Metric), " 3 a| 3a") ~ "Likely can report" ,
                                        str_detect(str_to_lower(Metric), " family partnerships|input") ~ "Likely can report" ,
                                        str_detect(str_to_lower(Metric), " 3 b| 3b") ~ "Likely can report" ,
                                        str_detect(str_to_lower(Metric), " participat") ~ "Likely can report" ,
                                        str_detect(str_to_lower(Metric), " 3 c| 3c") ~ "Likely can report" ,
                                        str_detect(str_to_lower(Metric), " participat") ~ "Likely can report" ,
                                        str_detect(str_to_lower(Metric), " 4 a| 4a") ~ "Cannot report, consider interim assessments" ,
                                        str_detect(str_to_lower(Metric), "caaspp|sbac|statewide ass") ~ "Cannot report, consider interim assessments" ,
                                        str_detect(str_to_lower(Metric), " 4 b| 4b") ~ "Cannot report" ,
                                        str_detect(str_to_lower(Metric), "elpac") ~ "Cannot report" ,
                                        str_detect(str_to_lower(Metric), " 4 c| 4c") ~ "Cannot report" ,
                                        str_detect(str_to_lower(Metric), " eap|early assessment program") ~ "Cannot report" ,
                                        str_detect(str_to_lower(Metric), " 4 d| 4d") ~ "Can report on last year's testing" ,
                                        str_detect(str_to_lower(Metric), "progress") ~ "Can report on last year's testing" ,
                                        str_detect(str_to_lower(Metric), " 4 e| 4e") ~ "Can report based on last year's testing" ,
                                        str_detect(str_to_lower(Metric), "reclass") ~ "Can report based on last year's testing" ,
                                        # str_detect(str_to_lower(Metric), " 4 f| 4f") ~ "Cannot report" ,
                                        # str_detect(str_to_lower(Metric), "elpac") ~ "Cannot report" ,
                                        # str_detect(str_to_lower(Metric), " 4 g| 4g") ~ "Cannot report" ,
                                        # str_detect(str_to_lower(Metric), "elpac") ~ "Cannot report" ,
                                        str_detect(str_to_lower(Metric), " 5 a| 5a") ~ "Likely can report" ,
                                        str_detect(str_to_lower(Metric), "attendance") ~ "Likely can report" ,
                                        str_detect(str_to_lower(Metric), " 5 b| 5b") ~ "Likely can report" ,
                                        str_detect(str_to_lower(Metric), "chronic") ~ "Likely can report" ,
                                        str_detect(str_to_lower(Metric), " 5 c| 5c") ~ "Likely can report" ,
                                        str_detect(str_to_lower(Metric), "middle school drop|dropout|drop out") ~ "Likely can report" ,
                                      str_detect(str_to_lower(Metric), " 5 d| 5d") ~ "Likely can report" ,
                                      str_detect(str_to_lower(Metric), "high school drop") ~ "Likely can report" ,  
                                      str_detect(str_to_lower(Metric), " 5 e| 5e") ~ "Cannot report" ,
                                      str_detect(str_to_lower(Metric), "graduation") ~ "Cannot report" ,
                                        str_detect(str_to_lower(Metric), " 6 a| 6a|6 b|6b") ~ "Likely can report" ,
                                        str_detect(str_to_lower(Metric), "suspension|expulsion|suspen|expel") ~ "Likely can report" ,
                                        str_detect(str_to_lower(Metric), " 6 c| 6c") ~ "Unlikely to report" ,
                                        str_detect(str_to_lower(Metric), "school climate") ~ "Unlikely to report" ,
                                        str_detect(str_to_lower(Metric), " 7 a| 7a") ~ "Likely can report" ,
                                        str_detect(str_to_lower(Metric), "course access|broad course|programs and services") ~ "Likely can report" ,
                                        str_detect(str_to_lower(Metric), " 7 b| 7b|7 c|7c") ~ "Likely can report" ,
                                      str_detect(str_to_lower(Metric), "priority 8") ~ "Likely can report" ,
                                      str_detect(str_to_lower(Metric), "renaissance|star ") ~ "Likely can report" ,
                                      str_detect(str_to_lower(Metric), "a-g|a to g") ~ "Cannot report" ,
                                      str_detect(str_to_lower(Metric), "cte") ~ "Cannot report" ,
                                      str_detect(str_to_lower(Metric), "ap ass|ap exam|advanced placement") ~ "Cannot report" ,
                                      str_detect(str_to_lower(Metric), "survey") ~ "Unlikely to report" ,
                                      
              
                                        
                                        ),
           `Possible Data Source` = case_when(str_detect(str_to_lower(Metric), " 1 a| 1a") ~ "CDE will release a new data file in November;  historically done with internal school records",
                                     str_detect(str_to_lower(Metric), "credential| appropriately assigned|misass") ~ "CDE will release a new data file in November;  historically done with internal school records",
                                     str_detect(str_to_lower(Metric), " 1 b| 1b") ~ "SARC" ,
                                     str_detect(str_to_lower(Metric), "materials") ~ "SARC", 
                                     str_detect(str_to_lower(Metric), " 1 c| 1c") ~ "SARC" ,
                                     str_detect(str_to_lower(Metric), "facilities|good repair| fit") ~ "SARC", 
                                     str_detect(str_to_lower(Metric), " 2 a| 2a") ~ "Locally determined, possibly Class schedules" ,
                                     str_detect(str_to_lower(Metric), "state academic standards|performance standards|state standards") ~ "Locally determined, possibly Class schedules" ,
                                     str_detect(str_to_lower(Metric), " 2 b| 2b") ~ "Locally determined, possibly Class schedules" ,
                                     str_detect(str_to_lower(Metric), "eld standards") ~ "Locally determined, possibly Class schedules" ,
                                     str_detect(str_to_lower(Metric), " 3 a| 3a") ~ "Locally determined, possibly IEP records, Parent Conference records, Parent Meeting records" ,
                                     str_detect(str_to_lower(Metric), " family partnerships|input") ~ "Locally determined, possibly IEP records, Parent Conference records, Parent Meeting records" ,
                                     str_detect(str_to_lower(Metric), " 3 b| 3b") ~ "Locally determined, possibly sign-in sheets" ,
                                     str_detect(str_to_lower(Metric), " participat") ~ "Locally determined, possibly sign-in sheets" ,
                                     str_detect(str_to_lower(Metric), " 3 c| 3c") ~ "Locally determined, possibly sign-in sheets" ,
                                     str_detect(str_to_lower(Metric), " participat") ~ "Locally determined, possibly sign-in sheets" ,
                                     str_detect(str_to_lower(Metric), " 4 a| 4a") ~ "CAASPP End of Year testing - CERS or CAASPP-ELPAC.org" ,
                                     str_detect(str_to_lower(Metric), "caaspp|sbac|statewide ass") ~ "CAASPP End of Year testing - CERS or CAASPP-ELPAC.org" ,
                                     str_detect(str_to_lower(Metric), " 4 b| 4b") ~ "ELPAC End of Year testing - CERS or CAASPP-ELPAC.org" ,
                                     str_detect(str_to_lower(Metric), "elpac") ~ "ELPAC End of Year testing - CERS or CAASPP-ELPAC.org" ,
                                     str_detect(str_to_lower(Metric), " 4 c| 4c") ~ "CERS, CAASPP_ELPAC.org or perhaps College Board" ,
                                     str_detect(str_to_lower(Metric), " eap|early assessment program") ~ "CERS, CAASPP_ELPAC.org or perhaps College Board" ,
                                     str_detect(str_to_lower(Metric), " 4 d| 4d") ~ "CERS or CAASPP_ELPAC.org" ,
                                     str_detect(str_to_lower(Metric), "progress") ~ "CERS or CAASPP_ELPAC.org" ,
                                     str_detect(str_to_lower(Metric), " 4 e| 4e") ~ "SIS" ,
                                     str_detect(str_to_lower(Metric), "reclass") ~ "SIS" ,
                                     # str_detect(str_to_lower(Metric), " 4 f| 4f") ~ "Cannot report" ,
                                     # str_detect(str_to_lower(Metric), "elpac") ~ "Cannot report" ,
                                     # str_detect(str_to_lower(Metric), " 4 g| 4g") ~ "Cannot report" ,
                                     # str_detect(str_to_lower(Metric), "elpac") ~ "Cannot report" ,
                                      str_detect(str_to_lower(Metric), " 5 a| 5a") ~ "SIS" ,
                                     str_detect(str_to_lower(Metric), "attendance") ~ "SIS" ,
                                     str_detect(str_to_lower(Metric), " 5 b| 5b") ~ "SIS and Dashboard reports" ,
                                     str_detect(str_to_lower(Metric), "chronic") ~ "SIS and Dashboard reports" ,
                                     str_detect(str_to_lower(Metric), " 5 c| 5c") ~ "SIS" ,
                                     str_detect(str_to_lower(Metric), "middle school drop|dropout|drop out") ~ "SIS" ,
                                     str_detect(str_to_lower(Metric), " 5 d| 5d") ~ "SIS" ,
                                     str_detect(str_to_lower(Metric), "high school drop") ~ "SIS" ,
                                     
                                     str_detect(str_to_lower(Metric), " 5 e| 5e") ~ "SIS, Dashboard reports" ,
                                     str_detect(str_to_lower(Metric), "graduation") ~ "SIS, Dashboard reports" ,
                                     
                                     str_detect(str_to_lower(Metric), " 6 a| 6a|6 b|6b") ~ "SIS or SWIS,  Dashboard reports" ,
                                     str_detect(str_to_lower(Metric), "suspension|expulsion|suspen|expel") ~ "SIS or SWIS,  Dashboard reports" ,
                                     str_detect(str_to_lower(Metric), " 6 c| 6c") ~ "Locally determined, often survey done at end of the year" ,
                                     str_detect(str_to_lower(Metric), "School Climate") ~ "Locally determined, often survey done at end of the year" ,
                                     str_detect(str_to_lower(Metric), " 7 a| 7a") ~ "Locally determined, possibly Class schedules" ,
                                     str_detect(str_to_lower(Metric), "course access|broad course|programs and services") ~ "Locally determined, possibly Class schedules" ,
                                     str_detect(str_to_lower(Metric), " 7 b| 7b|7 c|7c") ~ "Locally determined, possibly Class schedules" ,
                                     str_detect(str_to_lower(Metric), "priority 8") ~ "Locally determined" ,
                                     str_detect(str_to_lower(Metric), "renaissance|star ") ~ "Proprietary System" ,
                                     str_detect(str_to_lower(Metric), "a-g|a to g") ~ "SIS or Dashboard Additional Report" ,
                                     str_detect(str_to_lower(Metric), "cte") ~ "SIS or Dashboard Additional Report" ,
                                     str_detect(str_to_lower(Metric), "ap ass|ap exam|advanced placement") ~ "College Board or Dashboard Additional Report" ,
                                     str_detect(str_to_lower(Metric), "survey") ~ "survey" ,
                                     
                                     
           )
           
           ) %>%
    mutate(`District Name` = replace(`District Name`, `District Name` == "LCAP Account", "Santa Rita"))
 #   recode(`District Name`, "LCAP Account" = "Santa Rita" )


sises <- tribble(
    ~`District Name`, ~SIS,
    # "South Monterey County Joint Union High School District", "Aeries",
    # "Soledad Unified School District", "Aeries",
    # "Pacific Grove Unified School District", "Synergy"
    #  "Alisal Union Elementary School District"  ,"",             
    #  "Big Sur Unified School District"            ,"",           
    #  "Bradley Elementary School"                    ,"",         
    #  "Buena Vista Middle School"                      ,"",       
    #  "Carmel Unified School District"                   ,"",     
    #  "Chualar Union Elementary School District"           ,"",   
    # "Gonzales Unified School District"                      ,"",
    #  "Graves Elementary School"                 ,"",             
    #  "Greenfield Union School District"           ,"",           
    # "King City Union School District"               ,"",        
    #  "Lagunita Elementary School"                     ,"",       
    #  "Santa Rita"                                       ,"",     
    #  "Mission Elementary School"                          ,"",   
    #  "Monterey Peninsula Unified School District"           ,"", 
    #  "North Monterey County Unified School District"         ,"",
    #  "Open Door Charter"                    ,"",                 
    "Pacific Grove Unified School District"   ,"Synergy",              
    #  "Salinas Community School"                 ,"",             
    #  "Salinas Union High School District"         ,"",           
    # "San Antonio Elementary School"                 ,"",        
    #  "San Ardo Union Elementary School District"      ,"",       
    #  "San Lucas Elementary School"                      ,"",     
     "Soledad Unified School District"                    ,"Aeries",   
     "South Monterey County Joint Union High School District","Aeries",
    #  "Spreckels Union School District"                       ,"",
    # "Washington Union Elementary School District" ,""
)



ss <- "https://docs.google.com/spreadsheets/d/1WjFBvLxwhfbmJdCattvM3hd-Hak71N2EEd6pzIBmK3M/edit#gid=1341274390"

leas <- unique(advice$`District Name`)[25]                                  

for (i in leas) {
 
mySIS <- sises %>%
    filter(`District Name` == i) %>%
    select(SIS) %>%
    unlist()
    
    
    if (is_empty(mySIS)) {
        mySIS <- "SIS"
    }
    
    print(mySIS)


    advice %>%
        filter(`District Name` == i) %>%
        mutate(`Possible Data Source` =  str_replace(`Possible Data Source`,"SIS",mySIS)) %>%
    write_sheet(ss, sheet = i)
 
}


# When a district put multiple indicators into a single metric, this tool will usually not work (e.g. Washington)
# When districts put indicators in a seperate appendix this will not work (e.g. San Ardo)
# When not done in DTS, it will not appear, (e.g. Salinas City)

