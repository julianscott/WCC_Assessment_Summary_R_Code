# Analysis of Watershed Condition Classification data stored in the WCATT Forest Service database.
# This code is used to create Tables 1 through Table 6 in the document titled:
# "Watershed Condition Classification Attribute Assessments: A summary 2010 to 2018".
# The report and the csv and xlsx files with the data used in this analysis is included in the gitup repository.
# See report for discussion of analysis.

# Julian A. Scott 08/06/2019
# 970-295-5974
# julianscotta@gmail.com
# jascott@fs.fed.us

# Update working directory to direct the code to wherever the csv data is stored on your machine. 
setwd("C:\\Users\\jascott\\Documents\\Project\\WCF\\WCC_Summary")

# List of the required libraries
packages <- c("sp", "raster", "sf", "tidyverse", "maps", "rasterVis","doParallel","viridis","xlsx","stargazer")
# packages

# Check to see if each is installed, and install if not.
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {    
  install.packages(setdiff(packages, rownames(installed.packages())))  
}

# Use the lapply function to load the installed libraries in the packages list 
lapply(packages,library,character.only=TRUE)

# Watershed Condition Classification data, by year (includes indicator class, score, and attribute data)
# One assessment, per year, per row. I.e., if there are multiple assessments for a watershed, it has multiple rows, 1/year. 
# Exported "WCATT_Attribute_Ratings_by_Assessment_Year-SAQ" from WIT database on 3/29/2018; Saved as Export_WCC_Indic_Atrrib_byYear.csv
sum_table <- data.frame(read.csv("Export_WCC_Indic_Atrrib_byYear_032918.csv",colClasses =c(HUC12_Code = "character")))

# Exported from WIT database on 7/2/2018
prio_wsheds <- read.xlsx("Completed_priority_watersheds_070218.xlsx",sheetName = "Sheet1",stringsAsfactors = FALSE)

# create a list of complected priority watersheds
prio_HUCs <- prio_wsheds$WATERSHED_CODE

sum_table <- sum_table %>% #
  left_join(count(sum_table,HUC12_Code),by="HUC12_Code") %>% # add a column that provides the number of assessments for that watershed
  rename(n_assmnts = n)%>% # rename.
  mutate(region = floor(FS_Unit_ID/100))

# Table 1. Number of watersheds reporting by year. 
wa.year <- sum_table %>%
  group_by(Assessment_Year) %>%
  summarise(n_by_year = n())

# Table 1
stargazer(wa.year,
          type = "text",
          #title = "Number of Watersheds Reporting by Year",
          summary = FALSE,
          rownames = FALSE)
# write.csv(wa.year,"table1.csv")

### Table 2. The number of watersheds reporting single and multiple assessments, by region.

# Identifying HUC12s suitable for trend analysis
HUC12s <- sum_table %>% # rename to work
  group_by(HUC12_Code,region) %>% # group by HUC12 and region
  summarise(n_assmnts = n()) %>% # count number of assessments for each HUC12
  group_by(n_assmnts,region) %>% # group by # of assessments and region
  summarise(n_watersheds = n()) %>% # count the number of watersheds in each catagory of n_assmnts, by region
  spread(region,n_watersheds,fill = 0) # reformat table to be wide
# format region names
names(HUC12s)[2:10] <- paste0("R",names(HUC12s)[2:10])
# add an overall total column
HUC12s$Overall <- apply(HUC12s[,2:10],1,sum) 

# Table 2
stargazer(HUC12s,
          type = "text",
          summary = FALSE,
          rownames = FALSE)
# write.csv(HUC12s,"table2.csv")

# Table 3
# Build function that will create data.frame, each unique watershed gets a single row
# columns are HUC identifiers, number of assessments done overall between 2010 - 2019, 
# and the initial and final year and overall assessment. 
build_df_fun <- function(sum_table) {
  foreach (i = iter(unique(sum_table$HUC12_Code)),.combine = rbind) %dopar% {
    subdf = dplyr::filter(sum_table,HUC12_Code == i) # create a subset of sum_table for watershed i
    tmpdf = data.frame(HUC12_Code = unique(subdf$HUC12_Code), # watershed HUC
                       HUC12_Name = unique(subdf$HUC12_Name), # watershed Name
                       n_assmnts = unique(subdf$n_assmnts), # number of assessments done on watershed
                       initial_c = subdf[subdf$Assessment_Year == min(subdf$Assessment_Year),"Watershed_Class_FS_Land"], # initial WCC
                       initial_y = min(subdf$Assessment_Year), # initial assessment year
                       final_c = subdf[subdf$Assessment_Year == max(subdf$Assessment_Year),"Watershed_Class_FS_Land"], # final WCC
                       final_y = max(subdf$Assessment_Year)) # final assessment year
    tmpdf
  }
}
# run the function using sum_table as the argument. Uses parallel processing.
system.time({
  UseCores <- detectCores() - 1 # define number of cores to use
  cl <- makeCluster(UseCores) # make a cluster
  registerDoParallel(cl) # enable parallel processing
  
  rdf <- build_df_fun(sum_table) # run function 
  
  stopCluster(cl)  # end parallel processing
})

# set rdf$HUC12_Code to character
rdf$HUC12_Code <- as.character(rdf$HUC12_Code)

# Dataframe of HUC12Codes, n_assmnts, and the initial year of assessment
initial_assmnt <- select(rdf,HUC12_Code,n_assmnts,initial_y)

t2.3 <- sum_table %>%  # with sum_table
  select(HUC12_Code,Assessment_Year,region) %>%                                   # select relevent columns
  left_join(initial_assmnt[,c("HUC12_Code","initial_y")],by="HUC12_Code") %>%     # join with the initial_assmnt data
  mutate(AssmntY_Equal_Initial = Assessment_Year == initial_y) %>%                # Logical test if the assessment year is equal to the initial assessment year
  group_by(Assessment_Year,region,AssmntY_Equal_Initial) %>%                      # group year, region, and logical test result
  summarize(n_watersheds = n()) %>%                                               # count watersheds in each logical test result group, by year and region 
  spread(AssmntY_Equal_Initial,n_watersheds,fill = 0) %>%                         # spread dataset by logical test result
  mutate(n_watersheds = paste0(`TRUE`,"(",`FALSE`,")")) %>%                       # add new column with formated false and true counts 
  select(Assessment_Year,region,n_watersheds) %>%                                 # select columns
  spread(region,n_watersheds,fill = "0(0)")                                       # spread dataset by region

# format column names
names(t2.3)[2:10] <- paste0("R",names(t2.3)[2:10])

# add column for overall totals
overall.t23 <- sum_table %>%  # with sum_table
  select(HUC12_Code,Assessment_Year,region) %>%                                   # select relevent columns
  left_join(initial_assmnt[,c("HUC12_Code","initial_y")],by="HUC12_Code") %>%     # join with the initial_assmnt data
  mutate(AssmntY_Equal_Initial = Assessment_Year == initial_y) %>%                # Logical test if the assessment year is equal to the initial assessment year
  group_by(Assessment_Year,AssmntY_Equal_Initial) %>%                             # group year and logical test result
  summarize(n_watersheds = n()) %>%                                               # count watersheds in each logical test result group, by year a 
  spread(AssmntY_Equal_Initial,n_watersheds,fill = 0) %>%                         # spread dataset by logical test result
  mutate(n_watersheds = paste0(`TRUE`,"(",`FALSE`,")")) %>%                       # add new column with formated false and true counts 
  select(Assessment_Year,n_watersheds)                                            # select columns

# add overall column to results 
t2.3$Overall <- overall.t23$n_watersheds

# Table 3. 
# Number of watersheds reporting by year and region, 
# where the first number is the number of watersheds assessed for the first time, 
# while the number in parenthesis is the number of re-assessed watersheds for the given year and region. 

stargazer(t2.3,
          type = "text",
          #title = "",
          summary = FALSE,
          rownames = FALSE)
# write.csv(t2.3,"table3.csv")

### Table 4: For those watersheds reporting greater than one assessment for the years 2010 through 2018, 
# the change in watershed condition class (wcc), and the corresponding number of watersheds. A +2 indicates 
# a 2 unit improvement in wcc, 0 indicates no change in wcc, and -2 indicates a 2 unit degredation in wcc. 
# This table made up from Table 4a and Table 4b.

rdf2 <- rdf %>%
  filter(n_assmnts != 1) %>%              # remove those watersheds with just one assessment
  mutate(delta_c = initial_c-final_c) %>% # + numbers indicate an improvement in condition class, - numbers are a reduction in condition. For example, 2-1 = +1
  group_by(delta_c) %>%                   # group by delta catagory
  summarise(d_c_n = n()) %>%              # count watersheds in each group
  rename(Change_WCC = delta_c,
         n_watersheds = d_c_n)

# Table 4a
stargazer(rdf2,
          type = "text",
          #title = "",
          summary = FALSE,
          rownames = FALSE)
# write.csv(rdf2,"Table4a.csv")

# Table 4b work. Same as Table 4a, except that the data summarized here use the wcc override, 
#if one is present for a given watershed assessment. The total number of wcc overrides is 95 for the period 2010-2018. 
#Change_wcc calculated by subtracting the watershed condition class corresponding to the last year (final) from the first year (initial).

sum_table_ovr <- sum_table %>% # start with origial data, sum_table
  mutate(wcc_wOvr = ifelse(is.na(Watershed_Override_Class),Watershed_Class_FS_Land,Watershed_Override_Class)) # use override when present

build_df_fun_ovr <- function(sum_table) {
  foreach (i = iter(unique(sum_table$HUC12_Code)),.combine = rbind) %dopar% {
    subdf = dplyr::filter(sum_table,HUC12_Code == i) # create a subset of sum_table for watershed i
    tmpdf = data.frame(HUC12_Code = unique(subdf$HUC12_Code), # watershed HUC
                       HUC12_Name = unique(subdf$HUC12_Name), # watershed Name
                       n_assmnts = unique(subdf$n_assmnts), # number of assessments done on watershed
                       initial_c = subdf[subdf$Assessment_Year == min(subdf$Assessment_Year),"wcc_wOvr"],
                       initial_y = min(subdf$Assessment_Year), # initial assessment year
                       final_c = subdf[subdf$Assessment_Year == max(subdf$Assessment_Year),"wcc_wOvr"],
                       final_y = max(subdf$Assessment_Year)) # final assessment year
    tmpdf
  }
}

# run the function using sum_table_ovr as the argument. Uses parallel processing.
system.time({
  UseCores <- detectCores() - 1 # define number of cores to use
  cl <- makeCluster(UseCores) # make a cluster
  registerDoParallel(cl) # enable parallel processing
  
  rdf_ovr <- build_df_fun_ovr(sum_table_ovr) # run function 
  
  stopCluster(cl)  # end parallel processing
})

head(rdf_ovr)
rdf2_ovr <- rdf_ovr %>%
  filter(n_assmnts != 1) %>%
  mutate(delta_c = initial_c-final_c) %>% # # + numbers indicate an improvement in condition class, - numbers are a reduction in condition. For example, 2-1 = +1
  group_by(delta_c) %>%
  summarise(d_c_n = n()) %>%
  rename(Change_WCC = delta_c,
         n_watersheds = d_c_n)

# Table 4b.
stargazer(rdf2_ovr,
          type = "text",
          #title = "",
          summary = FALSE,
          rownames = FALSE)
write.csv(rdf2_ovr,"Table4b.csv")

#### Look at initial vs final assessment for every attribute, Tables 5 and 6

#Table 5. Number of watersheds with an improved (pos_change), degraded (neg_change), or invariant (no_change) attribute class 
# over the period 2010-2018.  The percent of the total number of watersheds is provided, while the number of watersheds is in parenthesis.
# The n_watersheds column provides the total number of assessed watersheds, by attribute*. 

# this produces many rows per assessment!!! One for each indicator class, score, and attribute
Attr_long <- sum_table %>% 
  tidyr::gather(Measure,Value,colnames(sum_table)[14:61]) %>%                        # Gather indicator class, score, and attribute columns
                                                                              # Add column that differentiates whether the Measure is a 
                                                                              # indicator class, indicator score, or attribute class
  mutate(Measure_level = ifelse(grepl("Class",Measure),"Ind_Class",
                                ifelse(grepl("Score",Measure),"Ind_Score","Attribute_Class"))) %>%
  filter(!is.na(Value)) %>%                                                   # remove NA measure values
  filter(Measure_level == "Attribute_Class") %>%                              # filter for just Attribue class data
  unite(HUC12_Measure,HUC12_Code,Measure,remove = FALSE)                      # create new column that combines HUC+Measure

# remove those watersheds with only 1 assessment
Attr_long_other <- filter(Attr_long,n_assmnts != 1)

# For each unique HUC12+Measure, determine initial and final data, as for previous table
build_df_fun_attr <- function(Attr_long_other) {
  foreach (i = iter(unique(Attr_long_other$HUC12_Measure)),.combine = rbind) %dopar% {
    subdf = dplyr::filter(Attr_long_other,HUC12_Measure == i)
    tmpdf = data.frame(HUC12_Code = unique(subdf$HUC12_Code),
                       n_assmnts = unique(subdf$n_assmnts),
                       Measure = unique(subdf$Measure),
                       initial_c = subdf[subdf$Assessment_Year == min(subdf$Assessment_Year),"Value"],
                       initial_y = min(subdf$Assessment_Year),
                       final_c = subdf[subdf$Assessment_Year == max(subdf$Assessment_Year),"Value"],
                       final_y = max(subdf$Assessment_Year))
    tmpdf
  }
}

# run the function using sum_table_ovr as the argument. Uses parallel processing.
system.time({
  UseCores <- detectCores() - 1 # define number of cores to use
  cl <- makeCluster(UseCores) # make a cluster
  registerDoParallel(cl) # enable parallel processing
  
  Attr_long_if <- build_df_fun_attr(Attr_long_other) # run function 
  
  stopCluster(cl)  # end parallel processing
})

## Find the total number of watersheds that have had a given change attribute class, by attribute,
Attr_long_if2_tbl <- Attr_long_if %>%
  mutate(Change_WCC = initial_c-final_c) %>% # 2-1 = 1, so + numbers indicate an improvement in condition class, - numbers are a reduction in condition
  group_by(Measure,Change_WCC) %>%
  summarise(n_watersheds = n()) %>%
  spread(Change_WCC,n_watersheds,fill=0) %>%
  mutate(pos_change_real = sum(`1`,`2`),
         no_change_real = sum(`0`),
         neg_change_real = sum(`-1`,`-2`),
         overall_change_real = sum(`1`,`2`,`-1`,`-2`),
         pos_change = paste0(round(sum(`1`,`2`)/sum(`-1`,`-2`,`0`,`1`,`2`),2)*100,"% (",sum(`1`,`2`),")"),
         no_change = paste0(round(sum(`0`)/sum(`-1`,`-2`,`0`,`1`,`2`),2)*100,"% (",sum(`0`),")"),
         neg_change = paste0(round(sum(`-1`,`-2`)/sum(`-1`,`-2`,`0`,`1`,`2`),2)*100,"% (",sum(`-1`,`-2`),")"),
         overall_change = paste0(round(sum(`1`,`2`,`-1`,`-2`)/sum(`-1`,`-2`,`0`,`1`,`2`),2)*100,"% (",sum(`1`,`2`,`-1`,`-2`),")"),
         n_watersheds  = paste0("100% (",sum(`-1`,`-2`,`0`,`1`,`2`),")")) %>%
  arrange(-overall_change_real) %>%
  select(Measure,overall_change,pos_change,neg_change,no_change,n_watersheds)
Attr_long_if2_tbl$Measure <- as.character(Attr_long_if2_tbl$Measure)

Attr_long_if2_tbl$Measure <- sub("Attribute_","",Attr_long_if2_tbl$Measure)
Attr_long_if2_tbl$Measure <- gsub("_"," ",Attr_long_if2_tbl$Measure)
colnames(Attr_long_if2_tbl) <- c("WCC Attribute","Any change","Positive Change","Negative Change","No Change","# Watersheds")

# Table 5. 
stargazer(Attr_long_if2_tbl,
          type = "text",
          #title = "",
          summary = FALSE,
          rownames = FALSE)

# write.csv(Attr_long_if2_tbl,"Table5.csv")

## What is the total number of PRIORITY watersheds that have seen had a + change attribute class, by attribute?

# Table 6. Number of priority watersheds with an improved (pos_change), degraded (neg_change), or invariant (no_change) 
# attribute class over the period 2010-2018.  The percent of the total number of watersheds is provided, while the number of 
# watersheds is in parenthesis. The n_watersheds column provides the total number of assessed watersheds, by attribute**. 

# This is the same analysis as for Table 5, just filtered for priority watersheds. 
prio_attr <- Attr_long_if %>%
  filter(HUC12_Code %in% prio_HUCs) %>%
  mutate(Change_WCC = initial_c-final_c) %>% # 2-1 = 1, so + numbers indicate an improvement in condition class, - numbers are a reduction in condition
  group_by(Measure,Change_WCC) %>%
  summarise(n_watersheds = n()) %>%
  spread(Change_WCC,n_watersheds,fill=0) %>%
  mutate(pos_change_real = sum(`1`,`2`),
         no_change_real = sum(`0`),
         neg_change_real = sum(`-1`,`-2`),
         overall_change_real = sum(`1`,`2`,`-1`,`-2`),
         pos_change = paste0(round(sum(`1`,`2`)/sum(`-1`,`-2`,`0`,`1`,`2`),2)*100,"% (",sum(`1`,`2`),")"),
         no_change = paste0(round(sum(`0`)/sum(`-1`,`-2`,`0`,`1`,`2`),2)*100,"% (",sum(`0`),")"),
         neg_change = paste0(round(sum(`-1`,`-2`)/sum(`-1`,`-2`,`0`,`1`,`2`),2)*100,"% (",sum(`-1`,`-2`),")"),
         overall_change = paste0(round(sum(`1`,`2`,`-1`,`-2`)/sum(`-1`,`-2`,`0`,`1`,`2`),2)*100,"% (",sum(`1`,`2`,`-1`,`-2`),")"),
         n_watersheds  = paste0("100% (",sum(`-1`,`-2`,`0`,`1`,`2`),")")) %>%
  arrange(-overall_change_real) %>%
  select(Measure,overall_change,pos_change,neg_change,no_change,n_watersheds)
prio_attr$Measure <- as.character(prio_attr$Measure)
prio_attr$Measure <- sub("Attribute_","",prio_attr$Measure)
prio_attr$Measure <- gsub("_"," ",prio_attr$Measure)
colnames(prio_attr) <- c("WCC Attribute","Any change","Positive Change","Negative Change","No Change","# Watersheds")

# Table 6. 
stargazer(prio_attr,
          type = "text",
          #title = "",
          summary = FALSE,
          rownames = FALSE)

write.csv(prio_attr,"Table6.csv")



