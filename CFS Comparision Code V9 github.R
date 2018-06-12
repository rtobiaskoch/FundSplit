#CURRENT FUND SPLIT COMPARISON CODE DESCRIPTION####


#***********************************************************************************************************************************
#CFS Comparison Code V1 Purpose: The primary purpose of this code is to pull Vaccination Breakdown (vb) and Practice Profile (pp) data and format
#them into the same format at the Current Fund Split (cfs) table in order to determine the differences in splits between them and filter
#providers with diffaerences greater than X amount with a patient population equal to Y

#VERSION 2: instead of repeating code for current year and comp year for practice profile it runs code for both years then subsets
# practice profile by year after the fact.
#fixes NA and NaN data by filling missing data with 0 before calculations in all data.
#Version 3: 
#1. changes working directory to "Validation Production Environment" from "2015-2016 Toby Project" in order to clean directory 
#of clutter and make it all more official sounding
#2. changes import to specify col_types to prevent binding errors
#Version 4: 
#1. in CFS if TOTAL = 1 or 5 for box of 1 or 5 vaccines returns NA then removes na in mean calc for cfs split to return whole number for age group
#2. Removes flu as an object and adds to CFS Vaccine Info reference table in order for it to be updated outside of R code
#3. Splits CFS VAC Info and VB Vac Info into 2 seperate reference tables for easier reference 
#4. Create Cost_Delta calculated variable based off of the cost difference between the splits which are the percentages of 
#Vac_Cost (which is the total cost of VFC and STATE vaccines)
#5. adds upper limit cost of dose into VB. Cost is then calculated as cost of vaccine multiplied by number of patients
#6. Adds percentage of vaccines with unknown VFC status and the cost of those vaccines.
#Version 5: 
#1. Adds cost from vaccine orders excel file
#2. Recalculates cost Delta based off of vaccines ordered rather than vaccine breakdown
#3. Adds % of vaccines administered verse vaccine ordered-wastage in variable VB_Admin_vs_Order
#4. Changed column names to curr_y and comp_y to make code reproducible, names will change to numeric year at the end of code.
#5. Removes any splits with no data to prevent two negative splits when calculating Delta
#Version 6:
#adds version number to data output name
#breaks down vaccine order data into pedatric and adolescent costs
#creates folder based off of sys.date in Data Output file.
#exports graphs to new folder
#Version 7:
#Version 8.1:
#1.Calculated distance of providers VB and PP plot from x=y line and calculates the budget difference in changing them to 
#their x=y split if they are within distance of 0.05 and changing the rest of the providers that do not line up to the Population estimate survey
#2. Changes the ANY age group to be calculated as a product of the PED and ADO age groups 
#by adding a new column Group_Col to the cfs_comp table and groups by this column in the summary reports
#Version 8.2
#removes any missing data from PP or VB in the cfs_comp
#Version8.3
#uses orginal Vaccine Order File
#improved syntax
#Version 9
#1. introduces packrat
#2. changed ppbill filter year from 2017 to current year
#3. Removes merge file do.call for PP. See 8.3 to use 
#4. Alters directories to list.file and read files in folder year to prevent any naming issues with the files
#5. removes code for PP comp year to simplify code. 

#***********************************************************************************************************************************

#****************************************************************************************************
##            HEADER CODE ####
#****************************************************************************************************
#Load Necessary packages
# require(pacman)
# pacman:: p_load(dpylr, tidyr, stringr, lubridate, readxl, xlsx, scales, ggplot2, RColorBrewer, purrr)
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
library(ggplot2)
library(RColorBrewer)
library(readxl)
library(xlsx)
library(scales)
library(purrr)
library(hexbin)
#library(packrat)


#sets working directory (dir) and locations of data inputs (in) and outputs (out) within the directory and the
#comparison year (comp_year). vaccination breakdown = vb, practice profile = pp, current fund split = cfs
#You may also need to add new flu NDC's to the Flu Vac xlsx in the Reference Table

code_version <- "V9"
#these state limits will have to pulled off of SAMS for the year
vfc_limit <- 99068228.71 
state_limit <- 67200000
PES_STATE <- .4
PES_VFC <- .6
unknown_pct <- 0.05
Admin_Order_Lower <- .7
Admin_Order_Upper <- 1

#if analyzing a year that is not in the current year you must manually change these years
current_year <- year(Sys.Date()) 
#current_year <- 2017 #manually changing
comp_year <- current_year-1

#see CFS Comparison Code V9 Directory file on the S Drive for the directory code

#this is the data output file
data_exp <- file.path("/Data Output", Sys.Date())


#****************************************************************************************************
###           PRACTICE PROFILE Data for current year and comparison year ####
#****************************************************************************************************

#run code below to import the monthly Practice Profile data import and the consolidated vfc status reference table
setwd(dir)
pp_file <- list.files(pp_imp)
pp_raw <- read_excel(file.path(pp_imp, pp_file), 
                     sheet = 1,
                     col_types = c("text", "text", "text", "text", "numeric", "text"))

rt_insurance <- read_excel(ref_imp1, 
                             col_types = c("text", "text"))

pp <- pp_raw

#replace spaces in titles with "_" and deletes any ","
names(pp) <- gsub(" " , "_", names(pp))
names(pp) <- gsub("," , "", names(pp))

#coerces columns into correct class for faster merging
pp$Approval_Date <- mdy(pp$Approval_Date)
pp$Approval_Year <- year(pp$Approval_Date)

#removes test columns
pp <- pp %>% 
  filter(!grepl("TEST|STC|Mass", Provider_PIN)) 

#runs logical test and returns missing VFC status' to the reference table
missing_vfc_status <- pp%>% anti_join(rt_insurance, by ="VFC_Status") %>%
                                      select(VFC_Status)
if(nrow(missing_vfc_status$VFC_Status) != 0) {
  write.csv(missing_vfc_status, file = paste("Reference Tables/", "Missing rt_insurance", Sys.Date(), ".csv"))
  stop("VFC_Status in the Practice Profile do not match the rt_insurance Reference Table 
       file 'Missing VFC Status' now in Reference Table folder")
          }

#format raw data and add new calculated columns Age_Group, Year and rt_insurance
#Year column will need to be removed once new incoming data is reformated
pp <- pp %>% mutate(Age_Group = ifelse(AGE == "7-18", "ADO", "PED"))
pp <- left_join(pp, rt_insurance, by ="VFC_Status")


#create new split by age and pin filtered by VFC and STATE for Practice Profile Data
pp_num <-  pp %>% 
  filter(Con_VFC_Status %in% c("VFC","STATE")) %>%
  group_by(Provider_PIN, Age_Group, Con_VFC_Status, Approval_Year) %>% 
  summarise(Sum_of_Pts =sum(Number_of_Patients)) %>% 
  spread(Con_VFC_Status, Sum_of_Pts, fill =0) %>%
  arrange(Provider_PIN, Approval_Year) %>%
  ungroup()

pp_num$Tl_Pts <- rowSums(pp_num[,c("VFC", "STATE")])

pp_split <- pp_num %>% 
  group_by(Provider_PIN, Age_Group, Approval_Year)%>%
  mutate (STATE = STATE/Tl_Pts,
            VFC = VFC/Tl_Pts)

#NOTE removed calculated variable for ANY b/c no comparable split for ANY in the CFS see code V7 and eariler for code

cnames <- colnames(pp_split[,3:6])

#renames columns to explicitly state pp comp year or current year
colnames(pp_split)[3:6] <- paste("PP", "curr_y", colnames(pp_split[3:6]), sep = "_")

# PP BILLING ####
#isolates billing data to be merged with cfs_comp_diff data frame
pp_bill <- pp %>%  
  filter(Approval_Year == current_year) %>% #fixed in v9
  select(Provider_PIN, Data_Source) %>%
  distinct(Provider_PIN, Data_Source, .keep_all = T)


#consolidates data source types into three categories BILLING, IIS, and OTHER
pp_bill$Source1 <- ifelse(grepl("^.*MEDICAID_CLAIMS*|^.*BILLING_SYS", pp_bill$Data_Source),"BILLING", "")
pp_bill$Source2 <- ifelse(grepl("^.*BENCHMARK|^.*PROVIDER_DATA|^.*OTHER", pp_bill$Data_Source), "OTHER", "")
pp_bill$Source3 <- ifelse(grepl("^.*CPIR|^.*DOSES_ADMIN", pp_bill$Data_Source), "IIS", "")
pp_bill <- unite(pp_bill, PP_curr_y_Data_Source, Source1, Source2, Source3, sep = "/")

pp_bill$PP_curr_y_Data_Source <- 
as.character(pp_bill$PP_curr_y_Data_Source) %>%
  gsub("^\\/+", "", .) %>%
  gsub("\\/+$", "", .) %>%
  gsub("\\/\\/", "/", .) %>%
  gsub("^OTHER\\/IIS$|^IIS\\/OTHER$", "IIS", .) %>%
  gsub("^OTHER\\/BILLING$|^BILLING\\/OTHER$", "IIS", .) %>%
  gsub("^OTHER\\/.*\\/.*$|^.*\\/OTHER\\/.*$|^.*\\/>*\\/OTHER$", "BILLING/IIS", .) %>%
  as.factor(.)


pp_bill <- pp_bill[rep(seq_len(nrow(pp_bill)), each = 2),]
pp_bill$Age_Group <- c("ADO", "PED") 
pp_bill <- pp_bill %>% select(-Data_Source)


#**************************************************************************************************************
#####         VACCINE BREAKDOWN for comp_year data ####
#**************************************************************************************************************
vb_file <- list.files(vb_imp)
vb_raw <- read_excel(file.path(vb_imp, vb_file),
                               col_types = c("text","text", "text", 
                                             "text","numeric"))

#Reads in excel reference table
rt_vb_vax <- read_excel(ref_imp2, sheet = "VB Vac Info", col_types = c("text", "text")) 

#renames vb_raw vb
vb <- vb_raw

#rename columns to match merging tables
names(vb)[names(vb) == "NVL(EC.DESCRIPTION,'UNKNOWN')"] <- "VFC_Status"

#returns list of vaccines in vaccine breakdown that are not on the VB Vac Info Reference Table
miss_vb_vac <- vb %>% 
  anti_join(rt_vb_vax,by= c("ASIIS_DESCRIPTION" = "Vaccine_from_VB")) %>%
  select(ASIIS_DESCRIPTION) %>%
  distinct(.)

if(nrow(miss_vb_vac) != 0) {write.csv(miss_vb_vac, file = paste("Reference Tables/", "Missing VB Vac", Sys.Date(), ".csv"))
  stop("Not all vaccine names in VB are in Reference Table, missing names are now in a csv file in the
       Reference Table Folder") 
                             }

#merges tables using left join and creates Age_Group and Con_VFC_Status variables
vb <- left_join(vb, rt_vb_vax, 
                          by= c("ASIIS_DESCRIPTION" = "Vaccine_from_VB")
                          )
vb <- left_join(vb, rt_insurance,
                          by = "VFC_Status")
                                

#create new split by age and pin filtered by VFC and STATE for Vaccine Breakdown Data
vb_split1 <- vb %>% 
  filter(Con_VFC_Status %in% c("VFC", "STATE", "Unknown") & 
         Age_Group %in% c("PED", "ADO")) %>%
  group_by(VFC_PIN, Age_Group, Con_VFC_Status) %>% 
  summarise(Sum_of_Vax =sum(TOTAL)) %>%
  spread(Con_VFC_Status, Sum_of_Vax, fill=0) %>%
  summarise_all(funs(sum(.))) %>%
  group_by(VFC_PIN, Age_Group) %>% 
  mutate(Total = sum(VFC, STATE),
         STATE = STATE/Total,
          VFC = VFC/Total,
        Unknown_Pct = Unknown/(Total+Unknown)
) 

#NOTE Removed vb_total to calculate variable ANY b/c no comparable split for ANY 
#in the CFS see code V7 and eariler for code

#renames columns to identify that data is from vb before the join function
vb_col <- ncol(vb_split1)
colnames(vb_split1)[3:vb_col] <- paste("VB", "comp_y", colnames(vb_split1[3:vb_col]), sep = "_")

#renames VFC_PIN to match other objects as Provider_PIN
vb_split1 <- vb_split1 %>% 
  rename(Provider_PIN = VFC_PIN,
         VB_comp_y_Unknown_Doses = VB_comp_y_Unknown,
         Tl_Vac_Admin = VB_comp_y_Total)
  
#Filters out vaccines with no split for VFC and STATE for the Delta calculation
vb_split <- vb_split1 %>% 
  filter(Tl_Vac_Admin != 0)


#**************************************************************************************************************
######        VACCINE ORDER of Comparison Year #####
#**************************************************************************************************************

#imports data make sure the data for all orders including ped and ado is in the first sheet
vo_file <- list.files(vo_imp)
#ignore warning for this, this is only to get the number of columns in the file to convert to text
colnumbertemp <- read_excel(file.path(vo_imp, vo_file),
                            sheet = 1)
vo_raw <- read_excel(file.path(vo_imp, vo_file),
                     sheet = 1, 
                     col_types = rep("text", ncol(colnumbertemp)))
rm(colnumbertemp)
                     
vo <- vo_raw

#removes special characters and spaces
col_vo <- colnames(vo) %>%
  str_trim(.) %>%
  gsub(" ", "_", .) %>%
  gsub("\\/", "", .) %>%
  gsub("__", "_", .)

#renames columns
colnames(vo) <- col_vo
rm(col_vo)

#extracts price list for all vaccines
vac_price_list  <- vo %>% distinct(NDC, Description, Unit_Price)

#converts list of columns to numeric
num_col <- c("VFC_Qty", "STATE_Qty", "Quantity_Ordered","Unit_Price", "Net_value")
vo <- vo %>%
  mutate_at(num_col, as.numeric)

rm(num_col)
  
#imports reference table to match for age group
rt_cfs_vo_raw <- read_excel(ref_imp3) 

#pulls list of ndc numbers that are missing on the reference table from the vaccine ordering data
rt_cfs_vo <- rt_cfs_vo_raw %>% select(NDC, Vaccine_from_VB, Age_Group)

missing_ndc <- vo %>%
  select(NDC, Description) %>%
  left_join(rt_cfs_vo , by = "NDC") %>%
  filter(is.na(Age_Group)) %>%
  distinct(NDC, .keep_all = T)

if(nrow(missing_ndc) != 0) {write.csv(missing_ndc, file = paste("Reference Tables/", "Missing NDC in VO", Sys.Date(), ".csv", sep= ""))
  stop("Not all vaccine names in VB are in Reference Table, missing names are now in 
       Reference Table Folder") 
}

#merges vac_table to match age_group with the vaccine ordering data
vo_cost <- vo %>%
  left_join(rt_cfs_vo, by = "NDC") %>%
  filter(is.na(Cancelled),
         Ordering_Intention == "PED",
         Age_Group %in% c("PED", "ADO"),
         Fund_Type %in% c("SPL", "VFC", "S/L")) %>%
  distinct() %>%
  group_by(Provider_PIN, Age_Group) %>%
  summarise(Vac_Order_Cost_Tl = sum(Net_value),
            Tl_Doses_Ordered = sum(Quantity_Ordered))

#**************************************************************************************************************
#######       CURRENT FUND SPLIT (cfs) Data of current year ####
#**************************************************************************************************************

#imports Current Fund SPlit Data and rename to cfs_raw to cfs
cfs_file <- list.files(cfs_imp, pattern = ".csv")
cfs_raw <- read.csv(file.path(cfs_imp, cfs_file))
#removes ANY NDC vaccine to avoid confusion with the other calculate ANY variables from
#other data sources

cfs <- cfs_raw %>%
  filter(NDC != "ANY") %>%
  mutate(NDC = as.character(NDC))

#replaces column seperated with "." to "_" to keep formatting consistent
names(cfs) <- gsub("\\.", "_", names(cfs))
names(rt_cfs_vo) <- gsub("\\/", "", names(rt_cfs_vo))
names(rt_cfs_vo) <- gsub(" ", "_", names(rt_cfs_vo))

#tests if all vaccines in CFS are on the CFS Vac Info Reference Table. If True writes a list of those vaccines in the reference table
miss_cfs_vac <- cfs %>% 
  anti_join(rt_cfs_vo, by= "NDC") %>%
  select(NDC) %>%
  distinct(NDC) 

if(nrow(miss_cfs_vac) != 0) {write.csv(miss_cfs_vac, file = paste("Reference Tables/", "Missing NDC in CFS", Sys.Date(), ".csv"))
  stop("Not all NDC numbers in CFS are in Reference Table, file 'Missing CFS NDCs' are now in 
       Reference Table Folder") 
                            } 

#adds vaccine info variables from rt_cfs_vo to cfs and removes ANY
cfs1 <- left_join(cfs, rt_cfs_vo, by = "NDC")

#drop flu vaccine by NDC, makes all doses out of ten, then finds mean of split amongs all vaccines 
#by provider
cfs_split <- cfs1 %>% 
  filter(Age_Group != "FLU") %>%
  mutate(VFC = ifelse(Total %in% c(1,5), NA, VFC),
         STATE = ifelse(Total %in% c(1,5), NA, STATE)) %>%
  group_by(Provider_PIN, Age_Group) %>%
  summarise(CFS_VFC = mean(VFC, na.rm = T)/10,
            CFS_STATE = mean(STATE, na.rm = T)/10
                               ) %>% 
  select(Provider_PIN, Age_Group, CFS_STATE, CFS_VFC)  

#**************************************************************************************************************
########      MERGES DATA sources and calculates difference between data source splits####
#**************************************************************************************************************

#Following code coerces Provider_PIN into factor for faster merging and joins cfs_split to vb_split, pp_split
df_list <- list(cfs_split, vb_split, pp_split, vo_cost, pp_bill)
df_list <- lapply(df_list, transform, Provider_PIN = as.character(Provider_PIN))
df_list <- lapply(df_list, data.frame)
cfs_comp_raw <-  Reduce(function(x, y) 
  full_join(x, y, by = c("Provider_PIN", "Age_Group")),
  df_list)

round_na <- function(colm) {
  ifelse(is.nan(colm),
         NA,
         round(colm, 1)
         )
                           }
#subsets the split and converts them into a more readable format
cfs_read <- cfs_comp_raw %>%
  mutate(CFS_STATE = round_na(CFS_STATE),
         CFS_VFC  = round_na(CFS_VFC),
         PP_STATE = round_na(PP_curr_y_STATE),
         VB_STATE = round_na(VB_comp_y_STATE),
         PP_VFC   = round_na(PP_curr_y_VFC),
         VB_VFC   = round_na(VB_comp_y_VFC)) %>%
  select(Provider_PIN,Age_Group,CFS_STATE, PP_STATE ,VB_STATE,
         CFS_VFC, PP_VFC ,VB_VFC)
    
#following code arranges Pin by PED, ADO and ANY, then substracts VB, PP_comp and PP_curr by the CFS using the mutate each function 
#then removes the Approval year columns with the select function
cfs_comp <- cfs_comp_raw %>% 
  arrange(Provider_PIN, Age_Group) %>%
  mutate_at(c("PP_curr_y_STATE", "VB_comp_y_STATE"), funs(.-CFS_STATE)
              ) %>%
  mutate_at(c("PP_curr_y_VFC", "VB_comp_y_VFC"), funs(.-CFS_VFC)
              ) %>% 
  mutate(VB_STATE_Cost_Delta =        Vac_Order_Cost_Tl*round(VB_comp_y_STATE,1),
         VB_VFC_Cost_Delta =          Vac_Order_Cost_Tl*round(VB_comp_y_VFC,1),
         PP_curr_y_STATE_Cost_Delta = Vac_Order_Cost_Tl*round(PP_curr_y_STATE,1),
         PP_curr_y_VFC_Cost_Delta =   Vac_Order_Cost_Tl*round(PP_curr_y_VFC,1),
         Doses_Admin_v_Order =        Tl_Vac_Admin/Tl_Doses_Ordered,
         Group_Col            =       "ANY"
          ) #%>%
  #select(-c(ends_with("Approval_Year")))

#removes Providers that have not submitted a Practice Profile for the current year

cfs_comp <-  cfs_comp %>% 
  filter(!is.na(Tl_Vac_Admin),
        Tl_Vac_Admin != 0,
         !is.na(PP_curr_y_Tl_Pts),
        PP_curr_y_Tl_Pts != 0)

#good until here

#Rearranges columns to customized order
cfs_comp <- cfs_comp[c("Provider_PIN","Age_Group",
   "CFS_STATE","PP_curr_y_STATE", "VB_comp_y_STATE", "PP_curr_y_STATE_Cost_Delta", 
   "VB_STATE_Cost_Delta","CFS_VFC", "PP_curr_y_VFC","VB_comp_y_VFC","PP_curr_y_VFC_Cost_Delta",
   "VB_VFC_Cost_Delta","VB_comp_y_Unknown_Doses", "VB_comp_y_Unknown_Pct",# removed "VB_comp_y_Unknown_Cost"
   "PP_curr_y_Tl_Pts",  "Tl_Vac_Admin", "Tl_Doses_Ordered", "Doses_Admin_v_Order", "Vac_Order_Cost_Tl",
   "PP_curr_y_Data_Source", "Group_Col")]

#filters for ANY to prevent double counting in totals
cfs_comp_PED <- cfs_comp %>% filter(Age_Group == "PED")
cfs_comp_ADO <- cfs_comp %>% filter(Age_Group == "ADO")

#**************************************************************************************************************
#########     SUMMARY Reports ####
#**************************************************************************************************************

#correlation and statistical tests for state data
#pulls columns for correlation analysis
cfs_stat <- cfs_comp_raw %>%
  filter(!is.na(PP_curr_y_STATE),
         !is.na(VB_comp_y_STATE)) %>%
  select(CFS_STATE, PP_curr_y_STATE, VB_comp_y_STATE)

#normality test
shapiro.test(cfs_stat$PP_curr_y_STATE)
shapiro.test(cfs_stat$VB_comp_y_STATE)
hist(cfs_stat$PP_curr_y_STATE)
hist(cfs_stat$VB_comp_y_STATE)

#correlation coefficient
cor_tbl <- data.frame(cor(cfs_stat))

cor.test(cfs_stat$PP_curr_y_STATE, cfs_stat$VB_comp_y_STATE)

#linear regression
cfs_lm <- lm(formula = VB_comp_y_STATE ~ PP_curr_y_STATE, data = cfs_stat)
summary(cfs_lm)

#Customized Summary Report                                          
rsum <- function(x){
  round(sum(x, na.rm = T), 2)
}
rmean <- function (x) {round(mean(x, na.rm = T), 2)
}

#writes function to Summarize data by age_group and then by data source
sum_rep_fun <-  function(data, group_col) {
  data  %>%  
  group_by_(.dots = lazyeval::lazy(group_col)) %>%
  summarise(No_Providers = length(Provider_PIN),
            CFS_VFC_Split_Mean = rmean(CFS_VFC),
            VB_VFC_Mean_Delta = rmean(VB_comp_y_VFC),
            VB_VFC_Cost = rsum(VB_VFC_Cost_Delta),
            PP_VFC_Mean_Delta = rmean(PP_curr_y_VFC),
            PP_VFC_Cost = rsum(PP_curr_y_VFC_Cost_Delta),
            
            CFS_STATE_Split_Mean = rmean(CFS_STATE),
            VB_STATE_Mean_Delta = rmean(VB_comp_y_STATE),
            VB_STATE_Cost = rsum(VB_STATE_Cost_Delta),
            PP_STATE_Mean_Delta = rmean(PP_curr_y_STATE),
            PP_STATE_Cost = rsum(PP_curr_y_STATE_Cost_Delta),
            Total_Cost = rsum(Vac_Order_Cost_Tl)
  )
                                       }
summary_report_age <- bind_rows(sum_rep_fun(cfs_comp, Age_Group),
                                sum_rep_fun(cfs_comp, Group_Col) %>%
                                  rename(Age_Group = Group_Col))

#creates a summary grouped by the data source
summary_report_data_source <- sum_rep_fun(cfs_comp, PP_curr_y_Data_Source)

#creates a summary report of impact of unknown data
un_rep_fun <-  function(data, group_col) {
  data  %>%  
    group_by_(.dots = lazyeval::lazy(group_col)) %>%
    summarise( No_Providers = length(Provider_PIN),
               Unknown_Dose_Tl = rsum(VB_comp_y_Unknown_Doses),
               Unknown_Pct =     rmean(VB_comp_y_Unknown_Pct))
}

summary_report_unknown <- bind_rows(un_rep_fun(cfs_comp, Age_Group),
                                    un_rep_fun(cfs_comp, Group_Col) %>%
                                      rename(Age_Group = Group_Col))

#creates a budget summary
bud_rep_fun <- function(data, group_col) {
  data  %>%  
    group_by_(.dots = lazyeval::lazy(group_col)) %>%
    summarise(No_Providers = length(Provider_PIN),
            Total_Cost = rsum(Vac_Order_Cost_Tl),
            CFS_STATE_Cost = rsum(Vac_Order_Cost_Tl*CFS_STATE),
            VB_STATE_Adj_Cost = rsum(Vac_Order_Cost_Tl*CFS_STATE)+rsum(VB_STATE_Cost_Delta),
            PP_STATE_Adj_Cost = rsum(Vac_Order_Cost_Tl*CFS_STATE)+rsum(PP_curr_y_STATE_Cost_Delta),
            CFS_VFC_Cost = rsum(Vac_Order_Cost_Tl*CFS_VFC),
            VB_VFC_Adj_Cost = rsum(Vac_Order_Cost_Tl*CFS_VFC)+rsum(VB_VFC_Cost_Delta),
            PP_VFC_Adj_Cost = rsum(Vac_Order_Cost_Tl*CFS_VFC)+rsum(PP_curr_y_VFC_Cost_Delta) )
}

summary_report_budget <- bind_rows(bud_rep_fun(cfs_comp, Age_Group),
                                   bud_rep_fun(cfs_comp, Group_Col) %>%
                                     rename(Age_Group = Group_Col))

#creates a new object to reorganize data to fit into a bar graph
budget_data_graph <- summary_report_budget %>%
  filter(Age_Group == "ANY") %>%
  select(-Total_Cost, -No_Providers) %>%
  gather(Data, Cost, CFS_STATE_Cost:PP_VFC_Adj_Cost) %>% 
  separate(Data, into = c("Data", "Source", "a", "b"), sep ="_") %>%
  select(-c(a,b))%>%
  group_by(Data, Source)%>%
  summarise(Cost= sum(Cost))

summary_report_confPA <- bind_rows(bud_rep_fun(cfs_comp, Age_Group),
                                   bud_rep_fun(cfs_comp, Group_Col) %>%
                                     rename(Age_Group = Group_Col))

#**************************************************************************************************************
##########    GRAPHS DATA ####
#**************************************************************************************************************


#creates graphs to summarize and visualize data

plotfun <- function(data, fil, mn) {
  ggplot(data, aes(x = VB_comp_y_STATE, y = PP_curr_y_STATE, 
         size= Tl_Vac_Admin, fill = fil)) +
  geom_point(shape = 21, alpha = 0.6)  +
  scale_fill_brewer(palette = "Spectral")  +
  scale_size_continuous(range = c(1, 8)) +
  labs(x = "VB Split Delta", y = "PP Split Delta") +
  guides(fill=guide_legend(title="Data Source", override.aes = list(size = 10)), size= guide_legend(title= "Total Vaccines Administered")) +
  annotate("rect", xmin=0, xmax= 1, ymin= 0, ymax = 1, colour = "orange", alpha = .05, fill= "orange")+
  annotate("rect", xmin=0, xmax= -1, ymin= 0, ymax = -1, colour = "blue", alpha =  .05, fill= "blue")+
  annotate("text", x=0.3, y=0.9, label="STATE")+
  annotate("text", x=-0.3, y=-0.9, label="VFC")+
  annotate("pointrange", x = summary_report_age$VB_STATE_Mean_Delta[mn], 
                        y = summary_report_age$PP_STATE_Mean_Delta[mn],
                        ymin = summary_report_age$PP_STATE_Mean_Delta[mn],
                        ymax = summary_report_age$PP_STATE_Mean_Delta[mn],
                        colour = "yellow", size = 1.5)+
  annotate("text", x = summary_report_age$VB_STATE_Mean_Delta[mn], 
                   y = summary_report_age$PP_STATE_Mean_Delta[mn], label = "mu", parse = T) +
  labs(caption = paste0(strwrap(paste("Data based on the difference in the current fund split and the calculated splits between Vaccine Breakdown data from ", 
                                       comp_year, " and Practice Profile data by the ", round(summary_report_age$No_Providers[3]/2, 0),
                                       " Providers who have submited their Practice Profiles in ", current_year, sep = ""), 250), 
                        sep="", collapse="n"))
}    

#graph of vaccine breakdown to practice profile of ped only vaccines
scatter_any <- plotfun(cfs_comp, cfs_comp$PP_curr_y_Data_Source, 3) + 
  ggtitle("Vaccine Breakdown to Practice Profile Delta by Provider Size")

scatter_ped <- plotfun(cfs_comp_PED, cfs_comp_PED$PP_curr_y_Data_Source, 1) + 
  ggtitle("Pediatric Vaccine Breakdown to Practice Profile Delta by Provider Size")

#graph of vaccine breakdown to practice profile of ado only vaccines
scatter_ado <- plotfun(cfs_comp_ADO, cfs_comp_ADO$PP_curr_y_Data_Source, 2) +
  ggtitle("Adolescent Vaccine Breakdown to Practice Profile Delta by Provider Size")

#graph of providers who listed billing as their data source for the practice profile
bill <- filter(cfs_comp, PP_curr_y_Data_Source == "BILLING")
scatter_bill <- ggplot(bill, aes(x = VB_comp_y_STATE, y = PP_curr_y_STATE, 
                                 size= Tl_Vac_Admin, fill = PP_curr_y_Data_Source)) +
  geom_point(shape = 21) +
  scale_fill_manual(values = c("#d7191c")) +
  geom_smooth(method = lm, se = F) +
  labs(x = "VB Split Delta", y = "PP Split Delta") +
  guides(fill=guide_legend(title="Data Source"), size= guide_legend(title= "Total Vaccines Administered")) +
  annotate("rect", xmin=0, xmax= 1, ymin= 0, ymax = 1, colour = "orange", alpha = .05, fill= "orange")+
  annotate("rect", xmin=0, xmax= -1, ymin= 0, ymax = -1, colour = "blue", alpha =  .05, fill= "blue")+
  annotate("text", x=0.3, y=0.9, label="STATE")+
  annotate("text", x=-0.3, y=-0.9, label="VFC")+
  annotate("pointrange", x = summary_report_data_source$VB_STATE_Mean_Delta[1], 
           y = summary_report_data_source$PP_STATE_Mean_Delta[1],
           ymin = summary_report_data_source$PP_STATE_Mean_Delta[1],
           ymax = summary_report_data_source$PP_STATE_Mean_Delta[1],
           colour = "yellow", size = 1.5)+
  annotate("text", x = summary_report_data_source$VB_STATE_Mean_Delta[1], 
           y = summary_report_data_source$PP_STATE_Mean_Delta[1], label = "mu", parse = T)+
  labs(caption = paste("Data based on the difference in the current fund split and the calculated splits between Vaccine Breakdown data from ", 
                       comp_year, " and Practice Profile data by the ", round(summary_report_data_source$No_Providers[1]/2, 0),
                       " Providers who have submited their Practice Profiles and reported Billing as Data Source in ", current_year, sep = "")) +
  ggtitle("Vaccine Breakdown to Practice Profile Delta by Billing Data")
plot(scatter_bill)
bill_lm <- lm(formula = PP_curr_y_STATE ~ VB_comp_y_STATE, data = bill)

# #graph of vaccine breakdown to practice profile for category both ped and ado vaccines
# scatter_combo<- plotfun(cfs_comp, cfs_comp$Age_Group, 3) +
#   guides(fill=guide_legend(title="Vaccine Type"))

#graphs confidence in vaccine breakdown graphing percentage of unknown vfc status vaccines administered and percentage of vaccine administer to ordered
vbconf <- ggplot(filter(cfs_comp, Doses_Admin_v_Order < 2), 
                 aes(x = Doses_Admin_v_Order, y = VB_comp_y_Unknown_Pct)) + 
  stat_binhex(colour="black")+
  scale_fill_gradient(low= "#045a8d", high = "#bd0026")+
  scale_x_continuous(breaks = seq(0, 2, 0.25)) +
  annotate("rect", xmin = Admin_Order_Lower , xmax= Admin_Order_Upper, ymin= 0, 
           ymax = unknown_pct, colour = "white", alpha = .05, fill= "red")+
  labs(x= "Percentage of Doses Admin to Ordered", y = "Percentage of Unknown Doses") +
  ggtitle("Vaccine Breakdown Reliability Test")+
  labs(caption = paste("Data based on percentage of doses administered and the percentage of vaccines with unknown insurance status in the vaccine breakdown", 
                       comp_year, " by the ", round(summary_report_age$No_Providers[3]/2, 0),
                       " Providers who have submited their Practice Profiles in ", current_year, sep = ""))

confPA <- cfs_comp %>% 
  filter(Doses_Admin_v_Order >= Admin_Order_Lower,
         Doses_Admin_v_Order <= Admin_Order_Upper) %>%
  filter(VB_comp_y_Unknown_Pct <= unknown_pct) #%>%
#  filter(Tl_Vac_Admin > mean(cfs_comp$Tl_Doses_Ordered, na.rm = T))
nrow(confPA)
confPA_lm <- lm(formula = PP_curr_y_STATE ~ VB_comp_y_STATE, data = confPA)

  #potentially filter for billing source data only 
scatter_confPA <- ggplot(confPA, aes(x = VB_comp_y_STATE, y = PP_curr_y_STATE))+                
  geom_point()+
  geom_smooth(method = lm)

PP_compare <- ggplot(cfs_comp, aes(x = PP_comp_y_STATE, y = PP_curr_y_STATE))+
  geom_point()

#calculates changes in budget from delta's #MAKE INTO A FUNCTION TO BE REPEATED FOR OTHER METHODS
budget_fun <- function(source) {
  ggplot(source, aes(x = Source, y = Cost, fill = Data)) +
    geom_bar(stat="identity", position = position_dodge(), colour = "black", width = 0.8) +
    geom_text(aes(label = dollar(Cost)), vjust = 1.5, hjust = 0.5, colour = "white", position = position_dodge(.9), 
              size = 4) +
    scale_y_continuous(name="Cost", labels = dollar) +
    scale_fill_manual(values = c("#d7191c", "#abdda4", "#2b83ba")) +
    ggtitle("Potential Budget Impact of Applying Fund Split Changes")+
    labs(caption = paste("(Budgets based on total cost of vaccines ordered in ", comp_year, " by the ", 
                         round(summary_report_age$No_Providers[3]/2, 0),
                       " Providers who have submited practice profiles in ", current_year, ")",sep = ""))+
    annotate("segment", x = 0.5, xend = 1.5, y = state_limit, yend= state_limit, colour = "red", size = 2) +
    annotate("text", x = 0.6, y = (state_limit+2000000), label = "LIMIT", colour = "red") +
    annotate("segment", x = 1.55, xend = 2.5, y = vfc_limit, yend = vfc_limit, colour = "red", size = 2)+
    annotate("text", x = 1.65, y = (vfc_limit+2000000), label = "LIMIT", colour = "red") +
    theme(plot.title = element_text(face= "bold", size = 22))+
    theme(axis.title = element_text(face= "bold", size = 16))+
    theme(axis.text.x = element_text(face= "bold", size = 12))+
    theme(axis.text.y = element_text(face= "bold", size = 12, angle = 45))+
    theme(legend.text = element_text(size = 12))+
    theme(legend.title = element_text(size = 16))
}
cfs_budgetbar <- budget_fun(budget_data_graph)

#**************************************************************************************************************
###########   Creates NEW FUND SPLITS and their budget implications####
#**************************************************************************************************************
#METHOD 1: New Fund Split1 (NFS1) PP where PP is equal to VB within 0.05, 
#and changes the rest of the providers to the PES
#NFS2 changes to PP where the VB is greater than 70% admin to order and less than 5% unknown Vac
nfs <- cfs_comp %>% 
  transmute( 
    Provider_PIN = Provider_PIN,
    Age_Group = Age_Group,
    CFS_STATE = CFS_STATE,
    NFS1_STATE = if_else(
      abs(PP_curr_y_STATE-VB_comp_y_STATE) <= 0.05,
      round(PP_curr_y_STATE+CFS_STATE, 1),
      PES_STATE),
    NFS1_STATE_Cost_Delta = Vac_Order_Cost_Tl*(NFS1_STATE-CFS_STATE),
    NFS2_STATE = if_else(
      Doses_Admin_v_Order >= .70 &
        Doses_Admin_v_Order <=1 &
        VB_comp_y_Unknown_Pct <= .05,
      round(VB_comp_y_STATE+CFS_STATE, 1),
      PES_STATE),
    NFS2_STATE_Cost_Delta = Vac_Order_Cost_Tl*(NFS2_STATE-CFS_STATE),
    CFS_VFC = CFS_VFC,
    NFS1_VFC = if_else(
      abs(PP_curr_y_VFC-VB_comp_y_VFC) <= 0.05,
      round(PP_curr_y_VFC+CFS_VFC, 1),
      PES_VFC),
    NFS1_VFC_Cost_Delta = Vac_Order_Cost_Tl*(NFS1_VFC-CFS_VFC),
    NFS2_VFC = if_else(
      Doses_Admin_v_Order >= .70 &
      Doses_Admin_v_Order <=1 &
      VB_comp_y_Unknown_Pct <= .05,
      round(VB_comp_y_VFC+CFS_VFC, 1),
      PES_VFC),
    NFS2_VFC_Cost_Delta = Vac_Order_Cost_Tl*(NFS2_VFC-CFS_VFC),
    Vac_Order_Cost_Tl = Vac_Order_Cost_Tl,
    Group_Col = Group_Col
  )

nfs_bud_rep_fun <- function(data, group_col) {
  data  %>%  
    group_by_(.dots = lazyeval::lazy(group_col)) %>%
    summarise(No_Providers = length(Provider_PIN),
              Total_Cost = rsum(Vac_Order_Cost_Tl),
              NFS1_STATE_Mean_Delta = rmean(NFS1_STATE-CFS_STATE),
              NFS1_STATE_Cost = rsum(Vac_Order_Cost_Tl*NFS1_STATE),
              NFS2_STATE_Mean_Delta = rmean(NFS2_STATE-CFS_STATE),
              NFS2_STATE_Cost = rsum(Vac_Order_Cost_Tl*NFS2_STATE),
              CFS_STATE_Cost = rsum(Vac_Order_Cost_Tl*CFS_STATE),
              NFS1_VFC_Mean_Delta = rmean(NFS1_VFC-CFS_VFC),
              NFS1_VFC_Cost = rsum(Vac_Order_Cost_Tl*NFS1_VFC),
              NFS2_VFC_Cost = rsum(Vac_Order_Cost_Tl*NFS2_VFC),
              NFS2_VFC_Mean_Delta = rmean(NFS2_VFC-CFS_VFC),
              CFS_VFC_Cost = rsum(Vac_Order_Cost_Tl*CFS_VFC)
              )
}
nfs_summary <- bind_rows(nfs_bud_rep_fun(nfs, Age_Group),
                 nfs_bud_rep_fun(nfs, Group_Col) %>%
                   rename(Age_Group = Group_Col))

nfs_bud_data_graph <- nfs_summary %>%
  filter(Age_Group == "ANY") %>%
  select(-c(Total_Cost, No_Providers, NFS1_STATE_Mean_Delta, NFS2_STATE_Mean_Delta,
         NFS1_VFC_Mean_Delta, NFS2_VFC_Mean_Delta, Age_Group)) %>%
  gather(Data, Cost, NFS1_STATE_Cost:CFS_VFC_Cost) %>% 
  separate(Data, into = c("Data", "Source", "a", "b"), sep ="_") %>%
  select(-c(a,b))%>%
  group_by(Data, Source)%>%
  summarise(Cost= sum(Cost))

nfs_budgetbar <- budget_fun(nfs_bud_data_graph)

#METHOD 2 
nfs2 <- cfs_comp %>% 
  transmute( 
    Provider_PIN = Provider_PIN,
    Age_Group = Age_Group,
    CFS_STATE = CFS_STATE,
    NFS1_STATE = if_else(
      abs(PP_curr_y_STATE-VB_comp_y_STATE) <= 0.05,
      round(PP_curr_y_STATE+CFS_STATE, 1),
      CFS_STATE),
    NFS1_STATE_Cost_Delta = Vac_Order_Cost_Tl*(NFS1_STATE-CFS_STATE),
    NFS2_STATE = if_else(
      Doses_Admin_v_Order >= .70 &
        Doses_Admin_v_Order <=1 &
        VB_comp_y_Unknown_Pct <= .05,
      round(PP_curr_y_STATE+CFS_STATE, 1),
      CFS_STATE),
    NFS2_STATE_Cost_Delta = Vac_Order_Cost_Tl*(NFS2_STATE-CFS_STATE),
    CFS_VFC = CFS_VFC,
    NFS1_VFC = if_else(
      abs(PP_curr_y_VFC-VB_comp_y_VFC) <= 0.05,
      round(PP_curr_y_VFC+CFS_VFC, 1),
      CFS_VFC),
    NFS1_VFC_Cost_Delta = Vac_Order_Cost_Tl*(NFS1_VFC-CFS_VFC),
    NFS2_VFC = if_else(
      Doses_Admin_v_Order >= .70 &
        Doses_Admin_v_Order <=1 &
        VB_comp_y_Unknown_Pct <= .05,
      round(PP_curr_y_VFC+CFS_VFC, 1),
      CFS_VFC),
    NFS2_VFC_Cost_Delta = Vac_Order_Cost_Tl*(NFS2_VFC-CFS_VFC),
    Vac_Order_Cost_Tl = Vac_Order_Cost_Tl,
    Group_Col = Group_Col
  )

nfs2_summary <- bind_rows(nfs_bud_rep_fun(nfs, Age_Group),
                         nfs_bud_rep_fun(nfs, Group_Col) %>%
                           rename(Age_Group = Group_Col))

#**************************************************************************************************************
############  REFORMATS Data for Export####
#**************************************************************************************************************

#renames cfs_comp to rename columns too the current and comparison year
cfs_comp_final <- cfs_comp

#converts curr_y and comp_y to numeric years
names(cfs_comp_final) <- gsub("curr_y", current_year, names(cfs_comp_final))
names(cfs_comp_final) <- gsub("comp_y", comp_year, names(cfs_comp_final))
names(cfs_read) <- gsub("curr_y", current_year, names(cfs_read))
names(cfs_read) <- gsub("comp_y", comp_year, names(cfs_read))


#**************************************************************************************************************
############# EXPORT Data####
#**************************************************************************************************************


#creates a new folder for data export based off of the system date
if(file.exists(data_exp)){
   setwd(file.path(dir, data_exp))
} else {
  dir.create(file.path(dir, data_exp))
  setwd(file.path(dir, data_exp))
}

#creates file name and location for xlsx data export
filename1 <- paste(dir, data_exp, "/", code_version, " Current Fund Split Validation", Sys.Date(), ".xlsx", sep = "")

#function to write all desired objects to one workbook
save.xlsx <- function (file, ...)
{
  objects <- list(...)
  fargs <- as.list(match.call(expand.dots = TRUE))
  objnames <- as.character(fargs)[-c(1, 2)]
  nobjects <- length(objects)
  for (i in 1:nobjects) {
    if (i == 1)
      write.xlsx(objects[[i]], file, sheetName = objnames[i], showNA = F)
    else write.xlsx(objects[[i]], file, sheetName = objnames[i], showNA = F,
                    append = TRUE)
  }
  print(paste("Workbook", file, "has", nobjects, "worksheets."))
}
#first argument is the file name and path the rest of the arguments are the desired objects to be written into sheets
save.xlsx(filename1, summary_report_budget, summary_report_data_source, summary_report_age, summary_report_unknown , cfs_comp_final, cfs_read)

#writes graphs into one file
plots <- list(scatter_ped, scatter_ado, scatter_any, scatter_bill, vbconf, cfs_budgetbar, nfs_budgetbar)
pdf(paste(code_version, "validation graphs ",Sys.Date(), ".pdf"), onefile = T, 
    width = 11, height = 8.5)
plots
dev.off()
