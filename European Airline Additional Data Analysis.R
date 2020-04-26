#in this file we ran analysis to gain insights to get our final model, since there is
#a lot of insights not directly used, we decided to keep it in a separate file


library(readxl)
data <- read_excel("European Airline Case Spreadsheet Supplement.xls", sheet = 2)
View(data)

# Call packages
library(ggplot2) # use to plot
library(Hmisc) # use to describe stats
library(RColorBrewer) # use the color schemes


#-----------------------------------------------------------------------
# Creating PROFIT column
data$profit <- data[21]-data[22]
data$profit 

#-----------------------------------------------------------------------
# Creating CPA column

data$cpa <- data$`Total Cost`/data$`Total Volume of Bookings`
data$cpa

#-----------------------------------------------------------------------
View(data)
#-----------------------------------------------------------------------


# Data Rub
#-----------------------------------------------------------------------
# Categor.by Publiser
yahoo_us_data <- data[which(data$`Publisher Name` == 'Yahoo - US'),]
google_us_data <- data[which(data$`Publisher Name` == 'Google - US'),]
msn_us_data <- data[which(data$`Publisher Name` == 'MSN - US'),]
overture_us_data <- data[which(data$`Publisher Name` == 'Overture - US'),]
google_global_data <- data[which(data$`Publisher Name` == 'Google - Global'),]
msn_global_data <- data[which(data$`Publisher Name` == 'MSN - Global'),]
overture_global_data <- data[which(data$`Publisher Name` == 'Overture - Global'),]


#-----------------------------------------------------------------------
# Categorizing column F - Campaign
airfrance_french <- data[which(data$Campaign == "Air France Brand & French Destinations"),]
airfrance_branded <- data[which(data$Campaign == "Air France Branded"),]
airfrance_global <- data[which(data$Campaign == "Air France Global Campaign"),]
business_class <- data[which(data$Campaign == "Business Class"),]
french_destinations <- data[which(data$Campaign == "French Destinations"),]
general_terms <- data[which(data$Campaign == "General Terms"),]
geo_atlanta <- data[which(data$Campaign == "Geo Targeted Atlanta"),]
geo_boston <- data[which(data$Campaign == "Geo Targeted Boston"),]
geo_chicago <- data[which(data$Campaign == "Geo Targeted Chicago"),]
geo_cincinnati <- data[which(data$Campaign == "Geo Targeted Cincinnati"),]
geo_dc <- data[which(data$Campaign == "Geo Targeted DC"),]
geo_detroit <- data[which(data$Campaign == "Geo Targeted Detroit"),]
geo_houston <- data[which(data$Campaign == "Geo Targeted Houston"),]
geo_losangeles <- data[which(data$Campaign == "Geo Targeted Los Angeles"),]
geo_miami <- data[which(data$Campaign == "Geo Targeted Miami"),]
geo_newyork <- data[which(data$Campaign == "Geo Targeted New York"),]
geo_philadelphia <- data[which(data$Campaign == "Geo Targeted Philadelphia"),]
geo_sanfrancisco <- data[which(data$Campaign == "Geo Targeted San Francisco"),]
geo_seattle <- data[which(data$Campaign == "Geo Targeted Seattle"),]
google_yearlong <- data[which(data$Campaign == "Google_Yearlong 2006"),]
outside_westerneurope <- data[which(data$Campaign == "Outside Western Europe"),]
paris <- data[which(data$Campaign == "Paris & France Terms"),]
unassigned <- data[which(data$Campaign == "Unassigned"),]
western_europe <- data[which(data$Campaign == "Western Europe Destinations"),]


#-----------------------------------------------------------------------

# Skiping elements = 0
yahoo_us_data_nozero <- yahoo_us_data[which(yahoo_us_data$`Total Volume of Bookings` > 0),]
google_us_data_nozero <- google_us_data[which(google_us_data$`Total Volume of Bookings` > 0),]
msn_us_data_nozero <- msn_us_data[which(msn_us_data$`Total Volume of Bookings` > 0),]
overture_us_data_nozero <- overture_us_data[which(overture_us_data$`Total Volume of Bookings` > 0),]
google_global_data_nozero <- google_global_data[which(google_global_data$`Total Volume of Bookings` > 0),]
msn_global_data_nozero <- msn_global_data[which(msn_global_data$`Total Volume of Bookings` > 0),]
overture_global_data_nozero <- overture_global_data[which(overture_global_data$`Total Volume of Bookings` > 0),]


#-----------------------------------------------------------------------
# Skiping elements = 0 with Live Campaing
yahoo_us_data_nozero_live <- yahoo_us_data_nozero[which(yahoo_us_data_nozero$Status == 'Live'),]
google_us_data_nozero_live <- google_us_data_nozero[which(google_us_data_nozero$Status == 'Live'),]
msn_us_data_nozero_live <- msn_us_data_nozero[which(msn_us_data_nozero$Status == 'Live'),]
overture_us_data_nozero_live <- overture_us_data_nozero[which(overture_us_data_nozero$Status == 'Live'),]
google_global_data_nozero_live <- google_global_data_nozero[which(google_global_data_nozero$Status == 'Live'),]
msn_global_data_nozero_live <- msn_global_data_nozero[which(msn_global_data_nozero$Status == 'Live'),]
overture_global_data_nozero_live <- overture_global_data_nozero[which(overture_global_data_nozero$Status == 'Live'),]


#-----------------------------------------------------------------------
# Looking up missing values
colSums(is.na(data))
table(is.na(data))
summary(data)
is.null(data)


#-----------------------------------------------------------------------
# Descriptive analysis 
summary(data[12:25])

summary(google_global_data[12:25])
summary(google_us_data[12:25])
summary(msn_global_data[12:25])
summary(msn_us_data[12:25])
summary(overture_global_data[12:25])
summary(overture_us_data[12:25])
summary(yahoo_us_data[12:25])


#-----------------------------------------------------------------------
#-----------------------------------------------------------------------
# KEY METRICS ANALYSIS #


#-----------------------------------------------------------------------
# Pubslisher and CTR comparison 

mean(google_global_data$`Engine Click Thru %`)     # Answer: 8.994485
mean(google_us_data$`Engine Click Thru %`)         # Answer: 15.08975
mean(msn_global_data$`Engine Click Thru %`)        # Answer: 7.335605
mean(msn_us_data$`Engine Click Thru %`)            # Answer: 8.263521
mean(overture_global_data$`Engine Click Thru %`)   # Answer: 3.547863
mean(overture_us_data$`Engine Click Thru %`)       # Answer: 2.672846
mean(yahoo_us_data$`Engine Click Thru %`)          # Answer: 16.05902
# OUTCOME: most of the customers are clicking on AF ads through Google - US and Yahoo - US 


#-----------------------------------------------------------------------
# Publisher and CPC comparison

mean(google_global_data$`Avg. Cost per Click`)     # Answer: 2.224959
mean(google_us_data$`Avg. Cost per Click`)         # Answer: 2.383942
mean(msn_global_data$`Avg. Cost per Click`)        # Answer: 2.152998
mean(msn_us_data$`Avg. Cost per Click`)            # Answer: 2.86747
mean(overture_global_data$`Avg. Cost per Click`)   # Answer: 0.8047588
mean(overture_us_data$`Avg. Cost per Click`)       # Answer: 0.7639206
mean(yahoo_us_data$`Avg. Cost per Click`)          # Answer: 1.998876
# OUTCOME: CPC for Yahoo - US is relatively cheap given the fact that most customers ale clicking on ads through Yahoo - US 


#-----------------------------------------------------------------------
# Publisher and Amount (revenue) comparison 
Total_Revenue <- sum(yahoo_us_data$Amount, google_us_data$Amount, msn_us_data$Amount, overture_us_data$Amount, google_global_data$Amount, msn_global_data$Amount, overture_global_data$Amount)
# Answer: $4661913

G_Global_Rev <- sum(google_global_data[21])     # Answer: 929,549.8
G_US_Rev <- sum(google_us_data[21])         # Answer: 1,745,482
MSN_Global_Rev <- sum(msn_global_data[21])        # Answer: 145,524.2
MSN_US_Rev <- sum(msn_us_data[21])            # Answer: 181,549.8
Over_Global_Rev <- sum(overture_global_data[21])   # Answer: 430,084.7
Over_US_Rev <- sum(overture_us_data[21])       # Answer: 347,433.2
Yahoo_US_Rev <- sum(yahoo_us_data[21])          # Answer: 882,289

# OUTCOME: Yahoo - US earns relatively a small amount compared to Google - US. 


#-----------------------------------------------------------------------
# Publisher and Total Cost comparison 
Total_Cost <- sum(yahoo_us_data$`Total Cost`, google_us_data$`Total Cost`, msn_us_data$`Total Cost`, overture_us_data$`Total Cost`, google_global_data$`Total Cost`, msn_global_data$`Total Cost`, overture_global_data$`Total Cost`)
# Answer: $755315.9

G_Global_Cost <- sum(google_global_data[22])     # Answer: 120,946.7
G_US_Cost <- sum(google_us_data[22])         # Answer: 353,640.6
MSN_Global_Cost <- sum(msn_global_data[22])        # Answer: 12,160.36
MSN_US_Cost <- sum(msn_us_data[22])            # Answer: 16,098.49
Over_Global_Cost <- sum(overture_global_data[22])   # Answer: 64,295.86
Over_US_Cost <- sum(overture_us_data[22])       # Answer: 141,976.1
Yahoo_US_Cost <- sum(yahoo_us_data[22])          # Answer: 46,197.82


#-----------------------------------------------------------------------
# Publisher and Profit comparison
sum(google_global_data_nozero[25] + google_us_data_nozero[25] + msn_global_data_nozero[25] + msn_us_data_nozero[25] + overture_global_data_nozero[25] + overture_us_data_nozero[25] + yahoo_us_data_nozero[25])

sum(google_global_data_nozero[25])     # Answer: 27862.31 // 808,603.1
sum(google_us_data_nozero[25])         # Answer: 52017.07 // 1,391,841
sum(msn_global_data_nozero[25])        # Answer: 1105.416// 133,363.9
sum(msn_us_data_nozero[25])            # Answer: 505.456 // 165,451.3
sum(overture_global_data_nozero[25])   # Answer: 9107.19 // 365,788.8
sum(overture_us_data_nozero[25])       # Answer: 28859.71 // 205,457.2
sum(yahoo_us_data_nozero[25])          # Answer: 5052.422 // 836,091.1


#-----------------------------------------------------------------------
# Proportion of total cost on revenue by Publisher: Total Cost / Revenue
(sum(google_global_data[22])/sum(google_global_data[21]))*100       # Answer: 13.01132
(sum(google_us_data[22])/sum(google_us_data[21]))*100               # Answer: 20.26034
(sum(msn_global_data[22])/sum(msn_global_data[21]))*100             # Answer: 8.356245
(sum(msn_us_data[22])/sum(msn_us_data[21]))*100                     # Answer: 8.867257
(sum(overture_global_data[22])/sum(overture_global_data[21]))*100   # Answer: 14.94958
(sum(overture_us_data[22])/sum(overture_us_data[21]))*100           # Answer: 40.86427
(sum(yahoo_us_data[22])/sum(yahoo_us_data[21]))*100                 # Answer: 5.236133


#-----------------------------------------------------------------------
# Profit Margin by Publisher
(sum(google_global_data[25])/sum(google_global_data[21]))*100       # Answer: 86.98868
(sum(google_us_data[25])/sum(google_us_data[21]))*100               # Answer: 79.73966
(sum(msn_global_data[25])/sum(msn_global_data[21]))*100             # Answer: 91.64376
(sum(msn_us_data[25])/sum(msn_us_data[21]))*100                     # Answer: 91.13274
(sum(overture_global_data[25])/sum(overture_global_data[21]))*100   # Answer: 85.05042
(sum(overture_us_data[25])/sum(overture_us_data[21]))*100           # Answer: 59.13573
(sum(yahoo_us_data[25])/sum(yahoo_us_data[21]))*100                 # Answer: 94.76387


#-----------------------------------------------------------------------
# Campaign and Amount (revenue) comparison 
sum(airfrance_french$Amount)        # Answer: 788,641.9
sum(airfrance_branded$Amount)       # Answer: 2,349,871
sum(airfrance_global$Amount)        # Answer: 467,982
sum(business_class$Amount)          # Answer: 144.5
sum(french_destinations$Amount)     # Answer: 6,223.7
sum(general_terms$Amount)           # Answer: 1,977.95
sum(geo_atlanta$Amount)             # Answer: 170
sum(geo_boston$Amount)              # Answer: 2,878.95
sum(geo_chicago$Amount)             # Answer: 7,144.25
sum(geo_cincinnati$Amount)          # Answer: 0
sum(geo_dc$Amount)                  # Answer: 5,191.8
sum(geo_detroit$Amount)             # Answer: 923.95
sum(geo_houston$Amount)             # Answer: 7,065.2
sum(geo_losangeles$Amount)          # Answer: 2,183.65
sum(geo_miami$Amount)               # Answer: 470.05
sum(geo_newyork$Amount)             # Answer: 35,580.15
sum(geo_philadelphia$Amount)        # Answer: 434.35
sum(geo_sanfrancisco$Amount)        # Answer: 3,822.45
sum(geo_seattle$Amount)             # Answer: 2,817.75
sum(google_yearlong$Amount)         # Answer: 22,373.7
sum(outside_westerneurope$Amount)   # Answer: 0
sum(paris$Amount)                   # Answer: 136,393.5
sum(unassigned$Amount)              # Answer: 777,517.9
sum(western_europe$Amount)          # Answer: 42,103.9


#-----------------------------------------------------------------------
# Campaign and Profit comparison
sum(airfrance_french[25])        # Answer: 701,627.4
sum(airfrance_branded[25])       # Answer: 2,206,793
sum(airfrance_global[25])        # Answer: 405,922.5
sum(business_class[25])          # Answer: -3,124.55
sum(french_destinations[25])     # Answer: -2,208.037
sum(general_terms[25])           # Answer: 1,371.475
sum(geo_atlanta[25])             # Answer: -95.25
sum(geo_boston[25])              # Answer: 1,482.775
sum(geo_chicago[25])             # Answer: 5,562.575
sum(geo_cincinnati[25])          # Answer: -33.75
sum(geo_dc[25])                  # Answer: 3,920.438
sum(geo_detroit[25])             # Answer: 204.925
sum(geo_houston[25])             # Answer: 5,072.413
sum(geo_losangeles[25])          # Answer: 383.6625
sum(geo_miami[25])               # Answer: -168.475
sum(geo_newyork[25])             # Answer: 26,040.48
sum(geo_philadelphia[25])        # Answer: -311.625
sum(geo_sanfrancisco[25])        # Answer: 1,733.038
sum(geo_seattle[25])             # Answer: 1,643.5
sum(google_yearlong[25])         # Answer: -59,585.79
sum(outside_westerneurope[25])   # Answer: -597.8375
sum(paris[25])                   # Answer: 33,796.18
sum(unassigned[25])              # Answer: 571,246
sum(western_europe[25])          # Answer: 5,921.538


#-----------------------------------------------------------------------
# Campaign and profit comparison with "Live" campaigns only           
data_status_live <- data[which(data$Status == "Live"),]   


#-----------------------------------------------------------------------
# Categorizing column F - Campaign based on data_status_live 
airfrance_french_live <- data_status_live[which(data_status_live$Campaign == "Air France Brand & French Destinations"),]
airfrance_branded_live <- data_status_live[which(data_status_live$Campaign == "Air France Branded"),]
airfrance_global_live <- data_status_live[which(data_status_live$Campaign == "Air France Global Campaign"),]
business_class_live <- data_status_live[which(data_status_live$Campaign == "Business Class"),]
french_destinations_live <- data_status_live[which(data_status_live$Campaign == "French Destinations"),]
general_terms_live <- data_status_live[which(data_status_live$Campaign == "General Terms"),]
geo_atlanta_live <- data_status_live[which(data_status_live$Campaign == "Geo Targeted Atlanta"),]
geo_boston_live <- data_status_live[which(data_status_live$Campaign == "Geo Targeted Boston"),]
geo_chicago_live <- data_status_live[which(data_status_live$Campaign == "Geo Targeted Chicago"),]
geo_cincinnati_live <- data_status_live[which(data_status_live$Campaign == "Geo Targeted Cincinnati"),]
geo_dc_live <- data_status_live[which(data_status_live$Campaign == "Geo Targeted DC"),]
geo_detroit_live <- data_status_live[which(data_status_live$Campaign == "Geo Targeted Detroit"),]
geo_houston_live <- data_status_live[which(data_status_live$Campaign == "Geo Targeted Houston"),]
geo_losangeles_live <- data_status_live[which(data_status_live$Campaign == "Geo Targeted Los Angeles"),]
geo_miami_live <- data_status_live[which(data_status_live$Campaign == "Geo Targeted Miami"),]
geo_newyork_live <- data_status_live[which(data_status_live$Campaign == "Geo Targeted New York"),]
geo_philadelphia_live <- data_status_live[which(data_status_live$Campaign == "Geo Targeted Philadelphia"),]
geo_sanfrancisco_live <- data_status_live[which(data_status_live$Campaign == "Geo Targeted San Francisco"),]
geo_seattle_live <- data_status_live[which(data_status_live$Campaign == "Geo Targeted Seattle"),]
google_yearlong_live <- data_status_live[which(data_status_live$Campaign == "Google_Yearlong 2006"),]
outside_westerneurope_live <- data_status_live[which(data_status_live$Campaign == "Outside Western Europe"),]
paris_live <- data_status_live[which(data_status_live$Campaign == "Paris & France Terms"),]
unassigned_live <- data_status_live[which(data_status_live$Campaign == "Unassigned"),]
western_europe_live <- data_status_live[which(data_status_live$Campaign == "Western Europe Destinations"),]


#-----------------------------------------------------------------------
# Live Campaigns and CTR comparison
mean(airfrance_french_live$`Engine Click Thru %`)        # Answer: 7.533087
mean(airfrance_branded_live$`Engine Click Thru %`)       # Answer: 13.04553
mean(airfrance_global_live$`Engine Click Thru %`)        # Answer: NOT live
mean(business_class_live$`Engine Click Thru %`)          # Answer: 7.802088
mean(french_destinations_live$`Engine Click Thru %`)     # Answer: 15.34855
mean(general_terms_live$`Engine Click Thru %`)           # Answer: NOT live
mean(geo_atlanta_live$`Engine Click Thru %`)             # Answer: NOT live
mean(geo_boston_live$`Engine Click Thru %`)              # Answer: NOT live
mean(geo_chicago_live$`Engine Click Thru %`)             # Answer: 33.04265
mean(geo_cincinnati_live$`Engine Click Thru %`)          # Answer: NOT live
mean(geo_dc_live$`Engine Click Thru %`)                  # Answer: 18.50945
mean(geo_detroit_live$`Engine Click Thru %`)             # Answer: NOT live
mean(geo_houston_live$`Engine Click Thru %`)             # Answer: 6.946457
mean(geo_losangeles_live$`Engine Click Thru %`)          # Answer: 28.24408
mean(geo_miami_live$`Engine Click Thru %`)               # Answer: NOT live
mean(geo_newyork_live$`Engine Click Thru %`)             # Answer: 19.531
mean(geo_philadelphia_live$`Engine Click Thru %`)        # Answer: NOT live
mean(geo_sanfrancisco_live$`Engine Click Thru %`)        # Answer: 11.76356
mean(geo_seattle_live$`Engine Click Thru %`)             # Answer: 100
mean(google_yearlong_live$`Engine Click Thru %`)         # Answer: NOT live
mean(outside_westerneurope_live$`Engine Click Thru %`)   # Answer: NOT live
mean(paris_live$`Engine Click Thru %`)                   # Answer: 11.28021
mean(unassigned_live$`Engine Click Thru %`)              # Answer: 20.2381
mean(western_europe_live$`Engine Click Thru %`)          # Answer: 7.355819


#-----------------------------------------------------------------------
# Live Campaigns and CPC comparison 
mean(airfrance_french_live$`Avg. Cost per Click`)        # Answer: 2.789031
mean(airfrance_branded_live$`Avg. Cost per Click`)       # Answer: 0.859546
mean(airfrance_global_live$`Avg. Cost per Click`)        # Answer: NOT live
mean(business_class_live$`Avg. Cost per Click`)          # Answer: 4.020972
mean(french_destinations_live$`Avg. Cost per Click`)     # Answer: 1.600286
mean(general_terms_live$`Avg. Cost per Click`)           # Answer: NOT live
mean(geo_atlanta_live$`Avg. Cost per Click`)             # Answer: NOT live
mean(geo_boston_live$`Avg. Cost per Click`)              # Answer: NOT live
mean(geo_chicago_live$`Avg. Cost per Click`)             # Answer: 1.871077
mean(geo_cincinnati_live$`Avg. Cost per Click`)          # Answer: NOT live
mean(geo_dc_live$`Avg. Cost per Click`)                  # Answer: 1.538125
mean(geo_detroit_live$`Avg. Cost per Click`)             # Answer: NOT live
mean(geo_houston_live$`Avg. Cost per Click`)             # Answer: 1.1
mean(geo_losangeles_live$`Avg. Cost per Click`)          # Answer: 2.107729
mean(geo_miami_live$`Avg. Cost per Click`)               # Answer: NOT live
mean(geo_newyork_live$`Avg. Cost per Click`)             # Answer: 2.389065
mean(geo_philadelphia_live$`Avg. Cost per Click`)        # Answer: NOT live
mean(geo_sanfrancisco_live$`Avg. Cost per Click`)        # Answer: 2.416544
mean(geo_seattle_live$`Avg. Cost per Click`)             # Answer: 2.675
mean(google_yearlong_live$`Avg. Cost per Click`)         # Answer: NOT live
mean(outside_westerneurope_live$`Avg. Cost per Click`)   # Answer: NOT live
mean(paris_live$`Avg. Cost per Click`)                   # Answer: 1.664675
mean(unassigned_live$`Avg. Cost per Click`)              # Answer: 0.1609375
mean(western_europe_live$`Avg. Cost per Click`)          # Answer: 1.683122


#-----------------------------------------------------------------------
# Campaign and Amount (revenue) comparison 
sum(airfrance_french_live$Amount)        # Answer: 181,549.8
sum(airfrance_branded_live$Amount)       # Answer: 2,210,283
sum(airfrance_global_live$Amount)        # Answer: NOT live
sum(business_class_live$Amount)          # Answer: 144.5
sum(french_destinations_live$Amount)     # Answer: 5,552.2
sum(general_terms_live$Amount)           # Answer: NOT live
sum(geo_atlanta_live$Amount)             # Answer: NOT live
sum(geo_boston_live$Amount)              # Answer: NOT live
sum(geo_chicago_live$Amount)             # Answer: 4,403.85
sum(geo_cincinnati_live$Amount)          # Answer: NOT live
sum(geo_dc_live$Amount)                  # Answer: 0
sum(geo_detroit_live$Amount)             # Answer: NOT live
sum(geo_houston_live$Amount)             # Answer: 0
sum(geo_losangeles_live$Amount)          # Answer: 0
sum(geo_miami_live$Amount)               # Answer: NOT live
sum(geo_newyork_live$Amount)             # Answer: 33,439
sum(geo_philadelphia_live$Amount)        # Answer: NOT live
sum(geo_sanfrancisco_live$Amount)        # Answer: 0
sum(geo_seattle_live$Amount)             # Answer: 0
sum(google_yearlong_live$Amount)         # Answer: NOT live
sum(outside_westerneurope_live$Amount)   # Answer: NOT live
sum(paris_live$Amount)                   # Answer: 89,714.1
sum(unassigned_live$Amount)              # Answer: 0
sum(western_europe_live$Amount)          # Answer: 14,506.95


#-----------------------------------------------------------------------
# Live Campaigns and Total Cost comparison 
sum(airfrance_french_live$`Total Cost`)        # Answer: 15,966.91
sum(airfrance_branded_live$`Total Cost`)       # Answer: 100,231.2
sum(airfrance_global_live$`Total Cost`)        # Answer: NOT live
sum(business_class_live$`Total Cost`)          # Answer: 3,269.05
sum(french_destinations_live$`Total Cost`)     # Answer: 5,338.612
sum(general_terms_live$`Total Cost`)           # Answer: NOT live
sum(geo_atlanta_live$`Total Cost`)             # Answer: NOT live
sum(geo_boston_live$`Total Cost`)              # Answer: NOT live
sum(geo_chicago_live$`Total Cost`)             # Answer: 585.2
sum(geo_cincinnati_live$`Total Cost`)          # Answer: NOT live
sum(geo_dc_live$`Total Cost`)                  # Answer: 18.1875
sum(geo_detroit_live$`Total Cost`)             # Answer: NOT live
sum(geo_houston_live$`Total Cost`)             # Answer: 3.3
sum(geo_losangeles_live$`Total Cost`)          # Answer: 59.9
sum(geo_miami_live$`Total Cost`)               # Answer: NOT live
sum(geo_newyork_live$`Total Cost`)             # Answer: 6,958.962
sum(geo_philadelphia_live$`Total Cost`)        # Answer: NOT live
sum(geo_sanfrancisco_live$`Total Cost`)        # Answer: 55.55
sum(geo_seattle_live$`Total Cost`)             # Answer: 2.675
sum(google_yearlong_live$`Total Cost`)         # Answer: NOT live
sum(outside_westerneurope_live$`Total Cost`)   # Answer: NOT live
sum(paris_live$`Total Cost`)                   # Answer: 47,851.34
sum(unassigned_live$`Total Cost`)              # Answer: 0.9
sum(western_europe_live$`Total Cost`)          # Answer: 1,233.725


#-----------------------------------------------------------------------
# Live Campaigns and Profit comparison
sum(airfrance_french_live[25])        # Answer: 165,582.9
sum(airfrance_branded_live[25])       # Answer: 2,110,052
sum(airfrance_global_live[25])        # Answer: NOT live
sum(business_class_live[25])          # Answer: -3,124.55
sum(french_destinations_live[25])     # Answer: 213.5875
sum(general_terms_live[25])           # Answer: NOT live
sum(geo_atlanta_live[25])             # Answer: NOT live
sum(geo_boston_live[25])              # Answer: NOT live
sum(geo_chicago_live[25])             # Answer: 3,818.65
sum(geo_cincinnati_live[25])          # Answer: NOT live
sum(geo_dc_live[25])                  # Answer: -18.1875
sum(geo_detroit_live[25])             # Answer: NOT live
sum(geo_houston_live[25])             # Answer: -3.3
sum(geo_losangeles_live[25])          # Answer: -59.9
sum(geo_miami_live[25])               # Answer: NOT live
sum(geo_newyork_live[25])             # Answer: 26,480.04
sum(geo_philadelphia_live[25])        # Answer: NOT live
sum(geo_sanfrancisco_live[25])        # Answer: -55.55
sum(geo_seattle_live[25])             # Answer: -2.675
sum(google_yearlong_live[25])         # Answer: NOT live
sum(outside_westerneurope_live[25])   # Answer: NOT live
sum(paris_live[25])                   # Answer: 41,862.76
sum(unassigned_live[25])              # Answer: -0.9
sum(western_europe_live[25])          # Answer: 13,273.23

# OUTCOME: There are 14 live campaigns and half of them are not profitable! 

#-----------------------------------------------------------------------


#-----------------------------------------------------------------------
# Total CPA by Publisher
yahoo_us_data_nozero$CPA <- yahoo_us_data_nozero$`Total Cost` / yahoo_us_data_nozero$`Total Volume of Bookings`
summary(yahoo_us_data_nozero$CPA)

google_us_data_nozero$CPA <- google_us_data_nozero$`Total Cost` / google_us_data_nozero$`Total Volume of Bookings`
summary(google_us_data_nozero$CPA)

msn_us_data_nozero$CPA <- msn_us_data_nozero$`Total Cost` / msn_us_data_nozero$`Total Volume of Bookings`
summary(msn_us_data_nozero$CPA)

overture_us_data_nozero$CPA <- overture_us_data_nozero$`Total Cost` / overture_us_data_nozero$`Total Volume of Bookings`
summary(overture_us_data_nozero$CPA)

google_global_data_nozero$CPA <- google_global_data_nozero$`Total Cost` / google_global_data_nozero$`Total Volume of Bookings`
summary(google_global_data_nozero$CPA)

msn_global_data_nozero$CPA <- msn_global_data_nozero$`Total Cost` / msn_global_data_nozero$`Total Volume of Bookings`
summary(msn_global_data_nozero$CPA)

overture_global_data_nozero$CPA <- overture_global_data_nozero$`Total Cost` / overture_global_data_nozero$`Total Volume of Bookings`
summary(overture_global_data_nozero$CPA)


#-----------------------------------------------------------------------
# Average CPA per Publisher (effective) with "live" Campaign 
mean(yahoo_us_data_nozero_live$cpa) # Answer: $150.5049 invested on average by Air France to acquire one paying customer on a campaign or channel level
mean(google_us_data_nozero_live$cpa) # Answer: $208.6635 invested on average by Air France to acquire one paying customer on a campaign or channel level
mean(msn_us_data_nozero_live$cpa) # Answer: $56.16178 on average invested by Air France to acquire one paying customer on a campaign or channel level
mean(overture_us_data_nozero_live$cpa) # Answer: $0 on average invested by Air France to acquire one paying customer on a campaign or channel level
mean(google_global_data_nozero_live$cpa) # Answer: $0 on average invested by Air France to acquire one paying customer on a campaign or channel level
mean(msn_global_data_nozero_live$cpa) # Answer: $0 on average invested by Air France to acquire one paying customer on a campaign or channel level
mean(overture_global_data_nozero_live$cpa) # Answer: $0 on average invested by Air France to acquire one paying customer on a campaign or channel level


#-----------------------------------------------------------------------
# Total Cost Analysis by Publisher (effective)


total_cost_yahoo_us <- sum(yahoo_us_data_nozero$`Total Cost`) # Answer: $34912.21 invested per channel
total_cost_google_us <- sum(google_us_data_nozero$`Total Cost`) # Answer: $239386.1 invested per channel
total_cost_msn_us <- sum(msn_us_data_nozero$`Total Cost`) # Answer: $11508.94 invested per channel
total_cost_overture_us <- sum(overture_us_data_nozero$`Total Cost`) # Answer: $120605.9 invested per channel
total_cost_google_global <- sum(google_global_data_nozero$`Total Cost`) # Answer: $99418.2 invested per channel
total_cost_msn_global <- sum(msn_global_data_nozero$`Total Cost`) # Answer: $7467.812 invested per channel
total_cost_overture_global <- sum(overture_global_data_nozero$`Total Cost`) # Answer: $52272.74 invested per channel

sum(total_cost_yahoo_us, total_cost_google_us ,total_cost_msn_us, total_cost_overture_us, total_cost_google_global, total_cost_msn_global, total_cost_overture_global)
#result = 565571.9



#-----------------------------------------------------------------------
# Total Cost incurred by Pubisher per channel
sum(data$`Total Cost`) # Answer: $755315.9 total cost per channel

sum(yahoo_us_data$`Total Cost`) # Answer: $46197.82 total cost per channel
sum(google_us_data$`Total Cost`) # Answer: $35640.6 total cost per channel
sum(msn_us_data$`Total Cost`) # Answer: $16098.49 total cost per channel
sum(overture_us_data$`Total Cost`) # Answer: $14976.1  total cost per channel
sum(google_global_data$`Total Cost`) # Answer: $99418.2 total cost per channel
sum(msn_global_data$`Total Cost`) # Answer: $7467.812 total cost per channel
sum(overture_global_data$`Total Cost`) # Answer: $52272.74 total cost per channel


#-----------------------------------------------------------------------
# Total Revenue(Amount) by Pubisher per channel
Total_revenue <- sum(data$Amount) # Answer: $4661913 total cost per channel

yahoo_us_rev <- sum(yahoo_us_data$Amount) # Answer: $882289 total cost per channel
google_us_rev <- sum(google_us_data$Amount) # Answer: $1745482 total cost per channel
msn_us_rev <- sum(msn_us_data$Amount) # Answer: $181549.8 total cost per channel
overture_us_rev <- sum(overture_us_data$Amount) # Answer: $347433.2  total cost per channel
google_global_rev <- sum(google_global_data$Amount) # Answer: $929549.8 total cost per channel
sum(msn_global_data$Amount) # Answer: $145524.2 total cost per channel
sum(overture_global_data$Amount) # Answer: $430084.7 total cost per channel


#-----------------------------------------------------------------------
# Total Revenue(Amount) Analysis by Publisher (effective)
sum(yahoo_us_data_nozero$`Total Cost`, google_us_data_nozero$`Total Cost`, msn_us_data_nozero$`Total Cost`, overture_us_data_nozero$`Total Cost`, google_global_data_nozero$`Total Cost`, msn_global_data_nozero$`Total Cost`, overture_global_data_nozero$`Total Cost`)
# Answer: $565571.9 invested per channel

sum(yahoo_us_data_nozero$`Total Cost`) # Answer: $34912.21 invested per channel
sum(google_us_data_nozero$`Total Cost`) # Answer: $239386.1 invested per channel
sum(msn_us_data_nozero$`Total Cost`) # Answer: $115508.94 invested per channel
sum(overture_us_data_nozero$`Total Cost`) # Answer: $120605.9 invested per channel
sum(google_global_data_nozero$`Total Cost`) # Answer: $99418.2 invested per channel
sum(msn_global_data_nozero$`Total Cost`) # Answer: $7467.812 invested per channel
sum(overture_global_data_nozero$`Total Cost`) # Answer: $52272.74 invested per channel


#-----------------------------------------------------------------------
# Graphic Analysis:

# Bar plots for insights

# 1) Total Cost by Publisher (effective) vs # Total Cost incurred by Pubisher per channel
# Publisher and Amount (revenue) comparison 

# Total Amount (Revenue): $4661913

#google_global_data <- Answer: 929,549.8
#google_us_data <- Answer: 1,745,482
#msn_global_data <- Answer: 145,524.2
#msn_us_data <- Answer: 181,549.8
#overture_global_data <- Answer: 430,084.7
#overture_us_data <- Answer: 347,433.2
#yahoo_us_data <- Answer: 882,289

bar_revenue <- c(4661913, 929549.8, 1745482, 145524.2, 181549.8, 430084.7, 347433.2, 882289)
counts_bar_revenue <- bar_revenue

barplot(bar_revenue, horiz = FALSE,
names.arg = c("Total Revenue", "G. US Rev.", "G. Global Rev.", "MSN Global Rev.", "MSN US Rev.", "Over. Global Rev.", "Over. US Rev.", "Yahoo US Rev."))


#-------------------------------------------
# 2) Publisher and Total Cost comparison 
#Total_Cost <- $755315.9

#G_Global_Cost <- 120946.7
#G_US_Cost <- 353640.6
#MSN_Global_Cost <- 12160.36
#MSN_US_Cost <- 16098.49
#Over_Global_Cost <- 64295.86
#Over_US_Cost <- 141976.1
#Yahoo_US_Cost <- 46197.82

bar_cost <- c(755315.9, 120946.7, 353640.6, 12160.36, 16098.49, 64295.86, 141976.1, 46197.82)
counts_bar_cost <- bar_cost

barplot(bar_cost, horiz = FALSE,
        names.arg = c("Total Cost", "G. Global Cost", "G. US Rev.", "MSN Global Cost", "MSN US Cost", "Over. Global Cost", "Over. US Cost", "Yahoo US Cost"))


#-------------------------------------------
# 3) Publisher and Profit comparison

#G_Global_prof <- 27862.31 (Jose result) // 808,603.1 (Michell result)
#G_US_prof <- 52017.07 // 1,391,841
#MSN_Global_prof <- 1105.416// 133,363.9
#MSN_US_prof <- 505.456 // 165,451.3
#Over_Global_prof <- 9107.19 // 365,788.8
#Over_us_prof <- 28859.71 // 205,457.2
#Yahoo_US_prof <- 5052.422 // 836,091.1

bar_prof <- c(808603.1, 1391841, 133363.9, 165451.3, 365788.8, 205457.2, 836091.1)
counts_bar_prof <- bar_cost

barplot(bar_prof, horiz = FALSE,
        names.arg = c("G. Global Prof.", "G. US Prof.", "MSN Global Prof.", "MSN US Prof.", "Over. Global Prof.", "Over. US Prof.", "Yahoo US Rev."))


#-----------------------------------------------------------------------
# Histogram to understand the distribution for "Total Cost" and "Amount"

# Histogram for "Total Cost"
hist(yahoo_us_data_nozero$`Total Cost`, prob = TRUE, main = "Histogram for Total Cost by Publisher: Yahoo", xlab = "Total Cost", las = 1)
coord_flip()
lines(density(yahoo_us_data_nozero$`Total Cost`))
lines(density(yahoo_us_data_nozero$`Total Cost`), col = 2, lwd = 3)

