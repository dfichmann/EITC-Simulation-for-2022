#Author: Daniel Fichmann
#Data: March 2022

#packages
library(tidyverse)
library(tidyr)
library(expss)
library(gridExtra)

#Define EITC function (fling has to be either "married" or "single")
EITC_calc <- function(x, kid, filing){
  i = kid + 1
  if (filing == "single"){
    j = 6 
  }
  else if (filing == "married"){
    j = 8 
  }
  k = j + 1
  PhaseInRate <- schedule_matrix2022[i,2]
  PhaseInEnd <- schedule_matrix2022[i,3]
  PhaseOutRate <- schedule_matrix2022[i,5]
  PhaseOutStart <- schedule_matrix2022[i,j]
  PhaseOutEnd <- schedule_matrix2022[i,k]
  if (x <= PhaseInEnd){
    eitc = x*PhaseInRate
  }
  else if (x > PhaseInEnd & x < PhaseOutStart){
    eitc = PhaseInEnd*PhaseInRate
  }
  else if (x >= PhaseOutStart & x <= PhaseOutEnd){
    eitc = PhaseInEnd*PhaseInRate - (x-PhaseOutStart)*PhaseOutRate
  }
  else{
    eitc = 0
  }
  return(eitc)
}

#loading schedule data/matrix
schedule_names <- c("children", "Phase-in rate", "Phase-in ends",	
                    "Maximum credit amount",	"Phase-out rate",
                    "Phase-out start (single)",	"Phase-out ends (single)",
                    "Phase-out start (married)",	"Phase-out ends (married)")
r1 <- c(0,	0.0765,	7320,	560,	0.076,	9160,	16480,	15290,	22610)
r2 <- c(1, 	0.34,	10980,	3733,	0.1598,	20130,	43492,	26260,	49622)
r3 <- c(2,	0.4,	15410,	6164,	0.2106,	20130,	49399,	26260,	55529)
r4 <- c(3,	0.45,	15410,	6935,	0.2106,	20130,	53057,	26260,	59187)
schedule_matrix2022 <- rbind(r1, r2, r3, r4)
colnames(schedule_matrix2022) <- schedule_names

#https://www.irs.gov/pub/irs-drop/rp-21-45.pdf

########### set up ############
#set income range
obs <- 60000

#name rows and columns
row_names <- c()
col_names <- c("Income", "EITCsingle0", "EITCsingle1", "EITCsingle2", "EITCsingle3", 
               "EITCmarried0", "EITCmarried1", "EITCmarried2", "EITCmarried3" )
#create matrix
df <- matrix(nrow = obs, ncol = 9, dimnames = list(row_names, col_names))
#add income data
df[ ,1]<- c(1:obs)



for(i in seq(1,obs)){
  df[i,"EITCsingle0"] = EITC_calc(i, 0, "single")
}


for(i in seq(1,obs)){
  df[i,"EITCsingle1"] = EITC_calc(i, 1, "single")
}

for(i in seq(1,obs)){
  df[i,"EITCsingle2"] = EITC_calc(i, 2, "single")
}

for(i in seq(1,obs)){
  df[i,"EITCsingle3"] = EITC_calc(i, 3, "single")
}

for(i in seq(1,obs)){
  df[i,"EITCmarried0"] = EITC_calc(i, 0, "married")
}

for(i in seq(1,obs)){
  df[i,"EITCmarried1"] = EITC_calc(i, 1, "married")
}

for(i in seq(1,obs)){
  df[i,"EITCmarried2"] = EITC_calc(i, 2, "married")
}

for(i in seq(1,obs)){
  df[i,"EITCmarried3"] = EITC_calc(i, 3, "married")
}


########## data frame and subsetting #############
eitcdata <- data.frame(df)
myvars <- c("Income", "EITCsingle0", "EITCsingle1", "EITCsingle2", "EITCsingle3")
SingleEITC <- eitcdata[myvars]

myvars <- c("Income", "EITCmarried0", "EITCmarried1", "EITCmarried2", "EITCmarried3")
MarriedEITC <- eitcdata[myvars]

SingleEITC <- pivot_longer(SingleEITC, 
                           2:5, 
                           names_to = "Type", 
                           values_to = "Credit"
)

MarriedEITC <- pivot_longer(MarriedEITC, 
                            2:5, 
                            names_to = "Type", 
                            values_to = "Credit"
)

########## plots ###########################

a <- ggplot(SingleEITC, aes(x=Income, y=Credit, color=Type)) + 
  geom_line(size = 1.25) +
  scale_colour_discrete(name = "Scenarios:", 
                        labels = c("no child", "1 child", "2 children",
                                   "3 children or more")) +
  theme_minimal() +
  labs(x="Income ($)", y="EITC amount ($)") + 
  ggtitle("Single filers")

b <- ggplot(MarriedEITC, aes(x=Income, y=Credit, color=Type)) + 
  geom_line(size = 1.25) +
  scale_colour_discrete(name = "Scenarios:", 
                        labels = c("no child", "1 child", "2 children",
                                   "3 children or more")) +
  theme_minimal() +
  labs(x="Income ($)", y="EITC amount ($)") +
  ggtitle("Joint filers")

b

figure <- grid.arrange(a, b, ncol = 1, nrow = 2)
figure


