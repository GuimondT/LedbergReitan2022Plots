###############################################################################
# LedbergReitan2022Plots.r
# Created by: Tim Guimond, MD, FRCPC, PhD
# Date: 18 Sept 2022
###############################################################################

########################
# Library set-up       #
########################
library(tidyverse)

########################
# Read data set        #
########################
Table2Data <- read_csv(file = "LedbergTable2.csv")

Table3Data <- read_csv(file = "LedbergTable3.csv")

#######################################
# Parsing time parameters             #
#######################################

Table2DayParameters <- Table2Data %>% 
  filter(startsWith(parameter,"day")) %>% 
  mutate(dayStart=as.numeric(str_sub(parameter,
                          start = str_locate(parameter,pattern = " ")[,1]+1,
                          end = str_locate(parameter,pattern = " to ")[,1]-1)),
         dayEnd=as.numeric(str_sub(parameter,
                          start = str_locate(parameter,pattern = " to ")[,2],
                          end = str_length(parameter))))
Table3DayParameters <- Table3Data %>% 
  filter(startsWith(parameter,"day")) %>% 
  mutate(dayStart=as.numeric(str_sub(parameter,
                                     start = str_locate(parameter,pattern = " ")[,1]+1,
                                     end = str_locate(parameter,pattern = " to ")[,1]-1)),
         dayEnd=as.numeric(str_sub(parameter,
                                   start = str_locate(parameter,pattern = " to ")[,2],
                                   end = str_length(parameter))))

#######################################
# Plot time period parameters with CI #
#######################################

pdf("hazardplots.pdf")
plot(x=c(Table2DayParameters$dayStart[1],Table2DayParameters$dayEnd[1]),
     y=rep(Table2DayParameters$`hazard ratio`[1],2),
     type="l",xlim=c(0,400),ylim=c(0,1.1),
     xlab="Time(days)",ylab="Hazard ratio",
     main="Relative hazard rate estimates for death due to 'external causes'\nin the reference group (young males in the first year)")
for(i in 2:5) {
  rect(Table2DayParameters$dayStart[i],Table2DayParameters$`lower CI`[i],
       Table2DayParameters$dayEnd[i],Table2DayParameters$`upper CI`[i],
       col=paste0(rainbow(4,s=0.125),"7F")[i-1],border=rainbow(4)[i-1])
  lines(x=c(Table2DayParameters$dayStart[i],Table2DayParameters$dayEnd[i]),
        y=rep(Table2DayParameters$`hazard ratio`[i],2),
        col=rainbow(4)[i-1])
}

plot(x=c(Table3DayParameters$dayStart[1],Table3DayParameters$dayEnd[1]),
     y=rep(Table3DayParameters$`hazard ratio`[1],2),
     type="l",xlim=c(0,400),ylim=c(0,2.5),
     xlab="Time(days)",ylab="Hazard ratio",
     main="Relative hazard rate estimates for death due to 'other causes'\nin the reference group (young males in the first year)")
for(i in 2:5) {
  rect(Table3DayParameters$dayStart[i],Table3DayParameters$`lower CI`[i],
       Table3DayParameters$dayEnd[i],Table3DayParameters$`upper CI`[i],
       col=rainbow(4,s=0.125)[i-1],border=rainbow(4)[i-1])
  lines(x=c(Table3DayParameters$dayStart[i],Table3DayParameters$dayEnd[i]),
        y=rep(Table3DayParameters$`hazard ratio`[i],2),
        col=rainbow(4)[i-1])
}
dev.off()
