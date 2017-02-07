library(plyr)
library(choroplethr)
library(dplyr)
library(readr)
library(data.table)

dest = "https://www.fhwa.dot.gov/bridge/nbi/2016/delimited/AK16.txt"
tmp = fread(dest) 
tmp = as.tbl(tmp)
classes = sapply(tmp, class)

states= read_csv("http://pages.stat.wisc.edu/~karlrohe/classes/data/stateAbv.txt")
states=states[-(1:12),]
state.=states[1:8,]
state.[9,]=c("District of Columbia", "DC")
state.[10:51,]=states[9:50]
state.[52,]= c("Puerto Rico", "PR")
dat=list()

dest= rep("", 52)
for(i in 1:52) dest[i]=paste("https://www.fhwa.dot.gov/bridge/nbi/2016/delimited/", states[i,2],"16.txt", sep = "") 
x16 = ldply(dest, fread, colClasses = classes)

keep=c("STATE_CODE_001", "STRUCTURE_NUMBER_008" , "COUNTY_CODE_003", "LAT_016", "LONG_017", "ADT_029", "YEAR_BUILT_027",  "YEAR_RECONSTRUCTED_106",
       "BRIDGE_IMP_COST_094" ,    "ROADWAY_IMP_COST_095"  ,  "TOTAL_IMP_COST_096" ,"FUTURE_ADT_114" )

M = select(x16,one_of(keep))
us = as.tbl(M)

library(ggplot2)

ggplot(data = us) +geom_point(mapping = aes(y = LAT_016, x = LONG_017))
statebridgr_Num=group_by(us, STATE_CODE_001)
statebridgr_Num=dplyr::summarise(statebridgr_Num,count=n())
statebridgr_Num$states=state.

us <- map_data("state")

statebridge$region=tolower(statebridge$region)
Total <- merge(us, statebridge, by="region")

p <- ggplot()
p <- p + geom_polygon(data=Total, aes(x=long, y=lat, group = group, fill=Total$count),colour="white"
      ) + scale_fill_continuous(low = "thistle2", high = "darkred", guide="colorbar")
P1 <- p + theme_bw()  + labs(fill = "Bridge total from low to high" 
                            ,title = "Bridge total for each state", x="", y="")
P1 + scale_y_continuous(breaks=c()) + scale_x_continuous(breaks=c()) + theme(panel.border =  element_blank())


min2dec = function(x){
  substr(x,3,8) %>% return
}
hist(wi$LAT_016 %>% min2dec %>% as.numeric)

min2dec = function(x){
  as.numeric(substr(x,1,2)) + as.numeric(substr(x,3,8))/6e+05 %>% return
}
min2dec(mydata$LAT_016[1])
hist(mydata$LAT_016 %>% min2dec %>% as.numeric)

mydata_ = mutate(mydata,lat = min2dec(LAT_016), lon = min2dec(LONG_017))
ggplot(data =mydata_) +geom_point(mapping = aes(y = lat, x = lon))

ggplot(data = mydata_, mapping = aes(y = log(ADT_029), x =YEAR_BUILT_027, col = TOLL_020)) +geom_point() + geom_smooth()

ADT_change=mydata$FUTURE_ADT_114-mydata$ADT_029
plot(mydata$YEAR_BUILT_027, ADT_change)


