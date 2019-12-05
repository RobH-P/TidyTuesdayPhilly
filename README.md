# TidyTuesdayPhilly
#A look at parking violations from Philadelphia, USA in 2017

tickets <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-12-03/tickets.csv")

# read in the tickets data
tickets_raw <- "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-12-03/tickets.csv"
tickets <- read.csv(tickets_raw)

#checking the data

names(tickets)
class(tickets)

#checking and counting on specific parking violations


offence_numb <- tickets %>%
  group_by(violation_desc, fine) %>%
  summarise(count = n())

offence_numb2 <- offence_numb
#removing duplication
offence_numb2$violation_desc <- gsub(" CC","", offence_numb2$violation_desc, ignore.case = T)
offence_numb2$violation_desc <- gsub(" 73","", offence_numb2$violation_desc, ignore.case = T)
offence_numb2$violation_desc <- gsub(" PLACD TKT"," PLACED TICKT", offence_numb2$violation_desc, ignore.case = T)
offence_numb2$violation_desc <- gsub("IMPROPER-2WAY HWY 74"," IMPROPER ON 2WAY HWY", offence_numb2$violation_desc, ignore.case = T)
offence_numb2$violation_desc <- gsub("PARK PROHIBITED 04","PARK PROHIB PLACE", offence_numb2$violation_desc, ignore.case = T)
offence_numb2$violation_desc <- gsub("PARK PROHIB PLACE","PARKING PROHBITED", offence_numb2$violation_desc, ignore.case = T)
offence_numb2$violation_desc <- gsub("STOP/BLOCK HWY 33","STOP/BLOCK HWY", offence_numb2$violation_desc, ignore.case = T)
offence_numb2$violation_desc <- gsub("STOP/BLOCK HIWY","STOP/BLOCK HWY", offence_numb2$violation_desc, ignore.case = T)
offence_numb2$violation_desc <- gsub("STOP/BLOCK HIGHWAY","STOP/BLOCK HWY", offence_numb2$violation_desc, ignore.case = T)
offence_numb2$violation_desc <- gsub("STOPPED IN SAFE ZONE","STOPPED SAFE ZONE", offence_numb2$violation_desc, ignore.case = T)
offence_numb2$violation_desc <- gsub("STOPPING PROHIBITED","STOP PROHIBITED", offence_numb2$violation_desc, ignore.case = T)
offence_numb2$violation_desc <- gsub("VALET ZONE VIOLATION","VALET VIOLATION", offence_numb2$violation_desc, ignore.case = T)
offence_numb2$violation_desc <- gsub("PASSENGR LOADNG ZONE","PASSENGER LOADING ZONE", offence_numb2$violation_desc, ignore.case = T)

### plot of most common violations
options(scipen = 999)

offence_numb2 <- filter(offence_numb2, count >= 10000)

ggplot(offence_numb2, aes(x=reorder(violation_desc, count), y=count, fill= fine, colours(distinct = T))) +
  geom_bar(position="stack",stat = "identity") +
  labs(title = "Parking violations in Philly", x="Violation description", y="Numbers caught", caption = "Data from Open Data Philly") +
  theme(axis.title=element_text(face="bold",size="14",color="black"),axis.text=element_text(size=14,face="bold")) +
  coord_flip()

### sorting by fire hydrant and station violation

wheres_fire <- tickets  %>%
  filter(violation_desc == "FIRE HYDRANT" | violation_desc == "PK NEAR FIRE STAT 51")

names(wheres_fire)[2] <- "date"
names(wheres_fire)[7] <- "CODE"

wheres_fire <- wheres_fire[, c(1, 7, 2:6)]

# Plotting the fire offences

m <- leaflet(wheres_fire) %>%  
  setView(lng = -75.1181, lat = 40.0026, zoom = 12)%>%
  addTiles() %>% 
  addMarkers(~lon, ~lat)
m

philly_map <- leaflet(wheres_fire) %>%
  setView(lng = -75.1181, lat = 40.0026, zoom = 12)%>%
  addTiles() %>%
  addHeatmap(~lon,~lat, intensity = ~CODE, blur = 20, max = 0.05, radius = 15)
philly_map
