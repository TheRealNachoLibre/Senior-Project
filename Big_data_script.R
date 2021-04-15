#This is the topic on script file
library(tidyverse)
tidyverse_update()
library(hrbrthemes)
library(viridis)
library(plotly)
library(dash)
library(dashCoreComponents)
library(dashHtmlComponents)

#global CO2 emissions
vex <- c("region", "abrev", "Year", "emission")
annual_co2_emissions_per_countryUSA <- read_csv("annual-co2-emissions-per-countryUSA.csv")
colnames(annual_co2_emissions_per_countryUSA) <- vex 
co2_data <- annual_co2_emissions_per_countryUSA %>% 
  filter("World" == region) %>% 
  filter(1977 < Year) %>% 
  select(region, Year, emission)

#to check if the correct data is allocated
#seaice_1 <- read_csv("~/Desktop/CSCI_487/seaice.csv") %>% 
#  filter(1988 == Year)

#Sea ice average 
seaice <- read_csv("~/Desktop/CSCI_487/seaice.csv") %>% 
  select(Year, Extent) %>% 
  group_by(Year) %>% 
  summarise(n=n(), total_extent = sum(Extent))
seaice$avg <- seaice$total_extent/seaice$n 
seaice <- seaice %>% 
  select(Year, avg)

#Global temperatures 
vex <- c("Entity", "Year", "Median_Temp", "upper", "lower")
global_temp <- read_csv("/Users/grantgrisham/GitHub/Senior-Project/Senior-Project/temperature-anomaly.csv")
colnames(global_temp) <- vex
g_temp_data <- global_temp %>% 
  filter("Global" == Entity) %>% 
  filter(1977 < Year) %>% 
  select(Entity, Year, Median_Temp)
  
#world comparison 
comparison <- left_join(co2_data, seaice, by = "Year")
comparison <- left_join(comparison, g_temp_data, by = "Year") %>% 
  select(-Entity, -region)
colors <-c("SeaIce sq. km" = "red", "1/1000 Billion\ntons CO2" = "blue", "Avg. Global Temp." = "green")
comparison$emission <- comparison$emission / 1000
comparison$Median_Temp <- comparison$Median_Temp * 10
fig <- plot_ly(comparison, x = ~Year)
fig <- fig %>% add_lines(y = ~emission, name = "1/1000 Billions\ntons CO2")
fig <- fig %>% add_lines(y = ~avg, name = "Seaice sq. KM")
fig <- fig %>% add_lines(y = ~Median_Temp, name = "Avg. Global Temp")
fig <- fig %>% layout(
  title = "Global Warming Comparison",
  xaxis = list(
    rangeslider = list(type = "date", format="yyyy")),
  yaxis = list(title = "Data"))
  fig

write.csv(comparison, "comparison.csv")
  
  #ggplot()+
  #geom_line(aes(Year, emission, color="1/1000 Billion\ntons CO2"))+
  #geom_line(aes(Year, avg, color="SeaIce sq. km"))+ 
  #geom_line(aes(Year, Median_Temp, color="Avg. Global Temp."))+
  #labs(title="Global Warming Comparison", x = "Year (1978 - 2019)", color="Legend")+
  #theme(axis.title.y = element_blank())+
  #scale_x_continuous(breaks=seq(1978, 2019, 5))+
  #scale_y_continuous(breaks=seq(0, 50, 5))
  #abline(lm(Median_Temp ~ Year, data = comparison))

#breakdown comparison of China, India, USA, Russia, vs the rest of the world
vex <- c("region", "abrev", "Year", "emission")
annual_co2_emissions_per_countryUSA <- read_csv("annual-co2-emissions-per-countryUSA.csv")
colnames(annual_co2_emissions_per_countryUSA) <- vex 
co2_data_Big <- annual_co2_emissions_per_countryUSA %>%  
  filter(region %in% c("United States", "Asia", "India")) %>% 
  filter(1977 < Year) %>% 
  group_by(Year) %>% 
  summarise(n=n(), total_3 = sum(emission))
co2_data_Big <- co2_data_Big %>% 
  select(Year, total_3)
co2_data_Other <- annual_co2_emissions_per_countryUSA %>% 
  filter("United States" != region & "Asia" != region
         & "India" != region & "Russia" != region 
         & "International transport" != region & "World" != region) %>% 
  filter(1977 < Year) %>% 
  select(region, Year, emission) %>% 
  group_by(Year) %>% 
  summarise(n=n(), total_emission = sum(emission))
co2_data_Other <- co2_data_Other %>% 
  select(Year, total_emission)
#plot
comparison <- left_join(co2_data_Big, co2_data_Other, by = "Year")
colors <-c("Other" = "red", "Top 4" = "green")
comparison %>% 
  ggplot()+
  geom_line(aes(Year, total_3, color="Top 3"))+
  geom_line(aes(Year, total_emission, color="Other"))+
  labs(title="Top 3 Polluters vs Other", x = "Year (1978 - 2019)", 
      y = "Emissions (Ton)", color="Legend")


#bar graph of pollution 
df <- data.frame(Pollutants=c("Coal", "Oil", "Cement", "Flaring", "Other Industry"),
                 Million_Metric_Tons=c(14362.17, 12355.13, 1563.761, 429.4956, 115.121))
head(df)
p<-ggplot(data=df, aes(x=Pollutants, y=Million_Metric_Tons))+
  geom_bar(stat="identity", color="blue", fill="white")+
  scale_y_continuous(breaks=seq(0, 15000, 1000))+
  ggtitle("Pollution by types of 2019")
p




#heat map of data major pollutors  2018
global_pollution <- read.csv("/Users/grantgrisham/GitHub/Senior-Project/Senior-Project/co2-emissions-by-fuel-line.csv")
global_data <- global_pollution %>% 
  filter(2018 == Year) %>% 
  select(-Code, -Year, -Entity)
colnames(global_data) <- c("coal", "oil", "gas", "cement", "flaring", "other") 
entity_names <- global_pollution %>% 
  filter(2018 == Year) %>% 
  select(Entity) 
rownames(global_data) <- entity_names$Entity
global_data <- global_data %>% 
  filter(80 < coal)
data <- as.matrix(global_data)
plot_ly(x=colnames(data), y=rownames(data), z = data,type = "heatmap") %>% 
  layout(title="Major polluters by type (2018)")

#heat map of minor polluters
global_pollution <- read.csv("/Users/grantgrisham/GitHub/Senior-Project/Senior-Project/co2-emissions-by-fuel-line.csv")
global_data <- global_pollution %>% 
  filter(2018 == Year) %>% 
  select(-Code, -Year, -Entity)
colnames(global_data) <- c("coal", "oil", "gas", "cement", "flaring", "other") 
entity_names <- global_pollution %>% 
  filter(2018 == Year) %>% 
  select(Entity) 
rownames(global_data) <- entity_names$Entity
global_data <- global_data %>% 
  filter(80 > coal & 0 != coal)
data <- as.matrix(global_data)
plot_ly(x=colnames(data), y=rownames(data), z = data,type = "heatmap") %>% 
  layout(title="Minor polluters by type (2018)")






#heat map of data major pollutors 2017
global_pollution <- read.csv("/Users/grantgrisham/GitHub/Senior-Project/Senior-Project/co2-emissions-by-fuel-line.csv")
global_data <- global_pollution %>% 
  filter(2017 == Year) %>% 
  select(-Code, -Year, -Entity)
colnames(global_data) <- c("coal", "oil", "gas", "cement", "flaring", "other") 
entity_names <- global_pollution %>% 
  filter(2017 == Year) %>% 
  select(Entity) 
rownames(global_data) <- entity_names$Entity
global_data <- global_data %>% 
  filter(80 < coal)
data <- as.matrix(global_data)
plot_ly(x=colnames(data), y=rownames(data), z = data,type = "heatmap") %>% 
  layout(title="Major polluters by type (2017)")

#heat map of minor polluters
global_pollution <- read.csv("/Users/grantgrisham/GitHub/Senior-Project/Senior-Project/co2-emissions-by-fuel-line.csv")
global_data <- global_pollution %>% 
  filter(2017 == Year) %>% 
  select(-Code, -Year, -Entity)
colnames(global_data) <- c("coal", "oil", "gas", "cement", "flaring", "other") 
entity_names <- global_pollution %>% 
  filter(2017 == Year) %>% 
  select(Entity) 
rownames(global_data) <- entity_names$Entity
global_data <- global_data %>% 
  filter(80 > coal & 0 != coal)
data <- as.matrix(global_data)
plot_ly(x=colnames(data), y=rownames(data), z = data,type = "heatmap")%>% 
  layout(title="Minor polluters by type (2017)")



#heat map of data major pollutors 2016
global_pollution <- read.csv("/Users/grantgrisham/GitHub/Senior-Project/Senior-Project/co2-emissions-by-fuel-line.csv")
global_data <- global_pollution %>% 
  filter(2016 == Year) %>% 
  select(-Code, -Year, -Entity)
colnames(global_data) <- c("coal", "oil", "gas", "cement", "flaring", "other") 
entity_names <- global_pollution %>% 
  filter(2016 == Year) %>% 
  select(Entity) 
rownames(global_data) <- entity_names$Entity
global_data <- global_data %>% 
  filter(80 < coal)
data <- as.matrix(global_data)
plot_ly(x=colnames(data), y=rownames(data), z = data,type = "heatmap")%>% 
  layout(title="Major polluters by type (2016)")

#heat map of minor polluters
global_pollution <- read.csv("/Users/grantgrisham/GitHub/Senior-Project/Senior-Project/co2-emissions-by-fuel-line.csv")
global_data <- global_pollution %>% 
  filter(2016 == Year) %>% 
  select(-Code, -Year, -Entity)
colnames(global_data) <- c("coal", "oil", "gas", "cement", "flaring", "other") 
entity_names <- global_pollution %>% 
  filter(2016 == Year) %>% 
  select(Entity) 
rownames(global_data) <- entity_names$Entity
global_data <- global_data %>% 
  filter(80 > coal & 0 != coal)
data <- as.matrix(global_data)
plot_ly(x=colnames(data), y=rownames(data), z = data,type = "heatmap")%>% 
  layout(title="Minor polluters by type (2016)")


#3d heat map 2019
global_pollution <- read.csv("/Users/grantgrisham/GitHub/Senior-Project/Senior-Project/co2-emissions-by-fuel-line.csv")
global_data <- global_pollution %>% 
  filter(2019 == Year) %>% 
  filter("International transport" != Entity) %>% 
  filter("World" != Entity) %>% 
  filter("Asia" != Entity) %>% 
  select(-Code, -Year, -Entity)
colnames(global_data) <- c("coal", "oil", "gas", "cement", "flaring", "other") 
entity_names <- global_pollution %>% 
  filter(2019 == Year) %>% 
  filter("International transport" != Entity) %>% 
  filter("World" != Entity) %>% 
  filter("Asia" != Entity) %>% 
  select(Entity) 
rownames(global_data) <- entity_names$Entity
global_data <- global_data %>% 
  filter(80 < coal)
data <- as.matrix(global_data)
fig <- plot_ly(z = ~data, x=colnames(data), y=rownames(data)) %>% add_surface(
  contours = list(
    z = list(
      show=TRUE,
      usecolormap=TRUE,
      highlightcolor="#ff0000",
      project=list(z=TRUE)
    )
  )
)
fig <- fig %>% layout(title = "Pollution by country (2019)", 
  scene = list(
    camera=list(
      eye = list(x=1.87, y=0.88, z=-0.64)
    )
  )
)
fig







