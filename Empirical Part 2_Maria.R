###Empirical Part 2_Maria Puspa Kartika###

rm(list=ls()) 
cat('\014')

if (!require(haven)) install.packages("haven"); library(haven)
if (!require(tidyverse)) install.packages("tidyverse"); library(tidyverse)
if (!require(ggplot2)) install.packages("ggplot2"); library(ggplot2)
if (!require(statar)) install.packages("statar"); library(statar)

setwd("/Users/mariapkartika/Documents/Ec50/Empirical Project")
atlas <- read_dta("atlas.dta")

#My Census Tract: Tract 06037206010, Central LA, Los Angeles, CA

#subset data to the state of California
california <- subset(atlas, state == 06)

#subset data to Los Angeles county
losangelescounty <- subset(atlas,  state == 06 & county == 037)

#subset data to tract
tract <- subset(atlas,  state == 06 & county == 037 & tract == 206010)

#subset data to four tracts which encompassed the Chinatown area in downtown LA
chinatown <- subset(atlas, (state == 06 & county == 037 & tract %in% c(206010, 207102, 207103, 206020, 207101)))

#####Upward Mobility#####
#calculating mean of kfr_pooled_pooled_p25 in the US
mean(atlas$kfr_pooled_pooled_p25, na.rm = TRUE)

#calculating mean of kfr_pooled_pooled_p25 in the state of California
mean(california$kfr_pooled_pooled_p25, na.rm = TRUE)

#calculating mean of kfr_pooled_pooled_p25 in my census tract
mean(chinatown$kfr_pooled_pooled_p25, na.rm = TRUE)

#calculating mean of kfr_pooled_pooled_p25 in my census tract
mean(chinatown$kfr_pooled_pooled_p25, na.rm = TRUE)

############################# Chinatown Racial Composition #############################

#population density
mean(chinatown$popdensity2000, na.rm=TRUE)
mean(chinatown$popdensity2010, na.rm=TRUE)

#racial composition Asian
mean(chinatown$share_asian2010, na.rm=TRUE) - 
  mean(chinatown$share_asian2000, na.rm=TRUE)

#racial composition Black
mean(chinatown$share_black2010, na.rm=TRUE) - 
  mean(chinatown$share_black2000, na.rm=TRUE)

#racial composition White
mean(chinatown$share_white2010, na.rm=TRUE) - 
  mean(chinatown$share_white2000, na.rm=TRUE)

#racial composition Hispanic
mean(chinatown$share_hisp2010, na.rm=TRUE) -
  mean(chinatown$share_hisp2000, na.rm=TRUE)

library(ggplot2)
####### Rent and Upward Mobility #######

#create a scatter plot of LA County kfr_pooled_pooled_p25 and rent_twobed2015
ggplot(data = losangelescounty) + geom_point(aes(x = rent_twobed2015, y = kfr_pooled_pooled_p25)) +
  geom_smooth(aes(x = rent_twobed2015, y = kfr_pooled_pooled_p25), method = "lm", se = F) +
  geom_text(aes(x = rent_twobed2015, y = kfr_pooled_pooled_p25, label=tract), check_overlap = TRUE, size = 3)

#Save graph as .png 
ggsave("lacounty_scatter.png")

#correlation coefficient
cor(losangelescounty$kfr_pooled_pooled_p25, losangelescounty$rent_twobed2015, use="complete.obs")

cor(chinatown$kfr_asian_pooled_p25, chinatown$rent_twobed2015, use="complete.obs")

### Immigrants and Upward Mobility ###

#create a scatter plot of LA County kfr_pooled_pooled_p25 and foreign_share2010
ggplot(data = losangelescounty) + geom_point(aes(x = foreign_share2010, y = kfr_pooled_pooled_p25)) +
  geom_smooth(aes(x = foreign_share2010, y = kfr_pooled_pooled_p25), method = "lm", se = F) +
  geom_text(aes(x = foreign_share2010, y = kfr_pooled_pooled_p25, label=tract), check_overlap = TRUE, size = 3)

#Save graph as .png 
ggsave("lacounty_immigrant.png")

### Asian and Upward Mobility ###

#create a scatter plot of LA County kfr_pooled_pooled_p25 and share_asian2000
ggplot(data = losangelescounty) + geom_point(aes(x = share_asian2000, y = kfr_pooled_pooled_p25)) +
  geom_smooth(aes(x = share_asian2000, y = kfr_pooled_pooled_p25), method = "lm", se = F) +
  geom_text(aes(x = share_asian2000, y = kfr_pooled_pooled_p25, label=tract), check_overlap = TRUE, size = 3)

#Save graph as .png 
ggsave("lacounty_asian.png")

#correlation coefficient
cor(losangelescounty$kfr_pooled_pooled_p25, losangelescounty$share_asian2000, use="complete.obs")

### Black and Upward Mobility ###

#create a scatter plot of LA County kfr_pooled_pooled_p25 and share_black2000
ggplot(data = losangelescounty) + geom_point(aes(x = share_black2000, y = kfr_pooled_pooled_p25)) +
  geom_smooth(aes(x = share_black2000, y = kfr_pooled_pooled_p25), method = "lm", se = F) +
  geom_text(aes(x = share_black2000, y = kfr_pooled_pooled_p25, label=tract), check_overlap = TRUE, size = 3)

#Save graph as .png 
ggsave("lacounty_black.png")

#correlation coefficient
cor(losangelescounty$kfr_pooled_pooled_p25, losangelescounty$share_black2000, use="complete.obs")

### Hispanic and Upward Mobility ###

#create a scatter plot of LA County kfr_pooled_pooled_p25 and share_hisp2000
ggplot(data = losangelescounty) + geom_point(aes(x = share_hisp2000, y = kfr_pooled_pooled_p25)) +
  geom_smooth(aes(x = share_hisp2000, y = kfr_pooled_pooled_p25), method = "lm", se = F) +
  geom_text(aes(x = share_hisp2000, y = kfr_pooled_pooled_p25, label=tract), check_overlap = TRUE, size = 3)

#Save graph as .png 
ggsave("lacounty_hisp.png")

#correlation coefficient
cor(losangelescounty$kfr_pooled_pooled_p25, losangelescounty$share_hisp2000, use="complete.obs")


### White and Upward Mobility ###

#create a scatter plot of LA County kfr_pooled_pooled_p25 and share_white2000
ggplot(data = losangelescounty) + geom_point(aes(x = share_white2000, y = kfr_pooled_pooled_p25)) +
  geom_smooth(aes(x = share_white2000, y = kfr_pooled_pooled_p25), method = "lm", se = F) +
  geom_text(aes(x = share_white2000, y = kfr_pooled_pooled_p25, label=tract), check_overlap = TRUE, size = 3)

#Save graph as .png 
ggsave("lacounty_white.png")

#correlation coefficient
cor(losangelescounty$kfr_pooled_pooled_p25, losangelescounty$share_white2000, use="complete.obs")


### Poverty and Upward Mobility ###

#create a scatter plot of LA County kfr_pooled_pooled_p25 and poor_share1990
ggplot(data = losangelescounty) + geom_point(aes(x = poor_share1990, y = kfr_pooled_pooled_p25)) +
  geom_smooth(aes(x = poor_share1990, y = kfr_pooled_pooled_p25), method = "lm", se = F) +
  geom_text(aes(x = poor_share1990, y = kfr_pooled_pooled_p25, label=tract), check_overlap = TRUE, size = 3)

#Save graph as .png 
ggsave("lacounty_poor.png")

### Share of Single Parents and Upward Mobility ###

#create a scatter plot of LA County kfr_pooled_pooled_p25 and singleparent_share1990
ggplot(data = losangelescounty) + geom_point(aes(x = singleparent_share1990, y = kfr_pooled_pooled_p25)) +
  geom_smooth(aes(x = singleparent_share1990, y = kfr_pooled_pooled_p25), method = "lm", se = F) +
  geom_text(aes(x = singleparent_share1990, y = kfr_pooled_pooled_p25, label=tract), check_overlap = TRUE, size = 3)

#Save graph as .png 
ggsave("lacounty_singleparent.png")




# Calculate the mean of each racial group for 2000 and 2010
asian_means <- c(mean(chinatown$share_asian2000), mean(chinatown$share_asian2010))
white_means <- c(mean(chinatown$share_white2000), mean(chinatown$share_white2010))
black_means <- c(mean(chinatown$share_black2000), mean(chinatown$share_black2010))
hisp_means <- c(mean(chinatown$share_hisp2000), mean(chinatown$share_hisp2010))

# Combine the means into a data frame
race_data <- data.frame(group = c("Asian", "White", "Black", "Hispanic"),
                        year_2000 = c(asian_means[1], white_means[1], black_means[1], hisp_means[1]),
                        year_2010 = c(asian_means[2], white_means[2], black_means[2], hisp_means[2]))

# Melt the data frame so that it's in long format
library(reshape2)
race_data_melted <- melt(race_data, id.vars = "group")

# Create the bar graph
ggplot(race_data_melted, aes(x = group, y = value, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Race/Ethnicity", y = "Share of Population",
       title = "Racial Composition of Chinatown, 2000 and 2010") +
  scale_fill_manual(values = c("#0072B2", "#D55E00"), 
                    labels = c("2000", "2010")) +
  theme_minimal()

#Save graph as .png 
ggsave("racialcompos2000vs2010.png")

#Draw scatter plot with linear fit line for 2010 fraction asian vs 2000 fraction asian
ggplot(data = chinatown) + geom_point(aes(x = share_asian2000, y = share_asian2010)) +
  geom_smooth(aes(x = share_asian2000, y = share_asian2010), method = "lm", se = F) +
  geom_text(aes(x = share_asian2000, y = share_asian2010, label=tract), check_overlap = TRUE, size = 3)

#Save graph as .png 
ggsave("chinatownasian2000vs2010.png")

#Draw scatter plot with linear fit line for 2010 fraction black vs 2000 fraction black
ggplot(data = chinatown) + geom_point(aes(x = share_black2000, y = share_black2010)) +
  geom_smooth(aes(x = share_black2000, y = share_black2010), method = "lm", se = F) +
  geom_text(aes(x = share_black2000, y = share_black2010, label=tract), check_overlap = TRUE, size = 3)

#Save graph as .png 
ggsave("chinatownblack2000vs2010.png")

#Draw scatter plot with linear fit line for 2010 fraction hispanic vs 2000 fraction hispanic
ggplot(data = chinatown) + geom_point(aes(x = share_hisp2000, y = share_hisp2010)) +
  geom_smooth(aes(x = share_hisp2000, y = share_hisp2010), method = "lm", se = F) +
  geom_text(aes(x = share_hisp2000, y = share_hisp2010, label=tract), check_overlap = TRUE, size = 3)

#Save graph as .png 
ggsave("chinatownhisp2000vs2010.png")

#Draw scatter plot with linear fit line for 2010 fraction white vs 2000 fraction white
ggplot(data = chinatown) + geom_point(aes(x = share_white2000, y = share_white2010)) +
  geom_smooth(aes(x = share_hisp2000, y = share_hisp2010), method = "lm", se = F) +
  geom_text(aes(x = share_hisp2000, y = share_hisp2010, label=tract), check_overlap = TRUE, size = 3)

#Save graph as .png 
ggsave("chinatownwhite2000vs2010.png")

############################# LA County Racial Composition ############################# 


#Draw scatter plot with linear fit line for 2010 fraction black vs 2000 fraction black
ggplot(data = losangelescounty) + geom_point(aes(x = share_black2000, y = share_black2010)) +
  geom_smooth(aes(x = share_black2000, y = share_black2010), method = "lm", se = F) +
  geom_text(aes(x = share_black2000, y = share_black2010, label=tract), check_overlap = TRUE, size = 3)

#Save graph as .png 
ggsave("shareblack2000vs2010.png")

#Draw scatter plot with linear fit line for 2010 fraction hispanic vs 2000 fraction hispanic
ggplot(data = losangelescounty) + geom_point(aes(x = share_hisp2000, y = share_hisp2010)) +
  geom_smooth(aes(x = share_hisp2000, y = share_hisp2010), method = "lm", se = F) +
  geom_text(aes(x = share_hisp2000, y = share_hisp2010, label=tract), check_overlap = TRUE, size = 3)

#Save graph as .png 
ggsave("sharehisp2000vs2010.png")

#Draw scatter plot with linear fit line for 2010 fraction asian vs 2000 fraction asian
ggplot(data = losangelescounty) + geom_point(aes(x = share_asian2000, y = share_asian2010)) +
  geom_smooth(aes(x = share_asian2000, y = share_asian2010), method = "lm", se = F) +
  geom_text(aes(x = share_asian2000, y = share_asian2010, label=tract), check_overlap = TRUE, size = 3)

#Save graph as .png 
ggsave("shareasian2000vs2010.png")

#Draw scatter plot with linear fit line for 2010 fraction white vs 2000 fraction white
ggplot(data = losangelescounty) + geom_point(aes(x = share_white2000, y = share_white2010)) +
  geom_smooth(aes(x = share_white2000, y = share_white2010), method = "lm", se = F) +
  geom_text(aes(x = share_white2000, y = share_white2010, label=tract), check_overlap = TRUE, size = 3)

#Save graph as .png 
ggsave("sharewhite2000vs2010.png")


############################# Fraction of College Graduates ############################# 

#share of college graduate
mean(chinatown$frac_coll_plus2010, na.rm=TRUE) - 
  mean(chinatown$frac_coll_plus2000, na.rm=TRUE)

#Draw scatter plot with linear fit line
ggplot(data = losangelescounty) + geom_point(aes(x = frac_coll_plus2000, y = frac_coll_plus2010)) +
  geom_smooth(aes(x = frac_coll_plus2000, y = frac_coll_plus2010), method = "lm", se = F) +
  geom_text(aes(x = frac_coll_plus2000, y = frac_coll_plus2010, label=tract), check_overlap = TRUE, size = 3)

#Save graph as .png 
ggsave("frac_college2000vs2010.png")

#correlation coefficient
cor(chinatown$kfr_pooled_pooled_p25, chinatown$frac_coll_plus2000, use="complete.obs")


#Chinatown
#Draw scatter plot with linear fit line
ggplot(data = chinatown) + geom_point(aes(x = frac_coll_plus2000, y = frac_coll_plus2010)) +
  geom_smooth(aes(x = frac_coll_plus2000, y = frac_coll_plus2010), method = "lm", se = F) +
  geom_text(aes(x = frac_coll_plus2000, y = frac_coll_plus2010, label=tract), check_overlap = TRUE, size = 3)

#Save graph as .png 
ggsave("frac_college2000vs2010.png")

#correlation coefficient of kfr_pooled_pooled_p25 and pm25_1990 at census tract level
cor(chinatown$frac_coll_plus2000, chinatown$frac_coll_plus2010, use="complete.obs")


############################# Share Below Poverty Line ############################# 

#Draw scatter plot with linear fit line
ggplot(data = losangelescounty) + geom_point(aes(x = poor_share1990, y = poor_share2010)) +
  geom_smooth(aes(x = poor_share1990, y = poor_share2010), method = "lm", se = F) +
  geom_text(aes(x = poor_share1990, y = poor_share2010, label=tract), check_overlap = TRUE, size = 3)

#Draw scatter plot with linear fit line
ggplot(data = chinatown) + geom_point(aes(x = poor_share1990, y = poor_share2010)) +
  geom_smooth(aes(x = poor_share1990, y = poor_share2010), method = "lm", se = F) +
  geom_text(aes(x = poor_share1990, y = poor_share2010, label=tract), check_overlap = TRUE, size = 3)

#share of below poverty line
mean(chinatown$poor_share2010, na.rm=TRUE) - 
  mean(chinatown$poor_share2000, na.rm=TRUE)

mean(chinatown$poor_share2000, na.rm=TRUE) - 
  mean(chinatown$poor_share1990, na.rm=TRUE)

# Subset the relevant variables and calculate the mean
poverty_data <- chinatown[, c("poor_share1990", "poor_share2000", "poor_share2010")]
poverty_means <- colMeans(poverty_data)

# Create a data frame for the bar graph
poverty_df <- data.frame(
  Year = c("1990", "2000", "2010"),
  Poverty_Rate = poverty_means
)

# Create the bar graph using ggplot2
ggplot(poverty_df, aes(x = Year, y = Poverty_Rate)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Share of population below poverty line",
       y = "Poverty Rate",
       x = "Year")

#Save graph as .png 
ggsave("poorshareyears.png")


###### Social Capital ######

#Draw scatter plot with linear fit line
ggplot(data = losangelescounty) + geom_point(aes(x = mail_return_rate2010, y = kfr_pooled_pooled_p25)) +
  geom_smooth(aes(x = mail_return_rate2010, y = kfr_pooled_pooled_p25), method = "lm", se = F) +
  geom_text(aes(x = mail_return_rate2010, y = kfr_pooled_pooled_p25, label=tract), check_overlap = TRUE, size = 3)

#correlation coefficient of kfr_pooled_pooled_p25 and pm25_1990 at census tract level
cor(losangelescounty$mail_return_rate2010, losangelescounty$kfr_pooled_pooled_p25, use="complete.obs")

mean(chinatown$kfr_white_pooled_p25, na.rm = TRUE)
mean(chinatown$kfr_asian_pooled_p25, na.rm = TRUE)
mean(chinatown$kfr_pooled_pooled_p25, na.rm = TRUE)
