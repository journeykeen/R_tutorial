#Author: Richard J. Liu
#Date: 5/18/26
#Script to replicate figures in Black-White income gap, Gubbay and McKay (For All, Fall 2024) 
# https://www.minneapolisfed.org/article/2024/the-growing-income-gap-for-black-workers

#load our libraries
library(data.table)
library(ggplot2)
library(stringr)

#set working directory
setwd("ENTER YOUR DATA DIRECTORY HERE")

##################Part 1: "The Black-White earnings gap, 2005-2019"


#load in data
pctl <- fread(paste0(getwd(),'/pctl_of_inc_bee.csv'))

#Filter for data of interest, drop inflation-adjusted columns 
fig1 <- pctl[inc_var == 'TC' & samp == 'prime_age_working_w2' & group_var == 'xredXsex' & geo_var_val == 0L]

#Create new column, 'relative_value', which is the percentile value compared to the equivalent White Male statistic
fig1[,pctl := gsub("pctl","",pctl)][,pctl:=gsub("_",".",pctl)]
fig1 <- fig1[group_var_val == 'NH_White_Male',.(year,pctl, value_white_men = value)][fig1,on = c("year","pctl")]
#We use some fancy data.table syntax here. We do a one-to-many merge here, where the `one` is the value of the white male percentile (value_white_men), and many are the corresponding rows by percentile and year in the `fig1` table
fig1[1] # you can observe the result of the merge with this line

setcolorder(fig1,"value_white_men",after=ncol(fig1))
fig1[,relative_value := value/value_white_men]

#chart relative value of Black Male, Female earnings percentiles to White Male earnings percentiles between 2005 and 2019. 
pctls <- str_split_1("25 50 90 95 98 99 99.9"," ")
label <- setNames(c("Black female earnings compared with White male earnings", "Black male earnings compared with White male earnings") ,
                  c("NH_Black_Female","NH_Black_Male"))

ggplot(fig1[pctl %in% pctls & group_var_val %in% c("NH_Black_Female","NH_Black_Male")], 
       aes(x = year, y = relative_value, color = pctl)) +
  geom_line() +
  facet_wrap(~label[group_var_val]) +
  scale_y_continuous(labels = scales::percent) +
  ggtitle("The Black-White earnings gap, 2005-2019") +
  ylab("")

#Note: 
  #I create the 'label' vector above in the 'label <- setNames(...'. This is a named vector, which means that its indices correspond to 'names' that I have set. 
  #What I do in the 'facet_wrap(...)' line is to retrieve the label corresponding to the appropriate 'group_var_val'. 
  #you could just create a label column in the `fig1` table, but this way saves memory. 

##################Part 2: "The change in earnings gap between White men and other racial and ethnic groups, 2009-2019"

#subset relative values to 2009, 2019 values
fig2 <- fig1[year %in% c(2009,2019)]

#create change in earnings gap - % change between relative value of 2019 to 2009 by race/sex/percentile
setorder(fig2,"group_var_val","pctl","year") #here, I sort the table `fig2` within group_var_val, pctl, by year.
fig2[,chg_earnings_gap := (relative_value/shift(relative_value,type = "lag",n = 1L))-1, by = c("group_var_val","pctl")]
#^Now that the table is sorted in chronological order, I can create a `change` variable that takes the difference between adjacent rows, corresponds to the change in the values over time. 

#create labels for plotting
fig2[,xsex := fifelse(grepl("Female",group_var_val),"Female","Male")]
label_gvv <- setNames(
  as.vector(outer(str_split_1("AIAN Asian Black Hispanic NHOPI White Other"," "),c("men","women"),paste)),
  as.vector(outer(str_split_1("NH_AIAN NH_Asian NH_Black Hispanic NH_NHOPI NH_White NH_Other"," "),c("_Male","_Female"),paste0))
)
label_facet <- setNames(
  c("Change in earnings gap between White men and men of other racial/ethnic groups", "Change in earnings gap between White men and women of other racial/ethnic groups"),
  c("Male","Female")
)

#plot change in earnings gap at the 50,90,98th percentiles by race/sex
ggplot(fig2[pctl %in% c(50,90,98) & group_var_val %notin% c("NH_White_Male","NH_Other_Female","NH_Other_Male")], 
       aes(x = label_gvv[group_var_val], y = chg_earnings_gap, fill = pctl)) +
  geom_col(position="dodge") +
  facet_wrap(~label_facet[xsex],nrow = 2, scales = "free_x") +
  xlab("") +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "The change in earnings gap between White men and other racial and ethnic groups",
       subtitle = "2009-2019")


##################Part 3: "Median earnings growth for Black and White workers, 2014-2019"

#load in IDDA income changes module
changes <- fread(paste0(getwd(),"/inc_change_distributions_bee.csv"))
#filter to data we want - 2014-2019 changes, prime aged and w2 sample
fig3 <- changes[y0 == 2014 & lag == 5 & inc_var == 'TC' & samp == 'prime_age_working_w2']
#adjust annualized values to be totals
fig3 <- fig3[,value := value * lag]

#add labels for plotting
pctl_label <- setNames( 
  c("<25th","25th to 50th","50th to 75th","75th to 90th",">90th"),
  str_split_1("lt25 25t50 50t75 75t90 gt90"," "))

gvv_label <- setNames(
  c("Black earners","White earners"),
  c("NH_Black","NH_White")
)
level_order <- c("<25th","25th to 50th","50th to 75th","75th to 90th",">90th")

#Draw plots for median earnings changes between 2014 and 2019 
ggplot(fig3[group_var_val %in% c("NH_Black","NH_White") & pctl_y1 == "50"],
       aes(x = pctl_label[pctl_y0], y = value, fill = gvv_label[group_var_val])) +
  geom_col(position = "dodge", width = 0.8) +
  xlab("Percentile of income distribution in 2014") +
  scale_y_continuous(labels = scales::dollar) +
  scale_x_discrete(limits = level_order) +
  ggtitle("Median earnings growth for Black and White workers, 2014-2019") +
  labs(fill = "")



