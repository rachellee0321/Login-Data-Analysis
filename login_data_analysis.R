###################################################################
#
# Program:       Student Login Data EDA
#
###################################################################

rm(list=ls())         

# Load libraries 
library(haven)
library(dplyr)
library(wesanderson)
library(scales)
library(RColorBrewer)
library(ggplot2)
library(stringr)
library(tidyr)


#Set theme

theme1 <-  theme(axis.title=element_text(face='bold',size=15),
                 axis.text=element_text(size=15),
                 legend.title = element_text(size=12),
                 legend.text = element_text(size=12))

# Load Data


##########################################
####    1. Number of logins by hour                         
##########################################

# Convert "Hour" to factor variables so that every x tick can show up in the graphs -- this code is dirty
df_hourly$hour<-as.character(df_hourly$hour)
df_hourly$hour<- factor(df_hourly$hour,
                        levels=c("0","1","2","3","4","5","6","7","8", "9","10","11","12",
                                 "13","14","15","16","17","18","19","20","21","22","23"))

p1 <- ggplot(df_hourly)+
  geom_bar(aes(x=hour, y=..count..), fill = col_blue)+
  theme1 +
  ylab("Count") +
  scale_x_discrete("Hour of the day",
                   breaks=c("0","1","2","3","4","5","6","7","8", "9","10","11","12","13","14","15","16","17","18","19","20","21","22","23"),
                   labels=c("12AM","1","2","3","4","5","6","7","8", "9","10","11","12PM","1","2","3","4","5","6","7","8", "9","10","11")) 

plot(p1)
ggsave("hist_login_hourly.png")



##################################################
####    2. Number of days students logged in                
##################################################

p2 <- ggplot(df)+
  geom_bar(aes(x=days_login, y=..count..), fill = col_yellow)+
  theme1 +
  ylab("Number of Students") +
  scale_x_continuous("Number of Days Students Logged In",
                     breaks=seq(0,50,10),
                     labels=c("0","10","20","30","40","50+"))

plot(p2)
ggsave("histogram_days_login.png")




####################################################
####    3. Number of students logged in by date              
####################################################

# Assign weekend/weekday value to have different colors in the graph
df_daily$weekend[df_daily$day %in% c(0,6)] <- "Weekend"
df_daily$weekend[!df_daily$day %in% c(0,6)] <- "Weekday"

p3 <-  ggplot(df_daily)+
  geom_bar(aes(x=date, y=..count.., fill=weekend))+
  scale_fill_manual(values=c(col_yellow, col_darkblue))+
  ylab("Number of Students") +
  scale_x_date("Date",
               date_breaks="7 days",
               date_labels = "%b %e") +
  theme1 +
  theme(legend.title = element_blank(),
        legend.position = "bottom")

plot(p3)
ggsave("num_login_by_dates.png")


##################################################
#####   4.Scatterplot of logins ~ absences     
##################################################

# Cap absences at 40 - sometimes there are odd data values with very high value of absences
df$days_absent_capped40 <- df$days_absent
df$days_absent_capped40[df$days_absent >= 40] <- 40

p4<- ggplot(df,aes(y=weekdays_login,x=days_absent_capped40))+
  geom_jitter(alpha=0.15) +
  geom_smooth(method="lm", se=FALSE,size=2,color=col_tomato) +
  xlab("Days Absent (capped at 40)") +
  ylab("Number of Weekdays Students Logged In") +
  theme1

plot(p4)
ggsave("scatterplot_weekdays_login_absences.png")



#################################################
#####   5. Logins by student characteristics                
#################################################

# --- 5.1 Percent login ---#

stud_char_list <- c("Gender","Language Status", "Lunch Status", "Race","Disability Status","Chronically Absent in Current Year")

for (stud_char in stud_char_list){
  
  # Subset and order the subgroups
  plot_data <- subset(df_subgroup,group == stud_char)
  plot_data <- plot_data[order(plot_data$subgroup_order),]
  plot_data$subgroup_str <- factor(plot_data$subgroup_str, levels=plot_data$subgroup_str)

  # Create graph
  p5 <- ggplot(plot_data, aes(x=subgroup_str, y=pct_login)) +
    geom_bar(stat="identity",fill = col_blue) +
    scale_y_continuous(labels=percent_format(accuracy=1), # percent format
                       expand=c(0,0),                     # for a better layout
                       limits = c(0,1),                   # set range on y axis
                       breaks = c(0,.2,.4,.6,.8))+
    ylab("% of Students Who Logged In")+
    xlab(paste0(stud_char))+
    theme2+
    geom_text(label=paste0(round(plot_data$pct_login*100),"%"), nudge_y=0.03, size=5)

  # Plot and save
  plot(p5)
  ggsave(paste0("logins_by_",tolower(stud_char),".png"))

}

# ---5.2 pct login comparison: wweek4 vs week8 ---#

for (stud_char in stud_char_list){
  
  # subset and order the subgroups
  plot_data <- subset(df_subgroup,group == stud_char)
  plot_data <- plot_data[order(plot_data$subgroup_order),]
  plot_data$subgroup_str <- factor(plot_data$subgroup_str, levels=plot_data$subgroup_str)
  
  # Reorder race variable 
  if (stud_char == "Race"){
    plot_data <- plot_data[order(plot_data$cum_login_wk4),]
    plot_data$subgroup_str <- factor(plot_data$subgroup_str, levels=rev(plot_data$subgroup_str))
    
  }
  
  # Reorganize the dataframe so that 
  plot_data <-  plot_data %>%
    mutate(pct_diff = cum_login_wk8-cum_login_wk4) %>%
    select(cum_login_wk8, pct_diff, cum_login_wk4, n_subgroup, group, subgroup_str) %>%
    rename(week4=cum_login_wk4, week8=pct_diff) %>%
    gather(week, pct_login, week4:week8) 
  
  # Clean up some variables
  plot_data$label_y <- plot_data$pct_login 
  plot_data$label_y[plot_data$week=="week8"] <- plot_data$cum_login_wk8[plot_data$week=="week8"] 
  plot_data$n_stu <- plot_data$pct_login*plot_data$n_subgroup
  plot_data$week <- factor(plot_data$week, levels=c("week8", "week4"))
  plot_data$label <- paste0(round(plot_data$pct_login*100),"% (N=", round(plot_data$n_stu),")")
  
  # Create graphs
  p6 <- ggplot(plot_data, aes(x=subgroup_str, y=pct_login, fill=week)) +
    geom_bar(stat="identity") +
    scale_y_continuous("Percent of Students Who Logged In", labels=percent_format(accuracy=1), expand=c(0,0), limits = c(0,1),
                       breaks = c(0,.2,.4,.6,.8,1))+
    scale_fill_manual(labels=c("Additional Week 8", "Until Week 4"), values=c(col_darkblue, col_yellow))+
    ylab("% of Students Who Logged In")+
    xlab(paste0(stud_char))+
    theme2 +
    geom_text(aes(y=label_y), label=plot_data$label, 
              nudge_y = ifelse(plot_data$subgroup_str=="Ineligible" & plot_data$week=="week8", -0.015,-0.03), 
              size=ifelse(plot_data$group=="Race",4,5),
              color=ifelse(plot_data$week=="week4", "black","white"))
  
  # plot and save
  plot(p6)
  ggsave(paste0("pct_change_logins_by_",tolower(stud_char),".png"))
  
}




#####################################################
#### 6. Percent of Students who Log-in by week                      
#####################################################

# Reorganize data - get percent login for each week
df2 <-df %>%
  select(starts_with("login_wk")) %>%
  summarize_each(mean) %>%
  gather("login_wk","pct_login")


p7 <- ggplot(df2, aes(x=login_wk, y=pct_login)) +
  geom_line(aes(group=1), color=col_gray) +
  geom_point(size=5, color=col_blue) +
  scale_y_continuous("Percent of Students Logged in",
                     labels=percent_format(accuracy=1),
                     limits = c(0,.8))+
  scale_x_discrete("Weeks",
                   labels=c("1","2","3","4","5","6","7","8"))+
  theme1 +
  geom_text(label=paste0(round(df2$pct_login*100),"%"), nudge_y=0.03, size=5)

p7
ggsave("login_weekly.png")


