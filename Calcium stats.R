#ANOVA
#1-31-23
#Nikki Lukasko
#Ref: https://www.scribbr.com/statistics/anova-in-r/


library(ggplot2)
library(ggpubr)
library(tidyverse)
library(broom)
library(AICcmodavg)
library(readxl)


Calcium_data <- read_excel("C:/Users/nikki/Michigan State University/PSM.Hausbecklab - Nikki Lukasko - Nikki Lukasko/Botrytis/Calcium/Calcium data.xlsx", 
                           col_types = c("text", "text", "text", 
                                         "numeric", "numeric"))
View(Calcium_data)

Ca_data <- data.frame(Calcium_data)
Ca_data[is.na(Ca_data) | Ca_data=="Inf"] = NA


#one-way ANOVA
one.way <- aov(Disease_Severity ~ Treatment, data = Ca_data)
summary(one.way)

#two-way ANOVA
two.way <- aov(Disease_Severity ~ Trial + Crop, data = Ca_data)
summary(two.way)

#interaction
interaction <- aov(Disease_Severity ~ Trial*Crop, data = Ca_data)
summary(interaction)

#blocking (no blocks in the calcium trials)
#blocking <- aov(yield ~ fertilizer + density + block, data = crop.data)
#summary(blocking)


#Subset Petunia trials only

Ca_petunia <- subset(Ca_data, subset=Crop %in% "Petunia")
one.way.petunia <- aov(Disease_Severity ~ Treatment, data = Ca_petunia)
summary(one.way.petunia)
two.way.petunia <- aov(Disease_Severity ~ Trial + Treatment, data = Ca_petunia)
summary(two.way.petunia)
interaction.petunia <- aov(Disease_Severity ~ Trial*Treatment, data = Ca_petunia)
summary(interaction.petunia) #shows an interaction between trial and treatment: trials should be run separately


#Find the best model for your data
model.set.petunia <- list(one.way.petunia, two.way.petunia, interaction.petunia)
model.names.petunia <- c("one.way.petunia", "two.way.petunia", "interaction.petunia")
aictab(model.set.petunia, modnames = model.names.petunia)
#interaction.petunia is the best model based on output

Ca_petunia1 <- subset(Ca_petunia, subset=Trial %in% "1")
Ca_petunia2 <- subset(Ca_petunia, subset=Trial %in% "2")

one.way.petunia1 <- aov(Disease_Severity ~ Treatment, data = Ca_petunia1)
summary(one.way.petunia1)

#Check for homoscedasticity
par(mfrow=c(2,2))
plot(one.way.petunia1)
par(mfrow=c(1,1))

#If your model doesn’t fit the assumption of homoscedasticity, you can try the Kruskall-Wallis test instead.

#Tukey’s Honestly Significant Difference (Tukey’s HSD) post-hoc test for pairwise comparisons
tukey.petunia1 <-TukeyHSD(one.way.petunia1)
tukey.petunia1

tukey.plot.aov<-aov(Disease_Severity ~ Treatment, data=Ca_petunia1)
tukey.plot.test<-TukeyHSD(tukey.plot.aov)
plot(tukey.plot.test, las = 1)

mean.severity.data <- Ca_petunia %>%
  group_by(Trial, Treatment) %>%
  summarise(
    "Disease Severity" = mean(Disease_Severity)
  )

#how to add labels according to significant differences (in addition to error bars)
mean.yield.data$group <- c("a","bc","b","b","b","c")
mean.yield.data


Petunia_plot <- ggplot(Ca_petunia, aes(fill=Trial, y=Disease_Severity, x=Treatment)) + 
  geom_bar(position="dodge", stat="identity")


trt_order <- c('Noninoculated', 'Fungicide', 'Untreated', '300ppm', '750ppm', '1200ppm') 
#Ca_data$group <- c("a", "b",     #add as a label on the plot later?


#Bar plot without error bars

Ca_data_plot <- Ca_data %>%
  group_by(Trial, Treatment, Crop) %>%
  ggplot(aes(fill=Trial, y=Disease_Severity, x=factor(Treatment, level=trt_order))) + 
  geom_bar(position="dodge", stat="identity") +
  theme_light() +
  theme(strip.background = element_rect(fill="gray86", linewidth =1, linetype = "solid"),
        text = element_text(size = 12), 
        axis.text.x.bottom = element_text(size=12, color = "black", 
                                          margin = margin(t = 0, r = 0, b = 15, l = 0)),
        axis.title.y = element_text(size = 12, vjust = 2.5, color = "black"),
        panel.grid.major.x = element_blank(), panel.grid.major.y = element_line(color="black" ),
        panel.border = element_blank(),
        #axis.text.x=element_blank(),
        #axis.title.x=element_blank(),
        axis.ticks = element_blank(),
        strip.text = element_text(size = 12, color = "black")) +
  ylab("Disease Severity (0-100)") +
  xlab("Treatment") +
  facet_wrap(~Crop, ncol = 1) +
  scale_fill_manual(values = c("#336633", "#CC9966"))
Ca_data_plot




#Adding error bars to the plot

data_summary <- function(data, varname, groupnames){
  require(plyr)
  summary_func <- function(x, col){
    c(mean = mean(x[[col]], na.rm=TRUE),
      sd = sd(x[[col]], na.rm=TRUE))
}
  data_sum<-ddply(data, groupnames, .fun=summary_func,
                  varname)
  data_sum <- rename(data_sum, c("mean" = varname))
  return(data_sum)
}

Ca_data_sd <- data_summary(Ca_data, varname="Disease_Severity", 
                    groupnames=c("Crop", "Trial", "Treatment"))

# Convert treatment to a factor variable
Ca_data_sd$Treatment=as.factor(Ca_data_sd$Treatment)
head(Ca_data_sd)

#Graph with standard deviation- error bars
Ca_data_sd_plot <- Ca_data_sd %>%
  group_by(Trial, Treatment, Crop) %>%
  ggplot(aes(fill=Trial, y=Disease_Severity, x=factor(Treatment, level=trt_order))) + 
  geom_bar(position="dodge", stat="identity") +
  geom_errorbar(aes(ymin=Disease_Severity-sd, ymax=Disease_Severity+sd), width=.2,
                position=position_dodge(.9)) +
  facet_wrap(~Crop, ncol = 1) +
  theme_light() +
  theme(strip.background = element_rect(fill="gray86", linewidth =1, linetype = "solid"),
        text = element_text(size = 12), 
        axis.text.x.bottom = element_text(size=12, color = "black", 
                                          margin = margin(t = 0, r = 0, b = 15, l = 0)),
        axis.title.y = element_text(size = 12, vjust = 2.5, color = "black"),
        panel.grid.major.x = element_blank(), panel.grid.major.y = element_line(color="black" ),
        panel.border = element_blank(),
        #axis.text.x=element_blank(),
        #axis.title.x=element_blank(),
        axis.ticks = element_blank(),
        strip.text = element_text(size = 12, color = "black")) +
  ylab("Disease Severity (0-100)") +
  xlab("Treatment") 

Ca_data_sd_plot

#Graph with standard error- error bars (need to check SE calculations)
Ca_data_sd_plot <- Ca_data_sd %>%
  group_by(Trial, Treatment, Crop) %>%
  ggplot(aes(fill=Trial, y=Disease_Severity, x=factor(Treatment, level=trt_order))) + 
  geom_bar(position="dodge", stat="identity") +
  geom_errorbar(aes(ymin=Disease_Severity-(sd/sum(!is.na(Disease_Severity))),
                    ymax=Disease_Severity+(sd/sum(!is.na(Disease_Severity)))),
                width=.2,
                position=position_dodge(.9), na.rm = TRUE) +
  facet_wrap(~Crop, ncol = 1) +
  theme_light() +
  theme(strip.background = element_rect(fill="gray86", linewidth =1, linetype = "solid"),
        text = element_text(size = 12), 
        axis.text.x.bottom = element_text(size=12, color = "black", 
                                          margin = margin(t = 0, r = 0, b = 15, l = 0)),
        axis.title.y = element_text(size = 12, vjust = 2.5, color = "black"),
        panel.grid.major.x = element_blank(), panel.grid.major.y = element_line(color="black" ),
        panel.border = element_blank(),
        #axis.text.x=element_blank(),
        #axis.title.x=element_blank(),
        axis.ticks = element_blank(),
        strip.text = element_text(size = 12, color = "black")) +
  ylab("Disease Severity (0-100)") +
  xlab("Treatment") +
  scale_fill_manual(values = c("#336633", "#CC9966"))

summary(Ca_data_sd_plot)

Ca_data_sd %>%
  group_by(Trial, Treatment, Crop) %>%
  Ca_data_sd$Disease_Severity - (sd/count(!is.na(Ca_data_sd$Disease_Severity)))







