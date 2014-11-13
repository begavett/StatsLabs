# If you make a mistake with the hrs.data$Simple variable, you can erase it and start over again using the following command 
# (remove the comment):
# hrs.data$Simple <- NULL

# From pages 2-3
hrs.data$Simple[hrs.data$FreqRS == "More than once a week" & hrs.data$Importance == "Not too important"] <- 0
hrs.data$Simple[hrs.data$FreqRS == "More than once a week" & hrs.data$Importance == "Somewhat important"] <- 1
hrs.data$Simple[hrs.data$FreqRS == "More than once a week" & hrs.data$Importance == "Very important"] <- 2
hrs.data$Simple[hrs.data$FreqRS == "Once a week" & hrs.data$Importance == "Not too important"] <- 3
hrs.data$Simple[hrs.data$FreqRS == "Once a week" & hrs.data$Importance == "Somewhat important"] <- 4
hrs.data$Simple[hrs.data$FreqRS == "Once a week" & hrs.data$Importance == "Very important"] <- 5
hrs.data$Simple[hrs.data$FreqRS == "Two or three times a month" & hrs.data$Importance == "Not too important"] <- 6
hrs.data$Simple[hrs.data$FreqRS == "Two or three times a month" & hrs.data$Importance == "Somewhat important"] <- 7
hrs.data$Simple[hrs.data$FreqRS == "Two or three times a month" & hrs.data$Importance == "Very important"] <- 8
hrs.data$Simple[hrs.data$FreqRS == "One or more times a year" & hrs.data$Importance == "Not too important"] <- 9
hrs.data$Simple[hrs.data$FreqRS == "One or more times a year" & hrs.data$Importance == "Somewhat important"] <- 10
hrs.data$Simple[hrs.data$FreqRS == "One or more times a year" & hrs.data$Importance == "Very important"] <- 11
hrs.data$Simple[hrs.data$FreqRS == "Not at all" & hrs.data$Importance == "Not too important"] <- 12
hrs.data$Simple[hrs.data$FreqRS == "Not at all" & hrs.data$Importance == "Somewhat important"] <- 13
hrs.data$Simple[hrs.data$FreqRS == "Not at all" & hrs.data$Importance == "Very important"] <- 14
hrs.data$Simple <- factor(hrs.data$Simple, 
                          levels = 0:14, 
                          labels = c(
                            "MT1W.NTI",
                            "MT1W.SI",
                            "MT1W.VI",
                            "OW.NTI",
                            "OW.SI",
                            "OW.VI",
                            "TTTM.NTI",
                            "TTTM.SI",
                            "TTTM.VI",
                            "OMTY.NTI",
                            "OMTY.SI",
                            "OMTY.VI",
                            "NAA.NTI",
                            "NAA.SI",
                            "NAA.VI"))

# From pages 3-4
c1 <- c(-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,4,4,4)
c2 <- c(-1,-1,-1,-1,-1,-1,-1,-1,-1,3,3,3,0,0,0)
c3 <- c(-1,-1,-1,-1,-1,-1,2,2,2,0,0,0,0,0,0)
c4 <- c(-1,-1,-1,1,1,1,0,0,0,0,0,0,0,0,0)
c5 <- c(0,0,0,0,0,0,0,0,0,0,0,0,2,-1,-1)
c6 <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,1,-1)
c7 <- c(0,0,0,0,0,0,0,0,0,2,-1,-1,0,0,0)
c8 <- c(0,0,0,0,0,0,0,0,0,0,1,-1,0,0,0)
c9 <- c(0,0,0,0,0,0,2,-1,-1,0,0,0,0,0,0)
c10 <- c(0,0,0,0,0,0,0,1,-1,0,0,0,0,0,0)
c11 <- c(0,0,0,2,-1,-1,0,0,0,0,0,0,0,0,0)
c12 <- c(0,0,0,0,1,-1,0,0,0,0,0,0,0,0,0)
c13 <- c(2,-1,-1,0,0,0,0,0,0,0,0,0,0,0,0)
c14 <- c(0,1,-1,0,0,0,0,0,0,0,0,0,0,0,0)

simple.effects <- cbind(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14)

contrasts(hrs.data$Simple) <- simple.effects


# From pages 4-5
I.2x2.1 <- with(hrs.data[(hrs.data$FreqRS == "More than once a week" | hrs.data$FreqRS == "Once a week") & 
                           (hrs.data$Importance == "Not too important" | hrs.data$Importance == "Somewhat important"),], 
                aov(MNSSCORE ~ FreqRS*Importance))
I.2x2.2 <- with(hrs.data[(hrs.data$FreqRS == "More than once a week" | hrs.data$FreqRS == "Two or three times a month") & 
                           (hrs.data$Importance == "Not too important" | hrs.data$Importance == "Somewhat important"),], 
                aov(MNSSCORE ~ FreqRS*Importance))
I.2x2.3 <- with(hrs.data[(hrs.data$FreqRS == "More than once a week" | hrs.data$FreqRS == "One or more times a year") & 
                           (hrs.data$Importance == "Not too important" | hrs.data$Importance == "Somewhat important"),], 
                aov(MNSSCORE ~ FreqRS*Importance))
I.2x2.4 <- with(hrs.data[(hrs.data$FreqRS == "More than once a week" | hrs.data$FreqRS == "Not at all") & 
                           (hrs.data$Importance == "Not too important" | hrs.data$Importance == "Somewhat important"),], 
                aov(MNSSCORE ~ FreqRS*Importance))
I.2x2.5 <- with(hrs.data[(hrs.data$FreqRS == "More than once a week" | hrs.data$FreqRS == "Once a week") & 
                           (hrs.data$Importance == "Not too important" | hrs.data$Importance == "Very important"),], 
                aov(MNSSCORE ~ FreqRS*Importance))
I.2x2.6 <- with(hrs.data[(hrs.data$FreqRS == "More than once a week" | hrs.data$FreqRS == "Two or three times a month") & 
                           (hrs.data$Importance == "Not too important" | hrs.data$Importance == "Very important"),], 
                aov(MNSSCORE ~ FreqRS*Importance))
I.2x2.7 <- with(hrs.data[(hrs.data$FreqRS == "More than once a week" | hrs.data$FreqRS == "One or more times a year") & 
                           (hrs.data$Importance == "Not too important" | hrs.data$Importance == "Very important"),], 
                aov(MNSSCORE ~ FreqRS*Importance))
I.2x2.8 <- with(hrs.data[(hrs.data$FreqRS == "More than once a week" | hrs.data$FreqRS == "Not at all") & 
                           (hrs.data$Importance == "Not too important" | hrs.data$Importance == "Very important"),], 
                aov(MNSSCORE ~ FreqRS*Importance))
I.2x2.9 <- with(hrs.data[(hrs.data$FreqRS == "More than once a week" | hrs.data$FreqRS == "Once a week") & 
                           (hrs.data$Importance == "Somewhat important" | hrs.data$Importance == "Very important"),], 
                aov(MNSSCORE ~ FreqRS*Importance))
I.2x2.10 <- with(hrs.data[(hrs.data$FreqRS == "More than once a week" | hrs.data$FreqRS == "Two or three times a month") & 
                            (hrs.data$Importance == "Somewhat important" | hrs.data$Importance == "Very important"),], 
                 aov(MNSSCORE ~ FreqRS*Importance))
I.2x2.11 <- with(hrs.data[(hrs.data$FreqRS == "More than once a week" | hrs.data$FreqRS == "One or more times a year") & 
                            (hrs.data$Importance == "Somewhat important" | hrs.data$Importance == "Very important"),], 
                 aov(MNSSCORE ~ FreqRS*Importance))
I.2x2.12 <- with(hrs.data[(hrs.data$FreqRS == "More than once a week" | hrs.data$FreqRS == "Not at all") & 
                            (hrs.data$Importance == "Somewhat important" | hrs.data$Importance == "Very important"),], 
                 aov(MNSSCORE ~ FreqRS*Importance))
I.2x2.13 <- with(hrs.data[(hrs.data$FreqRS == "Once a week" | hrs.data$FreqRS == "Two or three times a month") & 
                            (hrs.data$Importance == "Not too important" | hrs.data$Importance == "Somewhat important"),], 
                 aov(MNSSCORE ~ FreqRS*Importance))
I.2x2.14 <- with(hrs.data[(hrs.data$FreqRS == "Once a week" | hrs.data$FreqRS == "One or more times a year") & 
                            (hrs.data$Importance == "Not too important" | hrs.data$Importance == "Somewhat important"),], 
                 aov(MNSSCORE ~ FreqRS*Importance))
I.2x2.15 <- with(hrs.data[(hrs.data$FreqRS == "Once a week" | hrs.data$FreqRS == "Not at all") & 
                            (hrs.data$Importance == "Not too important" | hrs.data$Importance == "Somewhat important"),], 
                 aov(MNSSCORE ~ FreqRS*Importance))
I.2x2.16 <- with(hrs.data[(hrs.data$FreqRS == "Two or three times a month" | hrs.data$FreqRS == "One or more times a year") & 
                            (hrs.data$Importance == "Not too important" | hrs.data$Importance == "Somewhat important"),], 
                 aov(MNSSCORE ~ FreqRS*Importance))
I.2x2.17 <- with(hrs.data[(hrs.data$FreqRS == "Two or three times a month" | hrs.data$FreqRS == "Not at all") & 
                            (hrs.data$Importance == "Not too important" | hrs.data$Importance == "Very important"),], 
                 aov(MNSSCORE ~ FreqRS*Importance))
I.2x2.18 <- with(hrs.data[(hrs.data$FreqRS == "One or more times a year" | hrs.data$FreqRS == "Not at all") & 
                            (hrs.data$Importance == "Not too important" | hrs.data$Importance == "Very important"),], 
                 aov(MNSSCORE ~ FreqRS*Importance))

summary(I.2x2.1)
summary(I.2x2.2)
summary(I.2x2.3)
summary(I.2x2.4)
summary(I.2x2.5)
summary(I.2x2.6)
summary(I.2x2.7)
summary(I.2x2.8)
summary(I.2x2.9)
summary(I.2x2.10)
summary(I.2x2.11)
summary(I.2x2.12)
summary(I.2x2.13)
summary(I.2x2.14)
summary(I.2x2.15)
summary(I.2x2.16)
summary(I.2x2.17)
summary(I.2x2.18)
