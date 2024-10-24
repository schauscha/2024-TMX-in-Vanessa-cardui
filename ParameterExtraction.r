#Extracting parameter values for Butterfly population simulation
# setwd("C:\\Users\\flinl\\Documents\\GitHub\\LarsButterfly")
#importing the data
wd <- getwd();
DataInPath <- paste(wd, "\\ButterflyLifeHistoryData.csv", sep = "");
ButterflyData <- read.csv(DataInPath, header = TRUE);

# What are we extracting?
    # Mortality (per day)
        # 3 life stages (Egg, Larva, Adult)
            #4 treatments (control, low, med, high)
                #Mean
                #95% Confidence intervals
    # Transition Rates (per day)
        # Eggs laid (per adult female, per day)
            #4 treatments (control, low, med, high)
                #Mean
                #95% Confidence intervals
        # Hatching rate (per day) - 1/(hatching age) |> we put this as 1, since we don't have any data (only recorded daily and had always hatched by then)
            #4 treatments (control, low, med, high)
                #Mean
                #95% Confidence intervals
        # Pupation rate (per day) - 1/(Emergence age - 1) ##Is 'Emergence Age' age since laying or since hatching? -going to assume since laying, but check with Lars
            #4 treatments (control, low, med, high)
                #Mean
                #95% Confidence intervals


#Our 4 treatment types (Control, Low, Medium, High), with the mean and both upper and lower confidence intervals.
Treatment <- c("Control", "ControlU", "ControlL", "Low", "LowU", "LowL", "Medium", "MediumU", "MediumL", "High", "HighU", "HighL");
ConfidenceInterval <- c("", "U", "L", "", "U", "L", "", "U", "L", "", "U", "L")

#Transition Rates (as a proportion of current individuals in a given stage which transition to the next)
#Data for eggs laid per day (per individual), including mean and 95% confidence intervals
LayMean <- ButterflyData$EggsLaidPerDay;
LayU <- ButterflyData$EggsLaidPerDay95CIUpper;
LayL <- ButterflyData$EggsLaidPerDay95CILower;
LayingRate <- c(LayMean[1], LayU[1], LayL[1], LayMean[2], LayU[2], LayL[2], LayMean[3], LayU[3], LayL[3], LayMean[4], LayU[4], LayL[4]);

#All eggs hatched within a day, no further data available (and no known differences between treatments)
HatchingRate <- rep(1, length(Treatment));

#Data for larval pupation rate (per day), with mean and 95% confidence intervals
PupMean <- 1 / (ButterflyData$EmergenceAge - 1); #because emergence age here includes hatching time we reduce it by 1 day
PupU <- 1 / (ButterflyData$EmergenceAge95CIUpper - 1);
PupL <- 1 / (ButterflyData$EmergenceAge95CILower - 1);
PupationRate <- c(PupMean[1], PupU[1], PupL[1], PupMean[2], PupU[2], PupL[2], PupMean[3], PupU[3], PupL[3], PupMean[4], PupU[4], PupL[4]);

#Mortality Rates (per day mortality)
#Data for Egg life stage (only lasts 1 day)
EggMort <- 1 - ButterflyData$HatchingRate / 100;
EggMortU <- 1 - ButterflyData$HatchingRate95CIUpper / 100;
EggMortL <- 1 - ButterflyData$HatchingRate95CILower / 100;
EggMortalityRate <- c(EggMort[1], EggMortU[1], EggMortL[1], EggMort[2], EggMortU[2], EggMortL[2], EggMort[3], EggMortU[3], EggMortL[3], EggMort[4], EggMortU[4], EggMortL[4]);

#Data for Larval life stage
LarvaMort <- (1 - ButterflyData$LarvalSurvival) / (ButterflyData$EmergenceAge - 1);
LarvaMortU <- (1 - ButterflyData$LarvalSurvival95CIUpper) / (ButterflyData$EmergenceAge95CIUpper - 1);
LarvaMortL <- (1 - ButterflyData$LarvalSurvival95CILower) / (ButterflyData$EmergenceAge95CILower - 1);
LarvaMortalityRate <- c(LarvaMort[1], LarvaMortU[1], LarvaMortL[1], LarvaMort[2], LarvaMortU[2], LarvaMortL[2], LarvaMort[3], LarvaMortU[3], LarvaMortL[3], LarvaMort[4], LarvaMortU[4], LarvaMortL[4]);


#Data for Adult life stage (all adults die), mean would probably be better than median (but it shouldn't matter)
AdultMort <- 1 / (ButterflyData$MedianAdultAge);
AdultMortU <- 1 / (ButterflyData$MedianAdultAge95CIUpper);
AdultMortL <- 1 / (ButterflyData$MedianAdultAge95CILower);
AdultMortalityRate <- c(AdultMort[1], AdultMortU[1], AdultMortL[1], AdultMort[2], AdultMortU[2], AdultMortL[2], AdultMort[3], AdultMortU[3], AdultMortL[3], AdultMort[4], AdultMortU[4], AdultMortL[4]);


#Put the vectors together into a dataframe
Parameters <- data.frame(Treatment, EggMortalityRate, LarvaMortalityRate, AdultMortalityRate, LayingRate, HatchingRate, PupationRate, ConfidenceInterval);

#now export the data
DataOutPath <- paste(wd, "\\Parameters.csv", sep = "");
write.csv(Parameters, DataOutPath, row.names = FALSE);