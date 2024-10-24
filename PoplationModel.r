#Butterfly population model, overlapping generations (Pupation failure and time incorporated into larval stage), with no seasonality
#Split out mortality into that under control and that due to treatment, but unsure this was necessary

#inport parameters extracted from data
wd <- getwd();
DataInPath <- paste(wd, "\\Parameters.csv", sep = "");
Parameters <- read.csv(DataInPath, header = TRUE);

#Basic input parameters
Treatments <- Parameters$Treatment; #list of treatment types (with upper and lower confidence intervals as seperate treatments)
Treats <- c("Control", "Low", "Medium", "High"); #Our four treatment types
# or by concentration, giving |> Treatments <- c(0, 0.1, 1, 10)
reps <- 300; #number of days we run the model for
InitialEgg <- 0; #number of eggs we start the model with
InitialLarvae <- 0; #number of larvae we start the model with
InitialAdult <- 50; #number of Adults we start the model with
R <- 0.5; #Population sex ratio (assumed constant)

#Egg population data (matrix)
E <- matrix(ncol = length(Treatments), nrow = reps + 1);
colnames(E)<- Treatments;

#Larval population data (matrix)
L <- matrix(ncol = length(Treatments), nrow = reps + 1);
colnames(L)<- Treatments;

#Adult population data (matrix)
A <- matrix(ncol = length(Treatments), nrow = reps + 1);
colnames(A)<- Treatments;

#Total population size data (matrix)
N <- matrix(ncol = length(Treatments), nrow = reps + 1);
colnames(N)<- Treatments;

#Population change metrics from time t-1 to t as a function of population at time t-1
EggsLaid <- function(a) {a*lambda*R};
EggsDied <- function(e) {Me*e}; 
LarvaDied <- function(l) {Ml*l}; 
AdultDied <- function(a) {Ma*a};
EggsHatch <- function(e) {h*(e - Me*e)}; 
LarvaPupate <- function(l) {p*(l - Ml*l)}; 


EggMortality <- function(Treatment, MeControl) {
    Parameters$EggMortalityRate[Parameters$Treatment == Treatment]
}

LarvalMortality <- function(Treatment, MlControl) {
  Parameters$LarvaMortalityRate[Parameters$Treatment == Treatment]
}

AdultMortality <- function(Treatment, MaControl) {
  Parameters$AdultMortalityRate[Parameters$Treatment == Treatment]
}

LayingRate <- function(Treatment) {
    Parameters$LayingRate[Parameters$Treatment == Treatment] 
}

HatchRate <- function(Treatment) {
    Parameters$HatchingRate[Parameters$Treatment == Treatment]
}

PupationRate <- function(Treatment) {
    Parameters$PupationRate[Parameters$Treatment == Treatment]
}

for (i in 1:length(Treatments)) { #first outer loop to cycle through our different treatments
    Treatment <- Treatments[i]; #define which treatment we're using
    print(Treatment); #progress tracking

    #get treatment specific transition rates
    lambda <- LayingRate(Treatment);
    h <- HatchRate(Treatment);
    p <- PupationRate(Treatment);


    #get treatment specific mortality rates
    Me <- EggMortality(Treatment);
    Ml <- LarvalMortality(Treatment);
    Ma <- AdultMortality(Treatment);
    
    
    #starting population size
    e <- InitialEgg;
    l <- InitialLarvae;
    a <- InitialAdult;

    #inputting population size at t=0
    E[1, i] <- e;
    L[1, i] <- l;
    A[1, i] <- a;
    N[1, i] <- e + l + a;

    for (ii in 1:reps) { #inner loop calculating per day population size change
        #cat(ii, ", ", sep = ""); #progress tracking

        #change the population size
        EggChange <- EggsLaid(a) - EggsDied(e) - EggsHatch(e);
        LarvaChange <- EggsHatch(e) - LarvaDied(l) - LarvaPupate(l);
        AdultChange <- LarvaPupate(l) - AdultDied(a);
        
        e <- e + EggChange;
        l <- l + LarvaChange;
        a <- a + AdultChange;

        #store new population size values
        E[ii + 1, i] <- e;
        L[ii + 1, i] <- l;
        A[ii + 1, i] <- a;
        N[ii + 1, i] <- e + l + a;
    }
}

PopulationData <- as.data.frame(N); #making the total population size matrix a dataframe
AdultData <- as.data.frame(A); #making the total Adult population size matrix a dataframe

PopulationData$Day <- c(0:reps); #adding a column for the day
AdultData$Day <- c(0:reps); #adding a column for the day


## Making the plots
#making a function to make the plots
PlotFunction <- function(PlotData, Stage) {
Plot <- ggplot(PlotData, aes(x = Day)) +
  geom_line(aes(y = Control, colour = "Control"), lwd = LineWidth) +
  geom_ribbon(aes(ymin=ControlL, ymax = ControlU), lty = CItype, lwd = CIwidth, alpha = opacity, fill = ControlCol, colour = ControlCol) +
  geom_line(aes(y = Low, colour = "0.1ng/g"), lwd = LineWidth) +
  geom_ribbon(aes(ymin=LowL, ymax=LowU), lty = CItype, lwd = CIwidth, alpha = opacity, fill = LowCol, colour = LowCol) +
  geom_line(aes(y = Medium, colour = "1.0ng/g"), lwd = LineWidth) +
  geom_ribbon(aes(ymin=MediumL, ymax=MediumU), lty = CItype, lwd = CIwidth, alpha = opacity, fill = MediumCol, colour = MediumCol) +
  geom_line(aes(y = High, colour = "10ng/g"), lwd = LineWidth) +
  geom_ribbon(aes(ymin=HighL, ymax=HighU), lty = CItype, lwd = CIwidth, alpha = opacity, fill = HighCol, colour = HighCol) +
  theme_minimal() +
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  coord_cartesian(xlim= c(0, PlotTime), ylim= c(0, PlotData$Control[PlotData$Day==PlotTime] + 100) ) +
  labs(y = paste(Stage, " population size", sep=""),
       colour = "Treatment") +
  scale_colour_manual(values = colours)
}

#setting the colours (and other params) for the plots
ControlCol <- "black";
LowCol <- "yellow";
MediumCol <- "orange";
HighCol <- "red";

LineWidth <- 1;
CIwidth <- 0.5; #line width for CIs
CItype <- 2; #line type for CIs, 2 = dashed
opacity <- 0.05; 
PlotTime <- 10; #days plotted over

#making a vector for the legend
colours <- c("Control" = ControlCol, "0.1ng/g" = LowCol, "1.0ng/g" = MediumCol, "10ng/g" = HighCol);


#making a plot for the adult population
AdultPlot <- PlotFunction(AdultData, "Adult");
AdultPlot

#making a plot for the total population
PopulationPlot <- PlotFunction(PopulationData, "Total");
PopulationPlot

#exporting the plots
ggsave(paste("PopulationPlotDays", PlotTime, ".png", sep = ""), plot = PopulationPlot, device = png, width = 1800, height = 1000, units = "px");
ggsave(paste("AdultPlotDays", PlotTime, ".png", sep = ""), plot = AdultPlot, device = png, width = 1800, height = 1000, units = "px");


#exporting the total population size data
DataOutPath <- paste(wd, "\\ButterflySim", sep = "");
write.csv(PopulationData, paste(DataOutPath, "TotalPop.csv", sep = ""), row.names = FALSE);
write.csv(AdultData, paste(DataOutPath, "AdultPop.csv", sep = ""), row.names = FALSE);