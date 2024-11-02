First, open up the parameter extraction file and set the working directory to wherever you want to store the data. Save the ButterflyLifeHistoryData file to the working directory and then run the ParameterExtraction script.

Now open up the PopulationModel script, specify the desired starting/initial population size (of adults, larva and eggs, default is 50 adults and 0 larva or eggs), the number of days you'd like the simulation to run for (default 300 days), and run the script.

The code should automatically generate (and save) some basic plots, which you can play around with, but will also save the total population data and adult population data for all the different treatment types to the working directory as a csv which you can use to generate plots however you like.
