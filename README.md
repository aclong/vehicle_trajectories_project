# vehicle-trajectories-project
Looking into vehicle trajectory data, lane changes etc

This project is looking into visualising and quantifying vehicle trajectory available from https://data.transportation.gov/Automobiles/Next-Generation-Simulation-NGSIM-Vehicle-Trajector/8ect-6jqj


The Peachtree data has been found to have a error in the "Global_Time" variable so analysis is going to change to a different area until this has been sorted out - see the "data munging" script  for how the error was found.

Different Approaches possible:

 - Looking at individual behaviours (the finest grained x/y of individual vehicles)
      Dangerous drivers
      Driver "types"
      Efficiency of driving
      How close vehicles are to one another

 - Group behaviours (aggregating from individual sets)
      Congestion
      Are driver "types" using certain O/D pairs/routes?

 - Predictions
      How would lane/road changes effect behaviour?
      
