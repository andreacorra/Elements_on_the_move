
# ==============================================================================
# Element on the move
# Description: Sensitivity analysis for the Netlogo parameters
# ==============================================================================

# Step 1: create an nl object
# Load the necessary package(s)
library(nlrx)

# Looking at where java is on the system, can set filepath in the Terminal
system("echo $JAVA_HOME")
Sys.setenv(JAVA_HOME="/Library/Java/JavaVirtualMachines/openjdk11.jdk/Contents/Home")

# check again after this to see where it actually is - should match filepath above
print(Sys.getenv("JAVA_HOME"))
print(Sys.getenv("CLASS_PATH"))
print(Sys.getenv("JAVA_OPTS"))

# Migrate model----

# Set NetLogo installation path (adjust to your needs!):
# file path to NetLogo itself
netlogopath <- file.path("~path/to/your/data/directory/NetLogo 6.3.0")

# file path for the NetLogo model you want to use to run the sensitivity analysis
modelpath <- file.path("~path/to/your/data/directory/TestSA_JustGPS_7-25-25.nlogo")

# the file path where you want the outputs to populate
outpath <- file.path("~path/to/your/data/directory")

# Create the nl object
nl <- nl(nlversion = "6.3.0",
         nlpath = netlogopath,
         modelpath = modelpath,
         jvmmem = 4096) # Java virtual machine memory capacity in megabytes


# Step 2: Attach an experiment
# We vary all numeric model parameters to estimate their sensitivity on the defined output metrics.
# Thus, we define parameter ranges and distribution functions for all our numeric model parameters (variables = ).
# We set the runtime of the model (runtime = 8759 ticks) 
# And clarify that we want the measurements (metrics = ) to be taken at the end of each simulation run (evalticks = 8759)

nl@experiment <- experiment(expname = "TestSA_JustGPS", 
                            outpath = outpath,
                            repetition = 1,   
                            tickmetrics = "true",
                            idsetup = "setup",  
                            idgo = "go",        
                            runtime = 8759,
                            evalticks = 8759,
                            metrics=c("count patches with [net-n-changed > -500 and net-n-changed < -400]",
                                      "count patches with [net-n-changed >= -400 and net-n-changed < -300]",
                                      "count patches with [net-n-changed >= -300 and net-n-changed < -200]",
                                      "count patches with [net-n-changed >= -200 and net-n-changed < -100]",
                                      "count patches with [net-n-changed >= -100 and net-n-changed < 0]",
                                      "count patches with [net-n-changed >= 0 and net-n-changed < 1]",
                                      "count patches with [net-n-changed >= 1 and net-n-changed < 100]",
                                      "count patches with [net-n-changed >= 100 and net-n-changed < 200]",
                                      "count patches with [net-n-changed >= 200 and net-n-changed < 300]",
                                      "count patches with [net-n-changed >= 300 and net-n-changed < 400]",
                                      "count patches with [net-n-changed >= 400 and net-n-changed < 500]",
                                      "count patches with [net-n-changed >= 500 and net-n-changed < 600]",
                                      "count patches with [net-n-changed >= 600 and net-n-changed < 700]",
                                      "count patches with [net-n-changed >= 700 and net-n-changed < 800]",
                                      "count patches with [net-n-changed >= 800]"),
                            variables = list("winter-daily-maintenance" = list(min=15, max=19, step=1, qfun="qunif"),
                                             "summer-daily-maintenance" = list(min=22, max=26, step=1, qfun="qunif"),
                                             "summer-perc-retained" = list(min=0.20, max=0.22, step=0.1, qfun="qunif"),
                                             "winter-perc-retained" = list(min=0.16, max=0.18, step=0.1, qfun="qunif"),
                                             "patch-radius" = list(min=17, max=23, step=1, qfun="qunif")),
                            
                            
                            constants = list())

# Step 3: Attach a simulation design
# We use the simdesgin_morris() function to attach a Morris Sensitivity Analysis design.
# The morrislevels parameter sets the number of different values for each parameter (sampling density).
# The morrisr paramater sets the number of repeated samplings (sampling size). 
# The morrisgridjump parameter sets the number of levels that are increased/decreased for computing the elementary effects.
# Morris recommendation is to set this value to levels / 2.
# We can increase the nseeds parameter in order to perform multiple runs of the same parameter matrix with different random seeds. 
# The variation between those repetitions is an indicator of the stochasticity effects within the model.
# More information on the Morris specific parameters can be found in the description of the morris function in the sensitivity package (?morris).

nl@simdesign <- simdesign_morris(nl=nl,
                                 morristype="oat",
                                 morrislevels=8,
                                 morrisr=3,
                                 morrisgridjump=4,
                                 nseeds=1
)



check<-nl@simdesign@siminput

# Check to see if variables are valid 
eval_variables_constants(nl)



# Step 4: Run simulations
# To execute the simulations, we can use the function run_nl_all().
# Sensitivity analyses typically have many runs that need to be simulated, 
# Thus, we recommend parallelizing model runs by adjusting the future plan 

library(future)
plan(multisession, workers = 4)
results <- run_nl_all(nl)


# Step 5: Investigate output
# First, we need to attach the results to the nl object.

setsim(nl, "simoutput") <- results
saveRDS(nl, file.path(nl@experiment@outpath, "morris.rds"))

# After results have been attached, we can use the analyze_nl() 
# function to calculate morris sensetivity indices.

morris <- analyze_nl(nl)

# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
#### Output into CSV ####
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
write.csv(results,"~path/to/your/data/directory/SAResults_9-25-25.csv")

write.csv(morris,"~path/to/your/data/directory/SAMorris_9-25-25.csv")

