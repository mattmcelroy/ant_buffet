# Toads versus Ants
# R analysis associated with Copeia publication "Ant morphology mediates diet preference in a neotropical toad". 
# Matt McElroy & David Donoso
# created: 25 April 2018
# updated: 9 July 2018

#GOALS:

# (1) Calculate linear selectivity metric for each ant species.
# (2) Use environmental abundances to simulate null expectations of selectivity metric for ant species.
# (3) Assign ant species into categories "preferred", "neutral" and "avoided".
# (4) Plot the ant species selectivity, null expectation, and color by preference. 
# (5) Export datafiles for downstream analysis.

##############################################################################################################

# Install libraries
library(Rmisc); library(plyr); library(dplyr); library(ggplot2); library(reshape2); library(ggbiplot)

# set working directory
# setwd("~/Google Drive/Ants-vs-Toads/Analysis-Matt/")

# import dataframe
df <- read.table("0-finalAntSpeciesCountsWithTraits.txt", header = T) 
head(df)

##############################################################################################################
# (1) Calculate linear selectivity metric for each ant species.
##############################################################################################################

# Selectivity Metric 
# Linear selectivity = (r - p)

preyprop = df$stomach / (sum(df$stomach)) # calculate proportion for prey species in stomach 
envprop = df$abundance / (sum(df$abundance)) # calculate proportion for prey species in environment 

Linear = (preyprop - envprop) # calculate selectivity for each ant species

df$Linear <- Linear # adding selectivity to dataframe
head(df)

##############################################################################################################
# (2a) Use environmental abundances to simulate null expectations of selectivity metric for ant species.
##############################################################################################################

species <- df$Ant_species # create species object (factor) for ease
df2 <- cbind.data.frame(species, Linear) # make a new df for simulated values
df2[species=="Ectatomma_ruidum",] # double check that values match species

for (i in 1:1000){                                          # number of loops
  j <- sample(2000:5000,1)                                  # number of ants sampled
  randomdraw <- sample(species,                             # draw species from env. proportion with replacement
                       size = j,
                       replace = TRUE, 
                       prob = envprop)                      
  df_randomdraw <- as.data.frame(table(randomdraw))         # dataframe of species counts
  preyprop_sim <- df_randomdraw$Freq/j                      # vector of simulated prey species proportions
  Linear_sim <- as.data.frame(preyprop_sim - envprop)       # simulated LinearSelectivity by subtracting REAL env_prop from SIMULATEd prey_prop + make dataframe
  colnames(Linear_sim) <- paste0("sim",i)                   # rename column from LinearS_sim --> sim1, sim2, sim3,...sim1000

  # add the simulated dataset to the dataframe with species, LinearS, sim1, sim2,...etc....  
  df2 <- cbind(df2,Linear_sim) 

}

# Check out the results
#head(df2)
df2[1:5,1:5]

##############################################################################################################
# (2b) Plot the null expectations and sampled selectivity metric for ant species.
##############################################################################################################

L_simulations <- df2 # rename df2 to Linear (Selectivity) Simulations

# Melt the dataframes for plotting
melted.df.L_real <- melt(L_simulations, id.vars = "species", measure.vars = c("Linear") )
melted.df.L_sim <- melt(L_simulations, id.vars = "species", measure.vars = paste0("sim",c(1:1000)) )

# Plotting the null distributions and sampled selectivity values
ggplot() +
  ggtitle("Null Distribution vs. Sampled Selectivity") +
  xlab("LinearS") +
  ylab("Ant species") +
  geom_point(data = melted.df.L_sim, aes(x = value, y = species), col = "gray") +
  geom_point(data = melted.df.L_real, aes(x = value, y = species), col = "red")

# Re-order the species names (factors) by "real" Linear Selectivity - apply to both datasets
melted.df.L_real$species <- factor(melted.df.L_real$species, levels = melted.df.L_real$species[order(melted.df.L_real$value)])
melted.df.L_sim$species <- factor(melted.df.L_sim$species, levels = melted.df.L_sim$species[order(melted.df.L_real$value)])

# Re-plot the graph but ordered by selectivity values...
ggplot() +
  ggtitle("Null Distribution vs. Sampled Selectivity") +
  xlab("LinearS") +
  ylab("Ant species") +
  geom_point(data = melted.df.L_sim, aes(x = value, y = species), col = "gray") +
  geom_point(data = melted.df.L_real, aes(x = value, y = species), col = "red")


##############################################################################################################
# (3) Assign ant species to categories --> "preferred" "avoided" "neutral"
##############################################################################################################

Linear_preference <- vector() # empty vector

i <- "Anochetus_diegensis" #testing loop with species A. diegensis

for (i in species){
    loop.frame <- L_simulations[L_simulations$species == i,] # subset df for given species
    S <- loop.frame$Linear # pull sampled linear selectivity value
    loop.frame.sim <- loop.frame[,(-1:-2)] # subset so only simulated values are left
    m <- min(loop.frame.sim) # minimum simulated value
    M <- max(loop.frame.sim) # maximum simulated value

    if (S < m) {
      temp <- "avoid"
      } else if (S > M) {
      temp <- "prefer"
      } else {
      temp <- "neutral"
      }

Linear_preference[i]<-temp
}

L_simulations <- cbind(L_simulations,Linear_preference)

head(L_simulations)


##############################################################################################################
# (4) Plot the ant species selectivity, null expectation, and color by preference. 
##############################################################################################################

# Visualize the plot
ggplot() +
  ggtitle("Selectivity colored by preference") +
  xlab("LinearS") +
  ylab("Ant species") +
  geom_point(data = melted.df.L_sim, aes(x = value, y = species), col = "gray") +
  geom_point(data = melted.df.L_real, aes(x = value, y = species, col = Linear_preference))+
  scale_color_manual(values=c("red", "black", "blue")) #avoid, null, prefer


df$Preference <- L_simulations$Linear_preference # adding selectivity metric to dataframe
head(df)

df2analyze <- df %>% select(Ant_species, HW, HL, WL, Sculpture, Pilosity, Spines, HeadColor, Linear, Preference)
df2analyze


##############################################################################################################
# Output files for downstream analysis
##############################################################################################################

# adding selectivity metric to dataframe
df$Preference <- L_simulations$Linear_preference

# remove nores, incidence, abundance, stomach...
# morphology dataset for all 84 species
df_84 <- df %>% select(Ant_species, HW, HL, WL, Sculpture, Pilosity, Spines, HeadColor, Linear, Preference)
# morphology + nuntrition dataset for 40 species
df_40 <- df %>% select(Ant_species, HW, HL, WL, Sculpture, Pilosity, Spines, HeadColor, Nitrogen, dN, Linear, Preference,)
df_40 <- na.omit(df_40)

write.table(df_84, file = "../FullSpeciesNoN.txt", sep = " ", quote = F, row.names = F, col.names = T)
write.table(df_40, file = "../ReducedSpeciesWithN.txt", sep = " ", quote = F, row.names = F, col.names = T)
