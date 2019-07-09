
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

##############################################################################################################

# Install libraries
library(Rmisc); library(plyr); library(dplyr); library(ggplot2); library(reshape2); library(ggbiplot)

# set working directory
setwd("~/Google Drive/Ants-vs-Toads/Analysis-Matt/")

# import dataframe
df <- read.table("0-finalAntSpeciesCountsWithTraits.txt", header = T) 
head(df)

##############################################################################################################
# (1) Calculate linear selectivity metric for each ant species.
##############################################################################################################

# Selectivity Metrics (Linear selectivity = r - p)

preyprop = df$stomach / (sum(df$stomach)) # calculate proportion for prey species in stomach 
envprop = df$abundance / (sum(df$abundance)) # calculate proportion for prey species in environment 

Linear = (preyprop - envprop)

df$Linear <- Linear # adding selectivity metric to dataframe
head(df)

##############################################################################################################
# (2) Use environmental abundances to simulate null expectations of selectivity metric for ant species.
##############################################################################################################

species <- df$Ant_species
specieslist <- as.list(species)                # create list of ant species in the dataset
df2 <- cbind.data.frame(species, Linear)

head(df2)
df2[species=="Ectatomma_ruidum",]

for (i in 1:1000){                                          # loop over the # of simulations
  j <- sample(2000:5000,1)                                  # sets the number of individual samples to obtain for the simulation
  randomdraw <- sample(species, 
                       size = j, 
                       replace = TRUE, 
                       prob = envprop)                      # draw species from env. proportion with replacement
  df_randomdraw <- as.data.frame(table(randomdraw))         # make a dataframe of random draw species counts
  preyprop_sim <- df_randomdraw$Freq/j                      # simulated prey species proportions
  Linear_sim <- as.data.frame(preyprop_sim - envprop)       # simulated LinearSelectivity by subtracting REAL env_prop from SIMULATEd prey_prop + make dataframe
  colnames(Linear_sim) <- paste0("sim",i)                   # rename column from LinearS_sim --> sim1, sim2, sim3,...sim1000
  
  df2 <- cbind(df2,Linear_sim) # add the simulated dataset to the dataframe with species, LinearS, sim1, sim2,...etc....
  
}


head(df2)

L_simulations <- df2 

melted.df.L_real <- melt(L_simulations, id.vars = "species", measure.vars = c("Linear") )
melted.df.L_sim <- melt(L_simulations, id.vars = "species", measure.vars = paste0("sim",c(1:1000)) )

# Re-order from alphabetical to the ordered by "real" Linear Selectivity - apply to both datasets
melted.df.L_real$species <- factor(melted.df.L_real$species, levels = melted.df.L_real$species[order(melted.df.L_real$value)])
melted.df.L_sim$species <- factor(melted.df.L_sim$species, levels = melted.df.L_sim$species[order(melted.df.L_real$value)]) #order the simulated data by the real data

ggplot() +
  ggtitle("Null Linear S vs. stomach contents") +
  xlab("LinearS") +
  ylab("Ant species") +
  geom_point(data = melted.df.L_sim, aes(x = value, y = species), col = "gray") +
  geom_point(data = melted.df.L_real, aes(x = value, y = species), col = "red")


##############################################################################################################
# (3) Assign ant species to categories --> "preferred" "avoided" "neutral"
##############################################################################################################

LinearS_preference <- vector() # empty vector

#i <- "Anochetus_diegensis" #testing loop with species A. diegensis

for (i in species){
    loop.frame <- L_simulations[L_simulations$species == i,]
    S <- loop.frame$Linear
    loop.frame.sim <- loop.frame[,(-1:-2)]
    m <- min(loop.frame.sim)
    M <- max(loop.frame.sim)

    if (S < m) {
      temp <- "avoid"
      } else if (S > M) {
      temp <- "prefer"
      } else {
      temp <- "neutral"
      }

LinearS_preference[i]<-temp
}

L_simulations <- cbind(L_simulations,LinearS_preference)

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
  geom_point(data = melted.df.L_real, aes(x = value, y = species, col = LinearS_preference))+
  scale_color_manual(values=c("red", "black", "blue")) #avoid, null, prefer


df$Preference <- L_simulations$LinearS_preference # adding selectivity metric to dataframe

df$Preference <- L_simulations$LinearS_preference # adding selectivity metric to dataframe
df2analyze <- df %>% select(Ant_species, HW, HL, WL, Sculpture, Pilosity, Spines, HeadColor, Linear, Preference)

df2analyze


