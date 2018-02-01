# What we run

source('src/Filter_Me.R')
source('src/my_loader.R')
my_loader(c("dplyr","ggplot2"))

filtered_50 <- Filter_Me(raw_50, f1:f50, 1, please.filter = T)

################ SUMMARY OF COVERAGE #################
A <- filtered_50 %>% group_by(label, strain) %>% tally() %>% rename(n_filtered=n)

B <- raw_50 %>% group_by(label, strain) %>% tally() %>% rename(n_raw=n)

Strain_recovery <- left_join(A,B) %>%
                    filter(label!="") %>%
                    mutate(percent_filtered = round(100 * n_filtered/n_raw, 2))

Strain_sum <- Strain_recovery %>%
  group_by(strain) %>%
  summarise(Total_filtered =sum(n_filtered), Total_raw=sum(n_raw)) %>%
  mutate(percent = round(100 * Total_filtered/Total_raw,2))


write.csv(Strain_recovery, "Strain_call_recovery.csv", row.names=F)
write.csv(Strain_sum, "Strain_total_recovery.csv", row.names=F)


###################################################################

# Interpolate Data #####
# This function wraps around Fit_row and applies it to the rows
# The idea is to give it the filtered_50 with the column vector and it will
# Handle the subset and (apply,1, FUN=...) call for you instead of having to 
# hard code everything
source('src/Interpolate_Me.R')
source('src/Fit_Row.R')

Interpolated_data <- Interpolate_Me(filtered_50, c(6:55), mymethod = 'splines')

######################

# Once the data has been interpolated, we prune


qq<-Prune_Me(filtered_50, Interpolated_data, 1,show.plot = F)

# This function outside here is temporary, it will be called by Prune_Me

QQ <- t(apply(qq, 1, function(y) copy_NA(y)))

##########

# Next step is adding adaptors for DTW algorithm ########

to.adapt <- data.frame(QQ, duration=filtered_50$duration)

# Remember! limit.rows argument is ONLY FOR DEBUGGING PURPOSES
# Also, this function has extra arguments for different types of data,
# be aware of what they do and what is your case

Adapted_50 <- add_adaptors(to.adapt,"f1:f50",adaptor.value = 0)

# Get all the extra columns you might need

Full_data <- data.frame(Adapted_50,
                        label=filtered_50$label,
                        strain=filtered_50$strain,
                        file=filtered_50$file)

Full_data <- data.frame(Adapted_50,
                        label=filtered_50$label)

# Fix unclear label
Full_data<-mutate(.data = Full_data, label=ifelse(label=="", "unclear", label))

# Data inspection with

shape_plot(Full_data, "start1:end10",tag = "label", sample.rows = 1000) + facet_wrap(~label)

### DTW with TADPOLE ######

set.seed(9696)

# we will partition the dataset to do dtw

to.dtw <- Adapted_50[sample(x = nrow(Adapted_50),
                            size = 2000),2:length(Adapted_50)]

pc.tadp <- tsclust(to.dtw, type = "tadpole", k = 20,
                   trace = TRUE,
                   control = tadpole_control(dc = 10000, window.size = 10L))

#this took ~ 1 minute to compute and produced this
# Time required for analysis:

#  user  system elapsed 
#  49.48    0.64   50.49 

# Cluster sizes with average intra-cluster distance:
  
#  size    av_dist
#1   192  73615.571
#2    40  36246.661
#3    19  25147.334
#4    56  74382.440
#5    17  99689.438
#6    44  97233.128
#7   140 127957.504
#8    19  54123.798
#9    15   9598.279
#10   61  29833.066
#11   28  31261.401
#12   78 104250.935
#13  197 111536.489
#14  301 110178.617
#15   73  38758.007
#16   37 132850.849
#17  240  79816.529
#18   34  66049.439
#19   37 209816.372
#20  372 122858.077
