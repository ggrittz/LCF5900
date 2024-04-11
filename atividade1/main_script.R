#######################
#######################
##### ATIVIDADE 1 #####
#######################
#######################

#A standardized Pan-Amazonian dataset for tropical timber species https://doi.org/10.1002/ecy.4135

#Creating directory
file_path <- here::here("atividade1")
if (!dir.exists(file_path)) 
  dir.create(file_path)

#Downloading
file_name <- "MADERA.csv"
url <- "https://zenodo.org/records/7063870/files/MADERA_Herrera_Alvarez_et_al_2023_v1.csv?download=1"
download.file(url, file.path(file_path, file_name), mode = "wb")

#Loading
MADERA <- readr::read_csv(file.path(file_path, "MADERA.csv"))


################################
##### Exploratory bar plot #####
################################
#Getting frequencies
family_freq <- table(MADERA$Accepted_family)

#Ordering frequencies
family_orde <- sort(family_freq, decreasing = TRUE)

#Ordered barplot
barplot(family_orde, main = "Most common families", xlab = "Family", ylab = "Frequency")
barplot(family_orde[1:5], main = "Most common families", xlab = "Family", ylab = "Frequency")

#How much do they represent overall?
sum(family_orde[1:5])/sum(family_orde) #41% of all Amazon timber species

##### Some simple linear regression just for the sake... #####
lm_model <- lm(MADERA$estimated_ind_Amazon ~ MADERA$number_individuals_ATDN_plots)
#Summary
lm_summa <- summary(lm_model)

#Plot parameters
plot(x = MADERA$number_individuals_ATDN_plots, y = MADERA$estimated_ind_Amazon,
     pch = 19, cex = 0.5, 
     xlab = "Observed", 
     ylab = "Estimated", 
     main = "Observed vs. Estimated number of trees for each timber species")
abline(lm_model$coefficients, col = "red", lwd = 2)
text(x = 500, y = 4e+09, paste0("R² = ", round(lm_summa$adj.r.squared, 3)))


#######################
#######################
##### ATIVIDADE 2 #####
#######################
#######################

#Cleaning things
rm(list=ls(all=TRUE)); gc()

# Define working directory
#setwd("/cloud/project/LF5900")

# Load packages
if(!require(tidyverse))
  install.packages("tidyverse")
library(tidyverse)

#Define github URL where climate data from Piracicaba is stored
#Always change tree by blob
url_1 <- "https://github.com/FlorestaR/dados/blob/main/X_PIRACLIM"
xls_2 <- "DadosClima_Piracicaba.csv"
prm_3 <- "?raw=true"
gitFile <- file.path(url_1, xls_2) %>% paste0(prm_3)

# Downloads Excel table as a tibble (dataframe) from github
df <- read.csv(gitFile, sep=";")  %>% tibble()                 # import

# Show column names and first rows from the table
colnames(df)
head(df)
tail(df)

df <- df %>%
  drop_na() %>%
  mutate_if(is.character,as.numeric) %>%
  filter(TMED <50)
str(df)

df2 <- df %>%
  group_by(ANO) %>%
  summarise(mean_max = mean(TMAX))

lm_model <- lm(df2$mean_max ~ df2$ANO)
sum_lm_model <- summary(lm_model)

plot(x = df2$ANO, y = df2$mean_max, cex = 1.2, pch = 19,
     xlab = "Ano", ylab = "Média das máximas temperaturas no mês")
abline(lm_model, col = "red", lwd = 2)
text(x = 2003, y = 31, paste0("R² = ", round(sum_lm_model$r.squared, 3)))

#Agrupar estiagens
#Apenas onde ocorreu estiagem
df3 <- df[df$Estiagem != 0, ]

#Dias em estiagem
dias_estiagem <- sort(table(df3$Estiagem), descending = TRUE)
barplot(rev(dias_estiagem), 
        xlab = "Máximo de dias sem chuva", 
        ylab = "Frequência",
        main = "Frequência das estiagens entre 2000 e 2024",
        ylim = c(0, 500))


