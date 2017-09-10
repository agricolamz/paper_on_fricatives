library(tidyverse); library(stringr)

# upload and clean data ---------------------------------------------------
setwd("/home/agricolamz/_DATA/OneDrive1/_Work/Articles/2017 I s (with Inna Sieber)/github/data")
LPC_df <- read_lines("LPC_results.csv")
LPC_df <- LPC_df[!str_detect(LPC_df, "freq\\(Hz\\)")]
write_lines(LPC_df, "LPC_results.csv")
rm(LPC_df)

LPC_df <- read_tsv("LPC_results.csv", col_names = FALSE)
names(LPC_df) <- c("frequency (Hz)", "power (Db/Hz)", "file name", "token")

LPC_df$utterance <- str_extract(LPC_df$token, "1|2|3|cf")
LPC_df$language <- str_extract(LPC_df$`file name`, "kabardian|bzhedugh|shapsugh|dzhuen|najxin|russian|beserman|chukchi")
LPC_df$dictor <- str_extract(LPC_df$`file name`, "_d._")

cepstral_df <- read_lines("cepstral_results.csv")
cepstral_df <- cepstral_df[!str_detect(cepstral_df, "freq\\(Hz\\)")]
write_lines(cepstral_df, "cepstral_results.csv")
rm(cepstral_df)

cepstral_df <- read_tsv("cepstral_results.csv", col_names = FALSE)
names(cepstral_df) <- c("frequency (Hz)", "power (Db/Hz)", "file name", "token")

cepstral_df$utterance <- str_extract(cepstral_df$token, "1|2|3|cf")
cepstral_df$language <- str_extract(cepstral_df$`file name`, "kabardian|bzhedugh|shapsugh|dzhuen|najxin|russian|beserman|chukchi")
cepstral_df$dictor <- str_extract(cepstral_df$`file name`, "_d._")

# draw the plot -----------------------------------------------------------
LPC_df %>% 
  ggplot(aes(x = `frequency (Hz)`, 
             y = `power (Db/Hz)`,
             color = dictor))+
  geom_line()+
  theme_bw()+
  labs(title = "LPC smoothing for different speakers")+
  facet_wrap(~utterance)

cepstral_df %>% 
  ggplot(aes(x = `frequency (Hz)`, 
             y = `power (Db/Hz)`,
             color = dictor))+
  geom_line()+
  theme_bw()+
  labs(title = "Cepstral smoothing for different speakers")+
  facet_wrap(~utterance)
