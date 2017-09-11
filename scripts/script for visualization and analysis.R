library(tidyverse); library(stringr)

# upload and clean data ---------------------------------------------------
setwd("/home/agricolamz/_DATA/OneDrive1/_Work/Articles/2017 II s (with Inna Sieber)/github/data")
LPC_df <- read_lines("LPC_results.csv")
LPC_df <- LPC_df[!str_detect(LPC_df, "freq\\(Hz\\)")]
write_lines(LPC_df, "LPC_results.csv")
rm(LPC_df)

LPC_df <- read_tsv("LPC_results.csv", col_names = FALSE)
names(LPC_df) <- c("frequency", "power", "file_name", "token")

LPC_df$utterance <- str_extract(LPC_df$token, "1|2|3|4|5|6|cf")
LPC_df$language <- str_extract(LPC_df$file_name, "kabardian|adyghe|nanai|udmurt|chukchi|russian")
LPC_df$dictor <- str_extract(LPC_df$file_name, "_d.{1,2}_")

cepstral_df <- read_lines("cepstral_results.csv")
cepstral_df <- cepstral_df[!str_detect(cepstral_df, "freq\\(Hz\\)")]
write_lines(cepstral_df, "cepstral_results.csv")
rm(cepstral_df)

cepstral_df <- read_tsv("cepstral_results.csv", col_names = FALSE)
names(cepstral_df) <- c("frequency", "power", "file_name", "token")

cepstral_df$utterance <- str_extract(cepstral_df$token, "1|2|3|4|5|6|cf")
cepstral_df$language <- str_extract(cepstral_df$file_name, "kabardian|adyghe|nanai|udmurt|chukchi|russian")
cepstral_df$dictor <- str_extract(cepstral_df$file_name, "_d.{1,2}_")

# draw the plot -----------------------------------------------------------
LPC_df %>% 
  filter(language == "adyghe") %>%
  ggplot(aes(x = frequency, 
             y = power,
             color = token))+
  geom_line()+
  theme_bw()+
  theme(legend.position = "none")+
  labs(title = "LPC smoothing for different speakers")+
  facet_wrap(~dictor)

cepstral_df %>% 
  ggplot(aes(x = frequency, 
             y = power,
             color = token))+
  geom_line()+
  theme_bw()+
  labs(title = "Cepstral smoothing for different speakers")+
  facet_wrap(~dictor)

cepstral_df %>%
  ggplot(aes(x = frequency, 
             y = power,
             color = dictor))+
  stat_summary(fun.data ="mean_sdl", geom = "smooth")+
  theme_bw()+
  labs(title = "Cepstral smoothing for different speakers")+
  facet_wrap(~language)

LPC_df %>%
    ggplot(aes(x = `frequency (Hz)`, 
               y = `power (Db/Hz)`))+
    stat_summary(fun.data ="mean_sdl", geom = "smooth")+
    theme_bw()+
    labs(title = "LPC smoothing for different speakers")

LPC_df %>%
  ggplot(aes(x = frequency, 
             y = power))+
  geom_line()+
  geom_smooth(method = "lm",formula = y ~ poly(x, 7))
  

fit <- lm(frequency~poly(power, 7), data = LPC_df)
summary(fit)

