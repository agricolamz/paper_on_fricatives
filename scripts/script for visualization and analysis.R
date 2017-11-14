library(dplyr); library(readr); library(tidyr); library(ggplot2); library(stringr)

# upload and clean data ---------------------------------------------------
setwd("/home/agricolamz/_DATA/OneDrive1/_Work/Articles/2017 II s (with Inna Sieber)/data")
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
  #slice(1:136732*58) %>% 
  filter(language == "udmurt") %>%
  ggplot(aes(x = frequency, 
             y = power,
             color = token))+
  geom_line()+
  theme_bw()+
  theme(legend.position = "none")+
  labs(title = "LPC smoothing for different speakers")+
  facet_wrap(~dictor)+
  ylim(-20, 60)

cepstral_df %>% 
  ggplot(aes(x = frequency, 
             y = power,
             color = token))+
  geom_line()+
  theme_bw()+
  labs(title = "Cepstral smoothing for different speakers")+
  facet_wrap(~dictor)

LPC_df %>%
  filter(grepl("chukchi", language)) %>%
  ggplot(aes(x = frequency, 
             y = power))+
  stat_summary(fun.data ="mean_sdl", geom = "smooth")+
  theme_bw()+
  theme(legend.position = "none")+
  labs(title = "LPC smoothing for speakers of Chukchi")+
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


# NEW DATA ----------------------------------------------------------------
df <- read_tsv("CoG_results.csv")

df$utterance <- str_extract(df$value, "1|2|3|4|5|6|cf")
df$language <- str_extract(df$soundname, "kabardian|adyghe|nanai|udmurt|chukchi|russian|ubykh")
df$dictor <- str_extract(df$soundname, "_d.{1,2}")
df$dictor <- str_replace_all(df$dictor, "_", "")
df$stimulus <- str_replace_all(df$value, "[0-9]$", "")
df$stimulus <- str_replace_all(df$stimulus, "[0-9]$", "")
df$stimulus <- str_replace_all(df$stimulus, "_", "")
df$stimulus <- str_replace(df$stimulus, "cf$", "")
df$stimulus <- str_replace(df$stimulus, "cf_$", "")

stimuli <- read_tsv("stimuli.csv")

stimuli$language <- tolower(stimuli$language)

df %>% 
  left_join(stimuli) %>% 
  filter(dictor != "d32")->
  df

df %>% 
  ggplot(aes(dictor, cog, color = language))+
  geom_point()+
  theme_bw()+
  ggtitle("Center of Gravity") -> picture_1

df %>% 
  filter(position == "asa" |
  position == "əsa" |
  position == "isa" |
  position == "#sa" |
  position == "#sɐ") %>%
  ggplot(aes(dictor, cog, color = language))+
  geom_point()+
  theme_bw()+
  ggtitle("Center of Gravity") -> picture_2

df %>% 
  filter(position != "asa" |
           position != "əsa" |
           position != "isa" |
           position != "#sa" |
           position != "#sɐ") %>%
  ggplot(aes(dictor, cog, color = language))+
  geom_point()+
  theme_bw()+
  ggtitle("Center of Gravity") -> picture_3

df %>% 
  ggplot(aes(dictor, sd, color = language))+
  geom_point()+
  theme_bw()+
  ggtitle("Standard deviation")

df %>% 
  ggplot(aes(dictor, skewness, color = language))+
  geom_point()+
  theme_bw()+
  ggtitle("Skewness")

df %>% 
  ggplot(aes(dictor, kurtosis, color = language))+
  geom_point()+
  theme_bw()+
  ggtitle("Kurtosis")

write_tsv(df, "selection.tsv")
selected <- read_tsv("selection.tsv")


selected %>% 
  ggplot(aes(dictor, cog, color = language))+
  geom_point()+
  theme_bw()+
  ggtitle("Center of Gravity")

selected$cog_bark <- 13*atan(0.00076*selected$cog) +
  3.5*atan((selected$cog/7500)^2)

selected %>% 
  ggplot(aes(dictor, cog_bark))+
  geom_boxplot(aes(fill = language))+
  geom_point()+
  theme_bw()+
  labs(title = "Center of Gravity (in barks)",
       subtitle = "sample from all our data")

selected %>% 
  group_by(dictor) %>% 
  summarise(iqr = IQR(cog_bark)*1.5,
            median = median(cog_bark)) %>% 
  right_join(selected) ->
