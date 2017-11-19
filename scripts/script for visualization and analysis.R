library(tidyverse)

# upload and clean data ---------------------------------------------------
setwd("/home/agricolamz/_DATA/OneDrive1/_Work/Articles/2017 II s (with Inna Sieber)/data")
LPC_df <- read_lines("LPC_results.csv")
LPC_df <- LPC_df[!str_detect(LPC_df, "freq\\(Hz\\)")]
write_lines(LPC_df, "LPC_results.csv")
rm(LPC_df)

LPC_df <- read_tsv("LPC_results.csv", col_names = FALSE)
names(LPC_df) <- c("Hertz", "power", "soundname", "token")

selected <- read_tsv("selection.tsv")

LPC_df %>% 
  semi_join(selected) ->
  LPC_df

write_lines(LPC_df, "LPC_df_selected.tsv")

LPC_df <- read_tsv("LPC_df_selected.tsv")

LPC_df$utterance <- str_extract(LPC_df$token, "1|2|3|4|5|6|cf")
LPC_df$language <- str_extract(LPC_df$soundname, "kabardian|adyghe|nanai|udmurt|chukchi|russian|ubykh")
LPC_df$dictor <- str_extract(LPC_df$soundname, "_d.{1,2}")
LPC_df$dictor <- str_replace_all(LPC_df$dictor, "_", "")
names(LPC_df)[1] <- "Hertz"
LPC_df$Bark <- 13*atan(0.00076*LPC_df$Hertz) +
  3.5*atan((LPC_df$Hertz/7500)^2)

# cepstral_df <- read_lines("cepstral_results.csv")
# cepstral_df <- cepstral_df[!str_detect(cepstral_df, "freq\\(Hz\\)")]
# write_lines(cepstral_df, "cepstral_results.csv")
# rm(cepstral_df)
# 
# cepstral_df <- read_tsv("cepstral_results.csv", col_names = FALSE)
# names(cepstral_df) <- c("Hertz", "power", "file_name", "token")
# 
# cepstral_df$utterance <- str_extract(cepstral_df$token, "1|2|3|4|5|6|cf")
# cepstral_df$language <- str_extract(cepstral_df$soundname, "kabardian|adyghe|nanai|udmurt|chukchi|russian|ubykh")
# cepstral_df$dictor <- str_extract(cepstral_df$soundname, "_d.{1,2}_")

# draw the plot -----------------------------------------------------------
LPC_df %>% 
  filter(language == "udmurt") %>%
  gather(key = measurement, value = value, c(Hertz, Bark)) %>% 
  ggplot(aes(x = value, 
             y = power,
             color = token))+
  geom_line()+
  theme_bw()+
  theme(legend.position = "none")+
  labs(title = "LPC smoothing for different Udmurt speakers")+
  facet_grid(dictor~measurement, scales = "free")

LPC_df %>% 
  filter(language == "adyghe") %>%
  gather(key = measurement, value = value, c(Hertz, Bark)) %>% 
  ggplot(aes(x = value, 
             y = power,
             color = token))+
  geom_line()+
  theme_bw()+
  theme(legend.position = "none")+
  labs(title = "LPC smoothing for different Adyghe speakers")+
  facet_grid(dictor~measurement, scales = "free")

LPC_df %>% 
  filter(language == "chukchi") %>%
  gather(key = measurement, value = value, c(Hertz, Bark)) %>% 
  ggplot(aes(x = value, 
             y = power,
             color = token))+
  geom_line()+
  theme_bw()+
  theme(legend.position = "none")+
  labs(title = "LPC smoothing for different Chukchi speakers")+
  facet_grid(dictor~measurement, scales = "free")

LPC_df %>% 
  filter(language == "nanai") %>%
  gather(key = measurement, value = value, c(Hertz, Bark)) %>% 
  ggplot(aes(x = value, 
             y = power,
             color = token))+
  geom_line()+
  theme_bw()+
  theme(legend.position = "none")+
  labs(title = "LPC smoothing for different Nanai speakers")+
  facet_grid(dictor~measurement, scales = "free")

LPC_df %>% 
  filter(language == "russian") %>%
  gather(key = measurement, value = value, c(Hertz, Bark)) %>% 
  ggplot(aes(x = value, 
             y = power,
             color = token))+
  geom_line()+
  theme_bw()+
  theme(legend.position = "none")+
  labs(title = "LPC smoothing for different Russian speakers")+
  facet_grid(dictor~measurement, scales = "free")

LPC_df %>% 
  filter(language == "ubykh") %>%
  gather(key = measurement, value = value, c(Hertz, Bark)) %>% 
  ggplot(aes(x = value, 
             y = power,
             color = token))+
  geom_line()+
  theme_bw()+
  theme(legend.position = "none")+
  labs(title = "LPC smoothing for Ubykh speaker")+
  facet_grid(dictor~measurement, scales = "free")

# cepstral_df %>% 
#   ggplot(aes(x = Hertz, 
#              y = power,
#              color = token))+
#   geom_line()+
#   theme_bw()+
#   labs(title = "Cepstral smoothing for different speakers")+
#   facet_wrap(~dictor)

LPC_df %>%
  filter(grepl("chukchi", language)) %>%
  ggplot(aes(x = Hertz, 
             y = power))+
  stat_summary(fun.data ="mean_sdl", geom = "smooth")+
  theme_bw()+
  theme(legend.position = "none")+
  labs(title = "LPC smoothing for speakers of Chukchi")+
  facet_wrap(~language)

# LPC_df %>%
#     ggplot(aes(x = `Hertz (Hz)`, 
#                y = `power (Db/Hz)`))+
#     stat_summary(fun.data ="mean_sdl", geom = "smooth")+
#     theme_bw()+
#     labs(title = "LPC smoothing for different speakers")

LPC_df %>%
  ggplot(aes(x = Hertz, 
             y = power))+
  geom_line()+
  geom_smooth(method = "lm",formula = y ~ poly(x, 7))
  

fit <- lm(Hertz~poly(power, 7), data = LPC_df)
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

df$cog_Bark <- 13*atan(0.00076*df$cog) +
  3.5*atan((df$cog/7500)^2)

df %>% 
  ggplot(aes(dictor, cog))+
  geom_boxplot(aes(fill = language))+
  geom_point()+
  theme_bw()+
  ggtitle("Center of Gravity (in Hertz)")

df %>% 
  ggplot(aes(dictor, cog_Bark))+
  geom_boxplot(aes(fill = language))+
  geom_point()+
  theme_bw()+
  ggtitle("Center of Gravity (in Bark)")




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
  filter(position == "asa" |
           position == "əsa" |
           position == "isa" |
           position == "#sa" |
           position == "#sɐ") %>%
  ggplot(aes(dictor, cog))+
  geom_boxplot(aes(fill = language))+
  geom_point()+
  theme_bw()+
  ggtitle("Center of Gravity (before [a]) in Hertz")

df %>% 
  filter(position == "asa" |
           position == "əsa" |
           position == "isa" |
           position == "#sa" |
           position == "#sɐ") %>%
  ggplot(aes(dictor, cog_Bark))+
  geom_boxplot(aes(fill = language))+
  geom_point()+
  theme_bw()+
  ggtitle("Center of Gravity (before [a]) in Bark")


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

selected <- read_tsv("selection.tsv")

selected$cog_Bark <- 13*atan(0.00076*selected$cog) +
  3.5*atan((selected$cog/7500)^2)

selected %>% 
  ggplot(aes(dictor, cog))+
  geom_boxplot(aes(fill = language))+
  geom_point()+
  theme_bw()+
  labs(title = "Center of Gravity", y = "Frequency (Hz)", x = "")+
  scale_fill_brewer(palette="Set1")+
  theme(legend.position="bottom")

selected %>% 
  ggplot(aes(dictor, cog_Bark))+
  geom_boxplot(aes(fill = language))+
  geom_point()+
  theme_bw()+
  labs(title = "Center of Gravity", y = "Bark", x = "")+
  scale_fill_brewer(palette="Set1")+
  theme(legend.position="bottom")

# get slopes and max ------------------------------------------------------

LPC_df %>% 
  filter(dictor == "d51") %>%
  gather(key = measurement, value = value, c(Hertz, Bark)) %>% 
  ggplot(aes(x = value, 
             y = power,
             color = token))+
  geom_line()+
  theme_bw()+
  labs(title = "LPC smoothing for different Udmurt speakers")+
  facet_grid(~measurement, scales = "free")

LPC_df %>% 
  filter(dictor == "d9",
         Bark > 10,
         Bark < 20.5) %>%
  group_by(token) %>% 
  summarise(min = which(power == min(power)),
            max = which(power == max(power)),
            min_Bark = Bark[which(power == min(power))],
            max_Bark = Bark[which(power == max(power))],
            Bark_slope = lm(power[min:max]~Bark[min:max])$coefficients[2]) %>% 
  select(token, max_Bark, Bark_slope) %>% 
  View()

LPC_df %>% 
  filter(dictor == "d9",
         Hertz > 1000,
         Hertz < 7000) %>%
  group_by(token) %>% 
  summarise(min = which(power == min(power)),
            max = which(power == max(power)),
            min_Hertz = Hertz[which(power == min(power))],
            max_Hertz = Hertz[which(power == max(power))],
            Hertz_slope = lm(power[min:max]~Hertz[min:max])$coefficients[2]) %>% 
  select(max_Hertz, Hertz_slope) %>% 
  View()



# analyse slopes ----------------------------------------------------------
selected %>% 
  ggplot(aes(slope_Bark, slope_Hertz, color = language)) +
  geom_point()+
  theme_bw()

selected %>% 
  ggplot(aes(max_Bark, max_Hertz, color = language)) +
  geom_point()+
  theme_bw()

selected %>% 
  ggplot(aes(dictor, max_Hertz))+
  geom_boxplot(aes(fill = language))+
  geom_point()+
  theme_bw()+
  labs(title = "Maximum of the first peak", y = "Frequency (Hertz)", x = "")+
  theme(legend.position="bottom")+
  scale_fill_brewer(palette="Set1")


selected %>% 
  ggplot(aes(dictor, max_Bark))+
  geom_boxplot(aes(fill = language))+
  geom_point()+
  theme_bw()+
  labs(title = "Slope", y = "Bark", x = "")+
  theme(legend.position="bottom")+
  scale_fill_brewer(palette="Set1")

selected %>% 
  ggplot(aes(dictor, slope_Hertz))+
  geom_boxplot(aes(fill = language))+
  geom_point()+
  theme_bw()+
  labs(title = "Slope", y = "Frequency (Hertz)", x = "")+
  theme(legend.position="bottom")+
  scale_fill_brewer(palette="Set1")

selected %>% 
  ggplot(aes(dictor, max_Bark))+
  geom_boxplot(aes(fill = language))+
  geom_point()+
  theme_bw()+
  labs(title = "Slope", y = "Bark", x = "")+
  theme(legend.position="bottom")+
  scale_fill_brewer(palette="Set1")

selected %>% 
  ggplot(aes(dictor, skewness))+
  geom_boxplot(aes(fill = language))+
  geom_point()+
  theme_bw()+
  labs(title = "Skewness", y = "skewness", x = "")+
  theme(legend.position="bottom")+
  scale_fill_brewer(palette="Set1")

selected %>% 
  ggplot(aes(dictor, kurtosis))+
  geom_boxplot(aes(fill = language))+
  geom_point()+
  theme_bw()+
  labs(title = "Kurtosis", y = "kurtosis", x = "")+
  theme(legend.position="bottom")+
  scale_fill_brewer(palette="Set1")



# PCA analysis ------------------------------------------------------------
pca_all <- prcomp(selected[,c(3:6, 14:18)], scale. = TRUE)
pca_bark <- prcomp(selected[,c(4:6, 14:15, 18)], scale. = TRUE)
pca_hertz <- prcomp(selected[,c(3:6, 16:17)], scale. = TRUE)

pca_importance <-
  data_frame(
    value = c(
      summary(pca_all)$importance[2, ],
      summary(pca_hertz)$importance[2, ],
      summary(pca_bark)$importance[2, ]
    ),
    pcas = c(
      names(summary(pca_all)$importance[2, ]),
      names(summary(pca_hertz)$importance[2, ]),
      names(summary(pca_bark)$importance[2, ])
    ),
    model = c(
      rep("all_variables", 9),
      rep("hertz_variables", 6),
      rep("bark_variables", 6)
    ),
    feature = "proportion of variance"
  )

cumulative_proportion <-
  data_frame(
    value = c(
      summary(pca_all)$importance[3, ],
      summary(pca_hertz)$importance[3, ],
      summary(pca_bark)$importance[3, ]
    ),
    pcas = c(
      names(summary(pca_all)$importance[3, ]),
      names(summary(pca_hertz)$importance[3, ]),
      names(summary(pca_bark)$importance[3, ])
    ),
    model = c(
      rep("all_variables", 9),
      rep("hertz_variables", 6),
      rep("bark_variables", 6)
    ),
    feature = "cumulative proportion"
  )

pca_imp <- rbind(pca_importance, cumulative_proportion)

pca_imp %>% 
  mutate(model = str_replace(model, "_", " ")) %>% 
  ggplot(aes(pcas, value, label = round(value, 2)))+
  geom_col(fill = "lightblue")+
  geom_text(aes(y = value + 0.05))+
  facet_grid(feature~model, scales = "free_x")+
  theme_bw()+
  labs(x = "", y = "",
       title = "Impact of each PC in different models")

# plot biplots -------------------------------------------------------------
rotations <- function(PC, x = "PC1", y = "PC2") {
  datapc <- data.frame(varnames = rownames(PC$rotation), PC$rotation)
  mult <- min((max(PC$x[, y]) - min(PC$x[, y]) / (max(datapc[, y]) - min(datapc[, y]))),
              (max(PC$x[, x]) - min(PC$x[, x]) / (max(datapc[, x]) - min(datapc[, x]))))
  datapc <- transform(datapc,
                      v1 = .7 * mult * (get(x)),
                      v2 = .7 * mult * (get(y)))}

biplot_labs <- function(PC, x = "PC1", y = "PC2") {
  paste0(names(summary(PC)$importance[2, c(x, y)]),
                 " (",
                 round(summary(PC)$importance[2, c(x, y)], 2) * 100,
                 "%)")
}

selected_all <- cbind(selected, pca_all$x)

selected_all %>% 
  ggplot(aes(x=PC1, y=PC2)) + 
  geom_hline(aes(yintercept = 0), size=.4, lty = 2)+
  geom_vline(aes(xintercept = 0), size=.4, lty = 2)+
  geom_point(aes(color = language)) +
  theme_bw()+
  coord_equal() + 
  geom_text(data=rotations(pca_all), 
            aes(x=v1, y=v2, label=varnames), size = 5, vjust=1, color="red")+
  geom_segment(data=rotations(pca_all),
               aes(x=0, y=0, xend=v1, yend=v2), arrow=arrow(length=unit(0.2,"cm")), alpha=0.75, color="red")+
  scale_color_brewer(palette="Set1")+
  labs(x = biplot_labs(pca_all)[1], y = biplot_labs(pca_all)[2],
       title = "PCA with all variables")+
  theme(legend.position="bottom")

selected_hertz <- cbind(selected, pca_hertz$x)

selected_hertz %>% 
  ggplot(aes(x=PC1, y=PC2)) + 
  geom_hline(aes(yintercept = 0), size=.4, lty = 2)+
  geom_vline(aes(xintercept = 0), size=.4, lty = 2)+
  geom_point(aes(color = language)) +
  theme_bw()+
  coord_equal() + 
  geom_text(data=rotations(pca_hertz), 
            aes(x=v1, y=v2, label=varnames), size = 5, vjust=1, color="red")+
  geom_segment(data=rotations(pca_hertz),
               aes(x=0, y=0, xend=v1, yend=v2), arrow=arrow(length=unit(0.2,"cm")), alpha=0.75, color="red")+
  scale_color_brewer(palette="Set1")+
  labs(x = biplot_labs(pca_hertz)[1], y = biplot_labs(pca_hertz)[2],
       title = "PCA with Hertz variables")+
  theme(legend.position="bottom")


selected_bark <- cbind(selected, pca_bark$x)

selected_bark %>% 
  ggplot(aes(x=PC1, y=PC2)) + 
  geom_hline(aes(yintercept = 0), size=.4, lty = 2)+
  geom_vline(aes(xintercept = 0), size=.4, lty = 2)+
  geom_point(aes(color = language)) +
  theme_bw()+
  coord_equal() + 
  geom_text(data=rotations(pca_bark), 
            aes(x=v1, y=v2, label=varnames), size = 5, vjust=1, color="red")+
  geom_segment(data=rotations(pca_bark),
               aes(x=0, y=0, xend=v1, yend=v2), arrow=arrow(length=unit(0.2,"cm")), alpha=0.75, color="red")+
  scale_color_brewer(palette="Set1")+
  labs(x = biplot_labs(pca_bark)[1], y = biplot_labs(pca_bark)[2],
       title = "PCA with Bark variables")+
  theme(legend.position="bottom")

# create ellipses ---------------------------------------------------------

selected_all %>% 
  ggplot(aes(x=PC1, y=PC2, color = language)) + 
  geom_hline(aes(yintercept = 0), size=.4, lty = 2)+
  geom_vline(aes(xintercept = 0), size=.4, lty = 2)+
  geom_point()+
  stat_ellipse()+
  theme_bw()+
  coord_equal() + 
  scale_color_brewer(palette="Set1")+
  labs(x = biplot_labs(pca_all)[1], y = biplot_labs(pca_all)[2],
       title = "PCA with all variables")+
  theme(legend.position="bottom")

selected_bark %>% 
  ggplot(aes(x=PC1, y=PC2, color = language)) + 
  geom_hline(aes(yintercept = 0), size=.4, lty = 2)+
  geom_vline(aes(xintercept = 0), size=.4, lty = 2)+
  geom_point()+
  stat_ellipse()+
  theme_bw()+
  coord_equal() + 
  scale_color_brewer(palette="Set1")+
  labs(x = biplot_labs(pca_bark)[1], y = biplot_labs(pca_bark)[2],
       title = "PCA with Bark variables")+
  theme(legend.position="bottom")

selected_hertz %>% 
  ggplot(aes(x=PC1, y=PC2, color = language)) + 
  geom_hline(aes(yintercept = 0), size=.4, lty = 2)+
  geom_vline(aes(xintercept = 0), size=.4, lty = 2)+
  geom_point()+
  stat_ellipse()+
  theme_bw()+
  coord_equal() + 
  scale_color_brewer(palette="Set1")+
  labs(x = biplot_labs(pca_hertz)[1], y = biplot_labs(pca_hertz)[2],
       title = "PCA with Hertz variables")+
  theme(legend.position="bottom")

selected_bark %>% 
  ggplot(aes(x=PC1, y=PC2, color = language)) + 
  geom_hline(aes(yintercept = 0), size=.4, lty = 2)+
  geom_vline(aes(xintercept = 0), size=.4, lty = 2)+
  geom_point()+
  stat_ellipse(aes(group = dictor))+
  theme_bw()+
  coord_equal() + 
  scale_color_brewer(palette="Set1")+
  labs(x = biplot_labs(pca_bark)[1], y = biplot_labs(pca_bark)[2],
       title = "PCA with Bark variables: ellipse for each speaker")+
  theme(legend.position="bottom")

selected_bark %>% 
  mutate(position = ifelse(position == "#sa", "#sa", "Vsa")) %>% 
  ggplot(aes(x=PC1, y=PC2, color = position)) + 
  geom_hline(aes(yintercept = 0), size=.4, lty = 2)+
  geom_vline(aes(xintercept = 0), size=.4, lty = 2)+
  geom_point()+
  stat_ellipse(aes(group = position))+
  theme_bw()+
  coord_equal() + 
  scale_color_brewer(palette="Set1")+
  labs(x = biplot_labs(pca_bark)[1], y = biplot_labs(pca_bark)[2],
       title = "PCA with Bark variables: ellipse for different phonological positions")+
  theme(legend.position="bottom")

# different utterance -----------------------------------------------------
selected_bark %>% 
  filter(utterance != "4") ->
  selected_bark_without_4
selected_bark_without_4 %>% 
  ggplot(aes(x=PC1, y=PC2)) + 
  geom_hline(aes(yintercept = 0), size=.4, lty = 2)+
  geom_vline(aes(xintercept = 0), size=.4, lty = 2)+
  geom_point(color = "grey")+
  geom_point(data = selected_bark_without_4[89:146,], aes(color = utterance))+
  stat_ellipse(data = selected_bark_without_4[89:146,], aes(color = utterance))+
  theme_bw()+
  coord_equal() + 
  scale_color_brewer(palette="Set1")+
  labs(x = biplot_labs(pca_bark)[1], y = biplot_labs(pca_bark)[2],
       title = "PCA with bark variables: different utterances for Russian speakers")+
  theme(legend.position="bottom")



# selected part -----------------------------------------------------------
selected_bark$label <- ""
selected_bark$label[c(61, 143, 25, 164, 169, 41)] <- c(
  "nəsasaqen (Chukchi)",
  "saxər (Russian)",
  "ɡiasa (Nanai)",
  "ħisap (Adyghe)",
  "sa (Ubykh)",
  "sajkok (Chukchi)"
)

selected_bark %>%   
  ggplot(aes(x=PC1, y=PC2, label = label)) + 
  geom_hline(aes(yintercept = 0), size=.4, lty = 2)+
  geom_vline(aes(xintercept = 0), size=.4, lty = 2)+
  coord_equal() + 
  geom_point(color = "grey")+
  geom_point(data = selected_bark[c(61, 143, 25, 164, 169, 41),])+
  geom_text(aes(y=PC2+0.4))+
  theme_bw()+
  labs(x = biplot_labs(pca_bark)[1], y = biplot_labs(pca_bark)[2],
       title = "Selected points from PCA (Bark)")


LPC_df %>% 
  semi_join(selected[c(61, 143, 25, 164, 169, 41),]) ->
  LPC_specific
replace(1:9, 1:2, "ss")
LPC_specific %>% 
  filter(token == "giasa_2" |
           token == "ħisap_2" |
           token == "sa_1" |
           token == "сахар_1" |
           token == "nəsasaqen_3" |
           token == "sajkok_2") %>% 
  mutate(token = replace(seq_along(token), seq_along(token),
                         rep(c("ɡiasa (Nanai)",
                               "sajkok (Chukchi)",
                               "nəsasaqen (Chukchi)",
                               "saxər (Russian)",
                               "ħisap (Adyghe)",
                                "sa (Ubykh)"), each = 4097))) %>% 
  gather(key = measurement, value = value, c(Hertz, Bark)) %>% 
  ggplot(aes(x = value, 
             y = power,
             color = token))+
  geom_line()+
  theme_bw()+
  theme(legend.position = "none") +
  facet_wrap(~token+measurement, scales = "free")

# map ---------------------------------------------------------------------

library(lingtypology)

map.feature(
  languages = c("Russian", "Adyghe", "Nanai", "Chukchi", "Udmurt", "Turkish"),
  label = c("русский", "адыгейский", "нанайский", "чукотский","бесермянский", "убыхский"),
  label.hide = FALSE
)


