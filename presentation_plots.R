raleigh_130 <- raleigh_data %>% 
  filter(speaker=='ral130', context=='nasal')

raleigh_395 <- raleigh_data %>% 
  filter(speaker=='ral395', context=='nasal') %>%
  mutate(vowel=factor(vowel, labels=c('V1', 'V2')))

means <- raleigh_395 %>%
  group_by(vowel) %>%
  summarise(F1=mean(F1), F2=mean(F2)) %>%
  ungroup()

raleigh_395 %>% 
  ggplot(aes(x=F2, y=F1)) +
  stat_ellipse(level=0.95, aes(color=vowel), show.legend=FALSE) +
  geom_point(alpha=0.5, aes(color=vowel), show.legend=FALSE) +
  geom_point(data=means, size=5, shape=21, aes(fill=vowel), show.legend=FALSE) +
  scale_x_reverse() + scale_y_reverse() + coord_fixed() +
  theme_bw() +
  theme(text=element_text(size=24),
        axis.text=element_text(size=24),
        plot.margin=margin(t=0, r=0, b=0, l=0, unit="pt"))
ggsave("empirical_distributions.png", 
       width=45, height=45, units = "mm")

m_F1 <- lm(F1 ~ vowel, data=raleigh_395)
m_F2 <- lm(F2 ~ vowel, data=raleigh_395)


F1_prediction <- plot(predict_response(m_F1, terms='vowel'), 
                      show_title=FALSE,
                      show_x_title=FALSE,
                      # show_y_title=FALSE,
                      connect_lines=TRUE) +
  scale_y_reverse() +
  theme_bw() +
  theme(text=element_text(size=24),
        axis.text=element_text(size=24),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        plot.margin=margin(t=0, r=0, b=2, l=0, unit="pt"))
F2_prediction <- plot(predict_response(m_F2, terms='vowel'), 
                      show_title=FALSE,
                      show_x_title=FALSE,
                      connect_lines=TRUE) +
  scale_y_reverse() +
  theme_bw() +
  theme(text=element_text(size=24),
        axis.text=element_text(size=24),
        plot.margin=margin(t=0, r=0, b=0, l=0, unit="pt"),
        axis.ticks.x=element_blank())

F1_prediction / F2_prediction +
  plot_annotation(theme=theme(
    plot.margin=margin(t=0, r=0, b=0, l=0, unit="null")))
ggsave("univariate_model_preds.png", 
       width=30, height=45, units = "mm")
