
library(readxl)
library(ggplot2)
library(dplyr)
library(tidyr)

library(officer)
library(rvg)

library(rethinking)

set.seed(12)

###prep data###

df = read.csv("mydata.csv")

df$System = factor(df$System, labels = c(1,2,3,4) )
df <- df %>%
  mutate(average = (Q1 + Q2 + Q3 + Q4 + Q5 + Q6 + Q7) / 7)
#this was added later and will break things below
df$centeredaverage = df$average - mean(df$average)
df$stdaverage = df$centeredaverage / sd(df$centeredaverage)

dfg = df %>%
  group_by(System) %>%
  summarise_at(vars(average), list(name = mean))

df_l = df %>% 
  select(-c("average","Rank")) %>% 
  pivot_longer(cols = c("Q1","Q2", "Q3", "Q4", "Q5", "Q6", "Q7"), names_to = "question", values_to = "answer" )

df_l$question =  factor(df_l$question)
df_l$answer = factor(df_l$answer)

dat <- list(
  S = df$System,
  Q1 = df$Q1,
  Q2 = df$Q2,
  Q3 = df$Q3,
  Q4 = df$Q4,
  Q5 = df$Q5,
  Q6 = df$Q6,
  Q7 = df$Q7,
  A = df$average
)

ppt <- read_pptx()



#####rank#####
rankplt = ggplot(df, aes(x = System, y = Rank)) +
  geom_jitter(alpha=0.5, size=2, shape=21, width=.1, height = 0)
editable_rank_graph <- dml(ggobj = rankplt)
ppt <- add_slide(ppt, layout = "Title and Content", master = "Office Theme")
ppt <- ph_with(ppt, value = editable_rank_graph, location = ph_location_type(type="body"))

####all Qs separate ####
ggplot(df_l, aes(x = System, y = answer, color = System)) +
  geom_jitter(width=.1, height = 0) +
  facet_grid(question ~ ., scales = "free_y")

ggplot(df_l, aes(x = System, y = as.numeric(answer), color = System)) +
  geom_jitter(width=.1, height = 0) +
  facet_grid(question ~ ., scales = "free_y") +
  guides(color = "none") +  # Remove the legend
  scale_x_discrete(
    labels = c("Baseline", "Naive Batch", "Advanced Batch", "Rolling Notes")
  ) +
  labs(y = "Rating", x = "Prompt Variant") +  # Change y-axis label
  scale_y_continuous(breaks = c(1, 3, 5))  # Set y-axis to only show 1, 3, 5

#####Q1#####
Q1plt = ggplot(df, aes(x = System, y = Q1)) +
  geom_jitter(alpha=0.5, size=2, shape=21, width=.1, height = 0)
editable_Q1_graph <- dml(ggobj = Q1plt)
ppt <- add_slide(ppt, layout = "Title and Content", master = "Office Theme")
ppt <- ph_with(ppt, value = editable_Q1_graph, location = ph_location_type(type="body"))

mQ1 <- ulam(
  alist(
    Q1 ~ dordlogit(phi,alpha),
    phi <- b_S[S]*S,
    b_S[S] ~ normal(0,0.5),
    alpha ~ normal(0,1)
  ) , data=dat , chains=4 , cores=4 )

precis(mQ1,2)
postQ1 <- extract.samples(mQ1)
testplt = plot_instr({dens(postQ1$b_S[,1], lwd=3,
                 col=2, xlab= "Posterior Q1" )
dens( postQ1$b_S[,2], lwd=3, col=4, add = TRUE)
dens( postQ1$b_S[,3], lwd=3, col=1, add = TRUE)
dens( postQ1$b_S[,4], lwd=3, col=3, add = TRUE)})

ppt <- add_slide(ppt, layout = "Title and Content", master = "Office Theme")
ppt <- ph_with(ppt, testplt, location = ph_location_type(type="body"))

#####Q2#####
Q2plt = ggplot(df, aes(x = System, y = Q2)) +
  geom_jitter(alpha=0.5, size=2, shape=21, width=.1, height = 0)
editable_Q2_graph <- dml(ggobj = Q2plt)
ppt <- add_slide(ppt, layout = "Title and Content", master = "Office Theme")
ppt <- ph_with(ppt, value = editable_Q2_graph, location = ph_location_type(type="body"))

mQ2 <- ulam(
  alist(
    Q2 ~ dordlogit(phi,alpha),
    phi <- b_S[S]*S,
    b_S[S] ~ normal(0,0.5),
    alpha ~ normal(0,1)
  ) , data=dat , chains=4 , cores=4 )

precis(mQ2,2)
postQ2 <- extract.samples(mQ2)
Q2densplt = plot_instr({dens(postQ2$b_S[,1], lwd=3,
                             col=2, xlab= "Posterior Q2" )
  dens( postQ2$b_S[,2], lwd=3, col=4, add = TRUE)
  dens( postQ2$b_S[,3], lwd=3, col=1, add = TRUE)
  dens( postQ2$b_S[,4], lwd=3, col=3, add = TRUE)})

ppt <- add_slide(ppt, layout = "Title and Content", master = "Office Theme")
ppt <- ph_with(ppt, Q2densplt, location = ph_location_type(type="body"))

#####Q3#####
Q3plt = ggplot(df, aes(x = System, y = Q3)) +
  geom_jitter(alpha=0.5, size=2, shape=21, width=.1, height = 0)
editable_Q3_graph <- dml(ggobj = Q3plt)
ppt <- add_slide(ppt, layout = "Title and Content", master = "Office Theme")
ppt <- ph_with(ppt, value = editable_Q3_graph, location = ph_location_type(type="body"))

mQ3 <- ulam(
  alist(
    Q3 ~ dordlogit(phi,alpha),
    phi <- b_S[S]*S,
    b_S[S] ~ normal(0,0.5),
    alpha ~ normal(0,1)
  ) , data=dat , chains=4 , cores=4 )

precis(mQ3,2)
postQ3 <- extract.samples(mQ3)
Q3densplt = plot_instr({dens(postQ3$b_S[,1], lwd=3,
                             col=2, xlab= "Posterior Q3" )
  dens( postQ3$b_S[,2], lwd=3, col=4, add = TRUE)
  dens( postQ3$b_S[,3], lwd=3, col=1, add = TRUE)
  dens( postQ3$b_S[,4], lwd=3, col=3, add = TRUE)})

ppt <- add_slide(ppt, layout = "Title and Content", master = "Office Theme")
ppt <- ph_with(ppt, Q3densplt, location = ph_location_type(type="body"))

#####Q4#####
Q4plt = ggplot(df, aes(x = System, y = Q4)) +
  geom_jitter(alpha=0.5, size=2, shape=21, width=.1, height = 0)
editable_Q4_graph <- dml(ggobj = Q4plt)
ppt <- add_slide(ppt, layout = "Title and Content", master = "Office Theme")
ppt <- ph_with(ppt, value = editable_Q4_graph, location = ph_location_type(type="body"))

mQ4 <- ulam(
  alist(
    Q4 ~ dordlogit(phi,alpha),
    phi <- b_S[S]*S,
    b_S[S] ~ normal(0,0.5),
    alpha ~ normal(0,1)
  ) , data=dat , chains=4 , cores=4 )

precis(mQ4,2)
postQ4 <- extract.samples(mQ4)
Q4densplt = plot_instr({dens(postQ4$b_S[,1], lwd=3,
                             col=2, xlab= "Posterior Q4" )
  dens( postQ4$b_S[,2], lwd=3, col=4, add = TRUE)
  dens( postQ4$b_S[,3], lwd=3, col=1, add = TRUE)
  dens( postQ4$b_S[,4], lwd=3, col=3, add = TRUE)})

ppt <- add_slide(ppt, layout = "Title and Content", master = "Office Theme")
ppt <- ph_with(ppt, Q4densplt, location = ph_location_type(type="body"))

#####Q5#####
Q5plt = ggplot(df, aes(x = System, y = Q5)) +
  geom_jitter(alpha=0.5, size=2, shape=21, width=.1, height = 0)
editable_Q5_graph <- dml(ggobj = Q5plt)
ppt <- add_slide(ppt, layout = "Title and Content", master = "Office Theme")
ppt <- ph_with(ppt, value = editable_Q5_graph, location = ph_location_type(type="body"))

mQ5 <- ulam(
  alist(
    Q5 ~ dordlogit(phi,alpha),
    phi <- b_S[S]*S,
    b_S[S] ~ normal(0,0.5),
    alpha ~ normal(0,1)
  ) , data=dat , chains=4 , cores=4 )

precis(mQ5,2)
postQ5 <- extract.samples(mQ5)
Q5densplt = plot_instr({dens(postQ5$b_S[,1], lwd=3,
                             col=2, xlab= "Posterior Q5" )
  dens( postQ5$b_S[,2], lwd=3, col=4, add = TRUE)
  dens( postQ5$b_S[,3], lwd=3, col=1, add = TRUE)
  dens( postQ5$b_S[,4], lwd=3, col=3, add = TRUE)})

ppt <- add_slide(ppt, layout = "Title and Content", master = "Office Theme")
ppt <- ph_with(ppt, Q5densplt, location = ph_location_type(type="body"))

#####Q6#####
Q6plt = ggplot(df, aes(x = System, y = Q6)) +
  geom_jitter(alpha=0.5, size=2, shape=21, width=.1, height = 0)
editable_Q6_graph <- dml(ggobj = Q6plt)
ppt <- add_slide(ppt, layout = "Title and Content", master = "Office Theme")
ppt <- ph_with(ppt, value = editable_Q6_graph, location = ph_location_type(type="body"))

mQ6 <- ulam(
  alist(
    Q6 ~ dordlogit(phi,alpha),
    phi <- b_S[S]*S,
    b_S[S] ~ normal(0,0.5),
    alpha ~ normal(0,1)
  ) , data=dat , chains=4 , cores=4 )

precis(mQ6,2)
postQ6 <- extract.samples(mQ6)
Q6densplt = plot_instr({dens(postQ6$b_S[,1], lwd=3,
                             col=2, xlab= "Posterior Q6" )
  dens( postQ6$b_S[,2], lwd=3, col=4, add = TRUE)
  dens( postQ6$b_S[,3], lwd=3, col=1, add = TRUE)
  dens( postQ6$b_S[,4], lwd=3, col=3, add = TRUE)})

ppt <- add_slide(ppt, layout = "Title and Content", master = "Office Theme")
ppt <- ph_with(ppt, Q6densplt, location = ph_location_type(type="body"))

#####Q7#####
Q7plt = ggplot(df, aes(x = System, y = Q7)) +
  geom_jitter(alpha=0.5, size=2, shape=21, width=.1, height = 0)
editable_Q7_graph <- dml(ggobj = Q7plt)
ppt <- add_slide(ppt, layout = "Title and Content", master = "Office Theme")
ppt <- ph_with(ppt, value = editable_Q7_graph, location = ph_location_type(type="body"))

mQ7 <- ulam(
  alist(
    Q7 ~ dordlogit(phi,alpha),
    phi <- b_S[S]*S,
    b_S[S] ~ normal(0,0.5),
    alpha ~ normal(0,1)
  ) , data=dat , chains=4 , cores=4 )

precis(mQ7,2)
postQ7 <- extract.samples(mQ7)
Q7densplt = plot_instr({dens(postQ7$b_S[,1], lwd=3,
                             col=2, xlab= "Posterior Q7" )
  dens( postQ7$b_S[,2], lwd=3, col=4, add = TRUE)
  dens( postQ7$b_S[,3], lwd=3, col=1, add = TRUE)
  dens( postQ7$b_S[,4], lwd=3, col=3, add = TRUE)})

ppt <- add_slide(ppt, layout = "Title and Content", master = "Office Theme")
ppt <- ph_with(ppt, Q7densplt, location = ph_location_type(type="body"))

######avg######
scatavg = ggplot(df, aes(x = System, y = average, fill= System)) +
  geom_jitter(alpha=0.5, size=2, shape=21, width=.1, height = 0)
editable_avg_graph <- dml(ggobj = scatavg)
ppt <- add_slide(ppt, layout = "Title and Content", master = "Office Theme")
ppt <- ph_with(ppt, value = editable_avg_graph, location = ph_location_type(type="body"))

m_avg <- quap(
  alist(
    A ~ dnorm(mu, sigma),
    mu <- a[S],
    a[S] ~ dnorm(0, 1),
    sigma ~ dunif(0,1)
  ), data = dat)

precis(m_avg, depth = 2)





post <- extract.samples(m_avg)

PI(post$a[,1])
PI(post$a[,2])
PI(post$a[,3])
PI(post$a[,4])

avg_dens_plt = plot_instr({dens(post$a[,1], lwd=3,
     col=2, xlab= "Posterior mean weight" )
dens( post$a[,2], lwd=3, col=4, add = TRUE)
dens( post$a[,3], lwd=3, col=1, add = TRUE)
dens( post$a[,4], lwd=3, col=3, add = TRUE)})

ppt <- add_slide(ppt, layout = "Title and Content", master = "Office Theme")
ppt <- ph_with(ppt, avg_dens_plt, location = ph_location_type(type="body"))

print(ppt, target = 'AthenaLLM_pLots.pptx')



#####complete ordered logit#####
datl <- list(
  S = df_l$System,
  A = df_l$answer,
  Q = df_l$question,
  P = df_l$ParticipantID
)
# Q7plt = ggplot(df, aes(x = System, y = Q7)) +
#   geom_jitter(alpha=0.5, size=2, shape=21, width=.1, height = 0)
# editable_Q7_graph <- dml(ggobj = Q7plt)
# ppt <- add_slide(ppt, layout = "Title and Content", master = "Office Theme")
# ppt <- ph_with(ppt, value = editable_Q7_graph, location = ph_location_type(type="body"))

set.seed(12)

mQs <- ulam(
  alist(
    A ~ dordlogit(phi,alpha),
    phi <- b_S[S]*S + b_Q[Q],
    b_S[S] ~ normal(0,0.5),
    b_Q[Q] ~ normal(0,0.5),
    alpha ~ normal(0,1)
  ) , data=datl , chains=4 , cores=4 )

precis(mQs,depth = 2, prob = 0.95)
postQs <- extract.samples(mQs)

dens(postQs$b_S[,1], lwd=3, col=2, xlab= "Posteriors", xlim = c(-3,4), ylim = c(0,3))
dens( postQs$b_S[,2], lwd=3, col=4, add = TRUE)
dens( postQs$b_S[,3], lwd=3, col=1, add = TRUE)
dens( postQs$b_S[,4], lwd=3, col=3, add = TRUE)

legend("topright", 
       legend = c("Baseline", "Naive Batch", "Advanced Batch", "Rolling Notes"),
       col = c(2, 4, 1, 3),
       lwd = 3,
       bty = "n")

Qsdensplt = plot_instr({dens(postQs$b_S[,1], lwd=3,
                             col=2, xlab= "Posterior Qs", xlim = c(-3,3))
  dens( postQs$b_S[,2], lwd=3, col=4, add = TRUE)
  dens( postQs$b_S[,3], lwd=3, col=1, add = TRUE)
  dens( postQs$b_S[,4], lwd=3, col=3, add = TRUE)})

ppt <- add_slide(ppt, layout = "Title and Content", master = "Office Theme")
ppt <- ph_with(ppt, Q7densplt, location = ph_location_type(type="body"))

