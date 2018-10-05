######
#test statistics
######
library(tidyverse)

births <- read_csv("../data/08e1DayOfBirth.csv")

expected <- sum(births$`Number of births`)/nrow(births)

ggplot(births,
       mapping = aes(x = Day,
                     y = `Number of births`)) +
  geom_col() +
  geom_hline(yintercept = expected, lty = 2, color = "red")

#use a chi square test
chisq.test(births$`Number of births`)
#will calculate a naieve expectation (as we did manually)

#chi squared test with a non null expectation
#expecations not just a single number

extinct <- read_csv("../data/08e6MassExtinctions.csv")

ggplot(extinct, mapping = aes(x = `Number of extinctions`,
                              y = Frequency)) +
  geom_col()
#lets assume this follows some statistical distribution
#negative binomials
#clustered data at the front, long tail, but discrete
#what's the probability of each value with a negative binomial distribution

#calculate some expectations (ie make a prob density)
freqs <- dnbinom(0:20, mu = 3.6, size = 3)
#rescale these to one, make sure they all sum to one
freqs<- freqs/sum(freqs)

extinct <- extinct %>%
  #probability times total number of events to generate an 
  #expected value for each number of extinctions
  mutate(Expected = freqs * sum(Frequency))

#plot
ggplot(extinct, mapping = aes(x = `Number of extinctions`,
                              y = Frequency)) +
  geom_col() +
  geom_point(mapping = aes(y = Expected), color = "red")

#do the chi squared test
chisq.test(extinct$Frequency, p = freqs)


#general workflow
#load data
#calculate expected values
#visualize
#do chi square test




###powerball example
#load
powerball <- read_csv("../data/08q02Powerball.csv")

expected_powerball <- sum(powerball$`Millions of tickets sold`/nrow(powerball))
ggplot(data = powerball, mapping = aes(x = Day, 
                                       y = `Millions of tickets sold`)) +
  geom_col() +
  geom_hline(yintercept = expected_powerball, color = "Red")

chisq.test(powerball$`Millions of tickets sold`)

#######
#load
boys <- read_csv("../data/08e5NumberOfBoys.csv")
#calculate expected values
freq_boys <- c(0.25, 0.5, 0.25)
boys$expected_boys <- freq_boys*sum(boys$Frequency)

#viz

ggplot(boys, mapping = aes(x = `Number of boys`,
                           y = Frequency)) +
  geom_col() +
  geom_point(mapping = aes(y = expected_boys), size = 5, color = "red")
#test
#give frequency data and probabilities of each value
chisq.test(x = boys$Frequency, p = freq_boys)


########
#contingency tables
parasite <- read_csv("../data/09e3ParasiteBrainWarp.csv")

#viz
#can either have infection status or number eaten on x,
#depending on what we are interested in

#first put infection on x

inf_x <- ggplot(data = parasite, mapping = aes(x = `infection status`,
                                               y = frequency,
                                               fill = eaten)) +
  geom_col()
inf_x

#now put eaten on x
eaten_x <- ggplot(data = parasite, mapping = aes(x = eaten,
                                               y = frequency,
                                               fill = `infection status`)) +
  geom_col()
eaten_x

#multipanel figures using patchwork
library(patchwork)

