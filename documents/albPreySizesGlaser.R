# Glaser (2010) examined 371 albacore stomachs: mean length 75.6, stdev = 8.9, range = 54-99 
# Glaser et al. 2015 show mean/sd lengths of prey items, some common ones are:
  # 4.92 (1.693) for 612 sardine
  # 3.47(0.997) for 5011 anchovy
  # 6.09(1.53) for 3530 hake
  # But also note larger size for saury: 11.91(6.83) for 374 saury. Due to presence of mostly juvenile saury
  # (~6cm), with a few adult saury (~18cm)
# Also see Fig. 2.3 in Glaser thesis (https://escholarship.org/content/qt0hw9q8gw/qt0hw9q8gw.pdf):
  # majority of prey <10cm, but a few items up to ~25-26cm, including adult saury

library(truncnorm) # generates truncated normal distn, can use a/b to set min/max

# Reconstruct albacore lengths
alb <- data.frame("fl" = rtruncnorm(n = 371, a = 54, b = 99, mean = 75.6, sd = 8.9))
hist(alb$fl, breaks = 50)
# Calculate likely maxilliary lengths
alb$ml <- 0.0823 * alb$fl + 1.758
hist(alb$ml, breaks = 50) # Mostly < 10cm

# Can reconstruct common prey sizes too if you want
# Sardine
sard <- rtruncnorm(612, a = 0, mean = 4.92, sd = 1.693)
hist(sard, breaks = 50)
# Anchovy
anch <- rtruncnorm(5011, a = 0, mean = 3.47, sd = 1.693)
hist(anch, breaks = 50)
# Hake
hake <- rtruncnorm(3530, a = 0, mean = 6.09, sd = 1.53)
hist(hake, breaks = 50)

