require(dplyr)
require(reshape2)

# This script generte synthetic data as inputs to TCI algorithms for testing
set.seed(1)

# The space is made of 3x3 gridcells, each is 1 mile x 1 mile
taz <- data.frame(taz.id=1:9,
                  expand.grid(x=c(-1,0,1), y=c(-1,0,1))
                  )
speed <- 1

id.max = max(taz$taz.id)
o.x <- matrix(0, nrow=id.max, ncol=id.max)
o.y <- matrix(0, nrow=id.max, ncol=id.max)
d.x <- matrix(0, nrow=id.max, ncol=id.max)
d.y <- matrix(0, nrow=id.max, ncol=id.max)
o.x[taz$taz.id, ] = taz$x
o.y[taz$taz.id, ] = taz$y
d.x <- t(o.x)
d.y <- t(o.y)

td.skims <- abs(o.x - d.x) + abs(o.y - d.y)
tt.skims <- td.skims / speed

n.hh <- 1000
n.emp <- 1000

# Scenario 1
##a. Randomly distribute households;
##b. Randomly distribute employment;
##c. One mode available, travel cost = euclidean distance / constant speed; 
##d. Randomly match households and employment;
hh <- data.frame(hh.id=1:n.hh, 
                 home.taz=sample(taz$taz.id, n.hh, replace=T))
emp <- data.frame(emp.id=1:n.emp,
                  loc.taz=sample(taz$taz.id, n.emp, replace=T))

hh$emp.id <- sample(emp$emp.id)
hh$emp.taz <- emp$loc.taz[hh$emp.id]

df.trips <- hh %>%
  group_by(home.taz, emp.taz) %>%
  summarize(trips=n())

m.trips <- matrix(0, nrow=id.max, ncol=id.max)
m.trips[with(df.trips, cbind(home.taz, emp.taz))] = df.trips$trips

m.trips * tt.skims



tc <- data.frame(home.taz=hh$home.taz,
                 emp.taz=emp.taz,
                 tc=tt.skims[cbind(hh$home.taz, emp.taz)] + tt.skims[cbind(emp.taz, hh$home.taz)])
tc %>%
  group_by(home.taz) %>%
  summarize(avg.tc=mean(tc))

# Scenario 1b0
##b. concentrate employment at the center;
emp <- data.frame(emp.id=1:n.emp,
                  loc.taz=5)


hh$emp.id <- sample(emp$emp.id)
hh$emp.taz <- emp$loc.taz[hh$emp.id]

df.trips <- hh %>%
  group_by(home.taz, emp.taz) %>%
  summarize(n=n())

m.trips <- matrix(0, nrow=id.max, ncol=id.max)
m.trips[with(df.trips, cbind(home.taz, emp.taz))] = df.trips$n

m.trips * tt.skims 

m.trips.hbw * tt.skims

tc <- data.frame(home.taz=hh$home.taz,
                 emp.taz=hh$emp.taz,
                 tc=tt.skims[cbind(hh$home.taz, hh$emp.taz)] + tt.skims[cbind(hh$emp.taz, hh$home.taz)])
tc %>%
  group_by(home.taz) %>%
  summarize(avg.tc=mean(tc))

# Scenario 1d
##d. Match households and employment ;


# Scenario 1cx2
##c. One cheaper mode introduced with 25% share, travel cost = euclidean distance / (constant speed * 2);

