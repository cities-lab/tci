# This script generte synthetic data as inputs to TCI algorithms for testing

# The space is made of 3x3 gridcells, each is 1 mile x 1 mile
tazes <- data.frame(id=1:16,
                    
                    )

# Scenario 1
##a. Randomly distribute households;
##b. Randomly distribute employment;
##c. One mode available, travel cost = euclidean distance / constant speed; 
##d. Randomly match households and employment;


# Scenario 1b0
##b. concentrate employment at the center;

# Scenario 1d
##d. Match households and employment ;


# Scenario 1cx2
##c. One cheaper mode introduced with 25% share, travel cost = euclidean distance / (constant speed * 2);

