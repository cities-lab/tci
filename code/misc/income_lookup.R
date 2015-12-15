# This script sets up lookup table for income

# hh.metro <- hh %>% 
# mutate(inc.level=cut(INCOME,
#                      breaks=c(1, 6, 11, 13),
#                      labels=c("lowInc", "midInc", "highInc"),   #allow alternative household grouping
#                      include.lowest=T, right=F)
# )

# Portland 1994 
# lowInc: $0 ~ $24,999; midInc: $25,000 ~ $49,999; highInc: $50,000 ~ or more 

# Value Label
# 1 $0 ‐ $4,999
# 2 $5,000 ‐ $9,999
# 3 $10,000 ‐ $14,999
# 4 $15,000 ‐ $19,999
# 5 $20,000 ‐ $24,999
# 6 $25,000 ‐ $29,999
# 7 $30,000 ‐ $34,999
# 8 $35,000 ‐ $39,999
# 9 $40,000 ‐ $44,999
# 10 $45,000 ‐ $49,999
# 11 $50,000 ‐ $54,999
# 13 $60,000 or more
# 12 $55,000 ‐ $59,999
# 14 DK/RF


# http://data.bls.gov/cgi-bin/cpicalc.pl?cost1=100&year1=1993&year2=2010
# $100 in 1993 has the same buying power as 150.90 in 2010. 

# Income cutoff for OHAS 2011
# lowInc: $0 ~ 37,724; midInc: $37,725 ~ 75,450; highInc: 75,450 ~ or more 

# INCOME	What is your total household income for 2010?
# 1	$0 - $14,999
# 2	$15,000 - $24,999
# 3	$25,000 - $34,999
# 4	$35,000 - $49,999
# 5	$50,000 - $74,999
# 6	$75,000 - $99,999
# 7	$100,000 - $149,999
# 8	$150,000 or more
# 99	REFUSED

# lowInc: $0 ~ $34,999; midInc: $35,000 ~ $74,999; hignInc:75,000 ~ or more




