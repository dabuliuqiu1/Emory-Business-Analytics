library(data.table)
library(igraph)
library(foreach)

execs = fread('/Users/huangzm/Desktop/Social Network/HW6/execs.csv')
ind_inv = fread('/Users/huangzm/Desktop/Social Network/HW6/individual_investors.csv')
deal = fread('/Users/huangzm/Desktop/Social Network/HW6/deal_details.csv')
investor = fread('/Users/huangzm/Desktop/Social Network/HW6/investor_details.csv')
people = fread('/Users/huangzm/Desktop/Social Network/HW6/people.csv')

deal$nnn = ifelse(deal$Gross_Profit==1,3,0)
