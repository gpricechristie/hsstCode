#Import causal data csv
causalData = read.csv('simpleCausalData.csv')

#Plot cancer incidence as a function of yellow fingers
plot(lungCancer~yellowFingers,data=simpleCausalData)

#Find linear regression coefficient
yf_regression = lm(lungCancer~yellowFingers,data=simpleCausalData)
summary(yf_regression)

#Plot cancer incidence as a function of smoking
plot(lungCancer~smoking,data=simpleCausalData)

#Plot yellow fingers as a function of smoking
plot(yellowFingers~smoking,data=simpleCausalData)

#Find linear regression conditioning on smoking as confounder
s_yf_regression = lm(lungCancer~yellowFingers+smoking,data=simpleCausalData)
summary(s_yf_regression)

