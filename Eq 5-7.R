# Defining parameters
G <-3.379650238





Ig <- 1.458928




g_bw <- 0.5081374*1





g_Tw <- 1/((G/Ig)+(1/g_bw)) 

# Example values, which you can replace with actual values
K <- 1


# Formula
g_sw <- 2 / ((1/g_Tw - 1/g_bw) + sqrt((1/g_Tw - 1/g_bw)^2 + (4 * K / (K + 1)^2) * (2 * 1/g_Tw - 1/g_bw) * 1/g_bw))

# Print results
print(g_sw)

g_sw2 <- (2*Ig*g_bw)/(Ig+2*G*g_bw)
g_sw2
