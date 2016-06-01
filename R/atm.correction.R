atm.correction <-
function()
{


# Set the years implemented with corrections values
local.y <- 1950:2010

# Write the values of corrections
atm.cor <- c(
0.46,
0.46,
0.46,
0.47,
0.47,
0.48,
0.48,
0.49,
0.49,
0.50,
0.50,
0.50,
0.52,
0.55,
0.58,
0.61,
0.63,
0.66,
0.69,
0.72,
0.75,
0.77,
0.80,
0.83,
0.86,
0.89,
0.92,
0.94,
0.97,
1.00,
1.03,
1.06,
1.08,
1.11,
1.14,
1.17,
1.20,
1.22,
1.25,
1.28,
1.31,
1.34,
1.37,
1.39,
1.42,
1.45,
1.48,
1.51,
1.53,
1.56,
1.59,
1.62,
1.65,
1.67,
1.7,
1.73,
1.76,
1.79,
1.81,
1.84,
1.87
)

# Create the object containing the correction values for each year in local.y
atm.cor <<- data.frame(year =  local.y,correction = atm.cor)
# Display the values in the console
return(atm.cor)
}
