df1 <- data.frame(c1 = c("A", "B", "C", "D"), 
                 c2 = c(1, 2, 3, 4),
                 c3 = c(2, 1, 2, 4),
                 c4 = c("c", "a", "b", "a"))

df2 <- df1[ order (df1[,3], df1[,4]),  ]

df3 <- data.frame(x = c(1, 2, 3), y = c(0, 10, NA), z=c(NA, 33, 22))

df4 <- df3[!is.na(df3$z),]
df5 <- subset(df3, !is.na(z))
df6 <- subset(df3, !is.na(df3[,3]))