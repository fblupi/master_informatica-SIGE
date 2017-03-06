require(ggplot)

a <- 7
b <- 8
v <- c(1, 2, a, b)
w <- -2:5

q <- c("Futbol", "Baloncesto", "Tenis", "Balonmano")
x <- 1:4
y <- -2:1
df <- data.frame(q, x, y)
names(df) <- c("Deportes", "Var_X", "Var_Y")

df[1]
df["Deportes"]
df[c("Deportes", "Var_X")]
df$Deportes

head(df)
head(df, 2)
df[1,]
df[df$Var_X>=3,]
df[df$Var_X>=3,c("Deportes")]
