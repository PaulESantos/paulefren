hex1 <- hexdf()

## generate some data to plot
n <- 43
den <- ceiling(n / 5)
x <- seq(-.525, .465, length.out = n) + rep(c(-.005, .005), ceiling(n/2))[1:n]
y <- seq(-.1, .1, length.out = ceiling(n / den))
y <- rep(c(y, rev(y)), ceiling(n / den))[1:n] + seq(.0, .45, length.out = n)
z <- c(rep("a", ceiling(n / den)), rep("b", ceiling(n / den)))
z <- rep(z, ceiling(n / den))[1:n]
pts <- data.frame(x, y, z)

## create base plot

p_ <- ggplot(hex1, aes(x, y)) +
  geom_polygon(fill = "#242424", colour = "#001030", size = 1) +
  geom_point(data = pts, aes(fill = z, colour = z), shape = 21, size = 1.75) +
  scale_fill_manual(values = c("#8CFF00", "#cc00ff")) +
  scale_colour_manual(values = c("#0A2200", "#110033")) +
  annotate("text", 0, -.36, label = "Paul Efren Santos\nAndrade",
           colour = "white", size = 8, fontface = "italic") +
  coord_fixed(ratio = 1, expand = TRUE) +
  coord_cartesian(xlim = range(hex1$x), ylim = range(hex1$y)) +
  theme_void()

## adjust margin to maximize sticker
t <- 6
r <- 5
b <- 9
l <- 8
p <- p_ +
  theme(legend.position = "none",
        plot.margin = margin(-t, -r, -b, -l, unit = "pt"))

#view plot in device
p
ggsave("D:/rworkdirectori/hexagon-logo.png", p, width = 1.73, height = 2,
  units = "in", bg = "transparent")
