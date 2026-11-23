jetfuel <- read.table(
  "JetFuelPrices.txt",
  header = TRUE,
  sep = "\t",
  dec = ".",
  na.strings = c("", "NA"),
  encoding = "UTF-8",
  quote = "\"",
  fill = TRUE
)

month_names <- c("May", "June", "July")
month_labels <- c("Май", "Июнь", "Июль")

cat("Средние и медианные цены по месяцам:\n")
for (i in seq_along(month_names)) {
  x <- suppressWarnings(as.numeric(jetfuel[[month_names[i]]]))
  x <- na.omit(x)
  cat(
    month_labels[i],
    ": среднее =", mean(x),
    ", медиана =", median(x),
    ", n =", length(x),
    "\n"
  )
}

boxplot(
  jetfuel[, month_names],
  names = month_labels,
  col = c("lightblue", "lightgreen", "khaki"),
  main = "Цены авиакеросина по месяцам",
  ylab = "руб. за тонну"
)

set.seed(2026)
for (m in c(20, 50, 100)) {
  x <- suppressWarnings(as.numeric(jetfuel$July))
  x <- sample(na.omit(x), m)

  hist(
    x,
    breaks = floor(1 + log2(m)),
    col = "steelblue",
    main = paste("Июль, случайная выборка n =", m),
    xlab = "Цена, руб.",
    ylab = "Частота"
  )
}

district_stats <- aggregate(
  jetfuel[, month_names],
  by = list(FedDistrict = jetfuel$FedDistrict),
  FUN = function(x) c(
    mean = mean(suppressWarnings(as.numeric(x)), na.rm = TRUE),
    median = median(suppressWarnings(as.numeric(x)), na.rm = TRUE)
  )
)

cat("\nСредние и медианы по федеральным округам:\n")
print(district_stats)

central <- subset(jetfuel, FedDistrict == "Central")

boxplot(
  central[, month_names],
  names = month_labels,
  col = c("lightblue", "lightgreen", "khaki"),
  main = "Центральный федеральный округ",
  ylab = "руб. за тонну"
)

boxplot(
  May ~ FedDistrict,
  data = jetfuel,
  las = 2,
  main = "Цены в мае по федеральным округам",
  ylab = "руб. за тонну"
)

moment_skewness <- function(x) {
  x <- na.omit(x)
  m2 <- mean((x - mean(x))^2)
  m3 <- mean((x - mean(x))^3)
  m3 / m2^(3/2)
}

find_tukey <- function(x) {
  x <- na.omit(suppressWarnings(as.numeric(x)))
  q1 <- quantile(x, 0.25)
  q3 <- quantile(x, 0.75)
  iqr <- q3 - q1
  lower <- q1 - 1.5 * iqr
  upper <- q3 + 1.5 * iqr

  cat("Границы Тьюки:", lower, upper, "\n")
  print(x[x < lower | x > upper])
}

find_z <- function(x) {
  x <- na.omit(suppressWarnings(as.numeric(x)))
  z <- (x - mean(x)) / sd(x)
  print(x[abs(z) > 3])
}

for (district in c("Central", "Ural")) {
  cat("\nОкруг:", district, "\n")

  for (i in seq_along(month_names)) {
    x <- suppressWarnings(as.numeric(
      subset(jetfuel, FedDistrict == district)[[month_names[i]]]
    ))
    x <- na.omit(x)

    cat(
      month_labels[i],
      ": среднее =", round(mean(x), 2),
      ", медиана =", median(x),
      ", асимметрия =", round(moment_skewness(x), 3),
      "\n"
    )
  }
}

central_may <- na.omit(suppressWarnings(as.numeric(central$May)))
ural_may <- na.omit(suppressWarnings(as.numeric(
  subset(jetfuel, FedDistrict == "Ural")$May
)))

qqnorm(central_may, main = "Q-Q plot, Центральный округ, май")
qqline(central_may)

qqnorm(ural_may, main = "Q-Q plot, Уральский округ, май")
qqline(ural_may)

cat("\nВыбросы Тьюки: Центральный округ, май\n")
find_tukey(central_may)

cat("\nВыбросы Тьюки: Уральский округ, май\n")
find_tukey(ural_may)

cat("\nВыбросы z-оценок: Центральный округ, май\n")
find_z(central_may)

cat("\nВыбросы z-оценок: Уральский округ, май\n")
find_z(ural_may)

cat("\nВыбросы Тьюки: Центральный округ, июль\n")
find_tukey(central$July)
