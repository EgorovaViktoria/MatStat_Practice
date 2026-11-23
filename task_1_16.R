banki <- read.table(
  "BankiRU.txt",
  header = TRUE,
  sep = ";",
  dec = ".",
  encoding = "UTF-8",
  check.names = FALSE
)

r <- banki$EffRate
n <- length(r)

cat("Объем выборки:", n, "\n")
cat("Минимум:", min(r), "%\n")
cat("Максимум:", max(r), "%\n")
cat("Размах:", diff(range(r)), "п.п.\n")

cat("\nТочечный вариационный ряд:\n")
print(table(r))

cat("\nОтносительные частоты:\n")
print(table(r) / n)

cat("\nИнтервальный вариационный ряд:\n")
k <- floor(1 + log2(n))
print(k)
print(table(cut(r, breaks = k, include.lowest = TRUE, right = TRUE)))

cat("\nОсновные характеристики:\n")
print(summary(r))
cat("Исправленная дисперсия:", var(r), "\n")
cat("Стандартное отклонение:", sd(r), "\n")
cat("Квартили:\n")
print(quantile(r, c(0, 0.25, 0.5, 0.75, 0.8, 1)))

q80 <- as.numeric(quantile(r, 0.8))
level_44 <- mean(r <= 4.4)
cv <- 100 * sd(r) / mean(r)

cat("Квантиль уровня 0.8:", q80, "%\n")
cat("Уровень квантиля для 4.4%:", level_44, "\n")
cat("Коэффициент вариации:", cv, "%\n")

moment_skewness <- function(x) {
  m2 <- mean((x - mean(x))^2)
  m3 <- mean((x - mean(x))^3)
  m3 / m2^(3/2)
}

moment_excess <- function(x) {
  m2 <- mean((x - mean(x))^2)
  m4 <- mean((x - mean(x))^4)
  m4 / m2^2 - 3
}

skew <- moment_skewness(r)
kurt <- moment_excess(r)

cat("Коэффициент асимметрии:", skew, "\n")
cat("Коэффициент эксцесса:", kurt, "\n")

hist(
  r,
  breaks = k,
  freq = TRUE,
  col = "lightblue",
  main = "Эффективные ставки по вкладам",
  xlab = "Ставка, %",
  ylab = "Частота"
)

boxplot(
  r,
  horizontal = TRUE,
  col = "lightgreen",
  main = "Boxplot эффективных ставок",
  xlab = "Ставка, %"
)

qqnorm(r, main = "Q-Q plot эффективных ставок")
qqline(r)

q1 <- as.numeric(quantile(r, 0.25))
q3 <- as.numeric(quantile(r, 0.75))
iqr <- q3 - q1

lower_tukey <- q1 - 1.5 * iqr
upper_tukey <- q3 + 1.5 * iqr

cat("\nГраницы выбросов по Тьюки:",
    lower_tukey, upper_tukey, "\n")
cat("Выбросы по Тьюки:\n")
print(banki[r < lower_tukey | r > upper_tukey, ])

z <- (r - mean(r)) / sd(r)
cat("Максимальная абсолютная z-оценка:", max(abs(z)), "\n")
cat("Выбросы по z-оценкам:\n")
print(banki[abs(z) > 3, ])
