customers <- read.table(
  "task_1_15_data.txt",
  header = TRUE,
  sep = ";",
  dec = ".",
  quote = "\"",
  encoding = "UTF-8"
)

cat("Размер выборки:", nrow(customers), "\n")
cat("Количество столбцов:", ncol(customers), "\n")

cat("\nСтруктура данных:\n")
str(customers)

cat("\nЭлементарная единица наблюдения: одна строка таблицы, то есть один потребитель.\n")
cat("Тип наблюдения: одномоментное наблюдение по группе потребителей.\n")

cat("\nПризнаки:\n")
cat("Interest — качественный, порядковый признак.\n")
cat("Purchases — количественный, интервальная шкала с натуральным нулем.\n")
cat("Region — качественный, номинальная шкала.\n")

cat("\nСтатистика по объему закупок:\n")
print(summary(customers$Purchases))
cat("Среднее:", mean(customers$Purchases), "\n")
cat("Медиана:", median(customers$Purchases), "\n")
cat("Стандартное отклонение:", sd(customers$Purchases), "\n")

cat("\nРаспределение по уровню интереса:\n")
print(table(customers$Interest))

cat("\nРаспределение по регионам:\n")
print(table(customers$Region))

barplot(
  table(customers$Interest),
  col = "steelblue",
  main = "Уровень интереса потребителей",
  ylab = "Количество потребителей"
)

boxplot(
  Purchases ~ Region,
  data = customers,
  las = 2,
  col = "lightgreen",
  main = "Объем закупок по регионам"
)
