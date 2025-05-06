install.packages("dplyr")
install.packages("ggplot2")
install.packages("readr")

library(dplyr)
library(ggplot2)
library(readr)

# Funzione per rinominare la colonna lunga
rename_npl <- function(df) {
  col_npl <- grep("Gross non-performing.*loans.*", names(df), value = TRUE)
  df |> rename(NPL_Ratio = all_of(col_npl)) |> select(`TIME PERIOD`, NPL_Ratio, Country)
}

# ApplicaRE mutate + rename ai dati importati
Italy_   <- Italy_   |> mutate(Country = "Italy")   |> rename_npl()
France_  <- France_  |> mutate(Country = "France")  |> rename_npl()
Germany_ <- Germany_ |> mutate(Country = "Germany") |> rename_npl()
Spain_   <- Spain_   |> mutate(Country = "Spain")   |> rename_npl()
EU_av_   <- EU_av_   |> mutate(Country = "EU Avg")  |> rename_npl()

# UniRE in unico dataset
npl_data <- bind_rows(Italy_, France_, Germany_, Spain_, EU_av_) |>
  rename(Quarter = `TIME PERIOD`) |>
  na.omit()


# Converte Quarter in character per evitare problemi con la linea verticale
npl_data$Quarter <- as.character(npl_data$Quarter)

# EstrARRE solo i valori di Quarter che terminano in "Q4"
q4_labels <- npl_data$Quarter[grepl("Q4$", npl_data$Quarter)]

# Grafico aggiornato
ggplot(npl_data, aes(x = Quarter, y = NPL_Ratio, color = Country, group = Country)) +
  geom_line(size = 1.3) +
  geom_point(size = 2) +
  geom_vline(xintercept = "2016Q1", linetype = "dashed", color = "black", size = 1) +
  labs(
    title = "NPL Ratio Comparison (2014–2023)",
    subtitle = "Italy vs EU countries – Non-Performing Loans as % of Gross Loans",
    x = "Quarter",
    y = "NPL Ratio (%)"
  ) +
  scale_x_discrete(breaks = q4_labels) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.title = element_blank()
  )




# Rinomina corretta per il dataset Italy_
npl_col <- grep("Gross non-performing.*loans.*", names(Italy_), value = TRUE)
Italy_ <- Italy_ |> rename(NPL_Ratio = all_of(npl_col))

summary(Italy_$NPL_Ratio)

Italy_$Quarter <- as.character(Italy_$`TIME PERIOD`)



ggplot(Italy_, aes(x = Quarter)) +
  geom_line(aes(y = NPL_Ratio, group = 1), color = "blue", size = 1.2, alpha = 0.7) +
  geom_line(aes(y = Predicted, group = 1), color = "red", linetype = "dashed", size = 1) +
  geom_vline(xintercept = "2016Q1", linetype = "dotted", color = "black", size = 1) +
  scale_x_discrete(breaks = q4_labels_italy) +
  labs(
    title = "Interrupted Time Series – NPL Ratio in Italy",
    subtitle = " GACS (2016)  inear Model",
    x = "Quarter",
    y = "NPL Ratio (%)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 15),
    plot.subtitle = element_text(size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

summary(model_its)


model_its <- lm(NPL_Ratio ~ Time + Post + Time_Post, data = Italy_)

summary(model_its)
summary(Italy_$NPL_Ratio)


Italy_$log_NPL <- log(Italy_$NPL_Ratio)
model_log <- lm(log_NPL ~ Time + Post + Time_Post, data = Italy_)
summary(model_log)


Italy_$log_NPL <- log(Italy_$NPL_Ratio)

model_its <- lm(NPL_Ratio ~ Time + Post + Time_Post, data = Italy_)
summary(model_its)

data.frame(
  Modello = c("Lineare", "Log-lineare"),
  Adj_R2 = c(summary(model_its)$adj.r.squared, summary(model_log)$adj.r.squared),
  Coeff_Post = c(coef(model_its)[3], coef(model_log)[3]),
  Pval_Post = c(summary(model_its)$coefficients[3, 4], summary(model_log)$coefficients[3, 4]),
  Coeff_Interazione = c(coef(model_its)[4], coef(model_log)[4]),
  Pval_Interazione = c(summary(model_its)$coefficients[4, 4], summary(model_log)$coefficients[4, 4])
)


colnames(Italy_)
summary(Italy_$NPL_Ratio)
summary(Italy_$Predicted)

Italy_$Quarter <- factor(Italy_$Quarter, levels = unique(Italy_$Quarter))

Italy_$Quarter <- as.character(Italy_$Quarter)

str(Italy_$Quarter)

Italy_$Quarter <- factor(Italy_$Quarter, levels = unique(Italy_$Quarter))

summary(Italy_$NPL_Ratio)
summary(Italy_$Predicted)

Italy_$Quarter_Index <- as.numeric(Italy_$Quarter)


# Estrarrei solo i livelli di Quarter che terminano in 'Q1'
q1_labels <- levels(Italy_$Quarter)[grepl("Q1$", levels(Italy_$Quarter))]

# Ottiengo le posizioni corrispondenti
q1_breaks <- which(levels(Italy_$Quarter) %in% q1_labels)

# Grafico aggiornato
ggplot(Italy_, aes(x = Quarter_Index)) +
  geom_line(aes(y = NPL_Ratio), color = "blue", size = 1.2) +
  geom_line(aes(y = Predicted), color = "red", linetype = "dashed", size = 1.2) +
  geom_vline(xintercept = which(Italy_$Quarter == "2016Q1"), linetype = "dotted", color = "black", size = 1) +
  scale_x_continuous(
    breaks = q1_breaks,
    labels = q1_labels
  ) +
  labs(
    title = "Interrupted Time Series – NPL Ratio in Italy",
    subtitle = "Con effetto GACS (2016) stimato dal modello lineare",
    x = "Quarter", y = "NPL Ratio (%)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

Comparative_tab <- data.frame(
  Modello = c("Linear", "Log-linear"),
  Adj_R2 = c(summary(model_its)$adj.r.squared, summary(model_log)$adj.r.squared),
  Coeff_Post = c(coef(model_its)["Post"], coef(model_log)["Post"]),
  Pval_Post = c(summary(model_its)$coefficients["Post", 4], summary(model_log)$coefficients["Post", 4]),
  Coeff_Interaction = c(coef(model_its)["Time_Post"], coef(model_log)["Time_Post"]),
  Pval_Interaction = c(summary(model_its)$coefficients["Time_Post", 4], summary(model_log)$coefficients["Time_Post", 4])
)

Comparative_tab

#  log  stessa lunghezza del dataset
Italy_$log_NPL <- ifelse(Italy_$NPL_Ratio > 0, log(Italy_$NPL_Ratio), NA)
Italy_$Predicted_Log <- predict(model_log)

# Ordina i quarter come fattore
Italy_$Quarter <- factor(Italy_$Quarter, levels = unique(Italy_$Quarter))

# Seleziono etichette Q1 per leggibilità
quarters_q1 <- Italy_$Quarter[grep("Q1", Italy_$Quarter)]


ggplot(Italy_, aes(x = Quarter)) +
  geom_line(aes(y = log_NPL, group = 1), color = "blue", size = 1.2, alpha = 0.7) +
  geom_line(aes(y = Predicted_Log, group = 1), color = "red", linetype = "dashed", size = 1) +
  geom_vline(xintercept = "2016Q1", linetype = "dotted", color = "black", size = 1) +
  scale_x_discrete(breaks = quarters_q1) +
  labs(
    title = "Interrupted Time Series – NPL Ratio in Italy",
    subtitle = "Estimated using log-linear model and GACS policy break (2016)",
    x = "Quarter", y = "Log NPL Ratio"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 15),
    plot.subtitle = element_text(size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

Comparative_tab
View(Comparative_tab)
