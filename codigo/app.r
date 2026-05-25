# ============================================================
# FASE 1 — MODELO MMM
# ============================================================

# Paso 1 — Librerias
install.packages("shiny","dplyr","readr","ggplot2","scales","tidyr","shinydashboard","lubridate","openxlsx","car","jtools")

library(shiny)
library(dplyr)
library(readr)
library(ggplot2)
library(scales)
library(tidyr)
library(shinydashboard)
library(lubridate)
library(openxlsx)
library(car)
library(jtools)

# Paso 2 — Rutas
ruta     <- "C:/Users/danie/OneDrive/Documentos/CONTADURIA/TERCER SEMESTRE/ANÁLISIS DE LOS NEGOCIOS/DATASET PROYECTO FINAL/"
ruta_jpg <- paste0(ruta, "graficos/")
dir.create(ruta_jpg, showWarnings = FALSE)

# Paso 3 — Adstock y carga
adstock <- function(x, decay = 0.3) {
  out <- numeric(length(x)); out[1] <- x[1]
  for (i in 2:length(x)) out[i] <- x[i] + decay * out[i - 1]
  return(out)
}

df <- read_csv(paste0(ruta, "base_mmm_final_completa.csv")) %>%
  mutate(
    Date             = as.Date(Date),
    google_adstock   = adstock(`Google Ads`),
    facebook_adstock = adstock(Facebook),
    tiktok_adstock   = adstock(TikTok),
    google_x_inf     = google_adstock   * inflation_yoy,
    facebook_x_inf   = facebook_adstock * inflation_yoy
  )

# Paso 4 — Modelos
modelo_base <- lm(
  Sales ~ google_adstock + facebook_adstock + tiktok_adstock +
    inflation_yoy + unemployment + retail_sales + wei_index +
    is_holiday + is_summer + is_q4,
  data = df
)

modelo_int <- lm(
  Sales ~ google_adstock + facebook_adstock + tiktok_adstock +
    inflation_yoy + google_x_inf + facebook_x_inf +
    unemployment + retail_sales + wei_index +
    is_holiday + is_summer + is_q4,
  data = df
)

b <- coef(modelo_int)

# Paso 5 — Columnas derivadas
df <- df %>%
  mutate(
    fitted            = fitted(modelo_int),
    residuals         = residuals(modelo_int),
    ventas_google     = b["google_adstock"]   * google_adstock + b["google_x_inf"]     * google_x_inf,
    ventas_facebook   = b["facebook_adstock"] * facebook_adstock + b["facebook_x_inf"] * facebook_x_inf,
    ventas_tiktok     = b["tiktok_adstock"]   * tiktok_adstock,
    comp_base         = b["(Intercept)"],
    comp_google       = b["google_adstock"]   * google_adstock,
    comp_facebook     = b["facebook_adstock"] * facebook_adstock,
    comp_tiktok       = b["tiktok_adstock"]   * tiktok_adstock,
    comp_google_inf   = b["google_x_inf"]     * google_x_inf,
    comp_facebook_inf = b["facebook_x_inf"]   * facebook_x_inf,
    comp_inflacion    = b["inflation_yoy"]    * inflation_yoy,
    comp_unemployment = b["unemployment"]     * unemployment,
    comp_retail       = b["retail_sales"]     * retail_sales,
    comp_wei          = b["wei_index"]        * wei_index,
    comp_holiday      = b["is_holiday"]       * is_holiday,
    comp_summer       = b["is_summer"]        * is_summer,
    comp_q4           = b["is_q4"]            * is_q4
  )

# Paso 6 — Tablas de resultados
fmt_coef <- function(modelo) {
  as.data.frame(summary(modelo)$coefficients) %>%
    tibble::rownames_to_column("Variable") %>%
    setNames(c("Variable","Estimado","Error_Std","t_valor","p_valor")) %>%
    mutate(
      Significancia = case_when(
        p_valor < 0.001 ~ "***", p_valor < 0.01 ~ "**",
        p_valor < 0.05  ~ "*",   p_valor < 0.1  ~ ".",
        TRUE ~ ""
      ),
      across(where(is.numeric), ~ round(.x, 4))
    )
}

coef_base <- fmt_coef(modelo_base)
coef_int  <- fmt_coef(modelo_int)

comparacion <- data.frame(
  Metrica = c("R²","R² Ajustado","AIC","BIC","F-statistic","Observaciones"),
  Modelo_Base = c(
    round(summary(modelo_base)$r.squared, 4),
    round(summary(modelo_base)$adj.r.squared, 4),
    round(AIC(modelo_base), 2),
    round(BIC(modelo_base), 2),
    round(summary(modelo_base)$fstatistic[1], 4),
    nobs(modelo_base)
  ),
  Modelo_Interacciones = c(
    round(summary(modelo_int)$r.squared, 4),
    round(summary(modelo_int)$adj.r.squared, 4),
    round(AIC(modelo_int), 2),
    round(BIC(modelo_int), 2),
    round(summary(modelo_int)$fstatistic[1], 4),
    nobs(modelo_int)
  )
)

vif_df <- as.data.frame(vif(modelo_int)) %>%
  tibble::rownames_to_column("Variable") %>%
  setNames(c("Variable","VIF")) %>%
  mutate(
    VIF         = round(VIF, 3),
    Diagnostico = case_when(VIF < 5 ~ "OK", VIF < 10 ~ "Moderado", TRUE ~ "Alto")
  )

anova_test <- as.data.frame(anova(modelo_base, modelo_int)) %>%
  tibble::rownames_to_column("Modelo") %>%
  mutate(across(where(is.numeric), ~ round(.x, 4)))

inflacion_niveles  <- 1:6
coef_google        <- coef(modelo_int)["google_adstock"]
coef_interacc_g    <- coef(modelo_int)["google_x_inf"]
coef_facebook      <- coef(modelo_int)["facebook_adstock"]
coef_interacc_f    <- coef(modelo_int)["facebook_x_inf"]

efecto_marginal <- data.frame(
  Inflacion_YoY   = inflacion_niveles,
  Efecto_Google   = round(coef_google   + coef_interacc_g * inflacion_niveles, 4),
  Efecto_Facebook = round(coef_facebook + coef_interacc_f * inflacion_niveles, 4)
)

# Paso 7 — Graficos Fase 1
guardar <- function(g, nombre, w = 12, h = 6) {
  ggsave(paste0(ruta_jpg, nombre, ".jpg"), plot = g,
         width = w, height = h, dpi = 200, bg = "white")
}

g1 <- ggplot(df, aes(x = Date)) +
  geom_line(aes(y = Sales,  color = "Real"),     linewidth = 0.9) +
  geom_line(aes(y = fitted, color = "Ajustado"), linewidth = 0.9, linetype = "dashed") +
  scale_color_manual(values = c("Real" = "#2c3e50", "Ajustado" = "#e74c3c")) +
  scale_y_continuous(labels = comma) +
  labs(title = "Ventas Reales vs Ajustadas", x = "Fecha", y = "Ventas ($)", color = "") +
  theme_minimal(base_size = 13)
guardar(g1, "G1_ventas_real_vs_ajustado")

g2 <- ggplot(df, aes(x = Date, y = residuals)) +
  geom_line(color = "#3498db", linewidth = 0.8) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  scale_y_continuous(labels = comma) +
  labs(title = "Residuos en el Tiempo", x = "Fecha", y = "Residuo ($)") +
  theme_minimal(base_size = 13)
guardar(g2, "G2_residuos")

g3 <- ggplot(efecto_marginal, aes(x = Inflacion_YoY)) +
  geom_line(aes(y = Efecto_Google,   color = "Google Ads"),   linewidth = 1.2) +
  geom_line(aes(y = Efecto_Facebook, color = "Facebook Ads"), linewidth = 1.2) +
  geom_point(aes(y = Efecto_Google,   color = "Google Ads"),   size = 3) +
  geom_point(aes(y = Efecto_Facebook, color = "Facebook Ads"), size = 3) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  scale_color_manual(values = c("Google Ads" = "#4285F4", "Facebook Ads" = "#1877F2")) +
  labs(title = "Efecto Marginal de Ads segun Nivel de Inflacion",
       x = "Inflacion YoY (%)", y = "Ventas por $ invertido", color = "") +
  theme_minimal(base_size = 13)
guardar(g3, "G3_efecto_marginal_inflacion")

# Paso 8 — Excel Fase 1
wb1 <- createWorkbook()
hojas1 <- list(
  "1. Coef Modelo Base"          = coef_base,
  "2. Coef Modelo Interacciones" = coef_int,
  "3. Comparacion Modelos"       = comparacion,
  "4. ANOVA"                     = anova_test,
  "5. VIF Multicolinealidad"     = vif_df,
  "6. Efecto Marginal"           = efecto_marginal
)
for (nombre in names(hojas1)) { addWorksheet(wb1, nombre); writeData(wb1, nombre, hojas1[[nombre]]) }
saveWorkbook(wb1, paste0(ruta, "fase1_modelos.xlsx"), overwrite = TRUE)

# ============================================================
# FASE 2 — DESCOMPOSICION
# ============================================================

# Paso 1 — Tabla de contribucion
contrib <- data.frame(
  Variable = c("Base","Google Ads","Facebook Ads","TikTok",
               "Google x Inflacion","Facebook x Inflacion",
               "Inflacion","Desempleo","Retail Sales","WEI",
               "Festivos","Verano","Q4"),
  Tipo = c("Base", rep("Publicidad Digital", 5), rep("Macro", 4), rep("Estacionalidad", 3)),
  Contribucion_Total = c(
    sum(df$comp_base),       sum(df$comp_google),
    sum(df$comp_facebook),   sum(df$comp_tiktok),
    sum(df$comp_google_inf), sum(df$comp_facebook_inf),
    sum(df$comp_inflacion),  sum(df$comp_unemployment),
    sum(df$comp_retail),     sum(df$comp_wei),
    sum(df$comp_holiday),    sum(df$comp_summer), sum(df$comp_q4)
  )
) %>%
  mutate(
    Contribucion_Total = round(Contribucion_Total, 2),
    Pct_Ventas         = round(Contribucion_Total / sum(df$Sales) * 100, 2)
  ) %>%
  arrange(desc(abs(Contribucion_Total)))

contrib_cat <- contrib %>%
  group_by(Tipo) %>%
  summarise(
    Contribucion_Total = round(sum(Contribucion_Total), 2),
    Pct_Ventas         = round(sum(Pct_Ventas), 2)
  ) %>%
  arrange(desc(abs(Contribucion_Total)))

resumen_ejecutivo <- data.frame(
  Hallazgo = c("Total ventas reales","Ventas explicadas por modelo",
               "R² modelo interacciones","R² ajustado",
               "Canal top contribuidor","Categoria top","Semanas analizadas"),
  Valor = c(
    paste0("$", format(round(sum(df$Sales), 0), big.mark = ",")),
    paste0("$", format(round(sum(df$fitted), 0), big.mark = ",")),
    paste0(round(summary(modelo_int)$r.squared * 100, 1), "%"),
    paste0(round(summary(modelo_int)$adj.r.squared * 100, 1), "%"),
    contrib$Variable[1],
    contrib_cat$Tipo[1],
    as.character(nrow(df))
  )
)

decomp_semanal <- df %>%
  select(Date, Sales, fitted, residuals,
         comp_base, comp_google, comp_facebook, comp_tiktok,
         comp_google_inf, comp_facebook_inf, comp_inflacion,
         comp_unemployment, comp_retail, comp_wei,
         comp_holiday, comp_summer, comp_q4) %>%
  rename(Fecha=Date, Ventas_Reales=Sales, Ventas_Ajustadas=fitted,
         Residuos=residuals, Base=comp_base, Google_Ads=comp_google,
         Facebook_Ads=comp_facebook, TikTok=comp_tiktok,
         Google_x_Inflacion=comp_google_inf, Facebook_x_Inflacion=comp_facebook_inf,
         Inflacion=comp_inflacion, Desempleo=comp_unemployment,
         Retail_Sales=comp_retail, WEI=comp_wei,
         Festivos=comp_holiday, Verano=comp_summer, Q4=comp_q4)

# Paso 2 — Graficos Fase 2
colores_decomp <- c(
  "Base"                 = "#95a5a6", "Google Ads"           = "#4285F4",
  "Facebook Ads"         = "#1877F2", "TikTok"               = "#333333",
  "Google x Inflacion"   = "#85B7EB", "Facebook x Inflacion" = "#74b9ff",
  "Inflacion"            = "#e17055", "Desempleo"            = "#d63031",
  "Retail Sales"         = "#e74c3c", "WEI"                  = "#fd79a8",
  "Festivos"             = "#00b894", "Verano"               = "#00cec9",
  "Q4"                   = "#6c5ce7"
)

g4 <- ggplot(contrib, aes(x = reorder(Variable, Contribucion_Total),
                           y = Contribucion_Total, fill = Tipo)) +
  geom_col() + coord_flip() +
  scale_fill_manual(values = c("Base"="#95a5a6","Publicidad Digital"="#3498db",
                                "Macro"="#e74c3c","Estacionalidad"="#2ecc71")) +
  scale_y_continuous(labels = comma) +
  labs(title = "Contribucion Total a Ventas por Variable",
       x = "", y = "Ventas Atribuidas ($)", fill = "") +
  theme_minimal(base_size = 13)
guardar(g4, "G4_contribucion_por_variable")

df_long <- df %>%
  select(Date, comp_base, comp_google, comp_facebook, comp_tiktok,
         comp_google_inf, comp_facebook_inf, comp_inflacion,
         comp_unemployment, comp_retail, comp_wei,
         comp_holiday, comp_summer, comp_q4) %>%
  pivot_longer(-Date, names_to = "Componente", values_to = "Valor") %>%
  mutate(Componente = case_match(Componente,
    "comp_base"         ~ "Base",           "comp_google"       ~ "Google Ads",
    "comp_facebook"     ~ "Facebook Ads",   "comp_tiktok"       ~ "TikTok",
    "comp_google_inf"   ~ "Google x Inflacion", "comp_facebook_inf" ~ "Facebook x Inflacion",
    "comp_inflacion"    ~ "Inflacion",      "comp_unemployment" ~ "Desempleo",
    "comp_retail"       ~ "Retail Sales",   "comp_wei"          ~ "WEI",
    "comp_holiday"      ~ "Festivos",       "comp_summer"       ~ "Verano",
    "comp_q4"           ~ "Q4"
  ))

g5 <- ggplot(df_long, aes(x = Date, y = Valor, fill = Componente)) +
  geom_area(position = "stack", alpha = 0.85) +
  scale_fill_manual(values = colores_decomp) +
  scale_y_continuous(labels = comma) +
  labs(title = "Descomposicion Semanal de Ventas por Componente",
       x = "Fecha", y = "Ventas ($)", fill = "") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")
guardar(g5, "G5_descomposicion_semanal", w = 14, h = 7)

g6 <- ggplot(contrib_cat, aes(x = "", y = abs(Pct_Ventas), fill = Tipo)) +
  geom_col(width = 1, color = "white") + coord_polar("y") +
  scale_fill_manual(values = c("Base"="#95a5a6","Publicidad Digital"="#3498db",
                                "Macro"="#e74c3c","Estacionalidad"="#2ecc71")) +
  geom_text(aes(label = paste0(round(abs(Pct_Ventas), 1), "%")),
            position = position_stack(vjust = 0.5), color = "white", size = 5) +
  labs(title = "Distribucion de Ventas por Categoria", fill = "") +
  theme_void(base_size = 13)
guardar(g6, "G6_pie_categorias", w = 8, h = 7)

# Paso 3 — Excel Fase 2
wb2 <- createWorkbook()
hojas2 <- list(
  "7. Contribucion por Variable"  = contrib,
  "8. Contribucion por Categoria" = contrib_cat,
  "9. Descomposicion Semanal"     = decomp_semanal,
  "10. Resumen Ejecutivo"         = resumen_ejecutivo
)
for (nombre in names(hojas2)) { addWorksheet(wb2, nombre); writeData(wb2, nombre, hojas2[[nombre]]) }
saveWorkbook(wb2, paste0(ruta, "fase2_descomposicion.xlsx"), overwrite = TRUE)

# ============================================================
# FASE 3 — ROI
# ============================================================

# Paso 1 — Tablas ROI
roi_global <- data.frame(
  Canal = c("Google Ads", "Facebook Ads", "TikTok", "Total Digital"),
  Gasto_Total = c(sum(df$`Google Ads`), sum(df$Facebook),
                  sum(df$TikTok),       sum(df$total_spend)),
  Ventas_Atribuidas = c(sum(df$ventas_google), sum(df$ventas_facebook),
                        sum(df$ventas_tiktok),
                        sum(df$ventas_google) + sum(df$ventas_facebook) + sum(df$ventas_tiktok))
) %>%
  mutate(
    ROI               = round(Ventas_Atribuidas / Gasto_Total, 2),
    Ganancia_Neta     = round(Ventas_Atribuidas - Gasto_Total, 0),
    Gasto_Total       = round(Gasto_Total, 0),
    Ventas_Atribuidas = round(Ventas_Atribuidas, 0)
  )

roi_anual <- df %>%
  group_by(year) %>%
  summarise(
    Gasto_Google    = sum(`Google Ads`), Gasto_Facebook  = sum(Facebook),
    Gasto_TikTok    = sum(TikTok),       Ventas_Google   = sum(ventas_google),
    Ventas_Facebook = sum(ventas_facebook), Ventas_TikTok = sum(ventas_tiktok)
  ) %>%
  mutate(
    ROI_Google   = round(Ventas_Google   / ifelse(Gasto_Google   > 0, Gasto_Google,   1), 2),
    ROI_Facebook = round(Ventas_Facebook / ifelse(Gasto_Facebook > 0, Gasto_Facebook, 1), 2),
    ROI_TikTok   = round(Ventas_TikTok   / ifelse(Gasto_TikTok   > 0, Gasto_TikTok,   1), 2),
    across(where(is.numeric), ~ round(.x, 2))
  )

roi_trimestral <- df %>%
  group_by(year, quarter) %>%
  summarise(
    Gasto_Google    = sum(`Google Ads`), Gasto_Facebook  = sum(Facebook),
    Gasto_TikTok    = sum(TikTok),       Gasto_Total     = sum(total_spend),
    Ventas_Google   = sum(ventas_google), Ventas_Facebook = sum(ventas_facebook),
    Ventas_TikTok   = sum(ventas_tiktok), .groups = "drop"
  ) %>%
  mutate(
    Periodo      = paste0(year, " Q", quarter),
    ROI_Google   = round(Ventas_Google   / ifelse(Gasto_Google   > 0, Gasto_Google,   1), 2),
    ROI_Facebook = round(Ventas_Facebook / ifelse(Gasto_Facebook > 0, Gasto_Facebook, 1), 2),
    ROI_TikTok   = round(Ventas_TikTok   / ifelse(Gasto_TikTok   > 0, Gasto_TikTok,   1), 2),
    across(where(is.numeric), ~ round(.x, 2))
  ) %>%
  select(Periodo, everything(), -year, -quarter)

eficiencia <- data.frame(
  Canal          = c("Google Ads", "Facebook Ads", "TikTok"),
  Coef_Directo   = round(c(b["google_adstock"], b["facebook_adstock"], b["tiktok_adstock"]), 4),
  Interpretacion = c(
    paste0("Cada $1 en Google genera $",   round(b["google_adstock"],   2), " en ventas"),
    paste0("Cada $1 en Facebook genera $", round(b["facebook_adstock"], 2), " en ventas"),
    paste0("Cada $1 en TikTok genera $",   round(b["tiktok_adstock"],   2), " en ventas")
  )
)

waterfall_data <- data.frame(
  Variable = c("Base","Google Ads","TikTok","Facebook Ads","Inflacion",
               "Int. Google","Int. Facebook","Retail Sales","WEI",
               "Desempleo","Festivos","Verano","Q4","Total"),
  Valor = c(
    sum(b["(Intercept)"]                               * rep(1, nrow(df))),
    sum(b["google_adstock"]   * df$google_adstock),
    sum(b["tiktok_adstock"]   * df$tiktok_adstock),
    sum(b["facebook_adstock"] * df$facebook_adstock),
    sum(b["inflation_yoy"]    * df$inflation_yoy),
    sum(b["google_x_inf"]     * df$google_x_inf),
    sum(b["facebook_x_inf"]   * df$facebook_x_inf),
    sum(b["retail_sales"]     * df$retail_sales),
    sum(b["wei_index"]        * df$wei_index),
    sum(b["unemployment"]     * df$unemployment),
    sum(b["is_holiday"]       * df$is_holiday),
    sum(b["is_summer"]        * df$is_summer),
    sum(b["is_q4"]            * df$is_q4),
    sum(df$Sales)
  )
) %>%
  mutate(
    Tipo  = case_when(
      Variable == "Base"  ~ "Base",  Variable == "Total" ~ "Total",
      Valor >= 0 ~ "Positivo",       TRUE ~ "Negativo"
    ),
    Valor = round(Valor, 0)
  )

# Paso 2 — Graficos Fase 3
g7 <- ggplot(roi_global %>% filter(Canal != "Total Digital"),
             aes(x = reorder(Canal, ROI), y = ROI, fill = Canal)) +
  geom_col(width = 0.5) +
  geom_text(aes(label = paste0("$", ROI, " por $1")), hjust = -0.1, size = 3.5) +
  coord_flip() +
  scale_fill_manual(values = c("Google Ads"="#4285F4","Facebook Ads"="#1877F2","TikTok"="#555555")) +
  scale_y_continuous(limits = c(0, max(roi_global$ROI[roi_global$Canal != "Total Digital"]) * 1.35),
                     labels = dollar_format(prefix = "$")) +
  labs(title = "ROI por Canal Publicitario", x = "", y = "ROI ($)") +
  theme_minimal(base_size = 13) + theme(legend.position = "none")
guardar(g7, "G7_roi_por_canal")

comparativa <- roi_global %>%
  filter(Canal != "Total Digital") %>%
  select(Canal, Gasto_Total, Ventas_Atribuidas) %>%
  pivot_longer(-Canal, names_to = "Tipo", values_to = "Valor") %>%
  mutate(Tipo = case_match(Tipo,
    "Gasto_Total"       ~ "Gasto",
    "Ventas_Atribuidas" ~ "Ventas Atribuidas"
  ))

g8 <- ggplot(comparativa, aes(x = Canal, y = Valor, fill = Tipo)) +
  geom_col(position = "dodge", width = 0.6) +
  scale_fill_manual(values = c("Gasto"="#e74c3c","Ventas Atribuidas"="#2ecc71")) +
  scale_y_continuous(labels = comma) +
  geom_text(aes(label = paste0("$", format(round(Valor/1000,0), big.mark=","), "K")),
            position = position_dodge(width = 0.6), vjust = -0.5, size = 3) +
  labs(title = "Gasto vs Ventas Atribuidas por Canal", x = "", y = "$", fill = "") +
  theme_minimal(base_size = 13)
guardar(g8, "G8_gasto_vs_ventas_canal")

g9 <- ggplot(roi_trimestral, aes(x = Periodo)) +
  geom_line(aes(y = ROI_Google,   color = "Google Ads",   group = 1), linewidth = 1) +
  geom_line(aes(y = ROI_Facebook, color = "Facebook Ads", group = 1), linewidth = 1) +
  geom_line(aes(y = ROI_TikTok,   color = "TikTok",       group = 1), linewidth = 1) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "gray50") +
  scale_color_manual(values = c("Google Ads"="#4285F4","Facebook Ads"="#1877F2","TikTok"="#555555")) +
  labs(title = "Evolucion ROI Trimestral", x = "", y = "ROI ($)", color = "") +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "bottom")
guardar(g9, "G9_roi_trimestral", w = 14, h = 6)

g10 <- ggplot(waterfall_data,
              aes(x = reorder(Variable, -abs(Valor)), y = Valor, fill = Tipo)) +
  geom_col(width = 0.65) +
  scale_fill_manual(values = c("Base"="#95a5a6","Positivo"="#2ecc71",
                                "Negativo"="#e74c3c","Total"="#2c3e50")) +
  scale_y_continuous(labels = comma) + coord_flip() +
  labs(title = "Waterfall — Contribucion Acumulada a Ventas", x = "", y = "Ventas ($)", fill = "") +
  theme_minimal(base_size = 12) + theme(legend.position = "none")
guardar(g10, "G10_waterfall", w = 12, h = 7)

# Paso 3 — Excel Fase 3
wb3 <- createWorkbook()
hojas3 <- list(
  "1. ROI Global"       = roi_global,
  "2. ROI por Ano"      = roi_anual,
  "3. ROI Trimestral"   = roi_trimestral,
  "4. Eficiencia"       = eficiencia,
  "5. Waterfall"        = waterfall_data
)
for (nombre in names(hojas3)) { addWorksheet(wb3, nombre); writeData(wb3, nombre, hojas3[[nombre]]) }
saveWorkbook(wb3, paste0(ruta, "fase3_roi.xlsx"), overwrite = TRUE)