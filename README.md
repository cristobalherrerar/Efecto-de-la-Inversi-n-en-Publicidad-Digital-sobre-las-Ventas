# Marketing Mix Model (MMM) — Publicidad Digital y Ventas

## ¿De qué trata?
Modelo de regresión múltiple que estima el efecto de la inversión en 
Google Ads, Facebook y TikTok sobre las ventas semanales, evaluando 
si la inflación modera dicho efecto. Período: 2018–2021 (200 semanas).

## Resultados principales
- R² = 91.3% — el modelo explica el 91% de la variación en ventas
- Google Ads: único canal con ROI positivo ($1.59 por $1 invertido)
- La inflación no modera significativamente el efecto de los ads (p > 0.05)

## ¿Cómo funciona?
1. Ejecutar `codigo/app.R` en RStudio
2. Requiere R 4.x y los paquetes: shiny, shinydashboard, dplyr, 
   readr, ggplot2, tidyr, lubridate, openxlsx, car, jtools
3. El script corre las 4 fases automáticamente y lanza el dashboard

## Estructura del repositorio
- `trabajo_escrito/` — informe final en PDF
- `base_de_datos/` — dataset con 200 semanas y 20 variables
- `codigo/` — script R completo (modelo + dashboard Shiny)
- `imagenes_y_tablas/` — gráficos JPG y tablas Excel por fase
