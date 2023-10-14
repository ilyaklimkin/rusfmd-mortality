options(scipen = 999)

library(readr)
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(shiny)
library(thematic)
library(bslib)
library(scales)

#### custom functions ####

# table for SDR & LE
sdr_le_table <- function(df, input_points, metric) {
  
  if (metric == "sdr") {
    value <- "sdr"
  } else {
    value <- "le"
  }
  
  brushedPoints(df, input_points, xvar = "year", yvar = value) %>%
    mutate(val = value) %>%
    arrange(reg_sk, desc(year)) %>%
    transmute(
      Sex = case_when(
        sex_sk == 3 ~ "Both sexes",
        sex_sk == 1 ~ "Males",
        sex_sk == 2 ~ "Females"
      ),
      Age = if_else(
        val == "sdr",
        case_when(
          age_sk == 999 ~ "All ages",
          age_sk == 0 ~ "0-14",
          age_sk == 15 ~ "15-49",
          age_sk == 50 ~ "50-64",
          age_sk == 65 ~ "65+"
        ),
        as.character(age_sk)
      ),
      Region = reg_sk,
      Year = as.integer(year),
      Value = get(value)
    )
  
}

# plot for SDR & LE
sdr_le_plot <- function(df, metric) {
  
  sex_choices <- c(
    "3" = "Both sexes",
    "1" = "Males",
    "2" = "Females"
  )
  
  sdr_age_choices <- c(
    "999" = "All ages",
    "0" = "0-14",
    "15" = "15-49",
    "50" = "50-64",
    "65" = "65+"
  )
  
  le_age_choices <- c(
    "0" = "Age 0",
    "15" = "Age 15",
    "50" = "Age 50",
    "65" = "Age 65"
  )
  
  if (metric == "sdr") {
    age_choices <- sdr_age_choices
    value <- "sdr"
    title_string <- "Age-standardized death rates (SDR) per 1,000 person-years"
  } else {
    age_choices <- le_age_choices
    value <- "le"
    title_string <- "Life expectancy (LE) in years"
  }
  
  df %>%
    ggplot(aes(year, get(value), color = reg_sk)) +
    geom_line(linewidth = 1, alpha = 0.75) +
    geom_point(size = 1.25, alpha = 0.75) +
    facet_grid(
      age_sk ~ sex_sk,
      labeller = labeller(sex_sk = sex_choices,
                          age_sk = age_choices),
      scales = "free"
    ) +
    scale_x_continuous(
      breaks = pretty_breaks(),
      minor_breaks = seq(1989, 2022, 1)
    ) +
    labs(
      x = "",
      y = "",
      title = title_string,
      color = "",
      caption = "Select an area on the graph to see the values"
    ) +
    theme_bw() +
    theme(
      legend.position = "top",
      legend.text=element_text(size=16), # colors
      title = element_text(size = 18),
      plot.title = element_text(hjust = 0.5),
      strip.text.y = element_text(size = 14, color = "white"),
      strip.text.x = element_text(size = 14, color = "white"),
      strip.background = element_rect(fill = "#325d88"),
      axis.text.y = element_text(size = 14),
      axis.text.x = element_text(size = 14, vjust = 0.5, angle = 90),
      panel.grid.minor.y = element_line(color = "white")
    )
  
}

# LT calculation
lt <- function(df, n) {
  
  df %>%
    filter(pop == n) %>%
    mutate(
      n = case_when(
        x == 0 ~ 1,
        x == 1 ~ 4,
        x %in% c(5:80) ~ 5),
      ax = case_when(
        x == 0 ~ 0.07 + 1.7 * mx,
        x %in% c(1:80) ~ n / 2,
        x == 85 ~ 1 / mx),
      qx = if_else(
        x < 85,
        n * mx / (1 + (n - ax) * mx), # Chiang
        1
      ),
      px = 1 - qx,
      lx = cumprod(lag(px, default = 100000)),
      dx = lx * qx,
      Lx = if_else(
        x < 85,
        ax * lx + (n - ax) * lead(lx),
        ax * lx
      ),
      Tx = rev(cumsum(rev(Lx))),
      ex = Tx / lx
    )
  
}

# LT rendering
lt_for_render <- function(df) {
  
  if (nrow(df) > 0) {
    
    df %>%
      transmute(
        Age = case_when(
          x == 0 ~ "0",
          x == 1 ~ "1-4",
          x %in% c(5:80) ~ paste0(x, "-", x + 4),
          x == 85 ~ "85+"
        ),
        mx = sprintf("%.5f", round(mx, 5)),
        qx = sprintf("%.5f", round(qx, 5)),
        ax = sprintf("%.2f",round(ax, 2)),
        lx = as.integer(lx),
        dx = as.integer(dx),
        Lx = as.integer(Lx),
        Tx = as.integer(Tx),
        ex = sprintf("%.2f",round(ex, 2))
      )
    
  }
  
}

# plots for LT
lt_plot <- function(df1, df2, metric) {
  
  if (metric == "mx") {
    value <- "mx"
    title_string <- "Age-specific death rate between ages x and x + n (mx)"
  } else if (metric == "qx") {
    value <- "qx"
    title_string <- "Probability of dying between ages x and x + n (qx)"
  } else if (metric == "lx") {
    value <- "lx"
    title_string <- "Number of people left alive at age x (lx)"
  } else {
    value <- "dx"
    title_string <- "Number of people dying between ages x and x + n (dx)"
  }
  
  bind_rows(df1, df2) %>%
    transmute(
      pop = if_else(pop == 1, "Population 1", "Population 2"),
      x = if (metric %in% c("mx","qx","dx")) {
        case_when(
          x == 0 ~ x + 0.5,
          x == 1 ~ x + 2,
          TRUE ~ x + 2.5
        )
      } else x
      , val = get(value), metric = value
    ) %>%
    ggplot(aes(x = x, y = val, color = pop)) +
    geom_line(linewidth = 1) +
    geom_point(size = 1.25) + {
      if (metric == "qx" | metric == "mx") scale_y_log10()
    } +
    scale_x_continuous(breaks = seq(0,85,5)) +
    labs(
      x = "Age",
      y = if (metric == "qx" | metric == "mx") "log scale" else "",
      title = title_string,
      color = ""
    ) +
    theme_bw() +
    theme(
      legend.position = if (metric == "qx" | metric == "mx") "top" else "bottom",
      legend.text=element_text(size=16), # colors
      title = element_text(size = 18),
      plot.title = element_text(hjust = 0.5),
      axis.text.y = element_text(size = 14),
      axis.text.x = element_text(size = 14, vjust = 0.5, hjust=0.95, angle = 90),
      panel.grid.minor.y = element_line(color = "white"),
      panel.grid.minor.x = element_line(color = "white")
    )
  
}

#### load data ####

sdr <- read_csv("data/sdr.csv")
le <- read_csv("data/le.csv")
dr <- read_csv("data/dr.csv")
reg <- read_xlsx("data/reg_codebook.xlsx")

#### ui ####

# create named list of Region
reg_list <- setNames(reg$reg_sk, reg$reg)

# create named list of Sex
sex_list <- c(
  "Both sexes" = 3,
  "Males" = 1,
  "Females" = 2
)

# create named list of Age
# for SDR
sdr_age_list <- c(
  "All ages" = 999,
  "0-14" = 0,
  "15-49" = 15,
  "50-64" = 50,
  "65+" = 65
)

# for LE
le_age_list <- c(
  "0" = 0,
  "15" = 15,
  "50" = 50,
  "65" = 65
)

ui <- fluidPage(
  theme = bs_theme(bootswatch = "sandstone"),
  fluidRow(
    column(
      HTML(
        "<center><font size = \"3\">This application allows you to briefly
        explore basic mortality metrics for the regions of the Russian
        Federation by calendar year, sex and age group. The application
        contains three tabs: <b>SDR</b> (Age-Standardized Death Rate),
        <b>LE</b> (Life Expectancy), and <b>LT</b> (Life Table). You can
        access the original data and the detailed information about the data
        source being utilized
        <a href=\"http://demogr.nes.ru/en/demogr_indicat/data_description\">here</a>,
        while the calculations and the application source code are available
        <a href=\"http://github.com/ilyaklimkin/rusfmd-mortality\">here</a>.</font><br>
        <font size = \"2\"><i>All the calculations are performed on
        age-specific mortality rates (ASMR) by five-year age groups up to age
        85+. In rare cases ASMRs were forecasted, e.g. for some small
        populations where the value of the ASMR at age 85+ was 0 (for the
        details see the source code).</i></font></center>"
      ),
      width = 12 
    )
  ),
  tabsetPanel(
    tabPanel(
      "SDR",
      fluidRow(
        column(
          HTML(
            "<center><font size = \"3\">In this tab, you'll find data on
            age-standardized death rate (SDR) dynamics. Select your
            preferred parameters on the left, including the WHO Population
            standard.</font></center><br>"
          ),
          offset = 1,
          width = 10
        )
      ),
      sidebarLayout(
        sidebarPanel(
          selectInput(
            "sdr_reg",
            "Region",
            choices = reg_list,
            selected = c(1145,1146,1103,1140),
            multiple = TRUE
          ),
          sliderInput(
            "sdr_year",
            "Year",
            min = 1989, max = 2022,
            value = c(1989,2022),
            step = 1,
            sep = ""
          ),
          checkboxGroupInput(
            "sdr_sex",
            "Sex",
            choices = sex_list,
            selected = sex_list[-1]
          ),
          checkboxGroupInput(
            "sdr_age",
            "Age",
            choices = sdr_age_list,
            selected = sdr_age_list
          ),
          radioButtons(
            "sdr_standard",
            "Population standard",
            choices = c(1976,2013),
            selected = 1976
          ),
          width = 3
        ),
        mainPanel(
          textOutput("no_data_sdr"),
          plotOutput(
            "sdr_plot",
            width = "100%",
            height = "600",
            brush = "sdr_brush"
          ),
          tableOutput(
            "sdr_data"
          ),
          width = 9
        )
      )
    ),
    tabPanel(
      "LE",
      fluidRow(
        column(
          HTML(
            "<center><font size = \"3\">In this tab, you'll find data on
            life expectancy (LE) dynamics. Select your preferred
            parameters on the left. The Life expectancy values were obtained
            by the construction of abridged life tables.</font></center><br>"
          ),
          offset = 1,
          width = 10
        )
      ),
      sidebarLayout(
        sidebarPanel(
          selectInput(
            "le_reg",
            "Region",
            choices = reg_list,
            selected = c(1145,1146,1103,1140),
            multiple = TRUE
          ),
          sliderInput(
            "le_year",
            "Year",
            min = 1989, max = 2022,
            value = c(1989,2022),
            sep = ""
          ),
          checkboxGroupInput(
            "le_sex",
            "Sex",
            choices = sex_list,
            selected = sex_list[-1]
          ),
          checkboxGroupInput(
            "le_age",
            "Age",
            choices = le_age_list,
            selected = le_age_list
          ),
          width = 3
        ),
        mainPanel(
          textOutput("no_data_le"),
          plotOutput(
            "le_plot",
            width = "100%",
            height = "600",
            brush = "le_brush"
          ),
          tableOutput("le_data"),
          width = 9
        )
      )
    ),
    tabPanel(
      "LT",
      fluidRow(
        column(
          HTML(
            "<center><font size = \"3\">In this tab, you'll find data on
            life table (LE) functions for two chosen populations. Select
            the populations to be compared by configuring the Region-Year-Sex
            combination right below.</font></center>"
          ),
          offset = 1,
          width = 10
        )
      ),
      fluidRow(
        column(
          h5("Population 1"),
          width = 6
        ),
        column(
          h5("Population 2"),
          width = 6
        )
      ),
      fluidRow(
        column(
          selectInput(
            "lt_reg1",
            "Region",
            choices = reg_list,
            selected = 1100
          ),
          width = 6
        ),
        column(
          selectInput(
            "lt_reg2",
            "Region",
            choices = reg_list,
            selected = 1100
          ),
          width = 6
        )
      ),
      fluidRow(
        style = "border-bottom: 1px solid #dfd7ca;",
        column(
          selectInput(
            "lt_year1",
            "Year",
            choices = c(2022:1989),
            selected = 2022
          ),
          width = 3
        ),
        column(
          selectInput(
            "lt_sex1",
            "Sex",
            choices = sex_list,
            selected = 1
          ),
          width = 3
        ),
        column(
          selectInput(
            "lt_year2",
            "Year",
            choices = c(2022:1989),
            selected = 2022
          ),
          width = 3
        ),
        column(
          selectInput(
            "lt_sex2",
            "Sex",
            choices = sex_list,
            selected = 2
          ),
          width = 3
        )
      ),
      fluidRow(
        style = 'padding-top:16px;',
        column(
          plotOutput(
            "mx_plot",
            width = "100%",
            height = "400"
          ),
          width = 6
        ),
        column(
          plotOutput(
            "qx_plot",
            width = "100%",
            height = "400"
          ),
          width = 6
        )
      ),
      fluidRow(
        column(
          plotOutput(
            "lx_plot",
            width = "100%",
            height = "400"
          ),
          width = 6
        ),
        column(
          plotOutput(
            "dx_plot",
            width = "100%",
            height = "400"
          ),
          width = 6
        )
      ),
      fluidRow(
        column(
          h5("Life Table for the Population 1"),
          tableOutput("lt_data1"),
          textOutput("no_data1"), # print only if Chechen R 1993-2002 selected
          offset = 1,
          width = 10
        )
      ),
      fluidRow(
        column(
          h5("Life Table for the Population 2"),
          tableOutput("lt_data2"),
          textOutput("no_data2"), # print only if Chechen R 1993-2002 selected
          offset = 1,
          width = 10
        )
      )
    )
  )
)

#### server function ####

server <- function(input, output, session) {
  
  # for ASDR panel
  # filter data
  sdr_df <- reactive({
    sdr %>%
      filter(
        (year >= input$sdr_year[1] & year <= input$sdr_year[2]),
        reg_sk %in% input$sdr_reg,
        sex_sk %in% input$sdr_sex,
        esp_year == input$sdr_standard,
        age_sk %in% input$sdr_age
      ) %>%
      left_join(reg, by = "reg_sk") %>%
      transmute(
        year, reg_sk = reg,
        sex_sk = factor(sex_sk, levels = c(3,1,2)), esp_year,
        age_sk = factor(age_sk, levels = c(999,0,15,50,65)), sdr
      )
  })
  
  # render plot
  output$sdr_plot <- renderPlot({
    req(input$sdr_reg)
    req(input$sdr_sex)
    req(input$sdr_age)
    sdr_le_plot(df = sdr_df(), metric = "sdr")
  })
  
  # in case if the input parameters are not selected
  output$no_data_sdr <- renderText({
    if (is.null(input$sdr_reg) | is.null(input$sdr_sex) | is.null(input$sdr_age)) {
      "Please, select at least one combination of Region-Sex-Age"
    }
  })
  
  # render table
  output$sdr_data <- renderTable({
    req(input$sdr_brush)
    sdr_le_table(df = sdr_df(), input_points = input$sdr_brush, metric = "sdr")
  }, spacing = "xs", width = "100%")
  
  # for LE panel
  # filter data
  le_df <- reactive({
    le %>%
      filter(
        (year >= input$le_year[1] & year <= input$le_year[2]),
        reg_sk %in% input$le_reg,
        sex_sk %in% input$le_sex,
        age_sk %in% input$le_age
      ) %>%
      left_join(reg, by = "reg_sk") %>%
      transmute(
        year, reg_sk = reg,
        sex_sk = factor(sex_sk, levels = c(3,1,2)),
        age_sk = factor(age_sk, levels = c(0,15,50,65)), le
      )
  })
  
  # render plot
  output$le_plot <- renderPlot({
    req(input$le_reg)
    req(input$le_sex)
    req(input$le_age)
    sdr_le_plot(df = le_df(), metric = "le")
  })
  
  # in case if the input parameters are not selected
  output$no_data_le <- renderText({
    if (is.null(input$le_reg) | is.null(input$le_sex) | is.null(input$le_age)) {
      "Please, select at least one combination of Region-Sex-Age"
    }
  })
  
  # render table
  output$le_data <- renderTable({
    req(input$le_brush)
    sdr_le_table(df = le_df(), input_points = input$le_brush, metric = "le")
  }, spacing = "xs", width = "100%")
  
  # for LT panel
  # filter data
  lt_df <- reactive({
    dr %>%
      filter(
        year == input$lt_year1,
        reg_sk == input$lt_reg1,
        sex_sk == input$lt_sex1
      ) %>%
      transmute(x = age, mx = dr, pop = 1) %>%
      bind_rows(
        dr %>%
          filter(
            year == input$lt_year2,
            reg_sk == input$lt_reg2,
            sex_sk == input$lt_sex2
          ) %>%
          transmute(x = age, mx = dr, pop = 2)
      ) %>%
      drop_na()
  })
  
  # render tables
  # population 1
  output$lt_data1 <- renderTable({
    lt_for_render(
      lt(df = lt_df(), n = 1)
    )
  }, spacing = "xs", width = "100%", bordered = TRUE, align = "r")
  
  output$no_data1 <- renderText({
    if (input$lt_reg1 == 1196 & input$lt_year1 %in% c(1993:2002)) {
      "No data available for the selected combination of Region-Year-Sex"
    }
  })
  
  # population 2
  output$lt_data2 <- renderTable({
    lt_for_render(
      lt(df = lt_df(), n = 2)
    )
  }, spacing = "xs", width = "100%", bordered = TRUE, align = "r")
  
  output$no_data2 <- renderText({
    if (input$lt_reg2 == 1196 & input$lt_year2 %in% c(1993:2002)) {
      "No data available for the selected combination of Region-Year-Sex"
    }
  })
  
  # render plots
  # mx
  output$mx_plot <- renderPlot({
    lt_plot(
      df1 = lt(df = lt_df(), n = 1),
      df2 = lt(df = lt_df(), n = 2),
      metric = "mx"
    )
  })
  
  # qx
  output$qx_plot <- renderPlot({
    lt_plot(
      df1 = lt(df = lt_df(), n = 1),
      df2 = lt(df = lt_df(), n = 2),
      metric = "qx"
    )
  })
  
  # lx
  output$lx_plot <- renderPlot({
    lt_plot(
      df1 = lt(df = lt_df(), n = 1),
      df2 = lt(df = lt_df(), n = 2),
      metric = "lx"
    )
  })
  
  # dx
  output$dx_plot <- renderPlot({
    lt_plot(
      df1 = lt(df = lt_df(), n = 1),
      df2 = lt(df = lt_df(), n = 2),
      metric = "dx"
    )
  })
  
  thematic_shiny()
  
}

shinyApp(ui = ui, server = server)
