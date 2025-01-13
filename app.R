
# Libraries ----

library(shiny)
library(bslib)
library(thematic)
library(showtext)
library(tidyverse)
library(reactable)
library(lubridate)
library(plotly)
library(bsicons)

# Data Setup ----

running_raw <- read_csv("Activities.csv", na = "--")

cols_remove <- running_raw %>% 
    select(Favorite, `Normalized Power® (NP®)`:`Max Power`, Decompression:`Number of Laps`) %>% 
    colnames

running <- running_raw %>% 
    select(-any_of(cols_remove)) %>% 
    mutate(
        Date = as_date(Date, tz = "America/New_York"),
        WeekStart = floor_date(Date, unit = "week", week_start = getOption("lubridate.week.start", 1)),
        Day = weekdays(Date) %>% factor(levels = weekdays(ISOdate(1, 1, 1:7))),
        Year = year(Date) %>% factor(),
        Month = month(Date, label = TRUE, abbr = TRUE) %>% factor(levels = month.abb),
        .after = `Activity Type`
    ) %>% 
    mutate(
        `Activity Type` = case_match(
            `Activity Type`,
            "Treadmill Running" ~ "Treadmill",
            "Running" ~ "Outdoor"
        ),
        `Activity Type` = case_match(
            Title,
            c(
                "EQT 10 Miler",
                "Pittsburgh - DICK'S Sporting Goods Pittsburgh Marathon",
                "Pittsburgh - Richard S. Caliguiri City of Pittsburgh Great Race"
            ) ~
                "Race",
            .default = `Activity Type`
        ),
        `Activity Type` = factor(`Activity Type`, levels = c("Treadmill", "Outdoor", "Race")),
        Title = if_else(`Activity Type` == "Race", str_remove(Title, "^Pittsburgh - "), NA)
    ) 

# # Wrote the following code to combine activities that were split in two but I forgot that I ran home from my 10k so I'm going to keep activities separate for now
# # Fix days where activity was split into two
# summarise(
#     .by = c(`WeekStart`:Date),
#     `Activity Type` = if_else(n_distinct(`Activity Type`)>1, "Outside", `Activity Type`[1]),
#     across(-contains(c("Avg", "Max", "Min", "Best"))& -where(is.character), ~ sum(.x, na.rm = TRUE)),
#     across(contains("Avg"), ~ mean(.x, na.rm = TRUE)),
#     across(contains(c("Max")), ~ ifelse(all(is.na(.x)), NA, max(.x, na.rm = TRUE))),
#     across(contains(c("Min", "Best")), ~ ifelse(all(is.na(.x)), NA, min(.x, na.rm = TRUE))),
#     across(where(is.difftime), ~  as.numeric(.x) %>% hms::as_hms())
# ) 

num_days <- as.numeric(max(running$Date) - min(running$Date)) 

str_date_range <- paste(
    running$Date %>% min %>% format("%b %d, %y"),
    running$Date %>% max %>% format("%b %d, %y"),
    sep = " - "
)

str_var_continuous <- running %>% 
    select(-(`Activity Type`:Title)) %>% colnames()

reverse_pace <- function(x){
    max(x, na.rm = TRUE)-x
}

# Components -----

## Theme ----
my_theme <- bs_theme(
    bootswatch = "sandstone"
)

# Some common theme variables to reference
col <- bslib::bs_get_variables(
  theme = my_theme,
  varnames = c(
    "primary",
    "secondary",
    "success",
    "info",
    "warning",
    "danger",
    "light",
    "dark"
  )
) %>% as.list

# Enable thematic
thematic::thematic_shiny(font = "auto")

# Change ggplot2's default "gray" theme
theme_set(theme_minimal(base_size = 15))

# Color palette for Activity Type
pal_activities <- c(col$success, col$info, col$warning) %>% 
  setNames(., c("Outdoor", "Treadmill", "Race"))

# for labeling with info tool tips
tooltip_label <- function(label, tip) {
  tooltip(
    trigger = list(
      label,
      bsicons::bs_icon("info-circle")
    ),
    options = list(customClass = "text-start"),
    tip
  )
}

nav_item_link <- function(bs_icon_name, text, link) {
  nav_item(
    tags$a(bs_icon(bs_icon_name), text, href = link, target = "_blank")
  )
}

info_links <- list(
  nav_item_link(
    "github", 
    "Code",
    "https://github.com/apelakh/garmin_shiny_app"
  ),
  
  nav_item_link(
    "send", 
    "email",
    "mailto:apelakh@gmail.com"
  ),
  
  nav_item_link(
    "house-fill", 
    "Website",
    "https://apelakh.github.io/"
  )
)


## Plot Input Controls ----
inputs_plots <- list(
  ### Plot 1 Controls ----
  accordion_panel(
    "Plot 1 Controls",
    
    input_switch(
      "plot1_show_activities",
      "Show Individual Runs",
      value = TRUE
    ),
    input_switch(
      "plot1_show_weekly",
      label = tooltip_label(
        "Weekly Summary",
        "Some variables (e.g., Distance, Time) are summed by week, others are averaged (e.g., Avg Pace, Max HR)"
      ),
      value = FALSE
    ),
    
  ),
  ### Plot 2 Controls ----
  accordion_panel(
    "Plot 2 Controls",
    selectizeInput(
      "plot2_var_x",
      label = "X-Axis Variable",
      choices = str_var_continuous %>% 
        str_subset("Distance", negate = TRUE)
    )
  ), 
  ### Plot 3 Controls ----
  accordion_panel(
    "Plot 3 Controls",
    selectizeInput(
      "plot3_var_x",
      label = "X-Axis Variable",
      choices = c("Activity Type", "Day", "Year", "Month"),
      selected = "Day"
    )
  )
)

## Value Boxes ----
value_boxes <- list(
    value_box(
        title = str_date_range,
        value = paste(num_days, "days"),
        showcase = icon("calendar"),
        full_screen = FALSE,
        class = "text-primary border-primary bg-light-subtle"
    ),
    value_box(
        title = "Total Distance (miles)",
        value = running$Distance %>% sum(),
        showcase = icon("ruler"),
        full_screen = FALSE,
        class = "text-primary border-primary bg-light-subtle"
    ),
    value_box(
        title = "Number of Runs",
        value = nrow(running),
        showcase = icon("person-running"),
        full_screen = FALSE,
        class = "text-primary border-primary bg-light-subtle"
        # Took this line out for now because it took up too much space
        # p(scales::percent((nrow(running)/num_days)) %>% paste("of days elapsed"))
    )
)

## Plots ----
plots <- list(
  card(
    id = "card_plot1",
    full_screen = TRUE,
    padding = 0, 
    card_header("Plot 1"),
    card_body(
      plotOutput("plot1")
    )
  ),
  
  card(
    id = "card_plot2",
    full_screen = TRUE,
    padding = 0, 
    card_header("Plot 2"),
    card_body(
      plotOutput("plot2")
    )
  ),
  
  card(
    id = "card_plot3",
    full_screen = TRUE,
    padding = 0, 
    card_header("Plot 3"),
    card_body(
      plotOutput("plot3")
    )
  )
)

# UI ----
ui <- page_navbar(
    
    title = "Garmin Running Data",
    bg = col$dark,
    theme = my_theme,
    
    ## Sidebar ----
    sidebar = sidebar(
    
      # input_dark_mode(mode = "light"), # This line adds a dark mode toggle
      # I am not using it because it takes up too much space where it is and I can't add a label.
      
      ### Global Input ----
      selectizeInput(
        "var_y",
        label = "Y-Axis (all plots)",
        choices = str_var_continuous,
        selected = "Distance"
      ),
      
      ### Accordion ----
      # Plot-specific controls
      accordion(
        open = TRUE, 
        !!!inputs_plots
      )
      
    ), 
    ## Navbar Panels ----
    
    
    ### Dashboard ----
    nav_panel(
      title = bs_icon("speedometer2"),
      value = "dashboard",
      
      layout_columns(
        fill = FALSE,
        !!!value_boxes
      ),
      
      layout_column_wrap(
        width = 1,
        heights_equal = "row",
        height_mobile = "250px",
        plots[[1]]
      ),

      layout_column_wrap(
        width = 1/2,
        height_mobile = "500px",
        plots[[2]],
        plots[[3]]
      )
    ),
    
    ### Data ----
    nav_panel(
      title = bs_icon("table"),
      card(
        reactableOutput("show_data") 
      )
    ),
    
    ### About ----
    nav_panel(
      title = bs_icon("info-circle-fill"),
      card(
        includeMarkdown("about.md")
      )
    ),
    
    ### Info and Links ----
    nav_spacer(),
    !!!info_links
    
)

# Server ----
server <- function(input, output) {
    
  # bs_themer() # This enables an interactive theme window when the app is running
    
  ## listeners ----
  
  observeEvent(input$var_y, {
    
    print(input$var_y)
    print(input$plot2_var_x)
    
    updateSelectizeInput(
      inputId = "plot2_var_x",
      choices = str_subset(str_var_continuous, input$var_y, negate = TRUE)
    )
    
  })
  
  ## Plot 1 ----
  output$plot1 <- renderPlot({
    
    var_y <- input$var_y
    show_activities <- input$plot1_show_activities
    show_weekly <- input$plot1_show_weekly
    pace_reverse <- str_detect(var_y, "Pace|GAP")
    
    weekly_stat <- if_else(
      var_y %in%
        str_subset(str_var_continuous, "Avg|Min|Max|Best|Aerobic"),
      "mean",
      "sum"
    )
    
    if (pace_reverse) {
      max_value <- running[[var_y]] %>% max(., na.rm = TRUE)
      
      running <- running %>%
        mutate(across(all_of(var_y), reverse_pace))
    }
    
    p <- running %>%
      ggplot(mapping = aes(x = Date, y = .data[[var_y]])) 
    
    if (show_weekly){
      p <- p +
        stat_summary(
          mapping = aes(group = 1, x = WeekStart + days(3)),
          fun = weekly_stat,
          geom = "area",
          linewidth = 1,
          fill = col$primary,
          color = col$primary,
          alpha = .1,
          outline.type = "upper"
        )
    }
    
    if (show_activities) {
      p <- p +
        geom_col(mapping = aes(fill = `Activity Type`),
                 position = "dodge")
    }
      p <- p +
      scale_x_date(
        date_breaks = "1 month",
        date_labels = "%b %Y",
        expand = expansion(mult = .01)
      ) +
      scale_fill_manual(values = pal_activities) +
      theme(plot.margin = unit(c(0, 0, 0, 0), units = "mm"))
    
    if (pace_reverse) {
      p <- p +
        scale_y_time(labels = ~ hms::as_hms(max_value - .x))
    }
    
    return(p)
  })
    
  ## Plot 2 ----
  output$plot2 <- renderPlot({
      
      var_x <- input$plot2_var_x
      var_y <- input$var_y
      
      pace_reverse_x <- str_detect(var_x, "Pace|GAP")
      pace_reverse_y <- str_detect(var_y, "Pace|GAP")
      
      if (pace_reverse_x) {
        max_value_x <- running[[var_x]] %>% max(., na.rm = TRUE)
        
        running <- running %>%
          mutate(across(all_of(var_x), reverse_pace))
      }
      
      if (pace_reverse_y) {
        max_value_y <- running[[var_y]] %>% max(., na.rm = TRUE)
        
        running <- running %>%
          mutate(across(all_of(var_y), reverse_pace))
      } 
      
      df_cor <- running %>% 
          select(all_of(c(var_x, var_y))) %>% 
          mutate(across(everything(), as.numeric)) %>% 
          rstatix::cor_test() %>% 
          mutate(
              p = rstatix::p_format(p) %>% rstatix::p_mark_significant()
          )
      
      p_title <- paste0(
          "R = ", df_cor$cor, ", p = ", df_cor$p 
      )
      
      p <- running %>% 
          ggplot(mapping = aes(x = .data[[var_x]], y = .data[[var_y]])) +
          geom_point(color = col$primary, alpha = .5, size = 2) +
          geom_smooth(method = 'loess', formula = 'y~x', color = col$warning, fill = col$warning) +
          # stat_bin_2d(bins = 20) +
          labs(title = p_title)
      
      if (pace_reverse_x) {
        p <- p +
          scale_x_time(labels = ~ hms::as_hms(max_value_x - .x))
      }
      
      if (pace_reverse_y) {
        p <- p +
          scale_y_time(labels = ~ hms::as_hms(max_value_y - .x))
      }
      
      return(p)
      
  })
  
  ## Plot 3 ----
  output$plot3 <- renderPlot({
      
      var_x <- input$plot3_var_x
      var_y <- input$var_y
      
      pace_reverse_y <- str_detect(var_y, "Pace|GAP")
      
      if (pace_reverse_y) {
        max_value_y <- running[[var_y]] %>% max(., na.rm = TRUE)
        
        running <- running %>%
          mutate(across(all_of(var_y), reverse_pace))
      } 
      
      p <- running %>% 
          ggplot(mapping = aes(x = .data[[var_x]], y = .data[[var_y]])) +
          geom_boxplot(fill = col$primary, color = col$primary, linewidth = .75, alpha = .1) +
          ggbeeswarm::geom_beeswarm(color = col$danger, alpha = .7, size = 2)
      
      if (pace_reverse_y) {
        p <- p +
          scale_y_time(labels = ~ hms::as_hms(max_value_y - .x))
      }
      
      return(p)
      
  })
  
  ## Data ----
  
  output$show_data <- renderReactable({
      running %>% reactable()
  })
}

# Run the application 
shinyApp(ui = ui, server = server)


  
        
               