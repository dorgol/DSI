#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
########################################################################################################################

library(shiny)

# DATA ####
TT <- read.csv("TT_jane.csv", stringsAsFactors = F, check.names = F)[,-1]
TT$per_code_factor = as.factor(TT$PER_CODE)
TT$per_code_factor = reorder(TT$per_code_factor, TT$total_days_since_first_collection) #for plot order later
vital_vars = str_extract(names(TT_blood_infection), "V_.*")[str_detect(names(TT_blood_infection), "V_.*")]

# UI ####

ui <- fluidPage(
   
   # Application title
   titlePanel("Investigate Burn Data"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(width = 2,
        
        selectInput("variable", "Variable:",
                    c("Heart Rate" = "V_HEART_RATE",
                      "Respiratory Rate" = "V_RESPIRATORY_RATE",
                      "Temperature" = "V_TEMPERATURE",
                      "White Blood Cells" = "V_WHITE_BC",
                      "Sodium" = "V_SODIUM",
                      "Potassium" = "V_POTASSIUM",
                      "Glucose" = "V_GLUCOSE"
                      )), 
        
        checkboxGroupInput("outcome", "Study Outcome:",
                    selected = "Completed protocol",
                    choiceNames = sort(unique(TT$`study outcome abbr.`)),
                    choiceValues = sort(unique(TT$`study outcome abbr.`))
        ),
        
        sliderInput("n_blood", "Number blood infections:",
                    min = 0, max = max(TT$n_blood, na.rm =T),
                    value = c(0,5), step = 1
        ),
        
        selectInput("order", "Order plots by:",
                    selected = "total_days_since_first_collection",
                    choices = c("study outcome abbr.", 
                                "total_days_since_first_collection",
                                "n_blood")
        ),
        
        numericInput("maxplots", "Max plots to show:", value = 20),
        
        numericInput("seed", "Seed for plot selection:", value = 1),
        
        selectInput("individual", "PER_CODE:",
                    choices = sort(unique(TT$PER_CODE))
        )
      ),
    
      mainPanel(
      
      tabsetPanel(type = "tabs",
                  tabPanel("Pop. Vitals", plotOutput("varPlot", width = "800px", height="800px")),
                  tabPanel("Individual Vitals", plotOutput("indPlot", width = "800px", height="800px"))
      )
    ) 
  )
)

# SERVER ####

server <- function(input, output) {
   
   output$varPlot <- renderPlot({
     
      set.seed(input$seed)
      var1    <- input$variable
      data.vital  <- reactive({ filter(TT, 
                                  `study outcome abbr.` %in% input$outcome,
                                  n_blood >= input$n_blood[1], 
                                  n_blood <= input$n_blood[2]
                                  ) %>% mutate(per_code_factor = droplevels(per_code_factor)) %>%
                           filter(per_code_factor %in% sample(levels(per_code_factor), size = input$maxplots)) 
                }) #for plot order later
      
      var.plot = ggplot(data.vital()) +
        geom_line(aes(x = days_since_first_collection, y = get(var1, pos = -1), group = per_code_factor, color = `study outcome abbr.`)) +
        geom_point(data = filter(data.vital(), !is.na(V_TIME_PERFORMED_1)),
                   aes(x = days_since_first_collection, y = get(var1, pos = -1), group = per_code_factor),
                   fill = "blue", stroke = .1, size = 1, alpha = .5, shape = 21) +
        geom_point(data = filter(data.vital(), !is.na(V_TIME_PERFORMED_2)),
                   aes(x = days_since_first_collection, y = get(var1, pos = -1), group = per_code_factor),
                   fill = "green", stroke = .1, size = .5, alpha = .5, shape = 21) +
        geom_point(data = filter(data.vital(), Blood), 
                   aes(x = days_since_first_collection, y = get(var1, pos = -1), group = per_code_factor),
                   fill = "red", stroke = .1, shape = 21, size = 2) +
        scale_fill_manual(labels = c("Blood CBC", "Blood Chemistry","Blood infection")) +
        theme_bw() +
        theme(legend.position = "top", axis.text.x = element_text(hjust = 1, angle = 45, size = 6),
              axis.text.y = element_blank(),
              legend.text = element_text(size = 8), 
              strip.text = element_text(size = 10),
              strip.background = element_blank()) +
        ylab(var1) + 
        facet_wrap(~per_code_factor, scales = "free_x") +
        ggtitle(paste(var1, "for patients"))
        
        # Vital-specific additions
        if (var1 == "V_TEMPERATURE") {
          var.plot = var.plot + geom_hline(yintercept = 39, linetype = 2, size = .1) +
                                geom_hline(yintercept = 36.5, linetype = 2, size = .1)
        }
      var.plot
   })
   
   output$indPlot <- renderPlot({
     
     person_to_look_at  <- input$individual # c("TT-001-00282", "TT-010-02112", "TT-001-01068") - some intersting ones
     data.ind <- reactive({
                      filter(TT, PER_CODE == person_to_look_at) %>% 
                      select(c(vital_vars,"Blood")) %>%
                      select(-one_of("V_ER_DEF_DATE_01","V_TIME_PERFORMED")) %>%
                      melt(id=c("V_DATE_COLLECTION", "V_TIME_PERFORMED_1", "V_TIME_PERFORMED_2", "V_TIME_PERFORMED_3", "Blood"))
                })

     ind.plot = ggplot(data.ind(), aes(x = V_DATE_COLLECTION, y = value, color = variable, group = variable)) +
       geom_line(inherit.aes = TRUE) + 
       geom_point(data = filter(data.ind(), Blood == "TRUE"),
                  inherit.aes = TRUE, fill = "blue", stroke = .1, size = 1, alpha = .5, shape = 21) +
       geom_point(data = filter(data.ind(), !is.na(V_TIME_PERFORMED_1)),
                  inherit.aes = TRUE, 
                  fill = "blue", stroke = .1, size = 1, alpha = .5, shape = 21) +
       geom_point(data = filter(data.ind(), !is.na(V_TIME_PERFORMED_2)),
                  inherit.aes = TRUE, 
                  fill = "blue", stroke = .1, size = 1, alpha = .5, shape = 21) +
       theme(legend.position = "top", axis.text.x = element_text(hjust = 1, angle = 90),
             axis.text.x = element_blank(),
             legend.text = element_text(size = 8),
             strip.text = element_text(size = 10)) + 
       guides(color = FALSE) + 
       ggtitle(paste("Individual vitals for",person_to_look_at)) +
       facet_wrap(~variable, scales = "free_y")
     
     ind.plot
   })
}

# RUN ####
shinyApp(ui = ui, server = server)

