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
library(dplyr)
library(forcats)
library(ggplot2)
library(reshape)

# DATA ####
TT <- read.csv("TT_jane.csv", stringsAsFactors = F, check.names = F)[,-1]
TT$per_code_factor = as.factor(TT$PER_CODE)
TT$per_code_factor = reorder(TT$per_code_factor, TT$total_days_since_first_collection) #for plot order later
vital_vars = str_extract(names(TT_blood_infection), "V_.*")[str_detect(names(TT_blood_infection), "V_.*")]
vital_vars = c("V_DATE_COLLECTION", "V_TIME_PERFORMED_1", "V_TIME_PERFORMED_2", "V_TIME_PERFORMED_3",
                          "V_HEART_RATE", "V_PB_SYSTOLIC", "V_BP_DIASTOLIC", "V_MEANARTERIAL_PRESSURE", "V_CENTRAL_VENOUS_PRESSURE",
                          "V_TEMPERATURE", "V_PAO2", "V_FIO2", "V_PACO2", "V_HCO3",
                          "V_RESPIRATORY_RATE", "V_SODIUM", "V_POTASSIUM", "V_BLOOD_UREA_NITROGEN", "V_CREATININE",
                          "V_WHITE_BC", "V_HEMOGLOBIN", "V_HEMATOCRIT",  "V_PLATELET_COUNT", "V_GLASCOWCOMA_SCALE", 
                          "V_GLUCOSE", "V_MODS_SCORE", "V_PH","V_BILIRUBIN") #for individual plot ordering later
                          
TT_Demo1 = read.csv("TT_Demo1.csv")[,-1]

# UI ####

ui <- fluidPage(
   
   # Application title
   titlePanel("Investigate Burn Data"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(width = 3,
        
        selectInput("variable", "Variable:",
                    c("Heart Rate" = "V_HEART_RATE",
                      "Respiratory Rate" = "V_RESPIRATORY_RATE",
                      "Temperature" = "V_TEMPERATURE",
                      "Platelets" = "V_PLATELET_COUNT",
                      "White Blood Cells" = "V_WHITE_BC",
                      "Glucose" = "V_GLUCOSE",
                      "Sodium" = "V_SODIUM",
                      "Potassium" = "V_POTASSIUM"
                      ), selected = "V_TEMPERATURE"), 
        
        checkboxGroupInput("outcome", "Study Outcome:",
                    selected = "Completed protocol",
                    choiceNames = sort(unique(TT$`study outcome abbr.`)),
                    choiceValues = sort(unique(TT$`study outcome abbr.`))
        ),
        
        sliderInput("n_blood", "Number blood infections:",
                    min = 0, max = max(TT$n_blood, na.rm =T),
                    value = c(1,5), step = 1
        ),
        
        selectInput("order", "Order plots by:",
                    selected = "total_days_since_first_collection",
                    choices = c("study outcome abbr.", 
                                "TBSA",
                                "Age",
                                "total_days_since_first_collection",
                                "Gender",
                                "n_blood",
                                "n_any",
                                "n_wound",
                                "n_urine",
                                "n_pneumonia")
        ),
        
        numericInput("maxplots", "Max plots to show:", value = 16),
        
        numericInput("seed", "Seed for random plot subset:", value = 1),
        
        selectInput("individual", "PER_CODE:",
                    choices = sort(unique(TT$PER_CODE))
        )
      ),
    
      mainPanel(
      
      tabsetPanel(type = "tabs",
                  tabPanel("Pop. Vitals", plotOutput("varPlot", width = "1000px", height="1000px")),
                  tabPanel("Individual Vitals", plotOutput("indPlot", width = "1200px", height="1000px"),
                           tableOutput("indText1"), tableOutput("indText2"), tableOutput("indText3"))
      )
    ) 
  )
)

# SERVER ####

server <- function(input, output) {
   
   output$varPlot <- renderPlot({
     
      set.seed(input$seed)
      var1    <- input$variable
      data.vital  <- reactive({ TT %>% mutate(per_code_factor = fct_reorder(per_code_factor, rank(TT[[input$order]], ties.method = "random"))) %>% 
                                filter( `study outcome abbr.` %in% input$outcome,
                                  n_blood >= input$n_blood[1], 
                                  n_blood <= input$n_blood[2]
                                ) %>% 
                                mutate(per_code_factor = droplevels(per_code_factor)) %>%
                                filter(per_code_factor %in% sample(levels(per_code_factor), size = input$maxplots))}) 
      
      var.plot = ggplot(data.vital())
      
      # Vital-specific additions to highlight normal ranges
      if (var1 == "V_TEMPERATURE") {
        var.plot = var.plot + geom_ribbon(aes(x = days_since_first_collection, min = 36.5, ymax = 39, fill= Gender), alpha = .3)
        #geom_hline(yintercept = 39, linetype = 2, size = .5) +
        #geom_hline(yintercept = 36.5, linetype = 2, size = .5)
      }
      if (var1 == "V_HEART_RATE") {
        var.plot = var.plot + geom_ribbon(aes(x = days_since_first_collection, ymin = 0, ymax = 110, fill = Gender),  alpha = .3)
      }
      if (var1 == "V_RESPIRATORY_RATE") {
        var.plot = var.plot +  geom_ribbon(aes(x = days_since_first_collection, ymin = 0, ymax = 25, fill = Gender), alpha = .3)
      }
      if (var1 == "V_PLATELET_COUNT") {
        var.plot = var.plot +  geom_ribbon(aes(x = days_since_first_collection, ymin = 0, ymax = 100, fill = Gender), alpha = .3)
      }
      if (var1 == "V_GLUCOSE") {
        var.plot = var.plot +  geom_ribbon(aes(x = days_since_first_collection, ymin = 0, ymax = 200, fill = Gender), alpha = .3)
      }
      
      var.plot = var.plot +
        geom_line(aes(x = days_since_first_collection, y = get(var1, pos = -1), group = per_code_factor, color = `study outcome abbr.`)) +
        
        # points for measurements taken
        geom_point(data = filter(data.vital(), !is.na(V_TIME_PERFORMED_1)),
                   aes(x = days_since_first_collection, y = get(var1, pos = -1), group = per_code_factor),
                   fill = "blue", stroke = .1, size = 2, alpha = .5, shape = 21) +
        geom_point(data = filter(data.vital(), !is.na(V_TIME_PERFORMED_2)),
                   aes(x = days_since_first_collection, y = get(var1, pos = -1), group = per_code_factor),
                   fill = "green", stroke = .1, size = 1, shape = 21) +
        
        # points for infections observed
        geom_point(data = filter(data.vital(), Any=="Yes"), 
                   aes(x = days_since_first_collection, y = get(var1, pos = -1), group = per_code_factor),
                   fill = "black", stroke = .1, shape = 21, size = 3) +
        geom_point(data = filter(data.vital(), Urine), 
                   aes(x = days_since_first_collection, y = get(var1, pos = -1), group = per_code_factor),
                   fill = "yellow", stroke = .1, shape = 21, size = 3) +
        geom_point(data = filter(data.vital(), Pneumonia), 
                   aes(x = days_since_first_collection, y = get(var1, pos = -1), group = per_code_factor),
                   fill = "gray", stroke = .1, shape = 21, size = 3) +
        geom_point(data = filter(data.vital(), Wound), 
                   aes(x = days_since_first_collection, y = get(var1, pos = -1), group = per_code_factor),
                   fill = "purple", stroke = .1, shape = 21, size = 3) +
        geom_point(data = filter(data.vital(), Blood), 
                   aes(x = days_since_first_collection, y = get(var1, pos = -1), group = per_code_factor),
                   fill = "red", stroke = .1, shape = 21, size = 3) +
        #scale_fill_manual(labels = c("Blood CBC", "Blood Chemistry","Blood infection")) +
        
        # theme
        theme_bw() +
        theme(legend.position = "top", axis.text.x = element_text(hjust = 1, angle = 45, size = 6),
              #axis.text.y = element_blank(),
              legend.text = element_text(size = 8), 
              strip.text = element_text(size = 12),
              strip.background = element_blank()) +
        ylab(var1) + 
        facet_wrap(~per_code_factor+TBSA+Age, scales = "free_x", labeller = label_context) +
        ggtitle(paste(var1, "for patients"))

      var.plot
   })
   
   output$indPlot <- renderPlot({
     
     person_to_look_at  <- input$individual # c("TT-001-00282", "TT-010-02112", "TT-001-01068") - some intersting ones
     
     data.ind <- reactive({
                      filter(TT, PER_CODE == person_to_look_at) %>% 
                      select(c(vital_vars,"Blood", "Any", "Urine", "Pneumonia", "Wound")) %>%
                      #select(-one_of("V_ER_DEF_DATE_01","V_TIME_PERFORMED")) %>%
                      melt(id=c("V_DATE_COLLECTION", "V_TIME_PERFORMED_1", "V_TIME_PERFORMED_2", "V_TIME_PERFORMED_3", "Blood", "Any", "Urine", "Pneumonia", "Wound"))
                })

     ind.plot = ggplot(data.ind(), aes(x = V_DATE_COLLECTION, y = value, color = variable, group = variable)) +
       geom_line(inherit.aes = TRUE) + 
       geom_point(data = filter(data.ind(), !is.na(V_TIME_PERFORMED_1)),
                  inherit.aes = TRUE, 
                  fill = "blue", stroke = .1, size = 2, alpha = .5, shape = 21) +
       geom_point(data = filter(data.ind(), !is.na(V_TIME_PERFORMED_2)),
                  inherit.aes = TRUE, 
                  fill = "green", stroke = .1, size = 1, shape = 21) +
       geom_point(data = filter(data.ind(), Any=="Yes"), 
                  inherit.aes = TRUE,
                  fill = "black", stroke = .1, shape = 21, size = 3) +
       geom_point(data = filter(data.ind(), Urine), 
                  inherit.aes = TRUE,
                  fill = "yellow", stroke = .1, shape = 21, size = 3) +
       geom_point(data = filter(data.ind(), Pneumonia), 
                  inherit.aes = TRUE,
                  fill = "gray", stroke = .1, shape = 21, size = 3) +
       geom_point(data = filter(data.ind(), Wound), 
                  inherit.aes = TRUE,
                  fill = "purple", stroke = .1, shape = 21, size = 3) +
       geom_point(data = filter(data.ind(), Blood == "TRUE"),
                  inherit.aes = TRUE,
                  fill = "red", stroke = .1, size = 3, shape = 21) +
       theme(legend.position = "top", 
             axis.text.x = element_blank(), #element_text(hjust = 1, angle = 90),
             legend.text = element_text(size = 8),
             strip.text = element_text(size = 9)) + 
       guides(color = FALSE) + 
       ggtitle(paste("Individual vitals for",person_to_look_at)) +
       facet_wrap(~variable, scales = "free_y")
     
     ind.plot
   })
   
   demo.ind = reactive({filter(TT_Demo1, Patient.ID == person_to_look_at) %>% 
         select(-one_of("Patient.ID","Daily2.Form.Count", "Age.Group", "TBSA.Group",
                        "Last.Exc.Graft.Date", "Discharge.Date", "APACHE.Date" ))})
   
   output$indText1 <- renderTable(demo.ind()[,1:(ncol(demo.ind())/3)])
   output$indText2 <- renderTable(demo.ind()[,(ncol(demo.ind())/3):(2*ncol(demo.ind())/3)])
   output$indText3 <- renderTable(demo.ind()[,(2*ncol(demo.ind())/3):ncol(demo.ind())])
                
}

# RUN ####
shinyApp(ui = ui, server = server)

