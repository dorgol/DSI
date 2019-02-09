#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# TO DO:
# Figure out why Crosstalk doesn't work across facets
# Coloring for blood chemistry test performed not matching in variable plot and individual plot
########################################################################################################################

library(shiny)
library(dplyr)
library(forcats)
library(ggplot2)
library(reshape)
library(crosstalk)
library(plotly)
library(scales)

# DATA ####
TT <- read.csv("TT_jane.csv", stringsAsFactors = F, check.names = F)[,-1]
TT$per_code_factor = as.factor(TT$PER_CODE)
TT$per_code_factor = reorder(TT$per_code_factor, TT$total_days_since_first_collection) #for plot order later
TT = TT %>% mutate(Blood_test_performed = case_when(
  !is.na(V_TIME_PERFORMED_1) & !is.na( V_TIME_PERFORMED_2) ~ "Blood CBC and Chemistry",
  is.na(V_TIME_PERFORMED_1) & !is.na( V_TIME_PERFORMED_2) ~ "Blood CBC",
  !is.na(V_TIME_PERFORMED_1) &  is.na(V_TIME_PERFORMED_2) ~ "Blood Chemistry")) #both NA auto goes to NA
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
        
        checkboxGroupInput("infection_type", "Show infection type:",
                           selected = "Blood",
                           choiceNames = list(
                             tags$span("Blood", style = "color: red;"),
                             tags$span("Any", style = "color: black;"), 
                             tags$span("Urine", style = "color: orange;"), 
                             tags$span("Pneumonia", style = "color: gray;"),
                             tags$span("Wound", style = "color: purple;")
                           ),
                           choiceValues = c("Blood", "Any", "Urine", "Pneumonia", "Wound")
        ),
        
        sliderInput("n_blood", "Number blood infections:",
                    min = 0, max = max(TT$n_blood, na.rm =T),
                    value = c(0,5), step = 1
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
        
        selectInput("individual", "PER_CODE:",
                    choices = sort(unique(TT$PER_CODE)), selected = sample(TT$PER_CODE,1)
        ),
        
        numericInput("seed", "Seed for random plot subset:", value = 1)
        
      ),
    
      mainPanel(
      
      tabsetPanel(type = "tabs",
                  tabPanel("Pop. Vitals", plotOutput("varPlot", width = "1200px", height="900px")),
                  tabPanel("Individual Vitals", plotlyOutput("indPlot", width = "1200px", height="900px"), br(),
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
                                mutate(per_code_factor = droplevels(per_code_factor),
                                       group = paste("PER_CODE:", per_code_factor, "\nTBSA:", TBSA, ", Age:", Age, ", Gender:", Gender)
                                ) %>%
                                filter(per_code_factor %in% sample(levels(per_code_factor), size = min(nlevels(per_code_factor),input$maxplots)))}) 
      
      var.plot = ggplot(data.vital())
      
      # Vital-specific additions to highlight normal ranges
      if (var1 == "V_TEMPERATURE") {
        var.plot = var.plot + geom_ribbon(aes(x = days_since_first_collection, min = 36.5, ymax = 39), fill= "gray", alpha = .5)
        #geom_hline(yintercept = 39, linetype = 2, size = .5) +
        #geom_hline(yintercept = 36.5, linetype = 2, size = .5)
      }
      if (var1 == "V_HEART_RATE") {
        var.plot = var.plot + geom_ribbon(aes(x = days_since_first_collection, ymin = 0, ymax = 110),  fill= "gray", alpha = .5)
      }
      if (var1 == "V_RESPIRATORY_RATE") {
        var.plot = var.plot +  geom_ribbon(aes(x = days_since_first_collection, ymin = 0, ymax = 25), fill= "gray", alpha = .5)
      }
      if (var1 == "V_PLATELET_COUNT") {
        var.plot = var.plot +  geom_ribbon(aes(x = days_since_first_collection, ymin = 0, ymax = 100), fill= "gray", alpha = .5)
      }
      if (var1 == "V_GLUCOSE") {
        var.plot = var.plot +  geom_ribbon(aes(x = days_since_first_collection, ymin = 0, ymax = 200), fill= "gray", alpha = .5)
      }
      
      var.plot = var.plot +
        geom_line(aes(x = days_since_first_collection, y = get(var1, pos = -1), group = per_code_factor, color = `study outcome abbr.`)) +
        
        # points for measurements taken
        geom_point(data = filter(data.vital(), !is.na(Blood_test_performed)),
                   aes(x = days_since_first_collection, y = get(var1, pos = -1), fill = Blood_test_performed, group = per_code_factor),
                   stroke = .1, size = 2, shape = 21) +
        scale_fill_manual(values = c("#a1dab4", "#225ea8", "#41b6c4"))
        #scale_fill_manual(labels = c("Blood CBC" (1), "Blood Chemistry" (2),"Blood infection")) +
      
        # points for infections observed
        if (sum(grepl(pattern = "Any", input$infection_type))) {
          var.plot = var.plot + geom_point(data = filter(data.vital(), Any=="Yes"), 
                   aes(x = days_since_first_collection, y = get(var1, pos = -1), group = per_code_factor),
                   fill = "black", stroke = .1, shape = 21, size = 3)
        }
        
        if (sum(grepl(pattern = "Urine", input$infection_type))) {
          var.plot = var.plot + geom_point(data = filter(data.vital(), Urine), 
                   aes(x = days_since_first_collection, y = get(var1, pos = -1), group = per_code_factor),
                   fill = "orange", stroke = .1, shape = 21, size = 3)
        }
      
        if (sum(grepl(pattern = "Pneumonia", input$infection_type))) {
          var.plot = var.plot + geom_point(data = filter(data.vital(), Pneumonia), 
                   aes(x = days_since_first_collection, y = get(var1, pos = -1), group = per_code_factor),
                   fill = "gray", stroke = .1, shape = 21, size = 3)
        }
        
        if (sum(grepl(pattern = "Wound", input$infection_type))) {
          var.plot = var.plot + geom_point(data = filter(data.vital(), Wound), 
                   aes(x = days_since_first_collection, y = get(var1, pos = -1), group = per_code_factor),
                   fill = "purple", stroke = .1, shape = 21, size = 3)
        }
      
        if (sum(grepl(pattern = "Blood", input$infection_type))) {
          var.plot = var.plot + geom_point(data = filter(data.vital(), Blood), 
                   aes(x = days_since_first_collection, y = get(var1, pos = -1), group = per_code_factor),
                   fill = "red", stroke = .1, shape = 21, size = 3)
        }
        
        # theme
        var.plot = var.plot +
          theme_bw() +
          theme(legend.position = "top", #legend.direction = "horizontal",
                axis.text.x = element_text(hjust = 1, angle = 45, size = 6),
              #axis.text.y = element_blank(),
              legend.text = element_text(size = 8), 
              strip.text = element_text(size = 12),
              strip.background = element_blank()) +
          ylab(var1) + 
          facet_wrap(~group, scales = "free_x", labeller = label_context) +
          ggtitle(paste(var1, "for patients"))

      var.plot
   })
   
   output$indPlot <- renderPlotly({
     
     person_to_look_at  <- input$individual # c("TT-001-00282", "TT-010-02112", "TT-001-01068") - some intersting ones
     person_gender = filter(TT, PER_CODE == person_to_look_at)$Gender[1]
     person_gender_color = ifelse(person_gender =="Female", hue_pal()(2)[1], hue_pal()(2)[2])
     person_age = filter(TT, PER_CODE == person_to_look_at)$Age[1]
     person_tbsa = filter(TT, PER_CODE == person_to_look_at)$TBSA[1]

     data.ind <- reactive({
                      SharedData$new(
                        filter(TT, PER_CODE == person_to_look_at) %>% 
                        select(c(vital_vars, "Blood_test_performed", "Blood", "Any", "Urine", "Pneumonia", "Wound", "study outcome abbr.")) %>%
                        #select(-one_of("V_ER_DEF_DATE_01","V_TIME_PERFORMED")) %>%
                        melt(id=c("V_DATE_COLLECTION", "V_TIME_PERFORMED_1", "V_TIME_PERFORMED_2", "V_TIME_PERFORMED_3",
                                  "Blood_test_performed", "Blood", "Any", "Urine", "Pneumonia", "Wound", "study outcome abbr."))
                      )  
                })
     
     ind.plot = ggplot(data.ind()$origData(), aes(x = V_DATE_COLLECTION, y = value, group = variable)) +
       geom_line(inherit.aes = TRUE, size = .3, aes(color = `study outcome abbr.`)) + 
       geom_point(data = filter(data.ind()$origData(), !is.na(Blood_test_performed)),
                  aes(fill = Blood_test_performed), stroke = .1, size = 1) +
       scale_fill_manual(values = c("#a1dab4", "#225ea8", "#41b6c4"))

       
       if (sum(grepl(pattern = "Any", input$infection_type))) {
            ind.plot = ind.plot + geom_point(data = filter(data.ind()$origData(), Any=="Yes"), 
                  inherit.aes = TRUE,
                  fill = "black", stroke = .1, shape = 21, size = 2)
       }
     
       if (sum(grepl(pattern = "Urine", input$infection_type))) {
         ind.plot = ind.plot + geom_point(data = filter(data.ind()$origData(), Urine), 
                  inherit.aes = TRUE,
                  fill = "orange", stroke = .1, shape = 21, size = 2)
       }
     
       if (sum(grepl(pattern = "Pneumonia", input$infection_type))) {
         ind.plot = ind.plot + geom_point(data = filter(data.ind()$origData(), Pneumonia), 
                  inherit.aes = TRUE,
                  fill = "gray", stroke = .1, shape = 21, size = 2)
       }
     
       if (sum(grepl(pattern = "Wound", input$infection_type))) {
         ind.plot = ind.plot + geom_point(data = filter(data.ind()$origData(), Wound), 
                  inherit.aes = TRUE,
                  fill = "purple", stroke = .1, shape = 21, size = 2)
       }
     
       if (sum(grepl(pattern = "Blood", input$infection_type))) {
         ind.plot = ind.plot + geom_point(data = filter(data.ind()$origData(), Blood == "TRUE"),
                  inherit.aes = TRUE,
                  fill = "red", stroke = .1, size = 2, shape = 21)
       }
       
       ind.plot = ind.plot + 
             theme(legend.position = "top", 
             axis.text.x = element_blank(), #element_text(hjust = 1, angle = 90),
             legend.text = element_text(size = 8),
             strip.text = element_text(size = 9),
             strip.background = element_blank(),
             #panel.border = element_rect(color = person_gender_color, fill = NA, size = .5)
             plot.title = element_text(size = 12, family = "sans-serif")
             ) + 
       guides(color = FALSE, fill = FALSE) + 
       ggtitle(paste("Individual vitals for", person_to_look_at, "\tTBSA: ", person_tbsa, "\tAge: ", person_age, "\tGender: ", person_gender)) +
       facet_wrap(~variable, scales = "free_y")
     
     ggplotly(ind.plot, tooltip = c("x", "y")) %>%
       layout(margin = list(0,0,0,0,1), showlegend = FALSE) %>%
       highlight(on = "plotly_selected", off = "plotly_deselect")
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

