# Jane Carlen's Shiny app for Burn EDA

# NOTES:
#  - Plotting color of infection days (circles) indicates type, as indicated by color of "Show infection type" labels at sidebar
#  - Plotting color of testing days (diamonds) indicate type of blood tests performed
#  - Plotting color of criteria met (squares), optionally turned on in "Show infection type" sidebar, indicate if a criteria (Greenhalgh or SIRS) was met based on my limited-data approximation
#  - Some plots have background colors to indicate normal (gray) ranges or abnormal (red) ranges.
#     I find them helpful and would like to add more. They come from Greenhalgh et al p. 779. (American Burn Association Consensus Conference)
#  - The TT_jane.csv and TT_Demo1.csv documents that are loaded are pre-processed versions of Transfusion Trigger data.
#       + They are processed in the first two code chucks (Setup, TT_Data) of burn_data_EDA.Rmd

# TO DO:
# Figure out why Crosstalk doesn't work across facets (columns)
# 

# Hypotheses generated from Shiny app:
# 
# - Older people are more likely to experience blood infection when their heart rate is in the normal range. Younger people seem more likely to be above it.
# - Cumulative days outside normal ranges are important.
# - Respiratory rate cutoff (>25 bpm) seems too low in the Greenhalgh criteria. 30 may be more appropriate.

########################################################################################################################

library(shiny)
library(dplyr)
library(ggplot2)
library(forcats)
library(reshape)
library(scales)
#library(plotly)

setwd("~/Documents/DSI/Burn/")

# DATA ####
# TT_jane.csv and TT_Demo1.csv
TT <- read.csv("TT_jane.csv", stringsAsFactors = F, check.names = F)[,-1]
TT$per_code_factor = as.factor(TT$PER_CODE)
TT$per_code_factor = reorder(TT$per_code_factor, TT$total_days_since_first_collection) #for plot order later
TT = TT %>% mutate(Blood_test_performed = as.factor(case_when(
  !is.na(V_TIME_PERFORMED_1) & !is.na( V_TIME_PERFORMED_2) ~ "Blood CBC and Chemistry",
  is.na(V_TIME_PERFORMED_1) & !is.na( V_TIME_PERFORMED_2) ~ "Blood CBC",
  !is.na(V_TIME_PERFORMED_1) &  is.na(V_TIME_PERFORMED_2) ~ "Blood Chemistry"))) #if both NA goes to NA
#vital_vars = names(TT)[grepl(x = names(TT), "V_.*")]
vital_vars = c("V_DATE_COLLECTION", "V_TIME_PERFORMED_1", "V_TIME_PERFORMED_2", "V_TIME_PERFORMED_3",
                          "V_HEART_RATE", "V_PB_SYSTOLIC", "V_BP_DIASTOLIC", "V_MEANARTERIAL_PRESSURE", "V_CENTRAL_VENOUS_PRESSURE",
                          "V_TEMPERATURE", "V_PAO2", "V_FIO2", "V_PACO2", "V_HCO3",
                          "V_RESPIRATORY_RATE", "V_SODIUM", "V_POTASSIUM", "V_BLOOD_UREA_NITROGEN", "V_CREATININE",
                          "V_PLATELET_COUNT", "V_WHITE_BC", "V_HEMOGLOBIN", "V_HEMATOCRIT",  "V_GLASCOWCOMA_SCALE", 
                          "V_GLUCOSE", "V_MODS_SCORE", "V_PH","V_BILIRUBIN") #for individual plot ordering later - this is all the variavbles in the TT data set beginning with V_ except for "V_ER_DEF_DATE_01", "V_TIME_PERFORMED"
                          
TT_Demo1 = read.csv("TT_Demo1.csv")[,-1]

# Add Greenhalgh and SIRS variables - Borrowed from decision tree section of burn_data_EDA.R ----

# Greenhalgh (note I excluded the cumulated variables created in burn_data_EDA.R)
TT.tree.data = TT %>% group_by(per_code_factor) %>%
  mutate(abnorm_temp = (V_TEMPERATURE > 39 | V_TEMPERATURE < 36.5),
         abnorm_heart_rate = V_HEART_RATE > 110,
         abnorm_resp_rate = V_RESPIRATORY_RATE > 25,
         abnorm_plat_count = V_PLATELET_COUNT < 100,
         abnorm_glucose = V_GLUCOSE > 200) %>%
  ungroup() %>%
  mutate(Greenhalgh = rowSums(cbind(abnorm_temp, abnorm_heart_rate, abnorm_resp_rate, abnorm_plat_count, abnorm_glucose), na.rm = T))

TT$Greenhalgh  = TT.tree.data$Greenhalgh >=3

# Sirs
TT.tree.data  <- TT.tree.data %>% mutate(SIRS = 
            rowSums(cbind( (V_TEMPERATURE > 38 | V_TEMPERATURE < 36),
            V_HEART_RATE > 90,
            #supposed to be maintenance of PaCO2 > 32 - what counts as maintenance?
            (V_RESPIRATORY_RATE > 20 | V_PACO2 < 32), 
            #note this also says "or left shift defined as > 10% of bands, what does that mean?
            (V_WHITE_BC < 4 | V_WHITE_BC > 12)), na.rm = T)
            )

TT$SIRS  = TT.tree.data$SIRS >=2

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
                      "Potassium" = "V_POTASSIUM",
                      "BP_Systolic" = "V_PB_SYSTOLIC",
                      "BP_Diastolic" =  "V_BP_DIASTOLIC",
                      "Mean Arterial Pressure" = "V_MEANARTERIAL_PRESSURE",  
                      "Central Veneous Pressure" = "V_CENTRAL_VENOUS_PRESSURE",
                      "Respiratory Rate" = "V_RESPIRATORY_RATE",     
                      "PAO2" = "V_PAO2",      
                      "FI02" = "V_FIO2",   
                      "PACO2" =  "V_PACO2", #Correlated with HC03  (see burn_data_EDA.R)     
                      "HC03" = "V_HCO3",    
                      "Blood Urea Nitrogen" = "V_BLOOD_UREA_NITROGEN", # COrrelated with Creatinine
                      "Creatinine" = "V_CREATININE",   
                      "Hemogolobin" = "V_HEMOGLOBIN",  # highly correlated to Hematocrit
                      "Hematocrit" = "V_HEMATOCRIT",   
                      "Glascowcoma Scale" = "V_GLASCOWCOMA_SCALE", 
                      "MODS Score" = "V_MODS_SCORE", 
                      "PH" = "V_PH",
                      "Bilirubin" = "V_BILIRUBIN"),
                    selected = "V_TEMPERATURE"), 
        
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
                             tags$span("Wound", style = "color: purple;"),
                             tags$span("Show Greenhalgh criteria met", style = "color: green;"),
                             tags$span("Show SIRS criteria met", style = "color: blue;")
                           ),
                           choiceValues = c("Blood", "Any", "Urine", "Pneumonia", "Wound", "Greenhalgh", "SIRS")
        ),
        
        sliderInput("n_blood", "Number blood infections:",
                    min = 0, max = max(TT$n_blood, na.rm =T),
                    value = c(0,5), step = 1
        ),
        
        selectInput("order", "Order plots by:",
                    selected = "total_days_since_first_collection",
                    choices = c("total_days_since_first_collection",
                                "study outcome abbr.", 
                                "Treatment Group",
                                "TBSA",
                                "Gender",
                                #"Race",
                                "Age",
                                #"Inhalation Injury",
                                "n_blood",
                                "n_any",
                                "n_wound",
                                "n_urine",
                                "n_pneumonia")
        ),
        
        numericInput("maxplots", "Max plots to show:", value = 16),
        
        selectInput("individual", "Individual ID (PER_CODE):",
                    choices = sort(unique(TT$PER_CODE)), selected = sample(TT$PER_CODE,1)
        ),
        
        numericInput("seed", "Seed for random plot subset:", value = 1)
        
      ),
    
      mainPanel(
      
      tabsetPanel(type = "tabs",
                  tabPanel("Pop. Vitals", plotOutput("varPlot", width = "1200px", height="900px")),
                  tabPanel("Individual Vitals", plotOutput("indPlot", width = "1200px", height="900px"), hr(),
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
      data.vital  <- reactive({ TT %>% mutate(per_code_factor = fct_reorder(per_code_factor, rank(TT[[input$order]], ties.method = "random")),
                                              group = factor(paste("PER_CODE:", per_code_factor, "\nTBSA:", TBSA, "; Age", Age, Gender, "; Trt:", `Treatment Group`)),
                                              group = fct_reorder(group, rank(TT[[input$order]]))) %>% 
                                filter( `study outcome abbr.` %in% input$outcome,
                                  n_blood >= input$n_blood[1], 
                                  n_blood <= input$n_blood[2] 
                                ) %>% 
                                mutate(per_code_factor = droplevels(per_code_factor)) %>%
                                filter(per_code_factor %in% sample(levels(per_code_factor), size = min(nlevels(per_code_factor),input$maxplots)))
                              }) 
      
      var.plot = ggplot(data.vital())
      
      # Vital-specific additions to highlight normal ranges
      # See Greenhalgh et al p. 779 for the ranges
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
        var.plot = var.plot +  geom_ribbon(aes(x = days_since_first_collection, ymin = 0, ymax = 100), fill= "red", alpha = .3)
      }
      if (var1 == "V_GLUCOSE") {
        var.plot = var.plot +  geom_ribbon(aes(x = days_since_first_collection, ymin = 0, ymax = 200), fill= "gray", alpha = .5)
      }
      
      var.plot = var.plot +
        geom_line(aes(x = days_since_first_collection, y = get(var1, pos = -1), group = per_code_factor, color = `study outcome abbr.`)) +
        scale_color_manual(values = setNames(hue_pal()(n_distinct(TT$"study outcome abbr.")),
                                             unique(TT$"study outcome abbr."))[data.vital()$"study outcome abbr."]) +
        #points for measurements taken
        geom_point(data = filter(data.vital(), !is.na(Blood_test_performed)),
                   aes(x = days_since_first_collection, y = get(var1, pos = -1), fill = Blood_test_performed, group = per_code_factor),
                   stroke = .1, size = 2, shape = 23, alpha = .8) +
        scale_fill_manual(values = c("#d8b365", "#225ea8", "#41b6c4"))
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
      
        if (sum(grepl(pattern = "Greenhalgh", input$infection_type))) {
          var.plot = var.plot + geom_point(data = filter(data.vital(), Greenhalgh), 
                   aes(x = days_since_first_collection, y = get(var1, pos = -1), group = per_code_factor),
                   fill = "green", stroke = .1, shape = 22, size = 3)
        }
        
        if (sum(grepl(pattern = "SIRS", input$infection_type))) {
          var.plot = var.plot + geom_point(data = filter(data.vital(), SIRS), 
                   aes(x = days_since_first_collection, y = get(var1, pos = -1), group = per_code_factor),
                   fill = "blue", stroke = .1, shape = 22, size = 3)
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
      #var.ggplotly = ggplotly(var.plot) %>% highlight(on = "plotly_hover", selectize = TRUE) %>% rangeslider()
   })
   
   output$indPlot <- renderPlot({
     
     person_to_look_at  <- input$individual # c("TT-001-00282", "TT-010-02112", "TT-001-01068") - some intersting ones
     person_gender = filter(TT, PER_CODE == person_to_look_at)$Gender[1]
     person_gender_color = ifelse(person_gender =="Female", hue_pal()(2)[1], hue_pal()(2)[2])
     person_age = filter(TT, PER_CODE == person_to_look_at)$Age[1]
     person_tbsa = filter(TT, PER_CODE == person_to_look_at)$TBSA[1]
     person_inhalation = filter(TT, PER_CODE == person_to_look_at)$"Inhalation Injury"[1]
     person_race = filter(TT, PER_CODE == person_to_look_at)$"Race"[1]
     person_treatment_group = filter(TT, PER_CODE == person_to_look_at)$"Treatment Group"[1]
     
     data.ind <- reactive({
                      #SharedData$new(
       
                        data = filter(TT, PER_CODE == person_to_look_at) %>% 
                        select(c(vital_vars, "Blood_test_performed", "Blood", "Any", "Urine", "Pneumonia", "Wound", "study outcome abbr.",
                                 "Greenhalgh", "SIRS")) %>%
                        melt(id=c("V_DATE_COLLECTION", "V_TIME_PERFORMED_1", "V_TIME_PERFORMED_2", "V_TIME_PERFORMED_3",
                                  "Blood_test_performed", "Blood", "Any", "Urine", "Pneumonia", "Wound", "study outcome abbr.",
                                  "Greenhalgh", "SIRS"))
                      
                      # ~V_DATE_COLLECTION
                      #)  
                })
     
     #ind.plot = lapply(lapply(split(data.ind()$data(), data.ind()$data()$variable), SharedData$new, group = "ind"),  function(x) {
       
     ind.plot = ggplot(data.ind(), aes(x = V_DATE_COLLECTION, y = value, group = variable)) +
        geom_line(inherit.aes = TRUE, size = .3, aes(color = `study outcome abbr.`)) + 
        scale_color_manual(values = setNames(hue_pal()(n_distinct(TT$"study outcome abbr.")),
                                             unique(TT$"study outcome abbr."))[data.ind()$"study outcome abbr."]) +
        geom_point(data = filter(data.ind(), !is.na(Blood_test_performed)),
                  aes(fill = Blood_test_performed), stroke = .1, size = 2, shape = 23, alpha = .8) +
        scale_fill_manual(values = c("Blood CBC" = "#d8b365",
                                     "Blood CBC and Chemistry" = "#225ea8",
                                      "Blood Chemistry" = "#41b6c4"), breaks=levels(TT$Blood_test_performed))
       
       if (sum(grepl(pattern = "Any", input$infection_type))) {
            ind.plot = ind.plot + geom_point(data = filter(data.ind(), Any=="Yes"), 
                  inherit.aes = TRUE,
                  fill = "black", stroke = .1, shape = 21, size = 3)
       }
     
       if (sum(grepl(pattern = "Urine", input$infection_type))) {
         ind.plot = ind.plot + geom_point(data = filter(data.ind(), Urine), 
                  inherit.aes = TRUE,
                  fill = "orange", stroke = .1, shape = 21, size = 3)
       }
     
       if (sum(grepl(pattern = "Pneumonia", input$infection_type))) {
         ind.plot = ind.plot + geom_point(data = filter(data.ind(), Pneumonia), 
                  inherit.aes = TRUE,
                  fill = "gray", stroke = .1, shape = 21, size = 3)
       }
     
       if (sum(grepl(pattern = "Wound", input$infection_type))) {
         ind.plot = ind.plot + geom_point(data = filter(data.ind(), Wound), 
                  inherit.aes = TRUE,
                  fill = "purple", stroke = .1, shape = 21, size = 3)
       }
     
       if (sum(grepl(pattern = "Blood", input$infection_type))) {
         ind.plot = ind.plot + geom_point(data = filter(data.ind(), Blood == "TRUE"),
                  inherit.aes = TRUE,
                  fill = "red", stroke = .1, size = 3, shape = 21)
       }
     
       if (sum(grepl(pattern = "Greenhalgh", input$infection_type))) {
          ind.plot = ind.plot + geom_point(data = filter(data.ind(), Greenhalgh), 
                  inherit.aes = TRUE,
                   fill = "green", stroke = .1, shape = 22, size = 3)
       }
     
       if (sum(grepl(pattern = "SIRS", input$infection_type))) {
          ind.plot = ind.plot + geom_point(data = filter(data.ind(), SIRS), 
                  inherit.aes = TRUE,
                   fill = "blue", stroke = .1, shape = 22, size = 3)
        }
       
       ind.plot = ind.plot + 
             theme_bw() + 
             theme(legend.position = "top", 
             axis.text.x = element_blank(), #element_text(hjust = 1, angle = 90),
             legend.text = element_text(size = 8),
             strip.text = element_text(size = 11),
             strip.background = element_blank(),
             #panel.border = element_rect(color = person_gender_color, fill = NA, size = .5)
             plot.title = element_text(size = 12, family = "Helvetica")) + #guides(color = FALSE) + 
         ggtitle(paste("Individual vitals for", person_to_look_at, "\tTBSA: ", person_tbsa, "; \tAge", person_age, person_gender,
                       "; \tTreatment group:", person_treatment_group, "; \tInhalation injury: ", person_inhalation,
                       "\nFirst column is Greenhalgh criteria variables. More individual info. below plots"))
        # Add faceting
       ind.plot = ind.plot +
         facet_wrap(~variable, scales = "free_y")
        
       # Add plotly
       # ind.plot = ggplotly(ind.plot, tooltip = c("x", "y")) %>%
        # layout(margin = list(0,0,0,0,1), showlegend = FALSE) %>%
        # highlight(on = "plotly_selected", off = "plotly_deselect") %>% hide_legend()

       ind.plot
      # }) 
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

