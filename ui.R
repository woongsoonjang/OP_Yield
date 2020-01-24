### This is ui.R

library(shiny)
library(DT)

navbarPage(title = "OP-Yield",
    tabPanel("Input Values", 
        fluidRow(
            column(3, 
                wellPanel(
                    numericInput("plant.yr", "Planting Year", as.integer(format(Sys.Date(), "%Y"))),
                    sliderInput("plant.den", "Planting Density (tpa):",
                            min = 130, max = 1210, value = 303),
                    sliderInput("si", "Site Index (ft):",
                            min = 75, max = 125, value = 105),
                    sliderInput("ini.plant.surv", "Initial Planting Survival %:",
                            min = 40, max = 100, value = 85),
                    sliderInput("max.sdi", "Maximum SDI:",
                            min = 365, max = 500, value = 400),
                    selectInput("reineke.term", "SDI Slope:",
                            choices = c(-1.605, -1.7712, -1.661, -1.7653),  selected = -1.605),
                    selectInput("pct.age", "PCT Age:", choices = c(0, 5:15), selected = 0)
                    )
                ),
            column(3,
                wellPanel(
                    sliderInput("pct.tgt", "PCT Target (tpa):", min = 80, max = 300, value = 180),
                    sliderInput("bg.mort", "Annual Background Mortality %:", min = 0, max = 1, value = 0),
                    sliderInput("cf.merch.lim", "Merchantability Limit (inch):", 
                            min = 4, max = 14, step=0.1, value = 11.5),
                    selectInput("ineq.cf", "Cubic Foot Volume Eq.:",
                            choices = c("Oliver & Powers 0_1","Wensel & Olson 0_1", "MacLean&Berger 4_1"), 
                            selected = "Oliver & Powers 0_1"),
                    selectInput("ineq.bf", "Board Foot Volume Eq.:",
                            choices = c("Wensel & Olson 8_1","Wensel & Olson 6_1", "Wensel & Olson 4_1",
                                        "MacLean&Berger v_1"), selected = "Wensel & Olson 4_1"),
                    sliderInput("reten.tgt", "Target Rotation Retention (tpa):",
                            min = 50, max = 180, value = 80),
                    sliderInput("GS.Table.Age", "Growing Stock Table Age:",
                            min = 5, max = 60, value = 40),
                    helpText("For yield estimate, click the submit button"),
                    actionButton("submit", "Submit")
                )
            ),
        # Main panel for displaying outputs ----
            column(6, 
                mainPanel(
                    h4("Density Guide"),
                    DTOutput("Den.guide")
                    )
                )
            )
        ),
    
    tabPanel("Summaries",
        fluidRow(
            
            
            
            column(3, 
                wellPanel(      
                    h4("Export Tables"),
                    hr(),
                    h5("Export Yield Summary"),
                    radioButtons("Tblform1", "File format", c("CSV", "XLSX"),
                             inline = TRUE),
                    downloadButton("table_out1", "Save Table"),
                    hr(),
                    h5("Export Growing Stock Table"),
                    radioButtons("Tblform2", "File format", c("CSV", "XLSX"),
                             inline = TRUE),
                    downloadButton("table_out2", "Save Table"),
                    hr()
                )
            ),
            
            
            column(6, 
                mainPanel(
                    conditionalPanel(condition = "input.submit == 0",
                                     helpText("For display and export, please enter input values in the 'Input Values' tab and click the Submit button")
                    ),
                    conditionalPanel(condition = "input.submit != 0",
                        h4("Yield Summary"),
                        DTOutput("yield.summary"),
                        hr(),
                        h4("Growing Stock Table"),
                        DTOutput("grow.stock"),
                        hr(),
                        h4("Precommercial Thinning Summary"),
                        DTOutput("PCT.summary"),
                        hr(),
                        h4("TPA for PCT as a function of QMD and UMZ SDI"),
                        DTOutput("TPA.for.PCT")
                    )
                )
            )
        )
    ),
            
    tabPanel("Figures",
        fluidRow(
            
            column(3, 
                   wellPanel(      
                       h4("Export Figures"),
                       hr(),
                       h5("Export Volume Increment Chart"),
                       radioButtons("IMGformat1", "Image format", c("PDF", "PNG","TIFF"),
                                    inline = TRUE),
                       downloadButton("image_out1", "Save Figure"),
                       hr(),
                       h5("Export Stand Density plot"),
                       radioButtons("IMGformat2", "Image format", c("PDF", "PNG","TIFF"),
                                    inline = TRUE),
                       downloadButton("image_out2", "Save Figure"),
                       hr(),
                       h5("Export Density by Crown Class plot"),
                       radioButtons("IMGformat3", "Image format", c("PDF", "PNG","TIFF"),
                                    inline = TRUE),
                       downloadButton("image_out3", "Save Figure"),
                       hr()
                   )
            ),
            
            
            column(6,
                mainPanel(
                    conditionalPanel(condition = "input.submit == 0",
                        helpText("For display and export, please enter input values in the 'Input Values' tab and click the Submit button")
                        ),
                    conditionalPanel(condition = "input.submit != 0",
                        h4("Annual Net Cubit Foot Volume Increment"),
                        plotOutput("Ann.View", height="400px", width = "600px"),
            
                        h4("Density by Crown Class"),
                        plotOutput("Den.View", height="400px", width = "600px"),
            
                        h4("Stand Density"),
                        plotOutput("Den.View2", height="400px", width = "600px")
                        )
                )
            )
        )
    ),
    tabPanel("About",
             fluidRow(
                 column(9,
                        includeMarkdown("README.md")
                 )
             )
    )
)

            




