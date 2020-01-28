### This is ui.R

library(shiny)
library(DT)
library(bsplus)

navbarPage(title = "OP-Yield",
    tabPanel("Summary", 
        fluidRow(
            column(3, 
                wellPanel(
                    numericInput("plant.yr", "Planting Year", as.integer(format(Sys.Date(), "%Y")))%>%
                        bs_embed_tooltip(title = "Planting year is a bookeeping value only. It has no impact on tabular values"),
                    sliderInput("plant.den", "Planting Density (tpa):",
                            min = 130, max = 1210, value = 303),
                    sliderInput("si", "Site Index (ft):",
                            min = 75, max = 125, value = 105)%>%
                        bs_embed_tooltip(title = "At total (base) age 50"),
                    sliderInput("ini.plant.surv", "Initial Planting Survival %:",
                            min = 40, max = 100, value = 85)%>%
                        bs_embed_tooltip(title = "Proportion of planted trees surviving the first year"),
                    sliderInput("max.sdi", "Maximum SDI:",
                            min = 365, max = 500, value = 400),
                    selectInput("reineke.term", "SDI Slope:",
                            choices = c(-1.605, -1.7712, -1.661, -1.7653),  selected = -1.605)%>%
                        bs_embed_tooltip(title = "Slope for SDI function: \n -1.771: Oliver and Powers (1978) \n -1.605: Reineke (1933) \n -1.66: Edminister (1988) \n -1.7653: DeMars and Barrett (1985)"),
                    selectInput("pct.age", "PCT Age:", choices = c(0, 5:15), selected = 0)%>%
                        bs_embed_tooltip(title = "PCT age of zero indicates no PCT to be perfromed")
                    )
                ),
            column(3,
                wellPanel(
                    sliderInput("pct.tgt", "PCT Target (tpa):", min = 80, max = 300, value = 180)%>%
                        bs_embed_tooltip(title = "Trees per acre retained in a precommercial thin"),
                    sliderInput("bg.mort", "Annual Background Mortality %:", min = 0, max = 1, value = 0)%>%
                        bs_embed_tooltip(title = "Percent of stems annual mortality"),
                    sliderInput("cf.merch.lim", "Merchantability Limit (inch):", 
                            min = 4, max = 14, step=0.1, value = 11.5)%>%
                        bs_embed_tooltip(title = "Lower diameter limit for merchantability calculations"),
                    selectInput("ineq.cf", "Cubic Foot Volume Eq.:",
                            choices = c("Oliver & Powers 0_1","Wensel & Olson 0_1", "MacLean & Berger 4_1"), 
                            selected = "Oliver & Powers 0_1")%>%
                        bs_embed_tooltip(title = "Select CF Volume equations. \n Following two numbers represent top and stump sizes for volume calculation."),
                    selectInput("ineq.bf", "Board Foot Volume Eq.:",
                            choices = c("Wensel & Olson 8_1","Wensel & Olson 6_1", "Wensel & Olson 4_1",
                                        "MacLean & Berger v_1"), selected = "Wensel & Olson 4_1")%>%
                        bs_embed_tooltip(title = "Select BF Volume equations. \n Following two numbers represent top and stump sizes for volume calculation."),
                    sliderInput("reten.tgt", "Target Rotation Retention (tpa):",
                            min = 50, max = 180, value = 80)%>%
                        bs_embed_tooltip(title = "How many trees do you want to retain when you conduct a commertial thin?"),
                    sliderInput("GS.Table.Age", "Growing Stock Table Age:",
                            min = 5, max = 60, value = 40)%>%
                        bs_embed_tooltip(title = "Set the age to view in the Growing-Stock Table"),
                    helpText("For yield estimate, click the submit button"),
                    actionButton("submit", "Submit")
                )
            ),
            
            
            column(6, 
                   mainPanel(
                       conditionalPanel(condition = "input.submit == 0",
                                        h3("IMPORTANT: for display and export, please enter input values and click the", span("Submit", style = "color:red"), "button.")
                       ),
                       conditionalPanel(condition = "input.submit != 0",
                                        h4("Yield Summary"),
                                        DTOutput("yield.summary")
                                        
                                        
                       )
                   )
            )
        ),
        
        fluidRow(
            column(6, 
                   h4("Density Guide"),
                   DTOutput("Den.guide")
            ),
            column(6, conditionalPanel(condition = "input.submit != 0",
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
    ),
        
    tabPanel("SDMD",
        fluidRow(
            column(6,
                    mainPanel(
                        conditionalPanel(condition = "input.submit == 0",
                             h3("IMPORTANT: for display and export, please go back to the ", 
                                span("Summary", style = "color:red"), " tab, enter input values, and click the", span("Submit", style = "color:red"), "button.")
                                ),
                        conditionalPanel(condition = "input.submit != 0",
                             h4("Stand Density Management Diamgram"),
                             plotOutput("SDMD.View", height="1200px", width = "800px")
                        )
                    )
             ),
            column(6,
                   mainPanel(
                       conditionalPanel(condition = "input.submit != 0",
                            h4("Stand Summary"),
                            DTOutput("SDMD.Stand.summary"),
                            hr()
                       )
                   )
            )
         )
        ),
    
    tabPanel("Figures",
        fluidRow(
            column(6,
                mainPanel(
                    conditionalPanel(condition = "input.submit == 0",
                        h3("IMPORTANT: for display and export, please go back to the ", 
                            span("Summary", style = "color:red"), " tab, enter input values, and click the", span("Submit", style = "color:red"), "button.")
                            ),
                    conditionalPanel(condition = "input.submit != 0",
                        h4("Annual Net Cubit Foot Volume Increment"),
                        plotOutput("Ann.View", height="400px", width = "600px"),
            
                        h4("Density by Crown Class"),
                        plotOutput("Den.View", height="400px", width = "600px")
                        )
                )
            ),
            column(6,
                   mainPanel(
                       conditionalPanel(condition = "input.submit != 0",
                            h4("Stand Density"),
                            plotOutput("Den.View2", height="400px", width = "600px")
                       )
                   )
            )
        )
    ),
    
   
    
    tabPanel("Report",
        fluidRow(
            column(3, 
                wellPanel(      
                    h4("Export Tables"),
                    hr(),
                    h5("Export Yield Summary Table"),
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
            column(3, 
                wellPanel(      
                    h4("Export SDMD"),
                    hr(),
                    h5("Export Stand Density Management Diagram"),
                    radioButtons("SDMDform1", "Image format", c("PDF", "PNG","TIFF"),
                                 inline = TRUE),
                    downloadButton("SDMD_out1", "Save Figure"),
                    hr(),
                    h5("Export Stand Summary"),
                    radioButtons("SDMDTblform1", "File format", c("CSV", "XLSX"),
                                 inline = TRUE),
                    downloadButton("SDMD_table_out1", "Save Table"),
                    hr()
                    )
                 ),
            column(3,     
                 wellPanel(      
                     h4("Export Figures"),
                     hr(),
                     h5("Export Volume Increment Chart"),
                     radioButtons("IMGformat1", "Image format", c("PDF", "PNG","TIFF"),
                                  inline = TRUE),
                     downloadButton("image_out1", "Save Figure"),
                     hr(),
                     h5("Export Density by Crown Class plot"),
                     radioButtons("IMGformat3", "Image format", c("PDF", "PNG","TIFF"),
                                  inline = TRUE),
                     downloadButton("image_out3", "Save Figure"),
                     hr(),
                     h5("Export Stand Density plot"),
                     radioButtons("IMGformat2", "Image format", c("PDF", "PNG","TIFF"),
                                  inline = TRUE),
                     downloadButton("image_out2", "Save Figure"),
                     hr()
                 )
            ),
            column(3,     
                wellPanel(
                    h4("Export Report"),
                    hr(),
                    h5("Download Report"),
                    radioButtons("Repform", "Document format", c("PDF", "HTML", "Word"),
                              inline = TRUE),
                    downloadButton("downloadReport","Create Report")
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

            




