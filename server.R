library(shiny)
library(DT)
library(rmarkdown)
library(openxlsx)



function(input, output) {
 
  
plant.den <- eventReactive(input$submit, as.numeric(input$plant.den))
si <- eventReactive(input$submit, as.numeric(input$si))
ini.plant.surv <- eventReactive(input$submit, as.numeric(input$ini.plant.surv)/100)
max.sdi <- eventReactive(input$submit, as.numeric(input$max.sdi))
pct.age <- eventReactive(input$submit, as.numeric(input$pct.age))
pct.tgt <- eventReactive(input$submit, as.numeric(input$pct.tgt))
bg.mort <- eventReactive(input$submit, as.numeric(input$bg.mort))
cf.merch.lim <- eventReactive(input$submit, as.numeric(input$cf.merch.lim))
ineq.cf <- eventReactive(input$submit, input$ineq.cf)
ineq.bf <- eventReactive(input$submit, input$ineq.bf)
reten.tgt <- eventReactive(input$submit, as.numeric(input$reten.tgt))
reineke.term <- eventReactive(input$submit, as.numeric(input$reineke.term)) 
Plant.yr <- eventReactive(input$submit, as.numeric(input$plant.yr)) 
GS.Table.Age <- eventReactive(input$submit, as.numeric(input$GS.Table.Age)) 


 


OP.tab.cal <-function(){
  cal.tab <- OP.cal.table(                  
    plant.den = plant.den(),
    si =si(),
    ini.plant.surv =ini.plant.surv(),
    max.sdi = max.sdi(),
    pct.age = pct.age(),
    pct.tgt = pct.tgt(),
    bg.mort = bg.mort(),
    cf.merch.lim = cf.merch.lim(),
    ineq.cf = ineq.cf(),
    ineq.bf = ineq.bf(),
    reten.tgt = reten.tgt(),
    reineke.term = reineke.term())
  cal.tab
  }

output$yield.summary <- renderDT({
  yth1 <- switch(ineq.cf(),
               "Oliver & Powers 0_1"= "Oliver and Powers (1978) Total Stem CF Volume, 1-foot stump",
               "Wensel & Olson 0_1" = "Wensel and Olson (1995) Total Stem CF Volume, 1-foot stump",
               "MacLean&Berger 4_1" = "MacLean and Berger (1976) CF Volume, 4-inch top, 1-foot stump")
  yth2 <- switch(ineq.bf(),
                 "Wensel & Olson 8_1" = "Wensel and Olson (1995): Scribner BF Volume 8-inch top, 1-foot stump",
                 "Wensel & Olson 6_1" = "Wensel and Olson (1995): Scribner BF Volume 6-inch top, 1-foot stump",
                 "Wensel & Olson 4_1" = "Wensel and Olson (1995): Scribner BF Volume 4-inch top, 1-foot stump",
                 "MacLean&Berger v_1" = "MacLean and Berger (1976) Scribner BF Volume, 4-inch top, 1-foot stump")
  
  yield.header = htmltools::withTags(table(
    class = 'display',
    thead(
      tr(
        th(colspan = 10, yth1)
      ),
      tr(
        th(colspan = 10, yth2),
        tr(
          lapply(c("Total Age", "Total CF Vol", "Merch CF Vol", "Total BF Vol", "Crop BF Vol", 
                   "Comm Thin BF Vol", "SDI", "TPA", "QMD", "BA"), th)
        )  
      )
    )
  ))

 datatable(yield.summary.table(OP.tab.cal()), container =  yield.header , rownames = FALSE, 
           options = list(paging = FALSE, ordering=F, sDom  = '<"top">rt<"bottom">',
                          columnDefs=list(list(targets= '_all', class="dt-right"))               
           ))
 
  })

output$grow.stock <- renderDT({

  gsth1 <- paste("at Age:", GS.Table.Age(), "Year:", GS.Table.Age()+Plant.yr(), sep="\t")
  gst.header = htmltools::withTags(table(
    class = 'display',
    thead(
      tr(
        th(colspan = 10, gsth1)
      ),
      tr(
          lapply(c("Crown Class", "CFVol Merch", "BFVol", "CT BFVol", "CT TPA", "Diameter", 
                   "Height", "TPA", "BFV:CFV", "BA"), th)
        )  
      )
    )
  )

  datatable(Growing.stock.table(OP.tab.cal(), cf.merch.lim = cf.merch.lim(), Age=GS.Table.Age()),
            container =  gst.header , rownames = FALSE, 
            options = list(paging = FALSE, ordering=F, sDom  = '<"top">rt<"bottom">',
                           columnDefs=list(list(targets= '_all', class="dt-right"))               
                           ))
})

output$PCT.subtitle <-  renderUI({
  
})
output$PCT.summary <- renderDT({
  
  pcth1 <- paste("PCT Year:", ifelse(pct.age()>0, pct.age()+Plant.yr(), "No PCT"), sep="\t")
  pct.header = htmltools::withTags(table(
    class = 'display',

    thead(
      tr(
        th(colspan = 3, pcth1)
      ),
      tr(
        lapply(c("PCT Age (year)", "Total Removal (TPA)", "Slash Biomass (Ton/Acre)"), th)
      )  
    )
  )
  )
  datatable(PCT.summary.table(OP.tab.cal()),
            container =  pct.header , rownames = FALSE, 
            options = list(paging = FALSE, ordering=F, sDom  = '<"top">rt<"bottom">',
                           columnDefs=list(list(targets= '_all', class="dt-right"))               
            ))          
  
            
})

p1<-function() Ann.vol.inc.view(OP.tab.cal())
output$Ann.View <- renderPlot(p1())

p2<-function() Den.by.class.view(OP.tab.cal())
output$Den.View <- renderPlot(p2())

p3<-function() Density.view(OP.tab.cal(), max.sdi= max.sdi())
output$Den.View2 <- renderPlot(p3())

output$TPA.for.PCT <- renderDT({
  
  TPAPCT.header = htmltools::withTags(table(
    class = 'display', style = "text-align: center",
    thead(
      tr(
        th(rowspan = 2, "UMZ SDI"),
        th(colspan = 9, "Quadratic Mean Diameter (inch)", style = "text-align: center")
      ),
      tr(
          lapply(c("10", "11", "12", "13", "14", "15", "16", "17", "18"), th)
        )  
      )
    )
  )
  
  datatable(TPA.for.PCT.table(OP.tab.cal(), reineke.term = reineke.term()),
            container =  TPAPCT.header , rownames = FALSE, 
            options = list(paging = FALSE, ordering=F, sDom  = '<"top">rt<"bottom">',
                           columnDefs=list(list(targets= '_all', class="dt-right"))               
            ))          
  
})

output$Den.guide <- renderDT({
  
  Guide.header = htmltools::withTags(table(
    class = 'display', 
    thead(
      tr(
        th(colspan = 2, "Initial", style = "text-align: center"),
        th(colspan = 2, "50% Thin", style = "text-align: center"),
        th(colspan = 2, "33% Thin", style = "text-align: center")
      ),
      tr(
        lapply(rep(c("Spacing", "TPA"),3), th)
      )  
    )
  )
  )
  
  datatable(Density.guide,
            container =  Guide.header , rownames = FALSE, 
            options = list(paging = FALSE, ordering=F, sDom  = '<"top">rt<"bottom">',
                           columnDefs=list(list(targets= '_all', class="dt-center"))               
            ))          
  
})

output$table_out1 <- downloadHandler(
  filename = function(){
    paste("Yield_Summary", switch(input$Tblform1, CSV="csv", XLSX="xlsx"), sep = "." )
  },
  content = function(file) {
    t_2<-yield.summary.table(OP.tab.cal())
    switch(
      input$Tblform1, 
      CSV=write.csv(t_2, file, row.names = FALSE),
      XLSX=write.xlsx(t_2, file, row.names = FALSE)
    )
    
  }
)

output$table_out2 <- downloadHandler(
  filename = function(){
    paste("Growing_Stock_Table", switch(input$Tblform2, CSV="csv", XLSX="xlsx"), sep = "." )
  },
  content = function(file) {
    t_2<-Growing.stock.table(OP.tab.cal(), cf.merch.lim = cf.merch.lim(), Age=GS.Table.Age())
    switch(
      input$Tblform2, 
      CSV=write.csv(t_2, file, row.names = FALSE),
      XLSX=write.xlsx(t_2, file, row.names = FALSE)
    )
    
  }
)

output$image_out1 = downloadHandler(
  filename = function() {
    paste("Vol_Inc_Plot", sep = ".", switch(
      input$IMGformat1, PDF = "pdf", PNG = "png", TIFF = "tiff")
    )
  },
  content = function(file) {
    switch(
      input$IMGformat1,
      PDF=pdf(file, width=12, height=8),
      PNG=png(file, width = 8, height = 6, units = 'in', pointsize=10, res = 300),
      TIFF=tiff(file, width = 8, height = 6, units = 'in', pointsize=10, res = 300))
    p1()
    dev.off()
  }
)

output$image_out2 = downloadHandler(
  filename = function() {
    paste("Stand_Den_Plot", sep = ".", switch(
      input$IMGformat2, PDF = "pdf", PNG = "png", TIFF = "tiff")
    )
  },
  content = function(file) {
    switch(
      input$IMGformat2,
      PDF=pdf(file, width=12, height=8),
      PNG=png(file, width = 8, height = 6, units = 'in', pointsize=10, res = 300),
      TIFF=tiff(file, width = 8, height = 6, units = 'in', pointsize=10, res = 300))
    p3()
    dev.off()
  }
)
output$image_out3 = downloadHandler(
  filename = function() {
    paste("Den_CClass_Plot", sep = ".", switch(
      input$IMGformat3, PDF = "pdf", PNG = "png", TIFF = "tiff")
    )
  },
  content = function(file) {
    switch(
      input$IMGformat3,
      PDF=pdf(file, width=12, height=8),
      PNG=png(file, width = 8, height = 6, units = 'in', pointsize=10, res = 300),
      TIFF=tiff(file, width = 8, height = 6, units = 'in', pointsize=10, res = 300))
    p2()
    dev.off()
  }
)



}



