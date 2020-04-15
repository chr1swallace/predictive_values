#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(data.tree)
library(DiagrammeR)
library(personograph)
library(ggplot2)
library(markdown)
source("helpers.R")

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("How accurate is that antibody test?"),
  includeMarkdown("intro.md"),
# Sidebar with a slider input for number of bins 
sidebarLayout(
  sidebarPanel(
    sliderInput("sens",
                "Sensitivity (%)",
                min = 0,
                max = 100,
                value = 94),
    sliderInput("spec",
                "Specificity (%)",
                min = 0,
                max = 100,
                value = 91),
    sliderInput("imm",
                "Immune fraction (%)",
                min = 0,
                max = 100,
                value = 10)
    # selectInput("controller", "Show output", choices = c("waffle","tree"))
    
     ),
  
  # Show a plot of the generated distribution
  mainPanel(
    #  useShinyjs(),
    # div(id="start",
    #     textInput("root_name","root_name","1"),
    #     textInput("1_child","1_child","1.1"),
    #     actionButton("go","go")
    #     ),
    # collapsibleTreeOutput("forwardPlot", height="500px"),
    
    tabsetPanel(
      id = "switcher",
      tabPanel("show as a waffle plot", 
      h4("How tests are evaluated: sensitivity and specificity"),
    p(""),
    plotOutput('forwardPerson', width = "600px", height = "600px"),
    h4("What we care about if we get tested: predictive value"),
    plotOutput('reversePerson', width = "600px", height = "600px")),
      tabPanel("show as a tree", 
    h4("How tests are evaluated: sensitivity and specificity"),
    p(""),
    grVizOutput('forwardPlot', width = "600px", height = "400px"),
    h4("What we care about if we get tested: predictive value"),
    grVizOutput('reversePlot', width = "800px", height = "400px")
               )
    )
    
    
    # p("When we use the tests in the population, we want to start from the other side: given a test is positive or negative, what is the chance that a person is really immune or not immune? These are the positive and negative predictive values.  These depend not just on the sensitivity and specificity, but on what proportion of the population being tested are actually immune."),
    # plotOutput("reversePlot")
  )
)
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$forwardPlot <- renderGrViz({
    # renderCollapsibleTree({
    f <- getf(input)
    # df<-expand.grid(Immune=c("Not immune","Immune"),
    #                 Test=c("Positive","Negative"))
    # df$f <- ifelse(df$Immune=="Immune",
    #                ifelse(df$Test=="Positive",f$f.imm.pos,f$f.imm.neg),
    #                ifelse(df$Test=="Positive",f$f.nim.pos,f$f.nim.neg))
    # df$N <- round(df$f * f$N)
    # df$forward <- df$N / (f$N * ifelse(df$Immune=="Immune", input$imm, 100-input$imm)/100)
    # df$reverse <- df$N / (f$N * ifelse(df$Test=="Positive", (f$f.imm.pos + f$f.nim.pos), (f$f.imm.neg+f$nin.neg)))
    # 
    # collapsibleTree(df,
    #                 hierarchy=c("Immune","Test"),
    #                 collapse=FALSE, zoomable=FALSE,
    #                 root="Tested population",
    #                 tooltip = TRUE,
    #                 attribute="forward")
    
    
    x <- Node$new(paste0("Tested population: ",f$N))
    immune <- x$AddChild(paste0("Immune: ",round(f$N*f$imm)))
    nonimmune <- x$AddChild(paste0("Not immune: ",round(f$N*(1-f$imm))))
    ipos <- immune$AddChild(paste0("SENSITIVITY:\n\n",
                                   "Test positive: ",round(f$N*f$imm*f$sens),"\n\n",
                                   round(f$N*f$imm*f$sens),"/",round(f$N*f$imm)," = ",input$sens,"%"))
    ineg <- immune$AddChild(paste0("Test negative: ",round(f$N*f$imm*(1-f$sens))))
    npos <- nonimmune$AddChild(paste0("Test positive: ",round(f$N*(1-f$imm)*(1-f$spec))))
    nneg <- nonimmune$AddChild(paste0("SPECIFICITY:\n\n",
                                      "Test negative: ",round(f$N*(1-f$imm)*f$spec),"\n\n",
                                      round(f$N*(1-f$imm)*f$spec),"/",round(f$N*(1-f$imm))," = ",input$spec,"%"))
    # SetNodeStyle(ipos, inherit = FALSE, fillcolor = "Thistle", 
    #              fontcolor = "Firebrick", 
    #              tooltip = paste0("This is sensitivity: 100*",round(f$N*f$imm.pos),"/",round(f$N*f$imm)," = ",input$sens))
    SetGraphStyle(x, rankdir = "TB")
    SetEdgeStyle(x, color = "grey35", penwidth = 1)
    SetNodeStyle(x, style = "filled,rounded", shape = "box", color="grey35", fillcolor = "olivedrab", 
                 fontname = "helvetica", tooltip = GetDefaultTooltip)
    # SetNodeStyle(x$immune, fillcolor = "LightBlue", penwidth = "5px")
    # grViz(ToGraphViz(x),engine = "dot") #plot(x) 
    GetEdgeLabel <- function(node) {
          label = node$name
      if(grepl("^Immune",node$name))
        label = paste0(input$imm,"%")
      if(grepl("^Not",node$name))
        label = paste0(100-input$imm,"%")
      if(grepl("Test positive",node$name))
        label=if(grepl("^Immune",node$parent$name)) { roundpc(f$imm.pos/f$imm) } else { roundpc(f$nim.pos/f$nim)}  
      if(grepl("Test negative",node$name))
        label=if(grepl("^Immune",node$parent$name)) { roundpc(f$imm.neg/f$imm) } else { roundpc(f$nim.neg/f$nim)}  
      return (label)
    }
    SetEdgeStyle(x, fontname = 'helvetica', label = GetEdgeLabel, color="grey35")
    plot(x)
    # collapsibleTree(x,collapsed=FALSE)
  })
  
  output$reversePlot <- renderGrViz({
    f <- getf(input)
    y <- Node$new(paste0("Tested population: ",f$N))
    pos <- y$AddChild(paste0("Test positive: ",round(f$N*(f$imm.pos + f$nim.pos))))
    neg <- y$AddChild(paste0("Test negative: ",round(f$N*(f$imm.neg + f$nim.neg))))
    pimm <- pos$AddChild(paste0("POSITIVE PREDICTIVE VALUE:\n\n",
                                "Immune: ",round(f$N*f$imm.pos),"\n\n",
                                round(f$N*f$imm.pos),"/",round(f$N*(f$nim.pos+f$imm.pos))," = ",
                                roundpc(f$imm.pos/(f$nim.pos+f$imm.pos))))
    nimm <- pos$AddChild(paste0("Not immune: ",round(f$N*f$imm.neg)))
    pnimm <- neg$AddChild(paste0("Immune: ",round(f$N*f$nim.pos)))
    nnimm <- neg$AddChild(paste0("NEGATIVE PREDICTIVE VALUE:\n\n",
                                 "Not immune: ",round(f$N*f$nim.neg),"\n\n",
                                 round(f$N*f$nim.neg),"/",round(f$N*(f$nim.neg+f$imm.neg))," = ",
                                 roundpc(f$nim.neg/(f$nim.neg+f$imm.neg))))
    SetGraphStyle(y, rankdir = "TB")
    SetEdgeStyle(y, color = "grey35", penwidth = 1)
    SetNodeStyle(y, style = "filled,rounded", shape = "box", fillcolor = "DarkSlateBlue", 
                 color="grey35",
                 fontname = "helvetica", tooltip = GetDefaultTooltip)
    GetEdgeLabel <- function(node) {
          label = node$name
      if(grepl("^Test positive",node$name))
        label = roundpc((f$imm.pos+f$nim.pos))
      if(grepl("^Test negative",node$name))
        label = roundpc((f$imm.neg+f$nim.neg))
      if(grepl("^Not",node$name))
        label = paste0(100-input$imm,"%")
      if(grepl("Immune",node$name))
        label=if(grepl("^Test positive",node$parent$name)) { roundpc(f$imm.pos/(f$pos)) } else { roundpc(f$imm.neg/(f$neg))}  
      if(grepl("Not immune",node$name))
        label=if(grepl("^Test positive",node$parent$name)) { roundpc(f$nim.pos/(f$pos)) } else { roundpc(f$nim.neg/(f$neg))}  
      return (label)
    }
    SetEdgeStyle(y, fontname = 'helvetica', label = GetEdgeLabel, color="grey35")
    plot(y)
    
    # grViz(plot(y),engine = "dot") #plot(x) 
    
    # collapsibleTree(y,collapsed=FALSE)
  })
  
  output$reversePerson = renderPlot({
    muted6=c("#4878D0", "#6ACC64", "#D65F5F", "#956CB4", "#D5BB67", "#82C6E2")
    pastel6=c("#A1C9F4", "#8DE5A1", "#FF9F9B", "#D0BBFF", "#FFFEA3", "#B9F2F0")
    f <- getf(input)
    df=data.frame(what=c("imm.pos","imm.neg","nim.pos","nim.neg"),stringsAsFactors = FALSE)
    rownames(df)=df$what
    df$i=sub("\\..*","",df$what)
    df$t=sub(".*\\.","",df$what)
    df$f=f[df$what]
    df$ymin=ifelse(df$t=="pos",0,f$pos)
    df$ymax=ifelse(df$t=="pos",f$pos,1)
    df$xmin=0
    df$xmax=1
    df["imm.pos","xmin"] <- df["nim.pos","xmax"] <- f$nim.pos/f$pos
    df["imm.neg","xmin"] <- df["nim.neg","xmax"] <- f$nim.neg/f$neg
    ann=data.frame(x=.5,y=c(f$pos/2,1-f$neg/2),
                   label=paste0(c("Positive predictive value: of test positives, ","Negative predictive value: of test negatives, "),
                                roundpc(c(f$imm.pos/f$pos, f$nim.neg/f$neg))," are ",
                                c("immune","non-immune")))
    # translate=c(imm.neg="Immune, -",nim.neg="Non-immune, -",imm.pos="Immune, +",nim.pos="Non-immune, +")
    translate=c(imm.neg="Immune, test negative",nim.neg="Non-immune, test negative",imm.pos="Immune, test positive",nim.pos="Non-immune, test positive")
    pal=c(imm.neg=pastel6[1],imm.pos=muted6[1],nim.neg=pastel6[2],nim.pos=muted6[2])
    df
    ggplot(df) + 
      geom_rect(aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax,fill=what)) +
      geom_hline(yintercept=seq(0,1,length.out=20),colour="white") +
      geom_vline(xintercept=seq(0,1,length.out=20),colour="white") +
      geom_label(aes(label=label,x=x,y=y),data=ann,fill="#ffffffdd",size=5) +
      scale_fill_manual("", values=pal, labels=function(l) translate[l]) +
      theme_void(base_size = 20) +
      theme(legend.position="bottom") +
    guides(fill=guide_legend(nrow=2,byrow=TRUE))
  })
  
  output$forwardPerson = renderPlot({
    muted6=c("#4878D0", "#6ACC64", "#D65F5F", "#956CB4", "#D5BB67", "#82C6E2")
    pastel6=c("#A1C9F4", "#8DE5A1", "#FF9F9B", "#D0BBFF", "#FFFEA3", "#B9F2F0")
    f <- getf(input)
    df=data.frame(what=c("imm.pos","imm.neg","nim.pos","nim.neg"),stringsAsFactors = FALSE)
    rownames(df)=df$what
    translate=c(imm.neg="Immune, test negative",nim.neg="Non-immune, test negative",
                imm.pos="Immune, test positive",nim.pos="Non-immune, test positive")
    df$i=sub("\\..*","",df$what)
    df$t=sub(".*\\.","",df$what)
    df$f=f[df$what]
    df$ymin=ifelse(df$i=="imm",0,f$imm)
    df$ymax=ifelse(df$i=="imm",f$imm,1)
    df$xmin=0
    df$xmax=1
    df["imm.pos","xmin"] <- df["imm.neg","xmax"] <- f$imm.neg/f$imm
    df["nim.pos","xmin"] <- df["nim.neg","xmax"] <- f$nim.neg/f$nim
    ann=data.frame(x=.5,y=c(f$imm/2,1-f$nim/2),
                   label=paste0(c("Specificity: of immune, ","Sensitivity: of non-immune, "),
                                roundpc(c(f$imm.pos/f$imm, f$nim.neg/f$nim))," test ",
                                c("positive","negative")))
    pal=c(imm.neg=pastel6[1],imm.pos=muted6[1],nim.neg=pastel6[2],nim.pos=muted6[2])
    df
    ggplot(df) + 
      geom_rect(aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax,fill=what)) +
      geom_hline(yintercept=seq(0,1,length.out=20),colour="white") +
      geom_vline(xintercept=seq(0,1,length.out=20),colour="white") +
      geom_label(aes(label=label,x=x,y=y),data=ann,fill="#ffffffdd",size=5) +
      scale_fill_manual("", values=pal,labels=function(l) translate[l]) +
      theme_void(base_size=20) +
      theme(legend.position="bottom")+
    guides(fill=guide_legend(nrow=2,byrow=TRUE))
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

