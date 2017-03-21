library(shiny)
library(shinythemes)  #  for shinytheme
library(shinyjs)      #
library(colourpicker)
library(rhandsontable)
library(listviewer) 
# Define UI for dataset viewer application
#shinyAppDir("D:/dev/Svn/hsg/misc/rplotter")
setwd("D:/dev/Svn/hsg/misc/rplotter")
shinyUI(fluidPage(
  # select theme from shinytheme list
  theme = shinytheme("cerulean"),
  # Application title.
  titlePanel("Rplotter"),
  
  
  # Sidebar with controls to load control and dataset files and specify the
  # number of observations to view. The helpText function is
  # also used to include clarifying text. Most notably, the
  # inclusion of a submitButton defers the rendering of output
  # until the user explicitly clicks the button (rather than
  # doing it immediately when inputs change). This is useful if
  # the computations required to render output are inordinately
  # time-consuming.
  sidebarLayout(
    #begin of sidebar panel
    sidebarPanel(
      fileInput(
        "style_file",
        label = "Options File",
        accept = c('text/csv',
          'text/comma-separated-values',
		  '.xml',
          '.csv')
      ),
      fileInput(
        "graphdef_file",
        label = "Graphdef File",
        accept = c(
          'text/csv',
          'text/comma-separated-values',
          '.csv',
          '.xml'
        )
      ),
      fileInput(
        "data_file",
        label = "Data File",
        accept = c(
          'text/csv',
          'text/comma-separated-values',
          'text/tab-separated-values',
          'text/plain',
          '.csv',
          '.tsv',
          '.sas7bdat'
        )
      ),
      # tags$hr(),
      
      # checkboxInput('header', 'Header', TRUE),
      # conditionalPanel(
      #   condition = "input.data_file.type== 'csv'",
      # radioButtons('sep', 'Separator',
      #               c(Comma=',',
      #                 Semicolon=';',
      #                 Tab='\t'),
      #               ',',inline=TRUE)),
      #  radioButtons('quote', 'Quote',
      #               c(None='',
      #                'Single'="'",
      #                'Double'='"'),
      #               '"',inline=TRUE),
      #
      # tags$hr(),
      # numericInput("obs", "Number of observations to view:", 10),
       textInput( "plotdata",label = "Where to save plots data",value="plotdata"),
				  textInput( "plotoutput",
                         label = "Where to save plots",value="output"),
      
      
      #submitButton("Refresh"),
	  radioButtons('format_standards', 'Formatting Standards',
                     c(Merck='merck',
                       Other='other'),
                     'other',inline=TRUE),
      actionButton("save","Update"),
      #hr(),
		  actionButton("saveGraphdef","Save Data/Graphdef"),
		  actionButton("savePlot","Save Plot"),
		  helpText(
        "Note: This is a plotting tool"
      )
      
    ),
    #end of sidebar panel
    
    #beginn of main panel
    mainPanel(
      tabsetPanel(id="tabsetpanel",
        type = "tabs",
        tabPanel("Page",
                 fluidRow(
                   column(
                     6,
                     inputPanel(
                       selectInput(
                         "npage",
                         "Select a page from the list",
                         choices = NULL,
                         selected = NULL
                       ),
                       radioButtons(
                         "type",
                         "Plot Type:",
                         c("Forest" = "forest",
                           "Survival Curve" = "km"),
                         inline = TRUE
                       ),
                       radioButtons(
                         "page.size",
                         "Page Size:",
                         c(
                           "A4" = "a4",
                           "USA Letter" = "us",
                           "PPT" = "ppt","Custom"="custom"
                         ),
                         inline = TRUE
                       ),
                       radioButtons(
                         "orient",
                         "Page Orientation:",
                         c("landscape", "portrait"),
                         inline = TRUE
                       ),
                       radioButtons(
                         "output.type",
                         "Output Type:",
                         c(
                           "GUI" = "gui",
                           "SVG" = "svg",
                           "PDF" = "pdf",
                           "EMF" =  "emf"
                         ),
                         inline = TRUE
                       ),
                       
                       
                       numericInput("width", "Page width(inch)", 11.69),
                       numericInput("height", "Page height(inch)", 8.26),
                       radioButtons(
                         "suppress.tinyfooter",
                         "Hide footnote:",
                         c(
                           "Yes" = "true",
                           "No" = "false"
                         ),
                         inline = TRUE
                       ),
                       
                       # checkboxInput("suppress.tinyfooter",
                       #               "Hide footnote",
                       #               FALSE),
                       textInput("footer.text", "Footnote:", value =NULL),
                       
                       textInput("bottom.label", "Text to add below the plot:", value =NULL),
                       textInput("data", "Path to file (absolute or relative):",value =NULL),
                       textInput("filestem", "Filename prefix", value =NULL)
                       
                     )
                   ),
                   column(
                    6,
                     inputPanel(
                       sliderInput(
                         "titlespace",
                         "Percentage of the entire page to leave blank as a title space)",
                         min = 0,
                         max = 10.0,
                         value = 5.0,
                         step = 0.1
                       ),
                       sliderInput(
                         "footerspace",
                         "Percentage of the entire page to leave blank as footer space",
                         min = 0,
                         max = 10.0,
                         value = 5.0,
                         step = 0.1
                       ),
                       sliderInput(
                         "linespacing",
                         "Line spacing",
                         min = 0,
                         max = 10,
                         value = 5,
                         step = 0.1
                       ),
                       sliderInput(
                         "indentation.width",
                         "Number of spaces to use as the indentation level",
                         min = 0,
                         max = 10,
                         value = 5,
                         step = 1
                       ),
                       sliderInput(
                         "cex",
                         "Font scaling on this page",
                         min = 0,
                         max = 5,
                         value = 1,
                         step = 0.01
                       ),
                       sliderInput(
                         "line.width",
                         "Line width",
                         min = 0,
                         max = 5.0,
                         value = 1,
                         step = 0.1
                       ),
                       colourInput(
                         "fill.colour",
                         "Default point fill colour on this page",
                         "blue",
                         returnName = TRUE,
                         palette = "limited",
						  allowedCols = c(
                           "white",
                           "black",
                           "red",
                           "blue",
                           "yellow",
                           "purple",
                           "green",
                           "#DDD")
                       ),
                       colourInput(
                         "font.colour",
                         "Default font colour on this page",
                         "red",
                         returnName = TRUE,
                         palette = "limited",
						  allowedCols = c(
                           "white",
                           "black",
                           "red",
                           "blue",
                           "yellow",
                           "purple",
                           "green",
                           "#DDD")
                       ),
                       colourInput(
                         "line.colour",
                         "Default line colour on this page",
                         "black",
                         returnName = TRUE,
                         palette = "limited",
                         allowedCols = c(
                           "white",
                           "black",
                           "red",
                           "blue",
                           "yellow",
                           "purple",
                           "green",
                           "#DDD"
                         )
                       )
                     )
                   )
                 )),
        tabPanel("Figure",
                 fluidRow(
                   column(
                     6,
                     
                     inputPanel(
                       selectInput("nforest",
                                   "Select a forest plot from the list",
                                   ""),
                       
#                        selectizeInput(
#                          "columns",
#                          label = "Columns",
#                          choices = NULL,
#                          multiple = TRUE
#                        ),
# 					   selectizeInput(
#                          "columns.headings",
#                          label = "Column headings:",
#                          choices = NULL,
#                          multiple = TRUE
#                        ),
#                        
					   sliderInput(
                          "columns.headings.style",
                         label = "Column headings style:",
                         min = 0,
                         max = 10,
                         value = 2,
                         step = 1
                       ),
sliderInput(
  "columns.headings.line",
  label = "Column headings line:",
  min = 0,
  max = 3,
  value = 1,
  step = 1
), 
textInput(
                         "columns.pvalues",
                         label = "Column Pvalue:",
                         value =NULL
                       ),
                      textInput(
                         "range",
                         label = "X axis range:",
                         value =NULL
                       ),
					   textInput( "ticks",
                         label = "X axis ticks displayed with a numeric label",value=NULL),
                       
                       radioButtons("tickdirection",
                                    "Tick direction:",
                                    c("in"="in", "out"="out"),
                                    inline = TRUE),
                       
                       sliderInput(
                         "position",
                         "Position:",
                         min = 0,
                         max = 100,
                         value = c(62, 85),
                         step = 1
                       ),
                       
                       sliderInput(
                         "boxparm",
                         "Box size scaling adjustment:",
                         min = 0,
                         max = 5,
                         value = 1,
                         step = 0.01
                       ),
                       sliderInput(
                         "plot_font",
                         "Plot text font:",
                         min = 0,
                         max = 3.0,
                         value = 1.0,
                         step = 0.1
                       ),
                       
                       radioButtons(
                         "logscale",
                         "Log scale",
                         c(
                           "Yes" = "true",
                           "No" = "false"
                         ),
                         inline = TRUE
                       )
                     )
                     
                   ),
                   column(
                     6,
                     inputPanel(
                       
                       textInput(
                         'printvalue.heading',
                         "Label to display above the value labels:",
                         value =NULL
                       ),
                       textInput(
                         "axislabel",
                         "X axis label:",
                        
                       ),
                       textInput("axislabel.lower",
                                 "Axis label to use to left of no effect line:",
                                 value =NULL),
                       textInput("axislabel.upper",
                                 "Axis label to use to right of no effect line:",
                                 value =NULL)
                       
                     
                       
                       
                     )
                   )
                 )),
       
        tabPanel(
          "Data",
          checkboxGroupInput(
            "variable",
            "Variables to show:",
            choices = c(
              "Plot" = "plot",
              "Data" = "data",
              "Calculated Data" = "calculated"
            ), selected =  c(
              "plot",
             "data",
              "calculated"
            ),
              
            inline = TRUE
          ),
          DT::dataTableOutput("view")
        ),
        tabPanel("ListViewer", jsoneditOutput( "jsonedit1" ) ),
        tabPanel("Setting", textOutput("settings", container = pre)),
        tabPanel("Preview", rHandsontableOutput("tbl_coldefs"),hr(),rHandsontableOutput("tbl_text"),verbatimTextOutput("click_info"),plotOutput("preview", click = "plot_click")),
		tabPanel("Jasper", plotOutput("plot"), width = "100%")
      )
    )
  )
))