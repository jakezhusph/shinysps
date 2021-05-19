sidebar <- dashboardSidebar(
  width = 450,
  tags$style(".left-side, .main-sidebar {padding-top: 65px}"),

  sidebarMenu(      
    ## finally get the text wrapped
    tags$li(tags$style(".sidebar-menu{white-space: normal}")),
    menuItem("Spatial Pattern Design",icon = icon("pencil-ruler"), 
      menuSubItem("Show Pattern DashBoard",tabName = "dashboard", selected=TRUE,icon=icon("eye")),
      box(width=NULL,solidHeader=TRUE,status="warning",title="Choose CSV File",height=150,
        div(style = "margin-top:-20px"),
        fluidRow(width = 10,
          column(width=5,checkboxInput("header", "Header", TRUE)),
          column(width=5,checkboxInput("rownames", "Row Names", TRUE))),
        div(style = "margin-top:-50px"),
        fileInput("datafile", label=NULL,
          accept = c(
            "text/csv",
            "text/comma-separated-values,text/plain",
            ".csv")),
      ),

      box(width=NULL,solidHeader=TRUE,status="warning",title="Shape and Layout",
        selectInput('shape', 'Shape', c("Square","Circle","User Define Shape","User Define Spots")),
        fluidRow(
          column(width=6,selectInput('arrange', 'Spots Layout', c("Random","Grid"))),  
          column(width=6, numericInput('sid', 'Reproducible Seed', 1))
        ),
        sliderInput("numloc", "Expected Number of Spots:",min = 100, max = 10000, value = 1000, step = 100)
      ),


      box(width=NULL,title="Spatial Techniques",solidHeader=TRUE,status="warning",
        p(class="text-muted",
          "Note: preset mean and overdispersion parameter for the corresponding techniques"),
        selectInput('technique', label=NULL, c("ST","HDST","Slideseq","SlideseqV2"))
      )
    ),   ## end of the menuItem Dashboard

    menuItem("Count Data Generation",tabName = "simCounts", icon = icon("laptop-code")),

    fluidRow(
      column(width=12,
        column(width=5,  actionButton("exit", "Exit",icon=icon("sign-out-alt"))),
        column(width=7,actionButton("reload", "Restart Spatial Pattern",icon=icon("redo")))
      )
    )
  ) ## end of the sidebarMenu
)## end of the dashboardSidebar

body <- dashboardBody(
  tags$head(tags$style(
    HTML(
      "pre { white-space: pre-wrap; word-break: keep-all; word-wrap:break-word;} 
      #countbrush {
        color: white;
        background: blue;
        font-family: 'Times New Roman', Times, serif;
        font-size: 12px;
        font-style: italic;
      }
      #brush3 {
        color: red;
        background: yellow;
        font-family: 'Times New Roman', Times, serif;
        font-size: 14px;
        font-weight: bold;
      }
      #brush4 {
        color: blue;
        background: orange;
        font-family: Arial, Helvetica, sans-serif;
        font-size: 14px;
      }
      "),
    )
  ), ## end of the tags$head

  theme_jqz,

  tabItems(
    tabItem("dashboard",class='active',
      fluidRow(
        column(width=8, 
        box(title="Visualization",status="success",solidHeader=TRUE,width = NULL,
        # align="middle",plotlyOutput('plot1',inline=TRUE),height=700)),
           align="middle",plotlyOutput('plot1',inline=TRUE),height=700)),

        column(width=4,
          box(solidHeader=TRUE,status="warning",title="Figure Adjustment",width = NULL,
          sliderInput("ptsize","Point Size",min=0.5,max=10,value=1),
          sliderInput("textsize","Text Size",min=5,max=20,value=10)),

          box(width=NULL,solidHeader=TRUE,status="warning",title="Group Assignment",
            fluidRow(column(6,textInput('NewGroup', label = 'New Group ID')),
            column(6,numericInput('fc', 'Fold Change in Mean', 1))),
            fluidRow(
              column(width=12,
              column(width=6,actionButton('Change','Confirm Assignment',icon=icon("check-circle"))),
              column(width=6,actionButton("loc_pop", "View Location File", icon=icon("table")))
              
            ))
          ),
          box(solidHeader=TRUE,status="warning",title="Simple Guidance For the Interactive Plot",width = NULL,
              p("1. Move cursor over the figure, the plotly options will show up"), 
              p("2. Click and select points using the box or the lasso select option"), 
              p("3. Double click the figure to deselect the points"), 
              p("4. Single click the legend text to hide selected groups"), 
              p("5. Double click the legend text to hide all other groups"), 
          )
        )
      ), # end of the fluidRow 

      bsModal("modalExample", "Location Data Table", "loc_pop", size = "large",
                dataTableOutput("distTable"),
                downloadButton("downloadPop", "Download Location File")
              ),

      br(),
      fluidRow( 
        box(title="Interactive Selection Info",width=6,status="primary",solidHeader=TRUE,verbatimTextOutput('brush')),
        box(title="Group Summary", width=6,status="primary",solidHeader=TRUE,column(width=12,align="center",tableOutput('summary_table'))),
        verbatimTextOutput('brush3')
      ),
      verbatimTextOutput('brush4')
    ), ## end of tabItem dashboard

    tabItem("simCounts",
      fluidRow(
        width=12,
        column(width=3,
          box(solidHeader=TRUE,status="warning",title="Simulation Setting",width=NULL,
          numericInput("numSig", "Number of Signal Genes", 50, min = 0),
          numericInput("numNoise", "Number of Noise Genes", 50, min = 0),
          numericInput("count_sid", "Reproducible Seed", 1),
          # actionButton("countGenerate",'Generate New Data')),

          fluidRow(width=12,
            column(width=5,actionButton("countGenerate",'Generate New Data')),
            column(width=7,actionButton("count_pop", "View CountData File", icon=icon("table")))
          )
          ),
          verbatimTextOutput('countbrush'),
          box(title="Interactive Simulation Info",status="primary",width=NULL,solidHeader=TRUE,verbatimTextOutput('countInfobrush')),
          box(width=NULL,solidHeader=TRUE,status="warning",title="Display Option",
              # checkboxInput("dospot", "Show Spots Pattern", value = T),
              checkboxInput("dosignal", "Show Expression Pattern For Signal Genes", value = T),
              checkboxInput("donoise", "Show Expression Pattern For Noise Genes", value = T),

              fluidRow(
                column(width=6,
                       numericInput("sigidx", "Signal Gene Index", 1, min = 1, max = 10000,step=1)),
                column(width=6,
                       numericInput("noidx", "Noise Gene Index", 1, min = 1, max = 10000,step=1))
              )
            ),

          box(width=NULL,solidHeader=TRUE,status="warning",title="Figure Adjustment",
            sliderInput("ptsizeCount","Point Size",min=0.5,max=10,value=3),
            sliderInput("textsizeCount","Text Size",min=10,max=30,value=15)
          )
        ),

        column(width=9,
            box(solidHeader=TRUE,status="success",title="Spots Spatial Pattern",width=NULL,
              plotOutput('SpotPlot')
            ),
            hr(),

            box(solidHeader=TRUE,status="success",title="Spatial Expression Pattern",width=NULL,        
              plotOutput('ExpressionPlot')
            ),
            
            # box(solidHeader=TRUE,status="success",title="Spatial Expression Pattern",width=NULL,        
            #   dataTableOutput("pltdfcheck")
            # )
        ),

        bsModal("modalCount", "Count Data Table", "count_pop", size = "large",
        dataTableOutput("countTable"),downloadButton("downloadPopCount", "Download CountData File"))
      )
    )## end of tabItem simCounts
  )## end of the tabItems
)## end of dashboardBody


dashboardPage(
  title="Spatial Expression Data Generation",
  dashboardHeader(
    # title = span('Spatial Expression Pattern Visualization',style = "font-size: 100px"), 
    title = customLogo,
    titleWidth = 450,
    tags$li(
      class = "dropdown",
      tags$style(".main-header {max-height: 50px ;}"),
      tags$style(".main-header .logo {height: 50px;}"),
      tags$style(".sidebar-toggle {height: 50px; color: #FFFFFF !important}"),
      tags$style(".navbar {min-height:50px !important}"),
    # tags$script("document.getElementsByClassName('sidebar-toggle')[0].style.visibility = 'hidden';") ## hide the sidebar-toggle
    )
  ),
  sidebar,
  body
) ## end of ui (dashboardPage)
