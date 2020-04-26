## Example shiny app with rank list

library(shiny)
library(sortable)
library(nessy)
library(shinyjqui)
library(glue)
library(praise)
library(sortable)
library(beepr)




ui <- cartridge(
  title = "",
  tags$div(id="welcome"),
  tags$div(id="progress"),
  tags$div(id="getname"),
  tags$div(id="start"),
  tags$div(id="game"),
)

server <- function(input, output) {
 
  output$order <- renderPrint({ print(input$dest_order) })
  output$user <- renderText(user())
  output$userlevel <- renderText(userlevel())
  output$userfuel <- renderText(userfuel())
  
  user <- reactive({
    if (input$username == "") {
      glue("{sample(praise_parts$adjective, 1)}{sample(1:10000, 1)}")
    } else input$username
  })
  
  userfuel = reactive({
    5
  })
  
  userlevel = reactive({
    if(userfuel() > 5){
      1
    }else{
      0
    }
  })
  

  output$start <- renderText(
    paste(c("Thank goodness you are here", user(),"! Put the equation in order to earn tokens for Euclideo's trip home!"))
  )
  
  ## Handle welcome
  insertUI(selector="#welcome",
           ui = tags$div(
             tags$h3("Help Euclideo get rocket fuel to make it home!",
                     style="color:grey"),
             
             id = "welcomemsg"),
  )
  
  observeEvent(input$begin,
               removeUI(selector="#welcomemsg")
               )
  
  
  
  
  ## Handle progress
  
  observeEvent(input$begin,
      insertUI(selector="#progress",
        ui = tags$div(
          container_with_title(title="Progress",
                               "username:",
                               textOutput("user", inline = TRUE), 
                               HTML("&nbsp;&nbsp;&nbsp;&nbsp;"),
                               icon("gas-pump"),
                               "X",
                               textOutput("userfuel", inline = TRUE),  
                               HTML("&nbsp;&nbsp;&nbsp;&nbsp;"),
                               "level:", 
                               textOutput("userlevel", inline = TRUE)
                               ),
          id="progressbar"
        )
      )
  )
  
  ### Handle username
  
  insertUI(selector="#getname",
      ui = tags$div(
        container_with_title(
          "What is your name hero?",
          text_input("username", "username", 
               inline = TRUE,
               width='500px',
               placeholder="Dex"),
          button_primary("begin", "begin")),
        id = "#getusername",
      )
    )
  
  observeEvent(input$begin,
               removeUI(selector="#getusername")
  )
  
  
  ### Handle game block
  
  insertUI(selector="#game",
            container_with_title(
                "Put the equation in the right order",
                    bucket_list(
                      header = "Drag the items in any desired bucket",
                       group_name = "bucket_list_group",
                       orientation = "horizontal",
                       add_rank_list(
                         text = "Drag from here",
                         labels = list("5","2","x","3","=","13","+"),
                         input_id = "rank_list_1"
                       ),
                       add_rank_list(
                         text = "to here",
                         labels = NULL,
                         input_id = "rank_list_2"
                       )
                     )
                   )
    )


  

  
}

shinyApp(ui, server)


#https://community.rstudio.com/t/shinyapps-io-and-sound/19967/2