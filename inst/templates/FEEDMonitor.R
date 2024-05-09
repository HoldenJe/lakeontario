#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above. or shiny::runApp()

library(shiny)
library(DT)
library(dplyr)
library(RODBC)
# library(ggplot2)
# library(plotly)

# Access requires 32-bit R
Sys.getenv("R_ARCH") == "/i386" # requires TRUE

# # Connect to db ----
dbase <-"Raw/xSBT23.mdb"  # update this like with the live database
file.exists(dbase)
# #### load data ----
conn <- odbcConnectAccess(dbase, uid = "", pwd = "")
onStop(function(){
  cat("Database connection has been closed")
  odbcCloseAll()
})

ports <- sqlFetch(conn, "Codes_Ports")

# optional: group ports ----
# optional code to combine transects in to transects groups for oto collections
ports <- ports %>%
  mutate(TransectGroup = case_when(
    PortCode %in% c(632, 630) ~ "BQ",
    PortCode %in% c(640, 641) ~ "COB + WES",
    PortCode %in% c(642, 643) ~ "OSH + PIC",
    PortCode %in% c(693) ~ "RP",
    PortCode %in% c(694) ~ "EB",
    PortCode %in% c(645, 686) ~ "TOR + HAM"
  ))

fish <- sqlFetch(conn, "TR_7FISH")

## app file ----

shinyApp(
  ui = fluidPage(
    titlePanel("FEED Oto Counts"),
    sidebarLayout(
      sidebarPanel(
        selectInput(inputId = "transectgroup",
                    label = "Transect Group",
                    choices = unique(ports$TransectGroup)),
        selectInput(inputId = "selectedspecies",
                    label = "Species",
                    choices = unique(fish$Species))
      ),
      mainPanel(DTOutput('tbl'))
      )),

  server = function(input, output) {

    reactive_fish <- reactivePoll(
      intervalMillis = 1000,
      session = NULL,
      checkFunc = function(){
        sqlQuery(conn, "SELECT * FROM TR_7FISH")
      },
      valueFunc = function() {
        fish <- sqlFetch(conn, "TR_7FISH")
        fish
      }
    )

    reactive_op <- reactivePoll(
      intervalMillis = 1000,
      session = NULL,
      checkFunc = function(){
        sqlQuery(conn, "SELECT * FROM Calc_RVCAT OP")
      },
      valueFunc = function() {
        op <- sqlFetch(conn, "Calc_RVCAT OP")
        op
      }
    )

    output$tbl = renderDT({
      op <- reactive_op()
      op_summ <- inner_join(op, ports, by = c("PORT" = "PortCode")) %>%
        select(Description, SERIAL, PORT, TransectGroup)

      has_oto <- reactive_fish() %>%
        filter(LifeStage == 6) %>%
        select(Serial, Species, LifeStage, Length, AgeStructureYN) %>%
        mutate(SizGrp = cut(Length, seq(0,250, 10))) %>%
        mutate(HasOto = ifelse(AgeStructureYN == "Y", TRUE, FALSE)) %>%
        group_by(Serial, Species, SizGrp) %>%
        summarize(N_Bio = n(), N_Oto = sum(HasOto))

      to_shiny <- left_join(op_summ, has_oto, by =c("SERIAL" = "Serial"))
      to_shiny <- to_shiny %>%
        group_by(TransectGroup, Species, SizGrp) %>%
        summarize(N_Bio = sum(N_Bio, na.rm = T), N_oto = sum(N_Oto, na.rm = T)) %>%
        arrange(TransectGroup, Species, SizGrp)

      selected_transect <- input$transectgroup
      selected_species <- input$selectedspecies

      subsetted_data <- to_shiny %>%
        filter(TransectGroup == selected_transect,
               Species == selected_species)

      datatable(subsetted_data) %>%
      formatStyle(
        columns = names(subsetted_data)[c(4,5)],
        valueColumns = names(subsetted_data)[c(4,5)],
        backgroundColor = styleInterval(9, c('white', 'red'))
      )
    }
      )
  }
)
