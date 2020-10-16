#' original UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_original_ui <- function(id){
  ns <- NS(id)
  tagList(
    bs4Dash::bs4DashPage(
      sidebar_collapsed = TRUE,
      sidebar_mini = FALSE,
      body = bs4Dash::bs4DashBody(
        fresh::use_theme(hamiltonCovid19::theme_bs4Dash()),
        fluidRow(
          bs4Dash::column(
            width = 4,
            br(),
            selectInput(
              ns("VarShowInt"),
              label = "Select region to show:",
              choices = get_regions(),
              selected = "MAYNOOTH"
            ),
            h5(div("Figure guide:")),
            h6(div("The solid  line shows the 28-day centered sum of Notices Posted to RIP.ie for the selected region. The dashed line shows the average for the same period from 2015--2019. The dotted line shows the maximum for the same period."))
          ),
          bs4Dash::column(
            width = 8,
            br(),
            plotOutput(ns("plot1")) %>% hamiltonCovid19::with_load_spinner(),
            plotOutput(ns("plot2")) %>% hamiltonCovid19::with_load_spinner()
          )
        )
      )
    )
 
  )
}
    
#' original Server Function
#'
#' @noRd 
mod_original_server <- function(input, output, session){
  ns <- session$ns
  
  rk_grouped <- hamiltonExcessdeaths::rk_grouped
  ire <- hamiltonExcessdeaths::ire
  merged_rk_data <- hamiltonExcessdeaths::merged_rk_data

  bases <- reactive({
    merged_rk <- merged_rk_data %>%
      dplyr::filter(Group == input$VarShowInt) %>%
      dplyr::filter(Year == 2020)

    ref_level <- merged_rk_data %>%
      dplyr::filter(Year < 2020 & Year >=2015 ) %>%
      dplyr::ungroup() %>%
      dplyr::group_by(Group,Date) %>%
      dplyr::summarize(Monthly_Notices = sum(Monthly_Notices)) %>%
      dplyr::mutate(DOY = lubridate::yday(Date)) %>%
      dplyr::group_by(Group,DOY) %>%
      dplyr::mutate(
        Ref_Level = mean(Monthly_Notices),
        Prev_Max = max(Monthly_Notices)
      ) %>%
      dplyr::filter(Group == input$VarShowInt)

    list(
      merged_rk_data = merged_rk,
      ref_level = ref_level
    )

  })
  
  
  output$plot1 <- renderPlot({

    x <- rk_grouped$RoutingKey[which(rk_grouped$Group==input$VarShowInt)]
    x <- knitr::combine_words(x)

    ggplot2::ggplot()  +
      ggplot2::geom_line(
        data = bases()$merged_rk_data,
        ggplot2::aes(
          x = Date,
          y = Monthly_Notices,
          linetype = "2020"
        )
      ) +
      ggplot2::geom_line(
        data = bases()$ref_level,
        ggplot2::aes(
          x = as.Date(DOY, origin="2020-01-01"),
          y = Prev_Max,
          linetype = "Previous years max"
        )
      ) +
      ggplot2::geom_line(
        data = bases()$ref_level,
        ggplot2::aes(
          x = as.Date(DOY, origin="2020-01-01"),
          y = Ref_Level,
          linetype = "Previous years mean"
        )
      ) +
      ggplot2::facet_wrap(facets = ggplot2::vars(Group)) +
      ggplot2::ggtitle(
        paste0("Notices Posted in 2020 - Eircode: ", x)
      ) +
      ggplot2::labs(x = "",y = "Monthly Notices") +
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(angle = 90),
        legend.position = c(0.89, 0.85)
      )  +
      ggplot2::scale_x_date(
        date_breaks = "1 month",
        date_labels = "%b",
        limits = c(as.Date("2020-01-01"),as.Date("2020-09-01"))
      ) +
      ggplot2::labs(linetype = "") +
      ggplot2::scale_linetype_manual(values=c("solid", "dotted", "dashed"))
  })



  output$plot2 <- renderPlot({

    slct <- input$VarShowInt


    if(slct == 'DUBLIN NORTH'|| slct == 'DUBLIN SOUTH'|| slct == 'INNER CITY DUBLIN' || slct == 'NORTH INNER DUBLIN'|| slct == 'SOUTH INNER DUBLIN'|| slct == 'WEST INNER DUBLIN' || slct == 'DUBLIN WEST'){

      p <- ggplot2::ggplot()+
        ggplot2::geom_sf(data = ire %>% dplyr::filter(name %in% c("Dún Laoghaire–Rathdown","South Dublin","Dublin","Fingal")),
                         ggplot2::aes()) +
        ggplot2::labs(title = "                     Dublin county")
    }
    else {

      p <- ggplot2::ggplot()+
        ggplot2::geom_sf(data = ire,ggplot2::aes()) 
    }

    p + 
      ggplot2::geom_sf(data = rk_grouped %>% dplyr::filter(Group == slct), ggplot2::aes(), fill = hamiltonCovid19::status_para_cor("primary"))+
      ggplot2::theme(axis.title.x=ggplot2::element_blank(),
                     axis.text.x=ggplot2::element_blank(),
                     axis.ticks.x=ggplot2::element_blank(),
                     axis.title.y=ggplot2::element_blank(),
                     axis.text.y=ggplot2::element_blank(),
                     axis.ticks.y=ggplot2::element_blank(),
                     panel.background = ggplot2::element_blank()) 
      
    

  })

}
    
## To be copied in the UI
# mod_original_ui("original_ui_1")
    
## To be copied in the server
# callModule(mod_original_server, "original_ui_1")
 
