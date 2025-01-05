###################
# app.R
# 
# Front e Back do protótipo desenvolvido
###################

#1. libs
library(shiny)
library(bs4Dash)
library(tidyverse)
library(leaflet)
library(DT)


#2. carregar global.R
source('global.R')

# Colocar no relatório mês da análise de satélite
# COlocar valores de metro cúbico
# Colocar valores de solo exposto

#3. Ui da aplicação
ui <- bs4DashPage(
  title = "Team Swisi - IPAAM",
  header = bs4DashNavbar(title = "Team Swisi - IPAAM"),
  sidebar = bs4DashSidebar(
    skin = "dark",
    bs4SidebarMenu(
      bs4SidebarMenuItem("Metodologia", tabName = "methodology_tab", icon = icon("info-circle")),
      bs4SidebarMenuItem("Relatório Riscos", tabName = "relatorio_riscos", icon = icon("chart-bar"))    )
  ),
  body = bs4DashBody(
    bs4TabItems(
      
      # Relatório de Riscos
      bs4TabItem(
        tabName = "relatorio_riscos",
        fluidRow(width=12,
          column(width = 3,
          bs4Card(
            title = "Selecionar Número de Série Autex",
            width = 12,
            selectInput("serie_autex", "Número de Série Autex:", choices = NULL),
            actionButton("show_map", "Exibir Mapa")
          )
        ),
        column(width = 12,
          
               conditionalPanel(
            condition = "input.show_map > 0",
            fluidRow(
              bs4Card(
                title = "Agrupamento de Autorizações de Exploração por Espécie",
                width = 12,
                fluidRow(
                  column(12, 
                         HTML("
  <p><b>O mapa é interativo :)</b> Clique no agrupamento exibido e nos pontos para visualizar a <b>classificação de Risco</b>.</p>
  <p>Os pontos representam o <b>a comporação de volume transportado e volume autorizado</b> de diferentes espécies para o período de 1 ano, categorizados em <b>'Alto Risco'</b>, <b>'Médio Risco'</b>, <b>'Sem Risco'</b>, ou <b>'Fora do Escopo'</b>.</p>
")
                  ),
                  column(12, leafletOutput("map_filtered", height = 600))
                )
              )),
            fluidRow(
              uiOutput('descriptive_report')
            )
          )
        )
      )),
      # Metodologia
      bs4TabItem(
        tabName = "methodology_tab",
        fluidRow(
          bs4Card(
            title = "Metodologia de Análise",
            width = 12,
            solidHeader = TRUE,
            status = "info",
            HTML(paste0(metodologia_html))
          )
        )
      )
    )
  )
)
# Server
server <- function(input, output, session) {
  #### ---- Pagina 1 ---- ####
  
  output$map <- renderLeaflet({
    leaflet(data = sinaflor_dof_agregada_sf) %>%
      addTiles() %>%
      
      addProviderTiles(
        "Esri.WorldImagery",
        group = "Imagem de Satélite (ESRI)"
      ) %>% 
    addProviderTiles(
        "OpenStreetMap",
        group = "OpenStreetMap"
      ) %>%
      addProviderTiles(
        "CartoDB.Positron",
        group = "CartoDB.Positron"
      ) %>%
      addLayersControl(
        baseGroups = c(
          "Esri.WorldImagery", "OpenStreetMap", "CartoDB.Positron"
        ),
        position = "topright"
      ) %>% 
      addCircleMarkers(
        radius = 5,
        fillOpacity = 0.7,
        clusterOptions = markerClusterOptions(),
        color = ~case_when(
          categoria_risco == "alto risco" ~ "red",
          categoria_risco == "médio risco" ~ "orange",
          categoria_risco == "sem risco" ~ "green",
          TRUE ~ "blue"
        ),
        popup = ~ paste0(
          "<b>", "Ano: ", "</b>", ano.x, "<br>",
          "<b>", "Número de Série Autex: ", "</b>", numero_serie_autex, "<br>",
          "<b>", "Nome Científico: ", "</b>", nome_cientifico, "<br>",
          "<b>", "Volume Original Autorizado: ", "</b>", round(volume_original_autorizado_autex, 2), " m³<br>",
          "<b>", "Volume Remanescente Autex: ", "</b>", round(volume_remanescente_autex, 2), " m³<br>",
          "<b>", "Volume Transportado DOF: ", "</b>", round(volume_transportado_dof, 2), " m³<br>",
          "<b>", "Categoria de Risco: ", "</b>", categoria_risco, "<br>",
          "<b>", "Localização: ", "</b>", localizacao_autex
        ) %>% lapply(htmltools::HTML),
        label = ~ paste0(
          "<b>", "Categoria: ", "</b>", categoria_risco
        ) %>% lapply(htmltools::HTML)
      ) %>% 
      addPolygons(
        data = car_autex_sinaflor,
        fillOpacity = 0.1,
        color = 'black',
        weight = 2,
        highlight = highlightOptions(weight = 5, bringToFront = TRUE, color = 'red'),
        label = ~paste0("ID CAR: ", cod_imovel),
        labelOptions = labelOptions(
          style = list("font-size" = "19px", "font-weight" = "bold"),
          direction = 'auto'
        ),
        #popup = ~paste0(
        #  '<div style="font-size: 19px;">',
        #  "<b>Total de Pessoas: </b>", pessoas, "<br>",
        #  "<b>Total de Domicílios: </b>", domicls 
        #),
        popupOptions  = labelOptions(
          style = list("font-size" = "19px"),
          direction = 'auto'
        ),
        group = "Cadastro de Área Rural"
      ) %>% 
      addLayersControl(
        baseGroups = c("Imagem de Satélite","OpenStreetMap" ,"CartoDB.Positron"),
        overlayGroups = c('Cadastro de Área Rural'),
        options = layersControlOptions(collapsed = FALSE),
        position = "topright"
      )
  })
  
  
  #### ---- Pagina 2 ---- ####
  observe({
    updateSelectInput(session, "serie_autex", choices = unique(sinaflor_dof_agregada_sf$numero_serie_autex))
  })
  
  # Filtrar os dados com base no número de série selecionado
  filtered_data <- reactive({
    req(input$serie_autex)
    sinaflor_dof_agregada_sf %>% filter(numero_serie_autex == input$serie_autex)
  })
  
  # Exibir o mapa somente após o clique no botão
  observe({
    updateSelectInput(session, "serie_autex", choices = unique(sinaflor_dof_agregada_sf$numero_serie_autex))
  })
  
  # Filtrar os dados com base no número de série selecionado
  filtered_data <- eventReactive(input$show_map, {
    req(input$serie_autex)
    sinaflor_dof_agregada_sf %>% filter(numero_serie_autex == input$serie_autex)
  })
  
  # Exibir o mapa somente após o clique no botão
  output$map_filtered <- renderLeaflet({
    req(input$show_map)  # Requer o clique no botão
    req(filtered_data())
    
    leaflet(data = filtered_data()) %>%
      addTiles() %>%
      addProviderTiles(
        "Esri.WorldImagery",
        group = "Imagem de Satélite (ESRI)"
      ) %>%
      addProviderTiles(
        "OpenStreetMap",
        group = "OpenStreetMap"
      ) %>%
      addProviderTiles(
        "CartoDB.Positron",
        group = "CartoDB.Positron"
      ) %>%
      addLayersControl(
        baseGroups = c(
          "Imagem de Satélite (ESRI)", "OpenStreetMap", "CartoDB.Positron"
        ),
        position = "topright"
      ) %>%
      addCircleMarkers(
        radius = 5,
        fillOpacity = 0.7,
        clusterOptions = markerClusterOptions(),
        color = ~case_when(
          categoria_risco == "alto risco" ~ "red",
          categoria_risco == "médio risco" ~ "orange",
          categoria_risco == "sem risco" ~ "green",
          TRUE ~ "blue"
        ),
        # Atualizando os popups
        popup = ~ paste0(
          "<div style='font-size: 16px; font-weight: bold;'>",
          "<b>Ano:</b> ", ano.x, "<br>",
          "<b>Número de Série Autex:</b> ", numero_serie_autex, "<br>",
          "<b>Nome Popular:</b> ", nome_popular, "<br>",
          "<b>Município de Origem:</b> ", municipio_origem, "<br>",
          "<b>Volume Original Autorizado:</b> ", round(volume_original_autorizado_autex, 2), " m³<br>",
          "<b>Volume Remanescente Autex:</b> ", round(volume_remanescente_autex, 2), " m³<br>",
          "<b>Volume Transportado DOF:</b> ", round(volume_transportado_dof, 2), " m³<br>",
          "<b>Categoria de Risco:</b> ", categoria_risco, "<br>",
          "<b>Localização:</b> ", localizacao_autex,
          "</div>"
        ) %>% lapply(htmltools::HTML),
        # Atualizando os labels
        label = ~ paste0(
          "<span style='font-size: 14px; font-weight: bold;'>",
          nome_popular, " - ", categoria_risco,
          "</span>"
        ) %>% lapply(htmltools::HTML),
        labelOptions = labelOptions(
          style = list("font-weight" = "bold", "font-size" = "14px"),
          direction = "auto"
        )
      ) %>%
      addLegend(
        position = "bottomright",
        colors = c("red", "orange", "green", "blue"),
        labels = c("Alto Risco", "Médio Risco", "Sem Risco", "Outros"),
        opacity = 0.7,
        title = "Classificação de Risco"
      )
  })
  
  
  
  output$descriptive_report <- renderUI({
    req(filtered_data())  
    
    # Dados filtrados
    data <- filtered_data()
    
    # Obter o ano
    ano <- unique(data$ano.x)
    
    # Resumo das categorias de risco
    categoria_summary <- data %>%
      count(categoria_risco) %>%
      arrange(desc(n)) %>%
      mutate(summary = paste0("<b>", categoria_risco, "</b>: ", n)) %>%
      pull(summary) %>%
      paste(collapse = "<br>")
    
    # Lista de nomes populares e classificação de risco por espécie
    especies_risco <- data %>%
      group_by(nome_popular, categoria_risco) %>%
      summarise(
        volume_total_autorizado = sum(volume_original_autorizado_autex, na.rm = TRUE),
        volume_total_transportado = sum(volume_transportado_dof, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      arrange(desc(volume_total_transportado)) %>%
      mutate(
        info = paste0(
          "<b>", nome_popular, "</b>: ", categoria_risco,
          " (<b>Volume Autorizado:</b> <span style='color: red;'>", round(volume_total_autorizado, 2), " m³</span>, ",
          "<b>Volume Transportado:</b> <span style='color: red;'>", round(volume_total_transportado, 2), " m³</span>)"
        )
      ) %>%
      pull(info) %>%
      paste(collapse = "<br>")
    
    # Problemas identificados (alto risco)
    alto_risco <- data %>%
      filter(categoria_risco == "alto risco") %>%
      nrow()
    
    # Municípios de origem
    municipios <- data %>%
      distinct(municipio_origem) %>%
      pull(municipio_origem) %>%
      paste(collapse = ", ")
    
    # Texto final do relatório
    HTML(paste0(
      "<h4>Relatório Descritivo para Autorizações de Exploração Florestal: <b>", input$serie_autex, "</b></h4>",
      "<h5>Ano:</h5>",
      "<p><b>", ano, "</b></p>",
      "<h5>Município:</h5>",
      "<p><b>", municipios, "</b></p>",
      "<h5>Categorias de Risco:</h5>",
      "<p>", categoria_summary, "</p>",
      "<h5>Classificação de Risco por Espécie (Nome Popular):</h5>",
      "<p>", especies_risco, "</p>",
      "<h5>Potenciais Problemas:</h5>",
      "<ul>",
      "<li><b><span style='color: red;'>Casos de 'alto risco':</span></b> ", alto_risco, "</li>",
      "<li>Verifique transportes com volumes acima do autorizado.</li>",
      "</ul>"
    ))
  })
  
  
  

}

# Run app
shinyApp(ui, server)
