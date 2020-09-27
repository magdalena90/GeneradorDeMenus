
# Define server logic required to draw a histogram
server <- function(input, output, clientData, session) {
  
  # Funciones y variables auxiliares
  source('helper.R')
  
  # FILTROS DE BUSQUEDA DINAMICOS
  output$categoria = renderUI({
    if('grupo' %in% names(input)){
      selectInput('categoria',
                  label = h4(paste0('Tipo de ', lista_categorias[input$grupo %>% as.numeric])),
                  choices = crear_opciones_categoria(input$grupo),
                  selected = 1)
    }
  })
  
  output$subcategoria = renderUI({
    if('categoria' %in% names(input)){
      selectInput('subcategoria',
                  label = h4('Subcategoría'),
                  choices = crear_opciones_subcategoria(input$categoria),
                  selected = 1)
    }
  })
  
  # CREAR MENU
  crear_menu = eventReactive(input$hacer_menu, {
    
    # Seleccionar platillo base para crear el menu
    platillo_base = crear_platillo_base(input)
    
    # Crear el resto del menu
    menu = crear_resto_del_menu(platillo_base)
    
    # Si hubo algun problema al general el menu, la tabla menu va a tener cero filas y hay que crearlo otra vez
    print(class(menu))
    while(nrow(menu) == 0){
      platillo_base = crear_platillo_base(input)
      menu = crear_resto_del_menu(platillo_base)
    }
    
    return(menu)
    
  })
  
  # IMPRIMIR MENU
  output$menu = renderUI({
    
    menu_generado = crear_menu()
    
    # Primer tiempo
    menu_info = ''
    if(1 %in% menu_generado$Tiempo){
      primer_tiempo = menu_generado %>% filter(Tiempo == 1)
      menu_info = paste0('<br><h4>', primer_tiempo$Plato, '</h4>',
                         '<br><b>Receta:</b> ', primer_tiempo$Receta,
                         '<br><b>Recetario:</b> ', primer_tiempo$Recetario,
                         '<br><b>Folder de Drive:</b> <a  target="_blank" href="', 
                         urls_Drive[primer_tiempo$`Folder de Drive`],'">', 
                         primer_tiempo$`Folder de Drive`,'</a><br><br>')
    }
    
    # Plato Fuerte
    plato_fuerte = menu_generado %>% filter(Plato == 'Plato Fuerte')
    menu_info = paste0(menu_info,
                       '<br><h4>Plato Fuerte</h4>',
                       '<br><b>Receta:</b> ', plato_fuerte$Receta,
                       '<br><b>Recetario:</b> ', plato_fuerte$Recetario,
                       '<br><b>Folder de Drive:</b> <a  target="_blank" href="', 
                       urls_Drive[plato_fuerte$`Folder de Drive`],'">', 
                       plato_fuerte$`Folder de Drive`,'</a><br><br>')

    # Acompañamiento
    if('Acompañamiento' %in% menu_generado$Plato){
      acompanamiento = menu_generado %>% filter(Plato == 'Acompañamiento')
      menu_info = paste0(menu_info,
                         '<br><h4>Acompañamiento</h4>',
                         '<br><b>Receta:</b> ', acompanamiento$Receta,
                         '<br><b>Recetario:</b> ', acompanamiento$Recetario,
                         '<br><b>Folder de Drive:</b> <a  target="_blank" href="', 
                         urls_Drive[acompanamiento$`Folder de Drive`],'">', 
                         acompanamiento$`Folder de Drive`,'</a><br>')
    }
    
    HTML(menu_info)
  })

}
