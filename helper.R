

# CAEGAR BASE DE DATOS DE DRIVE
gs4_deauth()
db = read_sheet('18FC-oPXJCFc0T2opsyqdjrRvwrLjfMlONmp99A2SwXc') %>% filter(!is.na(Receta))


# LISTAS IMPORTANTES
urls_Drive = list('Arroz'='https://drive.google.com/drive/folders/1N7dD2HQeEVy_ncmrkY1dGBwfdKunjgCi',
                  'Carnes'='https://drive.google.com/drive/folders/1eVfFgr9L5aG_F44zjs8Q2987TDI65jqj',
                  'Ensaladas'='https://drive.google.com/drive/folders/1nw9O4iHR--ohYycM-frGRnNRnuNGeyIj',
                  'Pastas'='https://drive.google.com/drive/folders/1_iF6KhV0FH8u_8b_e9wFS55Bb8FQFZZj',
                  'Pescado'='https://drive.google.com/drive/folders/1uCMRpte2QD7KGL00gyVgsqZI6mByqZCa',
                  'Platillos Mexicanos'='https://drive.google.com/drive/folders/1Sw7-XYcDjrXPnkbUp9gKiCR3n1nizIoo',
                  'Platos Fuertes'='https://drive.google.com/drive/folders/1ESaxkVkdICmQS6Gq_g9J-vF6TLCDfNTv',
                  'Pollo'='https://drive.google.com/drive/folders/1ZPeZY-MUM-UO_Ju1BudBbXrdv3vP78g2',
                  'Sopas'='https://drive.google.com/drive/folders/1WLw5ViVro-4q8AJ0WvVRasiXGg10cztK',
                  'Verduras'='https://drive.google.com/drive/folders/1RV7_2ADFgRYSdZImOWWa6bc7QeMhf6O2')

lista_categorias = c('Proteínas', 'Carbohidratos', 'Frutas y Verduras')


# FUNCIONES PARA CREAR LOS FILTROS
crear_opciones_categoria = function(grupo){
  
  if(grupo == '1') {
    categorias = db %>% filter(Proteína == 1) %>% pull(Categoría) %>% table %>% names %>% sort
  }
  
  if(grupo == '2') { 
    categorias = db %>% filter(Proteína == 0 & Carbohidratos == 1) %>% pull(Categoría) %>% table %>% 
      names %>% sort
  }
  
  if(grupo == '3') { 
    categorias = db %>% filter(Proteína == 0 & Carbohidratos == 0 & `Frutas y Verduras` == 1) %>%
      pull(Categoría) %>% table %>% names %>% sort
  }
  
  lista_categorias = as.list(1:length(categorias))
  names(lista_categorias) = categorias
  
  return(categorias)
}

crear_opciones_subcategoria = function(categoria){
  
  subcategorias = db %>% filter(Categoría == categoria) %>% pull(Subcategoría) %>% table %>% names %>% sort
  
  subcategorias = c('Cualquiera', subcategorias)
  
  lista_subcategorias = as.list(1:length(subcategorias))
  names(lista_subcategorias) = subcategorias
  
  return(subcategorias)
}


# FUNCIONES PARA CREAR EL MENU
crear_platillo_base = function(input){
  
  opciones = db
  
  # Sano
  if(input$personalizar) { opciones = opciones %>% filter(Sano %in% as.numeric(input$sano)) }
  
  # Filtros por categoria o subcategoria
  if('categoria' %in% names(input)){
    
    if(input$subcategoria == 'Cualquiera') {
      opciones = opciones %>% filter(Categoría == input$categoria)
    } else {
      opciones = opciones %>% filter(Subcategoría == input$subcategoria)
    }
    
  }
  
  # Seleccionar un platillo al azar de las opciones
  platillo_base = opciones %>% slice_sample(1, weight_by = Calificación)
  
  return(platillo_base)
}

crear_resto_del_menu = function(platillo_base){
  
  # Extraer informacion del platillo base
  # NOTA: Convierto los 0s en inf porque solo me interesa registrar si tienen valor 1
  plato = platillo_base$Plato
  tiempo = ifelse(platillo_base$Tiempo == 1, 1, Inf)
  proteina = ifelse(platillo_base$Proteína == 1, 1, Inf)
  carbohidratos = ifelse(platillo_base$Carbohidratos == 1, 1, Inf)
  frutas_verduras = ifelse(platillo_base$`Frutas y Verduras` == 1, 1, Inf)
  
  # Empezar a armar el menu
  menu = platillo_base
  
  # Romper el ciclo si no existen platillos que cumplan con las condiciones generadas por el menu actual
  interrumpir_loop = FALSE
  
  # Seleccionar nuevos platillos hasta que la comida incluya proteinas, carbohidratos y frutas/verduras
  while((proteina + carbohidratos + frutas_verduras != 3) && !interrumpir_loop){
    
    
    # Obtener los platillos que podriamos incluir en el menu
    opciones = db %>% filter(`Proteína` != proteina, Carbohidratos != carbohidratos,
                             `Frutas y Verduras` != frutas_verduras, !Plato %in% plato, Tiempo != tiempo)
    
    # Revisar si existen platillos que cumplan las condiciones
    if(nrow(opciones) > 0){
      
      # Seleccionar un nuevo platillo del menu
      platillo_nuevo = opciones %>% slice_sample(1, weight_by = Calificación)
      
      # Actualizar filtros
      # NOTA: plato enlista todos los platos hasta el momento, las otras variables solo se actualizan cuando 
      # el nuevo platillo tiene un valor de 1
      plato = c(plato, platillo_nuevo$Plato)
      tiempo = ifelse(platillo_nuevo$Tiempo == 1, 1, tiempo)
      proteina = ifelse(platillo_nuevo$Proteína == 1, 1, proteina)
      carbohidratos = ifelse(platillo_nuevo$Carbohidratos == 1, 1, carbohidratos)
      frutas_verduras = ifelse(platillo_nuevo$`Frutas y Verduras` == 1, 1, frutas_verduras)
      
      # Actualizar menu
      menu = menu %>% add_row(platillo_nuevo)
      
    } else { # Ningun platillo cumple las condiciones, hay que volver a empezar
      menu = data.frame()
      interrumpir_loop = TRUE
    }
    
  }
  
  # Asegurarnos de que la comida incluye plato fuerte, si no, volver a empezar
  if(!'Plato Fuerte' %in% menu$Plato) { menu = data.frame() }
  
  return(menu)
  
}
