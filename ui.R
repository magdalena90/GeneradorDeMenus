
# Define UI for application that draws a histogram
ui <- fluidPage(
  
  titlePanel('Generador de Menús'),
  
  # Parametros de busqueda
  sidebarLayout(
    sidebarPanel(
      
      checkboxInput('personalizar', label = 'Añadir condiciones', value = FALSE),
      
      conditionalPanel(
        condition = 'input.personalizar == true',
        checkboxGroupInput('sano', label = h4('Menú saludable'), 
                           choices = list('Alto' = 1, 'Medio' = 2, 'Bajo' = 3),
                           selected = c(1,2,3)),
        radioButtons('grupo', label = h4('Agregar condiciones para:'),
                     choices = list('Proteínas' = 1, 'Carbohidratos' = 2, 'Frutas y Verduras' = 3), 
                     selected = 0)
      ),
      
      conditionalPanel(
        condition = 'input.personalizar == true && input.grupo > 0',
        uiOutput('categoria'),
        uiOutput('subcategoria')
      ),
      
      actionButton('hacer_menu', label = 'Hacer Menú')
    
  ),
    
    # Imprimir Menu 
    mainPanel(
      htmlOutput('menu')
    )
  )
)
