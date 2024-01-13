# AutoNato, generador aleatorio de ideas para dibujar tatuajes
# disponible en https://bastianoleah.shinyapps.io/autonatotattoo/

library(shiny)
library(fresh)
library(dplyr)
library(ggplot2)
library(glue)
library(shinycssloaders)

# color_fondo = "#000000"
# color_secundario = "#333333"
# color_detalle = "#444444"
# color_texto = "#BBBBBB"
# color_destacado = "#730517"

# colores ----
color_fondo = "#c386f1"
# color_fondo = "#bd7aff"
color_secundario = "#f89cfa"
color_secundario_alto = "#f772f9"
color_detalle = "#f8c2f9"
color_texto = "#8e5fb2"
color_destacado = "#89d1dc"

# ui ----
ui <- fluidPage(title = "AutoNatoTattoo", 
                lang = "es",
                
                use_googlefont("Pixelify Sans"), #cargar fuente o tipo de letra
                
                use_theme(create_theme(
                  theme = "default",
                  bs_vars_font(size_base = "22px", #aumentar globalmente tamaño de letra
                               family_sans_serif = "Pixelify Sans" #cargar fuente o tipo de letra
                  ), 
                  bs_vars_modal(content_bg = color_fondo, content_border_color = color_detalle, 
                                backdrop_bg = color_fondo, backdrop_opacity = "60%"),
                  bs_vars_button(
                    default_color = color_texto,
                    default_bg = color_secundario,
                    default_border = color_detalle,
                    border_radius_base = "10px"
                  )
                )),
                
                ## css ----
                
                tags$style(".recalculating { opacity: inherit !important; }"),
                
                tags$style(paste0(
                  "h1 {
                           font-size: 200%;
                           font-weight: bold;
                  color: ", color_destacado, ";
                           }"
                )),
                
                #cambiar color de fondo
                tags$style(
                  paste0(".container-fluid, body {
           background-color: ", color_fondo, ";
              }")
                ),
           
           #cambiar color de los enlaces
           tags$style(
             paste0("a, a:hover {
           color: ", color_destacado, ";
           text-decoration: underline;}"
             )
           ),
           
           #estilo de botones
           tags$style(
             paste0(".action-button, .action-button:focus, .btn-default {
           background-color: ", color_secundario, "; 
           letter-spacing: 1px;
           border-radius: 8px; border-width: 3px; 
           min-width: 100px;
           border-color: ", color_detalle, "; 
           color: ", color_texto, ";
    }
    
    .action-button:active, .action-button:hover, .action-button:active:hover {
    background-color: ", color_detalle, "; 
    border-color: ", color_secundario, "; 
    color: ", color_texto, ";
    }")
           ),
    
    #estilo de botonera de botones radiales
    tags$style(
      paste0(".btn.radiobtn.btn-default {
           color: ", color_texto, ";
           font-size: 68% !important;
           font-weight: normal;
           min-width: 100px;
           background-color: ", color_detalle, ";
           border-radius: 8px; border-color: ", color_detalle, "; border-width: 3px !important;
           }
           .btn.radiobtn.btn-default.active {
           color: ", color_texto, " !important;
           background-color: ", color_secundario, ";
           box-shadow: none;
           }"
      )
    ),
    
    #estilo de barra slider
    tags$style("
  /*fondo de barra, o sección inactiva*/
  .irs--shiny .irs-line {
  background: none;
  background-color: ", color_detalle, ";
  }
  
  /*sección izquierda de barra activa*/
  .irs--shiny .irs-bar {
  background-color: ", color_secundario, ";
  border: 3px solid ", color_detalle, ";
  }
   /*pelota de slider*/
  .irs--shiny .irs-handle {
  background-color: ", color_secundario, ";
  box-shadow: none;
  border: 3px solid ", color_detalle, ";
  height: 30px; width: 30px;
  }
  
  /*pelota de slider en hover o activa*/
  .irs--shiny .irs-handle:hover, .irs--shiny .irs-handle:active {
  background-color: ", color_detalle, ";
  }
  
  /*grosor de barra*/
  .irs--shiny .irs-line, .irs--shiny .irs-bar {
  top: 23px;
  height: 16px;
  border: 0;
  }
  
  /*espaciado entre barra y etiqueta*/
  .irs--shiny {
  margin-top: -18px;
  }
  
  /*etiqueta de barra*/
  .control-label {
  font-size: 75%;
  font-weight: normal;
  letter-spacing: 2px;
  margin-bottom: -30px;
  color: black; opacity: 0.35;
  }
  
  /*ocultar numeros de la barra*/
  .irs-min, .irs-max, .irs-single { 
             opacity: 0;
  }"),
  
  #animación del desafío semanal 
  tags$style(paste0("
  #desafio {
  animation: fadein 1s infinite;
}
@keyframes fadein {
  0% { background-color: ", color_secundario, "}
  50%   { background-color: ", color_secundario_alto, "}
  100%   { background-color: ", color_secundario, "}
}
  ")),

#estilo del popup
tags$style(paste0("
                  .modal-content {
                  border: 3px solid ", color_detalle, "!important;
                  box-shadow: 0 3px 18px rgba(0,0,0,0.2);
                  }
                  
.modal-header {
  padding: 15px;
  border-bottom: 3px solid ", color_detalle, ";
}

.modal-footer {
border-top: 3px solid ", color_detalle, ";
}
  ")),
  
  
  ## título ----
  div(style = "max-width: 640px; margin-left: auto; margin-right: auto;",
      fluidRow(
        column(12, style = glue("color: {color_destacado};"),
               # h1("Auto Nato"),
               h1(textOutput("titulo")), #título cambiante
               p("generador aleatorio de ideas para dibujar tatuajes e ilustraciones",
                 style = "line-height: 1.1em;")
        )
      ),
      
      ## cajita ----
      fluidRow(
        column(12, align = "center",
               # style = "background-image: url('animacion4.gif'); background-repeat: space; ", "fondo animado
               
               #estilo de la caja con textos generados
               div(style = glue("max-width: 480px; min-height: 200px;
                            margin: 18px; margin-top: 26px; margin-bottom: 26px; 
                            padding: 16px; background-color: {color_secundario}; 
                            border-radius: 8px; border: 3px solid; border-color: {color_detalle}; 
                            color: {color_texto};"),
                   htmlOutput("texto_generado") |> withSpinner(proxy.height = 180, color = color_texto)
               )
        )
      ),
      
      ## botones ----
      fluidRow(
        column(12, 
               align = "center", 
               style = "margin-bottom: 24px; margin-top: 12px;",
               div(
                 #botón principal
                 actionButton("generar", 
                              label = "generar!",
                              style = "width: 220px;"
                              # style = glue("width: 220px; background-color: {color_destacado};
                              # #             border-radius: 8px; border-color: {color_detalle}; border-width: 3px; color: {color_texto}")
                 )
                 
               ),
               
               div(style = "margin-top: 32px; width: 220px;",
                 actionButton("desafio", 
                                label = "desafío semanal",
                                style = "width: 220px;"
                                # style = glue("width: 220px; background-color: {color_destacado};
                                # border-radius: 8px; border-color: {color_detalle}; border-width: 3px; color: {color_texto};
                                # ")
               )),
               
               # botonera de opciones de ilustración
               div(style = "margin-top: 38px; width: 220px;",
                   shinyWidgets::radioGroupButtons("tipo", label = NULL, 
                                                   choices = c("tatuaje", "ilustración"), justified = TRUE, size = "xs"),
               ),
               
               #slider spicy
               div(style = "max-width: 220px; margin-top: 20px;",
                   #codigo para la barra de progreso controlada por dos botones
                   # actionButton("restar", "-", style = "margin: 4px; width: 44px; padding: 4px; display: inline-block;"),
                   #   div(style = "display: inline-block; height: 30px; width: 100px; margin: 10px;", 
                   #       shinyWidgets::progressBar(id = "barra", value = 20)
                   #       ),
                   #   actionButton("sumar", "+", style = "margin: 4px; width: 44px; padding: 4px; padding-left: 5px; display: inline-block;")
                   sliderInput("spicy", "spicyness", min = 1, max = 4, value = 2, step = 1, ticks = FALSE)
               ),
               
               #slider complejidad
               div(style = "max-width: 220px; margin-top: 20px;",
                   sliderInput("complejidad", "complejidad", min = 1, max = 3, value = 3, step = 1, ticks = FALSE)
               ),
               
               #botón ver más (gráfico y explicación), solo aparece si se generó texto
               conditionalPanel(
                 condition = "input.generar > 0",
                 div(style = "margin-top: 38px; width: 220px;",
                     # shinyWidgets::radioGroupButtons("magia", label = NULL, choices = "ver probabilidades", justified = TRUE, size = "xs")
                     actionButton("magia", label = "ver más info", style = "width: 220px; font-size: 70% !important; font-weight: normal; height: 30px; padding-top: 1px;")
                 )
               )
        )
      ),
      
      ## gráfico de posibilidades ----
      fluidRow(
        conditionalPanel(
          condition = "input.magia % 2 == 1",
          column(12, align = "center", 
                 style = "margin-top: 24px; margin-bottom: 24px; margin-left:auto;margin-right:auto;",
                 div(style = "max-width: 480px;",
                     
                     #gráfico de posibilidades
                     plotOutput("grafico_posibilidades", height = 420) |> withSpinner(proxy.height = 420, color = color_texto),
                     
                     div(style = "padding-top: 16px; padding-bottom: 16px;",
                     p("Cada columna es un conjunto de términos, y la oscuridad del color indica su probabilidad variable de salir elegido. 
                 Las columnas desvanecidas están desactivadas en base a las condiciones de las otras columnas. 
                 Los rectángulos destacados son los términos que fueron seleccionados.", 
                 style = glue("line-height: 1.1em; text-align: left; color: {color_texto}; margin-top: 6px; margin-bottom: -16px; font-size: 75%;")
                 )
                     ),
                 
                 #gráfico de distribución de resultados
                 conditionalPanel(
                   condition = "input.generar > 10",
                 plotOutput("grafico_resultados", height = 420) |> withSpinner(proxy.height = 180, color = color_texto)
                 )
                 )
          )
        )
      ),
      
      ## firma ----
      fluidRow(
        column(12, style = glue("color: {color_destacado}; margin-top: 16px;"),
               HTML("by <a href='http://instagram.com/natogatotattoo'>@natogatotattoo</a>")
        ),
        column(12, style = glue("color: {color_destacado}; margin-top: 6px; font-size: 75%; margin-bottom: 24px;"),
               HTML("programado en R, <a href='https://github.com/bastianolea/autotattoo'>código abierto en GitHub</a>")
        )
      ),
      
      # instrucciones ----
      fluidRow(
        column(12,
               p('Presiona "generar" para obtener una idea. 
                 ¡No es necesario que la sigas al pie de la letra!
                 Cambia "spicyness" para ideas más/menos extremas, y "complejidad" para controlar el nivel de detalle.',
                 style = paste("line-height: 1.1em; font-size: 75%; color:", color_texto, ";"))
        )
      )
  )
)

# server ----
server <- function(input, output, session) {
  
  #esto era código para dos botones que suman o restan un valor que se mostraba en una barra, pero era dificil de manejar en css
  # spicy_level <- reactiveVal() #crear valor reactivo (que es posible de cambiar dentro de la app y queda guardado)
  # spicy_level(20) #definir valor inicial de la variable reactiva
  # 
  # observeEvent(input$sumar, {
  #   spicy_level(spicy_level()+20) #modificar nivel de spicy
  #   if (spicy_level() > 100) spicy_level(100)
  #   message("spicy level: ", spicy_level())
  #   shinyWidgets::updateProgressBar(session = session, id = "barra", value = spicy_level())
  # })
  # 
  # observeEvent(input$restar, {
  #   spicy_level(spicy_level()-20) #modificar nivel de spicy
  #   if (spicy_level() < 0) spicy_level(0)
  #   message("spicy level: ", spicy_level())
  #   shinyWidgets::updateProgressBar(session = session, id = "barra", value = spicy_level())
  # })
  
  
  ## botones presionados ----
  botones <- reactiveValues(ultimo_presionado = character())
  
  observeEvent(input$generar, {
    if (input$generar > 0 ) {
      botones$ultimo_presionado = "generar"
    }
  })
  observeEvent(input$desafio, {
    if (input$desafio > 0 ) {
      botones$ultimo_presionado = "desafio"
    }
  })
  observeEvent(input$cerrar_popup, {
    # if (input$desafio > 0 ) {
      botones$ultimo_presionado = "generar"
    # }
  })
  
  
  # output$lastButtonCliked <- renderText({
  #   paste("Last button clicked: ", botones$ultimo_presionado)
  # })
  observeEvent(input$generar | input$desafio,
               message("botón presionado: ", botones$ultimo_presionado)
  )
  
  ## probabilidades ----
  #funciones que retornan true con cierta probabilidad, para aleatorizar cosas
  probabilidad_muy_baja <- function() sample(1:10, 1) == 1 #10%
  probabilidad_baja <- function() sample(1:5, 1) == 1 #20%
  probabilidad_media <- function() sample(1:2, 1) == 1 #50%
  probabilidad_alta <- function() sample(1:10, 1) > 3 #70%
  
  #multiplicadores para aumentar chances de algunos elementos
  prob_baja = 5
  prob_media = 8
  prob_alta = 16
  
  #retorna true si se eligió la opción tatuaje (default)
  es_tatuaje <- reactive(input$tipo == "tatuaje")
  
  #remueve el elemento si no se eligió tatuaje (lo deja missing)
  solo_tatuaje <- function(texto) {
    ifelse(es_tatuaje(), texto, NA)
  }
  
  #función sample que remueve missing de los vectores (permitiendo que se hagan filtros antes de samplear y que nunca retorne missing)
  elegir <- function(..., cantidad = 1) {
    conjunto <- c(...) |> na.exclude() |> as.character()
    sample(conjunto, size = cantidad)
  }
  
  #función que crea listas de objetos y les pone el nombre del objeto (tibble::lst hace lo mismo pero así nos ahorramos una dependencia)
  #https://stackoverflow.com/a/16951524/4467386
  namedList <- function(...) {
    L <- list(...)
    snm <- sapply(substitute(list(...)),deparse)[-1]
    if (is.null(nm <- names(L))) nm <- snm
    if (any(nonames <- nm=="")) nm[nonames] <- snm[nonames]
    setNames(L,nm)
  }
  
  compilar_posibilidades <- function(.conjunto, .elemento, .paso) {
    tibble::tibble("termino" = elegido()[[.conjunto]]) |> 
      dplyr::count(termino, name = "prob") |> 
      dplyr::mutate(elegido = ifelse(termino == elegido()[[.elemento]], "sí", "no"),
                    paso = .paso, conjunto = .conjunto)
  }
  
  ## spicyness ----
  # actúa como multiplicador de términos más extremos, donde el valor de 4 es por defecto. algunos términos lo usan dividido por 2
  
  # spicy normal: da 1, 4, 9, 16, 25
  spicy <- reactive({
    # valor = 3
    valor = input$spicy
    spicy = valor ^ 2 # c(1:5)^2
    return(spicy)
  })
  
  # spicy alto: casi el doble que normal: da 1, 6, 16, 30, 48
  spicy_plus <- reactive({
    # valor <- 1
    valor <- spicy()
    spicy_plus <- (valor-1)*2 # (c(1, 4, 9, 16, 25)-1)*2
    if (spicy_plus < 1) spicy_plus = 1
    if (spicy_plus > 20) spicy_plus = 20
    return(spicy_plus)
  })
  
  # la mitad del spicy normal: da 1, 3, 5, 9, 13 
  medium_spicy <- reactive({
    # valor <- 4
    valor <- spicy()
    medium_spicy <- ceiling(valor/2+0.1) # ceiling(c(1, 4, 9, 16, 25)/2+0.1)
    return(medium_spicy)
  })
  
  mild_spicy <- reactive(medium_spicy()) #alias
  
  # complejidad: de 1 a 3
  complejidad <- reactive(input$complejidad)
  
  
  
  # ELECCIÓN ----
  # objeto reactive que crea los conjuntos y ejecuta todas las probabilidades que eligen términos, y los retorna como una lista
  elegido <- reactive({
    # req(input$generar > 0)
    req(length(botones$ultimo_presionado) > 0)
    message("ejecutando elección...")
    
    
    # anular aleatoriedad si se genera para el desafío
    if (botones$ultimo_presionado == "desafio") {
      message("cambiando a semilla fija (semanal)")
      semana <- format(Sys.Date(), "%W") |> as.integer()
      set.seed(semana+0) #sumar 1 para ver los de las próximas semanas
    } else {
      message("cambiando a semilla aleatoria")
      set.seed(sample(1:1000, 1))
    }
    
    ## ideas ----
    #son los elementos principales del tatuaje, el sujeto del dibujo
    
    ideas_aves = c("queltehue", "loica", "tiuque", "gorrión", "gruya", "garza", "chincol", "pavo real" |> rep(medium_spicy()),
                   "lechuza" |> rep(medium_spicy()), "búho", "cuervo" |> rep(prob_baja), "tordo", "mirlo")
    
    ideas_animales = c(ideas_aves, 
                       "perrito", "gato" |> rep(medium_spicy()), "capibara" |> rep(medium_spicy()), "sapo", "hámster", "pato", "coipo", "mapache" |> rep(medium_spicy()), 
                       "pajarito", "tigre" |> rep(medium_spicy()),
                       "conejo" |> rep(prob_baja), "gatito" |> rep(prob_baja), "ratita" |> rep(prob_baja), "rana" |> rep(prob_baja), 
                       "animalito", "mascota que conozcas o hayas conocido", "animalito doméstico", "animal exótico" |> rep(medium_spicy()), "animal salvaje" |> rep(medium_spicy()))
    
    ideas_terror = c("demonio" |> rep(medium_spicy()), "súcubo" |> rep(medium_spicy()), "demonia" |> rep(prob_baja), "calabaza", "rata", "calavera", "cráneo" |> rep(prob_baja), 
                     "bruja", "zombie" |> rep(prob_baja), "ogro", "troll", "vampiresa" |> rep(medium_spicy()),
                     "esqueleto" |> rep(medium_spicy()), "chivo" |> rep(medium_spicy()), "cabra", "espectro", "cerbero", "cíclope", "minotauro", "arpía", "medusa", "monstruo", "araña", 
                     "insecto", "serpiente", "personaje de halloween", "cosa de halloween")
    
    ideas_objetos = c("jaula", "llave", "tijera", "cadena", "candado", "taza de té", "cafetera",
                      "flor de loto", "flor", "arreglo floral", "honguito", "hongo")
    
    # estas solo en estido oriental
    ideas_oriental = c("hannya", "tigre", "pez koi", "ave fénix", "dragón", "oni", "pez gato", "geisha", "maneki neko", 
                       #"ola Kanagawa", "monte Fuji", 
                       "daruma", "yokai", "kappa", "demonio japonés", "bestia mitológica japonesa")
    
    
    ## objetos usables
    # para animales y terror
    ideas_objetos_usables = c("casco", "armadura", "bufanda", "sombrero", "cadena", "chaleca" |> rep(medium_spicy()), "par de calcetines", "gorro de mago", 
                              "lentes ópticos", "túnica", "collar precioso", "cuernos de reno" |> rep(medium_spicy()), 
                              c("navaja", "hacha", "espada", "maza", "cuchilla", "manopla", "cachos de demonio",
                                "nunchaku", "corset", "lencería", "ropa interior", "arnés", "un solo ojo", "múltiples ojos") |> rep(spicy_plus())
    )
    
    ideas_otras = c("escarabajo" |> rep(prob_media), "polilla" |> rep(prob_media), "mariposa", "dragón",
                    "personaje de tu serie favorita")
    
    ### probabilidades de conjuntos ----
    # pool de ideas
    ideas = c("mujer" |> rep(prob_baja * spicy()), #se selecciona de su conjunto especial
              "personaje de videojuegos" |> rep(prob_media), #se selecciona de su conjunto especial
              "personaje de anime" |> rep(prob_baja * spicy()), #se selecciona de su conjunto especial
              "jojos" |> rep(prob_baja * spicy()), #se selecciona de su conjunto especial
              "pokemon" |> rep(prob_alta),
              ideas_animales, #con adjetivos extra
              ideas_terror |> rep(medium_spicy()), #con adjetivos extra
              ideas_oriental |> rep(medium_spicy()), #estilo oriental fijo
              ideas_objetos,
              ideas_otras)
    
    idea_base <- idea <- elegir(ideas) #elegir idea
    
    #texto de diagnóstico:
    message()
    message(glue("idea elegida: {toupper(idea)}"))
    message(glue("niveles de spicy: {spicy()}, plus: {spicy_plus()}, medium: {medium_spicy()}"))
    
    
    ### género ----
    # de todas las ideas, especificar acá cuales son de género femenino, para modificar el resto de opciones
    ideas_femeninas <- c("calabaza", "bruja", "flor", "mujer", "ratita", "arpía", "medusa", "cabra", "rana", "araña", "calavera", "cosa de halloween",
                         "loica", "jaula", "llave", "tijera", "navaja", "cuchilla", "manopla", "polilla", "mariposa", "serpiente", "demonia",
                         "hacha", "espada", "túnica", "maza", "cafetera", "cadena", "vampiresa", "lechuza", "armadura", "chaleca", "hannya", "geisha", "bestia mitológica japonesa"
    )
    es_femenino <- idea %in% ideas_femeninas
    articulo <- ifelse(es_femenino, "una", "un") # género del artículo de la idea
    
    
    
    ## conjuntos ----
    #grupos de palabras o frases que constituyen las oraciones. las intro son textos que van antes de la palabra
    
    intros = c(rep("tienes que dibujar", prob_alta), rep("podrías dibujar", prob_media), 
               "hoy te toca dibujar", "qué tal si dibujas", 
               "hay que dibujar", "te parece si dibujas", "dibujemos", "en volá podrías dibujar")
    
    
    #### estilos ----
    intro_estilos <- c("en estilo", "siguiendo un estilo", "en base a un estilo")
    estilos = c(solo_tatuaje("tradi"),  "cute",  "aesthetic", "oriental", 
                solo_tatuaje("neo oriental"), "misterioso", "arcano",
                "pastel", "tierno", "lineal", "retro", 
                solo_tatuaje("neo tradi"), solo_tatuaje("blackwork"), solo_tatuaje("new school"), 
                c("darks", "gótico", "tenebroso", "coquette", "cottagecore", "oscuro", 
                  "espeluznante", "creepy") |> rep(spicy())
    )
    # estilos complejos
    if (complejidad() > 2) {
      estilos <- c(estilos, "art noveau", "surrealista", "cubista", "barroco", "geométrico", "abstracto", "8 bits", "realista")
    }
    
    
    #### paletas ----
    intro_paletas <- c("con una paleta de colores" |> rep(prob_media), "usando colores", "siguiendo una paleta de colores", 
                       "usando una paleta de colores", "usando en algún detalle colores", "destacando un elemento con colores")
    
    paletas = c("morados" |> rep(prob_baja), "moraditos", "verdes y musgo", "morado y negro" |> rep(spicy()),
                "verdes", "grises", "rosados" |> rep(spicy()), "rojos", "azules", "turquesas", "anaranjados", "marrones", "tierra",
                "rosado, verde y amarillo", "morado y amarillo", "naranjo y azulado", "rojo y negro" |> rep(spicy()), "morado, verde y azul", 
                "cálidos", "fríos",
                "naranjos o salmones",
                "opacos", "saturados", "pasteles")
    
    
    #### extras ----
    # son elementos que se agregan como adorno al tatuaje. solo se agregan si la complejidad es 2 o más
    intro_extras <- c("con" |> rep(prob_media), #ifelse(es_femenino, "rodeada de", "rodeado de"), 
                      "con detalles de", ifelse(es_femenino, "detallada con", "detallado con"),
                      "con un poco de", "con algo de",
                      ifelse(es_femenino, "adornada con", "adornado con"))
    
    extras <- c("estrellitas", "calaveritas", "bichitos", "brillitos", "nubecitas", "lucecitas", "vegetación",
                "mariposas", "humito", "constelaciones", "pastito", "florcitas", "fuego", 
                c("cadenas", "insectos", "cráneos", "sangre", "cicatrices", "huesos", "telarañas") |> rep(spicy())
    )
    
    
    #### tamaños ----
    intro_tamaños <- c("para un" |> rep(prob_media), "pensando en un", "idealmente para un")
    tamaños = c("tamaño chico" |> rep(prob_alta-medium_spicy()), "tamaño mediano" |> rep(prob_alta-medium_spicy()), "tamaño grande" |> rep(prob_media), 
                "de brazo" |> rep(prob_media), "de antebrazo" |> rep(prob_media), "de muslo" |> rep(prob_media), 
                "de manga" |> rep(spicy()), "de espalda" |> rep(spicy()))
    
    
    ### conjuntos spicy ----
    
    # solo se integran si el usuario aumenta el nivel de spicy
    if (spicy() > 4) {
      message("usando conjuntos spicy")
      intros <- c(intros, "por el amor de dios, dibuja", "ya ql, dibuja", "mira perrita por qué no dibujai", 
                  "ya mierda, dibujemos", "ya culiao,", "ya ctm dibuja", "dibuja" |> rep(prob_media)) |> 
        rep(medium_spicy())
      
      paletas <- c(paletas, "vísceras de animal muerto", "interiores de un animal putrefacto", "putrefactos", "vomitivos", 
                   "asquerosos", "extravagantes", "exóticos", "chillones", "bien maracos", "bien putitos", "coquetones") |> 
        rep(medium_spicy())
      
      estilos <- c(estilos, "erótico", "coqueto", "horror cósmico", "horny", 
                   "violento", "horror", "gore", "seductor", "del inframundo", "del infierno", "del caos") |> 
        rep(spicy())
      
      extras <- c(extras, "violencia", "muerte", "lencería", "sustancias viscosas no especificadas", "horrores incomprensibles", 
                  "destrucción", "cadáveres", "fetos (opcional)", "ojos", "huesos", "sangre", "cadenas", "púas", 
                  "cuernos", "dientes afilados") |> 
        rep(spicy())
    }
    
    
    #obtener un elemento de cada conjunto, al azar
    intro <- elegir(intros)
    intro_estilo <- elegir(intro_estilos)
    estilo <- elegir(estilos)
    intro_paleta <- elegir(intro_paletas)
    paleta <- elegir(paletas)
    intro_extra <- elegir(intro_extras)
    extra <- elegir(extras)
    intro_tamaño <- elegir(intro_tamaños)
    tamaño <- elegir(tamaños)
    objeto_usable <- elegir(ideas_objetos_usables)
    
    
    
    ### conjuntos especiales ----
    #se usan igual que los otros conjuntos, pero solo se usan si se dan las condiciones (por ejemplo que salga elegida cierta idea)
    
    
    #### mujeres ----
    mujer_tipos = c("ruda", "cute", "nerdy", "de la antiguedad", "histórica", "atemorizada", 
                    "tierna", "gorda", "suavecita", "triste", "poderosa", "imponente", "fuerte", 
                    "chiquita",  "pensativa", "curiosa", "rellenita", "haciendo muecas", "fumando", 
                    "arlequín", "sonriente", "enternecida", "adorable", "pensativa", "vieja", "joven", 
                    # spicy
                    c("diabólica", "deprimida", "mutante", "del futuro", "malvada", "enfurecida", 
                      "demoniaca", "enojada", "sexy", "guapa", "aterrada", "soviética", "rubenesca", "bolchevique") |> rep(spicy())
    )
    
    mujer_objetos = c("peces", "ramos de flores", "flores", "joyas", "armadura", "textiles", "cuernitos", "lágrimas", 
                      "disfraz de animal", "disfraz de payasita", "alas", "botas", "vestido", "chasquilla", "pelo corto", 
                      "pelo largo", "cinturones",  "mascarilla", "máscara", "lentes ópticos", "trenzas", "un peinado elaborado",
                      # spicy
                      c("cadenas", "armas", "insectos", "heridas", "lencería", "corset", "ropa interior",
                        "una pose exuberante", "outfit sexy", "tatuajes", "espadas", "lágrimas", "arnés") |> rep(spicy_plus())
    )
    
    # más spicy
    if (spicy() > 4) {
      message("con adjetivos spicy")
      mujer_objetos <- c(mujer_objetos, "la piel descubierta", "un seno descubierto", 
                         "cometiendo actos pecaminosos", "incurriendo en pecado", 
                         "acariciándose", "sangre por todos lados", "vísceras") |> rep(spicy())
      mujer_tipos <- c(mujer_tipos, "ensangrentada", "sufriendo", "contemplando horrores más allá de todo lo imaginable",
                       "amarrada", "desfigurada", "experimentando tormentos incomprensibles", "en trance", "exorcizada",
                       "completamente desnuda", "desnuda") |> rep(spicy())
    }
    
    
    #### anime ----
    # conjunto de términos que se usa si sale la idea "personaje de anime"
    animes <- c("Evangelion", "Mikami", "Sanrio" |> rep(prob_baja), "Sailor Moon" |> rep(prob_baja), 
                "Studio Ghibli" |> rep(prob_media), "Jojo's" |> rep(prob_media + spicy()), 
                "Cowboy Bebop", "Sakura Card Captor" |> rep(prob_baja), "Avatar", 
                "tu anime favorito", "algún anime antiguo/retro", "algún anime nuevo")
    
    
    #### jojos ----
    # conjunto de términos que se usa si sale "Jojo's" en "personaje de anime"
    jojos <- c("Phantom Blood", "Battle Tendency", "Stardust Crusaders", "Diamond Is Unbreakable", "Golden Wind", "Stone Ocean")
    
    
    #### juegos ----
    videojuegos <- c("Kirby", "Link (Zelda)", "Zelda", "Samus (Metroid)", "Pikmin", "Bayonetta", 
                     "personaje del juego Street Fighter", "personaje del juego Darkstalkers", 
                     "personaje de Super Mario", "personaje de Dark Souls/Elden Ring/Bloodborne") 
    
    #### pokemon ----
    # install.packages("pokemon")
    library(pokemon)
    
    lista_pokemon <- pokemon::pokemon
    
    pokemones <- lista_pokemon |> 
      filter(id <= 251) |> 
      select(id, nombre = pokemon, tipo = type_1) |> 
      #para que salgan más dependiendo del tipo
      mutate(peso = ifelse(tipo %in% c("poison", "psychic", "bug", "dark"), 5, 1),
             peso = ifelse(tipo %in% c("ghost", "fairy"), 10, peso)) |> 
      filter(peso > 1) |>  #solo de tipos que nos interesan
      mutate(tipo2 = recode(tipo, 
                            "poison" = "veneno", 
                            "psychic" = "psíquico", 
                            "bug" = "insecto", 
                            "dark" = "oscuro",
                            "ghost" = "fantasma", 
                            "fairy" = "hada"))
    
    
    # elecciones
    mujer_tipo_base <- mujer_tipo <- elegir(mujer_tipos)
    mujer_objeto_base <- mujer_objeto <- elegir(mujer_objetos)
    anime <- elegir(animes)
    jojo <- elegir(jojos)
    pokemon <- sample(pokemones$nombre, size = 1, prob = pokemones$peso)
    pokemon_tipo <- pokemones |> filter(nombre == pokemon) |> pull(tipo2)
    personaje_juego <- elegir(videojuegos)
    
    
    ### adjetivos ----
    #solo las ideas de los conjuntos especificados llevan adjetivos (palabras inmediatamente después de la palabra, por ejemplo, conejo tierno)
    animales_adjetivos <- c(ifelse(es_femenino, "hermosa", "hermoso"), ifelse(es_femenino, "tierna", "tierno") |> rep(prob_baja), "alegre", "descansando", 
                            "bebé" |> rep(prob_media), "espacial", "astronauta", "con ropa",
                            ifelse(es_femenino, "chiquita", "chiquito") |> rep(prob_media), ifelse(es_femenino, "pensativa", "pensativo"),
                            "durmiendo", ifelse(es_femenino, "pequeña", "pequeño") |> rep(prob_baja), "grande", "adorable" |> rep(prob_baja), 
                            ifelse(es_femenino, "curiosa", "curioso")
    )
    if (spicy() > 4) {
      message("con adjetivos spicy")
      # spicy
      animales_adjetivos <- c(animales_adjetivos,
                              c("horrible", ifelse(es_femenino, "abandonado", "abandonado"), ifelse(es_femenino, "enojada", "enojado"), ifelse(es_femenino, "rabiosa", "rabioso"), 
                                ifelse(es_femenino, "roñosa", "roñoso"), ifelse(es_femenino, "malvada", "malvado"), ifelse(es_femenino, "enfermita", "enfermo"), 
                                ifelse(es_femenino, "enfurecida", "enfurecido"), "zombie" |> rep(prob_baja), "mutante" |> rep(prob_baja), "esqueleto" |> rep(prob_baja), 
                                ifelse(es_femenino, "ensangrentada", "ensangrentado") |> rep(prob_baja), "siamés", "guerrillero",
                                ifelse(es_femenino, "herida", "herido") |> rep(spicy()), ifelse(es_femenino, "oscura", "oscuro"), ifelse(es_femenino, "satánica", "satánico")
                              ) |> rep(medium_spicy())
      )
    }
    
    terror_adjetivos <- c("mutante" |> rep(spicy()), ifelse(es_femenino, "mágica", "mágico"), ifelse(es_femenino, "asesina", "asesino"), "fantasmal", 
                          ifelse(es_femenino, "malévola", "malévolo"), "espectral", 
                          "alimentándose de un cadáver", "luchando", "amenazante", ifelse(es_femenino, "envuelta en oscuridad", "envuelto en oscuridad"), "en metamorfosis", 
                          ifelse(es_femenino, "embrujada", "embrujado"), "decadente", ifelse(es_femenino, "satánica", "satánico") |> rep(spicy()),
                          ifelse(es_femenino, "ensangrentada", "ensangrentado") |> rep(spicy()), ifelse(es_femenino, "poseída", "poseído") |> rep(spicy()), 
                          ifelse(es_femenino, "moribunda", "moribundo") |> rep(spicy()), ifelse(es_femenino, "sedienta de sangre", "sediento de sangre") |> rep(spicy()),
                          ifelse(es_femenino, "mutilada", "mutilado") |> rep(spicy()), "en descomposición" |> rep(spicy()),
                          ifelse(es_femenino, "herida", "herido"), ifelse(es_femenino, "corrompida", "corrompido"), ifelse(es_femenino, "hechizada", "hechizado"),
                          c("contemplando horrores más allá de todo lo imaginable",
                            "experimentando tormentos incomprensibles", "en trance", ifelse(es_femenino, "exorcizada", "exorcizado")) |> rep(medium_spicy())
    )
    
    animales_adjetivo <- elegir(animales_adjetivos)
    terror_adjetivo <- elegir(terror_adjetivos)
    
    
    ### enmarcado (x) ----
    # if (probabilidad_muy_baja()) {
    #   marcos <- c("un cuadro", "un pergamino", "un talismán", "acuarela", "follaje")
    #   marco <- elegir(marcos)
    #   idea <- glue("{idea} (enmarcado en {marco})")
    # }
    
    # browser()
    
    
    
    ### modificar ideas en base a conjuntos y probabilidad ----
    
    # adjetivos para animales
    if (idea_base %in% ideas_animales & probabilidad_alta() & complejidad() >= 2) {
      message("con adjetivo animal")
      idea <- paste(idea, animales_adjetivo)
    } 
    
    # adjetivos para ideas de terror
    if (idea_base %in% ideas_terror & probabilidad_alta() & complejidad() >= 2) {
      message("con adjetivo terror")
      idea <- paste(idea, terror_adjetivo)
    } 
    
    # usar un objeto, solo para animales o personajes de terror
    if (idea_base %in% c(ideas_animales, ideas_terror) & probabilidad_media() & complejidad() >= 3) {
      # browser()
      # objeto_usable <- elegir(ideas_objetos_usables)
      objeto_es_femenino <- objeto_usable %in% ideas_femeninas
      articulo_objeto <- ifelse(objeto_es_femenino, "una", "un")
      idea <- glue("{idea} con {articulo_objeto} {objeto_usable}")
    }
    
    
    # #segunda opción de extras
    # if (probabilidad_media()) {
    #   mujer_objetos_filtrados <- mujer_objetos[mujer_objetos != mujer_objeto]
    #   mujer_objeto <- paste(mujer_objeto, "y", elegir(mujer_objetos_filtrados))
    # }
    
    # objeto para mujeres (solo complejidad 2 y 3)
    if (complejidad() > 1) {
      mujer_objeto_complejidad <- glue(" con {mujer_objeto}")
    } else {
      mujer_objeto_complejidad <- ""
    }
    
    # segundo objeto para mujeres (solo complejidad 3)
    if (probabilidad_media() & complejidad() >= 3) {
      message("con segundo objeto")
      mujer_objetos_filtrados <- mujer_objetos[mujer_objetos != mujer_objeto]
      mujer_objeto_segundo <- elegir(mujer_objetos_filtrados)
      mujer_objeto_complejidad <- paste(mujer_objeto_complejidad, "y", mujer_objeto_segundo)
    }
    
    # dificultad lettering
    if (probabilidad_baja() & complejidad() >= 3) {
      message("con dificultad")
      idea <- paste(idea, "(con lettering o un texto)")
    }
    
    # dificultad tiempo
    if (probabilidad_baja() & complejidad() >= 3) {
      if (probabilidad_media()) {
        message("con dificultad")
        idea <- paste(idea, "(en menos de 4 minutos)")
      } else {
        if (probabilidad_media()) {
          message("con dificultad")
          idea <- paste(idea, "(en menos de 1 minuto!)")
        } else {
          message("con dificultad")
          idea <- paste(idea, "(en menos de 2 minutos)")
        }
      }
    }
    
    
    ## correcciones ----
    # corregir estilos sin colores
    paleta <- ifelse(estilo == "blackwork", "de negros", paleta)
    
    estilo <- ifelse(idea_base %in% ideas_oriental, "oriental", estilo)
    
    
    ## exportar ----
    # browser()
    # ls() |> dput()
    
    # todos los objetos generados, pero hay que agregarlos manualmente
    resultados <- namedList(intros, intro, intro_estilos, intro_estilo, intro_extras, intro_extra, 
                            intro_paleta, intro_paletas, intro_tamaño, intro_tamaños,
                            paletas, paleta, 
                            tamaños, tamaño,
                            estilo, estilos, extra, extras, 
                            ideas_aves, 
                            ideas_animales, animales_adjetivos, animales_adjetivo, 
                            ideas_terror, terror_adjetivos, terror_adjetivo, 
                            ideas_objetos, ideas_objetos_usables, objeto_usable,
                            ideas_femeninas, articulo, 
                            mujer_objeto_base, mujer_objetos, mujer_objeto, mujer_objeto_complejidad,
                            mujer_tipos, mujer_tipo_base, mujer_tipo,  
                            animes, anime, 
                            jojos, jojo, 
                            pokemones, pokemon, pokemon_tipo,
                            videojuegos, personaje_juego, 
                            ideas, idea, idea_base
    )
    
    return(resultados)
  }) |> 
    bindEvent(input$generar, input$spicy, input$complejidad, input$desafio, input$cerrar_popup)
  
  
  
  
  # REDACCIÓN ----
  # en este reactive se carga el resultado de las elecciones como un objeto, que es una lista que contiene todos los conjuntos y términos
  redaccion <- reactive({
    # req(input$generar > 0 | input$generar_desafio > 0)
    req(elegido())
    # browser()
    message("ejecutando redacción...")
    # Sys.sleep(0.3)
    
    # agregar o no el extra dependiendo de la complejidad
    if (complejidad() >= 2) {
      extra <- glue(", {elegido()$intro_extra} {elegido()$extra}")
    } else {
      extra <- ""
    }
    
    ## redactar en base a condiciones ----
    # mensaje estándar, pero si la idea coincide con algún criterio especial, se sobreescribe
    mensaje <- glue::glue("{elegido()$articulo} <b>{elegido()$idea}</b>{extra}")
    
    #redacción mujeres
    if (elegido()$idea_base == "mujer") {
      # mensaje <- glue("una <b>mujer {elegido()$mujer_tipo} con {elegido()$mujer_objeto}</b>{extra}")
      mensaje <- glue("una <b>mujer {elegido()$mujer_tipo}{elegido()$mujer_objeto_complejidad}</b>{extra}")
    }
    
    #redacción anime
    if (elegido()$idea_base == "personaje de anime") {
      mensaje <- glue::glue("un <b>personaje de anime de {elegido()$anime}</b>{extra}")
    }
    
    #redacción jojos bizarre adventure
    if (elegido()$idea_base == "jojos") {
      message("jojo reference!")
      mensaje <- glue::glue("un <b>personaje de Jojo's</b> de la temporada {elegido()$jojo}")
    }
    
    #redacción pokemon
    if (elegido()$idea_base == "pokemon") {
      mensaje <- glue::glue("al Pokémon tipo {elegido()$pokemon_tipo}, <b>{elegido()$pokemon}</b>")
    }
    
    #redacción videojuegos
    if (elegido()$idea_base == "personaje de videojuegos") {
      mensaje <- glue::glue("{elegido()$articulo} <b>{elegido()$personaje_juego}</b>")
    }
    
    
    ## redacción final ----
    mensaje_final <- glue("{elegido()$intro} {mensaje}, {elegido()$intro_estilo} <b>{elegido()$estilo}</b>, {elegido()$intro_paleta} {elegido()$paleta}")
    
    
    if (es_tatuaje() & complejidad() > 1) {
      mensaje_final <- glue("{mensaje_final}, {elegido()$intro_tamaño} tatuaje {elegido()$tamaño}")
    }
    
    return(mensaje_final)
    # })
  })
  
  
  #título que cambia a medida que aprietas el botón de generar
  output$titulo <- renderText({
    if (input$generar < 2) {
      "AutoNato"
    } else if (input$generar == 2) {
      "AutoNatoGato"
    } else if (input$generar == 3) {
      "AutoNatoGatoTattoo"
    } else if (input$generar >= 4) {
      "AutoNatCatTat"
    } else if (input$generar > 7) {
      "AutoNattoCattoTatto"
    } else if (input$generar > 11) {
      "AutoNattyCattyTatty"
    } else if (input$generar > 16) {
      "AutoNatitoGatitoTatito"
    } else if (input$generar > 22) {
      "Natalia Automática"
    }
  })
  
  
  # output del texto generado
  output$texto_generado <- renderText({
    # browser()
    if (input$generar == 0) { #si no se ha presionado el botón, mostrar este texto
      texto <- '<br><br>presiona "generar"'
      
    # } else if (botones$ultimo_presionado == "desafio") {
    #   texto <- '<br><br>presiona "generar"'
    }  else {
      texto <- redaccion() #output del reactive
    }
    return(texto)
  })# |> bindEvent(input$generar)
  
  
  
  
  
  # DESAFÍO ----
  observeEvent(input$desafio, {
    message("boton desafío")
    shiny::updateSliderInput(session, inputId = "spicy", value = 2)
    shiny::updateSliderInput(session, inputId = "complejidad", value = 3)
    shiny::showModal(
      popup_desafio())
  })
  
  ## popup ----
  popup_desafio <- reactive(
    modalDialog(title = h1("desafío semanal"),fade = F,
                               div(style = paste("color:", color_texto),
                                 p("sube tu diseño o ilustración del desafío semanal a redes sociales con el hashtag",
                                      HTML("<a href='https://www.instagram.com/explore/tags/desafíoAutoNatoTattoo'>#desafíoAutoNatoTattoo</a>")
                                 ),
                                 
                                  # div(style = "margin-left:auto; margin-right: auto;",
                                 fluidRow(
                                   column(12,  align = "center",
                                 div(style = glue("max-width: 480px; min-height: 200px;
                            margin: 18px; margin-top: 26px; margin-bottom: 26px; 
                            padding: 16px; background-color: {color_secundario}; 
                            border-radius: 8px; border: 3px solid; border-color: {color_detalle}; 
                            color: {color_texto};"),
                                 htmlOutput("texto_generado_desafio")
                                 )
                                  )
                                 )
                               ),
                               footer = actionButton("cerrar_popup", "volver") #modalButton("volver"),
  )
  )
  
  observeEvent(input$cerrar_popup,{
    # values$modal_closed <- T
    removeModal()
  })
  
  output$texto_generado_desafio <- renderText({
    message("output desafío")
    # if (input$desafio > 0) {
    # set.seed(as.numeric(Sys.Date()))
    # }
    texto <- isolate(redaccion())
    return(texto)
  })
  
  # ALMACENAR ----
  valores <- reactiveValues()
  
  # almacenar todas las elecciones en un objeto
  observeEvent(elegido(),
               {
                 valores[[glue("idea_{input$generar}")]] <- elegido()$idea_base
                 # browser()
                 # valores$"1"
                 # names(valores)
               })
  
  
  
  # GRAFICAR ----
  
  ## posibilidades ----
  graficar <- reactive({
    req(input$generar > 0)
    req(elegido())
    req(input$magia %% 2 == 1)
    message("generando gráfico...")
    
    # browser()
    
    ideas <- compilar_posibilidades("ideas", "idea_base", 1)
    paletas <- compilar_posibilidades("paletas", "paleta", 2)
    estilos <- compilar_posibilidades("estilos", "estilo", 3)
    animes <- compilar_posibilidades("animes", "anime", 4)
    mujer_tipos <- compilar_posibilidades("mujer_tipos", "mujer_tipo_base", 5)
    mujer_objetos <- compilar_posibilidades("mujer_objetos", "mujer_objeto_base", 6)
    animales_adjetivos <- compilar_posibilidades("animales_adjetivos", "animales_adjetivo", 7)
    terror_adjetivos <- compilar_posibilidades("terror_adjetivos", "terror_adjetivo", 8)
    extras <- compilar_posibilidades("extras", "extra", 9)
    jojos <- compilar_posibilidades("jojos", "jojo", 10)
    videojuegos <- compilar_posibilidades("videojuegos", "personaje_juego", 11)
    objetos_usables <- compilar_posibilidades("ideas_objetos_usables", "objeto_usable", 12)
    tamaños <- compilar_posibilidades("tamaños", "tamaño", 13)
    
    # terror_adjetivos
    
    # elegido()$animales_adjetivos
    # elegido()$animales_adjetivo
    # elegido()$mujer_objeto_base
    # elegido()$mujer_objeto
    # elegido()$mujer_objetos
    
    # elegido()$ideas_animales
    # elegido()$ideas_terror
    
    # unir todos los datos compilados en un solo dataframe
    elegidos <- bind_rows(ideas, paletas, estilos, extras,
                          mujer_tipos, mujer_objetos,
                          animales_adjetivos, terror_adjetivos,
                          animes, jojos, videojuegos,
                          objetos_usables, tamaños
    ) |>
      mutate(conjunto = forcats::fct_reorder(conjunto, paso)) |> 
      #variable dicotómica de si el conjunto es elegible para la idea base que salió
      mutate(activado = case_when(paso %in% c(5, 6) & elegido()$idea_base != "mujer" ~ FALSE,
                                  paso == 4 & elegido()$idea_base != "personaje de anime" ~ FALSE,
                                  paso == 7 & !(elegido()$idea_base %in% elegido()$ideas_animales) ~ FALSE,
                                  paso == 8 & !(elegido()$idea_base %in% elegido()$ideas_terror) ~ FALSE, 
                                  # paso == 10 & elegido()$idea_base != "personaje de anime" & elegido()$anime != "Jojo's" ~ FALSE,
                                  paso == 10 & elegido()$idea_base != "jojos" ~ FALSE,
                                  paso == 11 & elegido()$idea_base != "personaje de videojuegos" ~ FALSE,
                                  paso == 12 & !(elegido()$idea_base %in% c(elegido()$ideas_animales, elegido()$ideas_terror)) ~ FALSE, 
                                  .default = TRUE)) 
    
    # browser()
    
    elegidos |>
      ggplot(aes(x = conjunto, y = termino)) +
      #términos posibles
      geom_tile(aes(fill = prob, alpha = activado), color = color_fondo, 
                linewidth = 1) +
      #términos elegidos
      geom_tile(data = elegidos |> filter(activado), 
                aes(color = elegido), fill = NA, linewidth = 2, linejoin = "round") +
      #tema
      scale_color_discrete(type = c("transparent", color_destacado)) +
      scale_fill_gradient(low = color_secundario, high = color_texto) +
      scale_alpha_discrete(range = c(0.3, 1)) +
      facet_wrap(~paso, nrow = 1, scales = "free") +
      # theme(legend.position = "none", 
      #       axis.title = element_blank(),
      #       axis.text.y = element_blank())
      theme_void() +
      theme(legend.position = "none", 
            axis.title = element_blank(),
            axis.text.y = element_blank(), 
            strip.background = element_blank(),
            panel.spacing = unit(0.1, "cm"),
            strip.text = element_text(face = "bold", colour = color_texto),
            plot.background = element_rect(fill = color_fondo, linewidth = 0))
  })
  
  #output del gráfico
  output$grafico_posibilidades <- renderPlot({
    graficar()
  })
  
  ## resultados ----
  output$grafico_resultados <- renderPlot({
    req(input$generar > 0)
    req(elegido())
    req(input$magia %% 2 == 1)
    
    # valores_obtenidos <- sapply(valores, as.character) |> unname()
    valores_obtenidos <- reactiveValuesToList(valores) |> unlist()
    
    # browser()
    # dev.new()
    
    tibble("ideas" = valores_obtenidos) |> 
      count(ideas) |> 
      ggplot(aes(n, ideas)) +
      geom_col(width = 0.3, fill = color_secundario) +
      scale_x_continuous(expand = expansion(0)) +
      theme_minimal() +
      theme(axis.title = element_blank(),
            panel.grid = element_blank(), 
            axis.text.x = element_blank(),
            axis.text.y = element_text(color = color_texto)) +
      theme(plot.background = element_rect(fill = color_fondo, linewidth = 0))
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
