# AutoNato, generador aleatorio de ideas para dibujar tatuajes
# disponible en https://bastianoleah.shinyapps.io/autonatotattoo/

library(shiny)
library(fresh)
library(dplyr)
library(ggplot2)
library(glue)

# color_fondo = "#000000"
# color_secundario = "#333333"
# color_detalle = "#444444"
# color_texto = "#BBBBBB"
# color_destacado = "#730517"

# colores ----
color_fondo = "#c386f1"
# color_fondo = "#bd7aff"
color_secundario = "#f89cfa"
color_detalle = "#f8c2f9"
color_texto = "#8e5fb2"
color_destacado = "#89d1dc"

# ui ----
ui <- fluidPage(
  
  use_googlefont("Pixelify Sans"), #cargar fuente o tipo de letra
  
  use_theme(create_theme(
    theme = "default",
    bs_vars_font(size_base = "22px", #aumentar globalmente tamaño de letra
                 family_sans_serif = "Pixelify Sans" #cargar fuente o tipo de letra
    )
  )),
  
  ## css ----
  
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
    paste0(".action-button {
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
           font-size: 70% !important;
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
  #spicy-label {
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
  
  
  
  ## título ----
  div(style = "max-width: 640px; margin-left: auto; margin-right: auto;",
  fluidRow(
    column(12, style = glue("color: {color_destacado};"),
           # h1("Auto Nato"),
           h1(textOutput("titulo")), #título cambiante
           p("generador aleatorio de ideas para dibujar tatuajes e ilustraciones")
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
               htmlOutput("texto_generado")
           )
    )
  ),
  
  ## boton ----
  fluidRow(
    column(12, 
           align="center", 
           style = "margin-bottom: 24px; margin-top: 24px;",
           div(
             #botón principal
             actionButton("generar", 
                          label = "generar!",
                          style = "width: 200px;"
                          #style = glue("background-color: {color_secundario}; 
                          #             border-radius: 8px; border-color: {color_detalle}; border-width: 3px; color: {color_texto}")
             )
           ),
           
           #botonera de opciones
           div(style = "margin-top: 38px; width: 200px;",
               shinyWidgets::radioGroupButtons("tipo", label = NULL, choices = c("tatuaje", "ilustración"), justified = TRUE, size = "xs"),
           ),
           div(style = "max-width: 200px; margin-top: 20px;",
               #codigo para la barra de progreso controlada por dos botones
             # actionButton("restar", "-", style = "margin: 4px; width: 44px; padding: 4px; display: inline-block;"),
             #   div(style = "display: inline-block; height: 30px; width: 100px; margin: 10px;", 
             #       shinyWidgets::progressBar(id = "barra", value = 20)
             #       ),
             #   actionButton("sumar", "+", style = "margin: 4px; width: 44px; padding: 4px; padding-left: 5px; display: inline-block;")
             sliderInput("spicy", "spicyness", min = 1, max = 5, value = 2, step = 1, ticks = FALSE)
           ),
           div(style = "margin-top: 38px; width: 200px;",
               # shinyWidgets::radioGroupButtons("magia", label = NULL, choices = "ver probabilidades", justified = TRUE, size = "xs")
               actionButton("magia", label = "ver probabilidades", style = "width: 200px; font-size: 70% !important; font-weight: normal; height: 30px; padding-top: 1px;")
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
               plotOutput("grafico")
           )
    )
    )
  ),
  
  ## firma ----
  fluidRow(
    column(12, style = glue("color: {color_destacado}; margin-top: 24px;"),
           HTML("by <a href='http://instagram.com/natogatotattoo'>@natogatotattoo</a>")
    ),
    column(12, style = glue("color: {color_destacado}; margin-top: 6px; font-size: 75%; margin-bottom: 24px;"),
           HTML("programado en R, <a href='https://github.com/bastianolea/autotattoo'>código abierto en GitHub</a>")
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
  
  #funciones que retornan true con cierta probabilidad, para aleatorizar cosas
  probabilidad_muy_baja <- function() sample(1:10, 1) == 1 #10%
  probabilidad_baja <- function() sample(1:5, 1) == 1 #20%
  probabilidad_media <- function() sample(1:2, 1) == 1 #50%
  probabilidad_alta <- function() sample(1:10, 1) > 3 #70%
  
  #multiplicadores para aumentar chances de algunos elementos
  prob_baja = 2
  prob_media = 4
  prob_alta = 8
  
  #retorna true si se eligió la opción tatuaje (default)
  es_tatuaje <- reactive(input$tipo == "tatuaje")
  
  
  ### spicyness ----
  # actúa como multiplicador de términos más extremos, donde el valor de 2 es por defecto. algunos términos lo usan dividido por 2
  # spicy <- reactive(input$spicy)
  
  if (shiny::isRunning()) { #para probar la app fuera del entorno interactivo de shiny
  spicy <- reactive({
    valor <- as.integer(input$spicy)
    if (valor < 1) valor = 1
    return(valor)
    })
  } else {
    spicy <- function() 2
  }
  
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
  
  
  # ELECCIÓN ----
  # objeto reactive que crea los conjuntos y ejecuta todas las probabilidades que eligen términos, y los retorna como una lista
  elegido <- reactive({
    req(input$generar > 0)
    message("ejecutando elección...")
    
    ### ideas ----
    #son los elementos principales del tatuaje, el sujeto del dibujo
    ideas_aves = c("queltehue", "loica", "tiuque", "gorrión", "chincol", "lechuza", "búho", "cuervo" |> rep(prob_baja), "tordo", "mirlo")
    ideas_animales = c(ideas_aves, 
                       "perrito", "conejo", "gato", "gatito" |> rep(prob_baja), "capibara", "ratita" |> rep(prob_baja), 
                       "sapo", "rana" |> rep(prob_baja), "hámster", "pato", "coipo", "mapache", "pajarito", 
                       "animalito", "animalito doméstico", "animal exótico", "animal salvaje")
    ideas_terror = c("demonio", "súcubo", "demonia", "calabaza", "rata", "calavera", "cráneo" |> rep(prob_baja), "bruja", "zombie",
                     "esqueleto", "monstruo", "araña", "insecto", "serpiente", "personaje de halloween", "cosa de halloween")
    
    ideas_objetos = c("jaula", "llave", "tijera", "cadena", "candado", "taza de té", "cafetera")
                      
    ideas_objetos_usables = c("casco", "armadura", "bufanda", "sombrero", "cadena", "chaleca", "par de calcetines", "gorro de mago", "lentes ópticos",
                              "túnica",
                              c("navaja", "hacha", "espada", "maza", "cuchilla", "manopla", "nunchaku") |> rep(spicy())
                              )
    
    #pool de ideas
    ideas = c("mujer" |> rep(prob_media), #se selecciona de su conjunto especial
              "personaje de anime" |> rep(prob_media + spicy()), #se selecciona de su conjunto especial
              "personaje de videojuegos" |> rep(prob_media), #se selecciona de su conjunto especial
              "jojos" |> rep(prob_media + spicy()), #se selecciona de su conjunto especial
              ideas_animales, #con adjetivos extra
              ideas_terror |> rep(spicy()/2), #con adjetivos extra
              ideas_objetos,
              "escarabajo" |> rep(prob_media), "polilla" |> rep(prob_media), "mariposa", "dragón",
              "flor", "arreglo floral", "honguito", "personaje de tu serie favorita")
    
    idea <- elegir(ideas) #elegir idea
    idea_base <- idea
    message()
    message("idea elegida: ", idea, ", nivel de spicy: ", spicy())
    
    
    ### género ----
    # de todas las ideas, especificar acá cuales son de género femenino, para modificar el resto de opciones
    ideas_femeninas <- c("calabaza", "bruja", "flor", "mujer", "ratita", "rana", "araña", "calavera", "cosa de halloween",
                         "loica", "jaula", "llave", "tijera", "navaja", "cuchilla", "manopla", "polilla", "mariposa", "serpiente", "demonia",
                         "hacha", "espada", "túnica", "maza", "cafetera", "cadena", "lechuza", "armadura", "chaleca")
    es_femenino <- idea %in% ideas_femeninas
    articulo <- ifelse(es_femenino, "una", "un") # género del artículo de la idea
    
    
    ## conjuntos ----
    #grupos de palabras o frases que constituyen las oraciones. las intro son textos que van antes de la palabra
    
    intros = c(rep("tienes que dibujar", prob_alta), rep("podrías dibujar", prob_media), 
               "hoy te toca dibujar", "qué tal si dibujas", 
               "hay que dibujar", "te parece si dibujas", "dibujemos", "en volá podrías dibujar")
    
    intro_estilos <- c("en estilo", "siguiendo un estilo", "en base a un estilo")
    estilos = c( solo_tatuaje("tradi"),  "cute",  "aesthetic", "oriental", 
                solo_tatuaje("neo oriental"), "misterioso", "arcano",
                "pastel", "tierno", "lineal", "art noveau", "retro", "8 bits", "geométrico", "abstracto",
                solo_tatuaje("neo tradi"), solo_tatuaje("blackwork"), "anime", "realista", solo_tatuaje("new school"), 
                c("darks", "gótico", "terror", "coquette", "cottagecore", "oscuro", 
                  "espeluznante", "creepy") |> rep(spicy())
                )
    
    intro_paletas <- c("con una paleta de colores" |> rep(prob_media), "usando colores", "siguiendo una paleta de colores", 
                       "usando una paleta de colores", "usando en algún detalle colores", "destacando un elemento con colores")
    paletas = c("morados" |> rep(prob_baja), "moraditos", "verdes y musgo", "morado y negro" |> rep(spicy()),
                "verdes", "grises", "rosados" |> rep(spicy()), "rojos", "azules", "turquesas", "anaranjados", "marrones", "tierra",
                "rosado, verde y amarillo", "morado y amarillo", "naranjo y azulado", "rojo y negro" |> rep(spicy()), "morado, verde y azul", 
                "cálidos", "fríos",
                "naranjos o salmones",
                "opacos", "saturados", "pasteles")
    
    intro_extras <- c("con" |> rep(prob_media), ifelse(es_femenino, "rodeada de", "rodeado de"), 
                      "con detalles de", ifelse(es_femenino, "detallada con", "detallado con"),
                      ifelse(es_femenino, "adornada con", "adornado con"))
    extras <- c("estrellitas", "calaveritas", "bichitos", "brillitos", "nubecitas", "lucecitas", "vegetación",
                 "mariposas", "humito", "constelaciones", "pastito", "florcitas", "fuego", 
                c("cadenas", "insectos", "cráneos", "huesos", "telarañas") |> rep(spicy())
                )
    
    intro_tamaños <- c("para un" |> rep(prob_media), "pensando en un", "idealmente para un")
    tamaños = c("tamaño chico" |> rep(prob_alta), "tamaño mediano" |> rep(prob_alta), "tamaño grande" |> rep(prob_media), 
                "de brazo" |> rep(prob_media), "de antebrazo" |> rep(prob_media), "de muslo" |> rep(prob_media), 
                "de manga" |> rep(prob_baja), "de espalda" |> rep(prob_baja))
    
    ### conjuntos spicy ----
    if (spicy() > 2) {
      message("usando conjuntos spicy")
      intros <- c(intros, "por el amor de dios, dibuja", "ya ql, dibuja", "mira perrita por qué no dibujai", 
                  "ya mierda, dibujemos", "ya culiao,", "ya ctm dibuja", "dibuja" |> rep(prob_media)) |> 
        rep(spicy()/2)
      paletas <- c(paletas, "vísceras de animal muerto", "interiores de un animal putrefacto", "putrefactos", "vomitivos", 
                   "asquerosos", "extravagantes", "exóticos", "chillones", "bien maracos", "bien putitos", "coquetones") |> 
        rep(spicy()/2)
      estilos <- c(estilos, "erótico", "coqueto", "horny", "violento", "horror", "gore", "seductor", "del inframundo", "del infierno", "del caos") |> 
        rep(spicy()/2)
      extras <- c(extras, "violencia", "muerte", "lencería", "sustancias viscosas no especificadas", 
                  "destrucción", "cadáveres", "fetos (opcional)", "ojos", "huesos", "cadenas", "púas", 
                  "cuernos", "dientes afilados") |> 
        rep(spicy()/2)
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
    mujer_tipos = c("ruda", "cute", "nerdy", "de la antiguedad", "histórica", "atemorizada",
                    "tierna", "gorda", "suavecita", "triste", "poderosa", "imponente", "fuerte", 
                    "chiquita",  "pensativa", "curiosa", "rellenita", "haciendo muecas", "fumando",
                    "arlequín", "sonriente", "enternecida", "adorable", "pensativa", "vieja", "joven",
                    # spicy
                    c("diabólica", "deprimida", "mutante", "del futuro", "malvada", "enfurecida", 
                      "demoniaca", "enojada", "sexy", "guapa", "aterrada") |> rep(spicy())
                    )
    
    mujer_objetos = c("peces", "ramos de flores", "flores", "joyas", "armadura", "textiles", "cuernitos", "lágrimas", 
                      "disfraz de animal", "disfraz de payasita", "alas", "botas", "vestido", "chasquilla", "pelo corto", 
                      "pelo largo", "cinturones",  "mascarilla", "máscara", "lentes ópticos", "trenzas", "un peinado elaborado",
                      # spicy
                      c("cadenas", "armas", "insectos", "heridas", "lencería",
                        "una pose exuberante", "outfit sexy", "tatuajes", "espadas", "lágrimas", "arnés") |> rep(spicy())
                      )
    
    # más spicy
    if (spicy() > 2) {
      message("con adjetivos spicy")
    mujer_objetos <- c(mujer_objetos, "la piel descubierta", "un seno descubierto", "cometiendo actos pecaminosos", "incurriendo en pecado", 
                       "acariciándose", "sangre por todos lados", "vísceras") |> rep(spicy()/2)
    mujer_tipos <- c(mujer_tipos, "ensangrentada", "sufriendo", "amarrada", "desfigurada", "completamente desnuda", "desnuda") |> rep(spicy()/2)
    }
    
    # conjunto de términos que se usa si sale la idea "personaje de anime"
    animes <- c("Evangelion", "Mikami", "Sanrio" |> rep(prob_baja), "Sailor Moon" |> rep(prob_baja), 
                "Studio Ghibli" |> rep(prob_media), "Jojo's" |> rep(prob_media + spicy()), 
                "Cowboy Bebop", "Sakura Card Captor" |> rep(prob_baja), "Avatar", 
                "tu anime favorito", "algún anime antiguo/retro", "algún anime nuevo")
    
    # conjunto de términos que se usa si sale "Jojo's" en "personaje de anime"
    jojos <- c("Phantom Blood", "Battle Tendency", "Stardust Crusaders", "Diamond Is Unbreakable", "Golden Wind", "Stone Ocean")
    
    videojuegos <- c("Kirby", "Link (Zelda)", "Zelda", "Samus (Metroid)", "Pikmin", "Bayonetta", 
                     "personaje del juego Street Fighter", "personaje del juego Darkstalkers", 
                     "personaje de Super Mario", "personaje de Pokemon", "personaje de Dark Souls/Elden Ring/Bloodborne") 
    
    mujer_tipo_base <- mujer_tipo <- elegir(mujer_tipos)
    mujer_objeto_base <- mujer_objeto <- elegir(mujer_objetos)
    anime <- elegir(animes)
    jojo <- elegir(jojos)
    personaje_juego <- elegir(videojuegos)
    
    
    ### adjetivos ----
    #solo las ideas de los conjuntos especificados llevan adjetivos (palabras inmediatamente después de la palabra, por ejemplo, conejo tierno)
    animales_adjetivos <- c(ifelse(es_femenino, "hermosa", "hermoso"), ifelse(es_femenino, "tierna", "tierno") |> rep(prob_baja), "alegre", "descansando", 
                            "bebé" |> rep(prob_media), 
                            ifelse(es_femenino, "chiquita", "chiquito") |> rep(prob_media), ifelse(es_femenino, "pensativa", "pensativo"),
                            "durmiendo", ifelse(es_femenino, "pequeña", "pequeño") |> rep(prob_baja), "grande", "adorable" |> rep(prob_baja), ifelse(es_femenino, "curiosa", "curioso")
                            )
    if (spicy() > 2) {
      message("con adjetivos spicy")
      # spicy
      animales_adjetivos <- c(animales_adjetivos,
                              c("horrible", ifelse(es_femenino, "abandonado", "abandonado"), ifelse(es_femenino, "enojada", "enojado"), ifelse(es_femenino, "rabiosa", "rabioso"), 
        ifelse(es_femenino, "roñosa", "roñoso"), ifelse(es_femenino, "malvada", "malvado"), ifelse(es_femenino, "enfermita", "enfermo"), 
        ifelse(es_femenino, "enfurecida", "enfurecido"), "zombie" |> rep(prob_baja), "mutante" |> rep(prob_baja), "esqueleto" |> rep(prob_baja), 
        ifelse(es_femenino, "ensangrentada", "ensangrentado") |> rep(prob_baja), 
        ifelse(es_femenino, "herida", "herido") |> rep(spicy()), ifelse(es_femenino, "oscura", "oscuro") |> rep(spicy())
      ) |> rep(spicy()/2)
      )
    }
    
    
    terror_adjetivos <- c("mutante" |> rep(spicy()), ifelse(es_femenino, "mágica", "mágico"), ifelse(es_femenino, "asesina", "asesino"), "fantasmal", 
                          ifelse(es_femenino, "malévola", "malévolo"), "espectral", 
                          "alimentándose de un cadáver", "luchando", "amenazante", "envuelto en oscuridad", "en metamorfosis", 
                          ifelse(es_femenino, "embrujada", "embrujado"), "decadente",
                          ifelse(es_femenino, "ensangrentada", "ensangrentado") |> rep(spicy()), ifelse(es_femenino, "poseída", "poseído") |> rep(spicy()), 
                          ifelse(es_femenino, "moribunda", "moribundo") |> rep(spicy()), ifelse(es_femenino, "sedienta de sangre", "sediento de sangre") |> rep(spicy()),
                          ifelse(es_femenino, "mutilada", "mutilado") |> rep(spicy()), "en descomposición" |> rep(spicy()),
                          ifelse(es_femenino, "herida", "herido"), ifelse(es_femenino, "corrompida", "corrompido"), ifelse(es_femenino, "hechizada", "hechizado"))
    
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
    if (idea_base %in% ideas_animales & 
        probabilidad_alta()) {
      message("con adjetivo animal")
      idea <- paste(idea, animales_adjetivo)
    } 
    
    if (idea_base %in% ideas_terror & 
        probabilidad_alta()) {
      message("con adjetivo terror")
      idea <- paste(idea, terror_adjetivo)
    } 
    
    #usar un objeto, solo para animales o personajes de terror
    if (idea_base %in% c(ideas_animales, ideas_terror) & 
        probabilidad_media()) {
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
    
    #segundo objeto para mujeres
    if (probabilidad_media()) {
      message("con segundo objeto")
      mujer_objetos_filtrados <- mujer_objetos[mujer_objetos != mujer_objeto]
      mujer_objeto <- paste(mujer_objeto, "y", elegir(mujer_objetos_filtrados))
    }
    
    if (probabilidad_baja()) {
      message("con dificultad")
      idea <- paste(idea, "(con lettering o un texto)")
    }
    
    
    if (probabilidad_baja()) {
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
         mujer_objeto_base, mujer_objetos, mujer_objeto, 
         mujer_tipos, mujer_tipo_base, mujer_tipo,  
         anime, animes, 
         jojo, jojos,
         videojuegos, personaje_juego, 
         ideas, idea, idea_base
         )
    
    return(resultados)
  }) |> 
    bindEvent(input$generar, input$spicy) #calcular con el botón o con el slider
    
  
  
  # REDACCIÓN ----
  # en este reactive se carga el resultado de las elecciones como un objeto, que es una lista que contiene todos los conjuntos y términos
  redaccion <- reactive({
    req(input$generar > 0)
    req(elegido())
    # browser()
    message("ejecutando redacción...")

    ## redactar en base a condiciones ----
    # mensaje estándar, pero si la idea coincide con algún criterio especial, se sobreescribe
    mensaje <- glue::glue("{elegido()$articulo} <b>{elegido()$idea}</b>, {elegido()$intro_extra} {elegido()$extra}")

    #redacción mujeres
    if (elegido()$idea_base == "mujer") {
      mensaje <- glue("una <b>mujer {elegido()$mujer_tipo} con {elegido()$mujer_objeto}</b>, adornada con {elegido()$extra}")
    }

    #redacción anime
    if (elegido()$idea_base == "personaje de anime") {
        mensaje <- glue::glue("un <b>personaje de anime de {elegido()$anime}</b>, con {elegido()$extra}")
    }
    
    if (elegido()$idea_base == "jojos") {
      message("jojo reference!")
      mensaje <- glue::glue("un <b>personaje de Jojo's</b> de la temporada {elegido()$jojo}")
    }

    #redacción videojuegos
    if (elegido()$idea_base == "personaje de videojuegos") {
      mensaje <- glue::glue("{elegido()$articulo} <b>{elegido()$personaje_juego}</b>")
    }


    ## redacción final ----
    mensaje_final <- glue("{elegido()$intro} {mensaje}, {elegido()$intro_estilo} <b>{elegido()$estilo}</b>, {elegido()$intro_paleta} {elegido()$paleta}")

    if (es_tatuaje()) {
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
    } else {
      texto <- redaccion() #output del reactive
    }
    return(texto)
  })
  
  
  
  # GRAFICAR ----
  graficar <- reactive({
    req(input$generar > 0)
    req(elegido())
    req(input$magia %% 2 == 1)
    message("generando gráfico...")
    
    # browser()
    
    ideas <- compilar_posibilidades("ideas", "idea_base", 1)
    paletas <- compilar_posibilidades("paletas", "paleta", 2)
    estilos <- compilar_posibilidades("estilos", "estilo", 3)
    extras <- compilar_posibilidades("extras", "extra", 4)
    mujer_tipos <- compilar_posibilidades("mujer_tipos", "mujer_tipo_base", 5)
    mujer_objetos <- compilar_posibilidades("mujer_objetos", "mujer_objeto_base", 6)
    animales_adjetivos <- compilar_posibilidades("animales_adjetivos", "animales_adjetivo", 7)
    terror_adjetivos <- compilar_posibilidades("terror_adjetivos", "terror_adjetivo", 8)
    animes <- compilar_posibilidades("animes", "anime", 9)
    jojos <- compilar_posibilidades("jojos", "jojo", 10)
    videojuegos <- compilar_posibilidades("videojuegos", "personaje_juego", 11)
    objetos_usables <- compilar_posibilidades("ideas_objetos_usables", "objeto_usable", 12)
    
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
                          objetos_usables) |>
      mutate(conjunto = forcats::fct_reorder(conjunto, paso)) |> 
      mutate(activado = case_when(paso %in% c(5, 6) & elegido()$idea_base != "mujer" ~ FALSE,
                                  paso == 9 & elegido()$idea_base != "personaje de anime" ~ FALSE,
                                  paso == 7 & !(elegido()$idea_base %in% elegido()$ideas_animales) ~ FALSE,
                                  paso == 8 & !(elegido()$idea_base %in% elegido()$ideas_terror) ~ FALSE, 
                                  # paso == 10 & elegido()$idea_base != "personaje de anime" & elegido()$anime != "Jojo's" ~ FALSE,
                                  paso == 10 & elegido()$idea_base != "jojos" ~ FALSE,
                                  paso == 11 & elegido()$idea_base != "personaje de videojuegos" ~ FALSE,
                                  paso == 12 & !(elegido()$idea_base %in% c(elegido()$ideas_animales, elegido()$ideas_terror)) ~ FALSE, 
                                  .default = TRUE)) 
    
    # browser()
    # dev.new()
    
    elegidos |>
      ggplot(aes(x = conjunto, y = termino)) +
      #términos posibles
      geom_tile(aes(fill = prob, alpha = activado), color = color_fondo, linewidth = 2) +
      #términos elegidos
      geom_tile(aes(color = elegido), fill = NA, linewidth = 2, linejoin = "round") +
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
            strip.text = element_text(face = "bold", colour = color_texto),
            plot.background = element_rect(fill = color_fondo, linewidth = 0))
    
  })
  
  #output del gráfico
  output$grafico <- renderPlot({
    graficar()
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
