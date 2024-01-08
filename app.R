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
           p("generador de ideas aleatorias para dibujar tatuajes")
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
                          style = "width: 200px"
                          #style = glue("background-color: {color_secundario}; 
                          #             border-radius: 8px; border-color: {color_detalle}; border-width: 3px; color: {color_texto}")
             )
           ),
           
           #botonera de opciones
           div(style = "margin-top: 38px; min-width: 200px;",
               shinyWidgets::radioGroupButtons("tipo", label = NULL, choices = c("tatuaje", "ilustración"), size = "xs"),
           ),
           div(style = "max-width: 200px; margin-top: 20px;",
               #codigo para la barra de progreso controlada por dos botones
             # actionButton("restar", "-", style = "margin: 4px; width: 44px; padding: 4px; display: inline-block;"),
             #   div(style = "display: inline-block; height: 30px; width: 100px; margin: 10px;", 
             #       shinyWidgets::progressBar(id = "barra", value = 20)
             #       ),
             #   actionButton("sumar", "+", style = "margin: 4px; width: 44px; padding: 4px; padding-left: 5px; display: inline-block;")
             sliderInput("spicy", "spicyness", min = 1, max = 10, value = 4, step = 2, ticks = FALSE)
           )
    )
  ),
  
  ## firma ----
  fluidRow(
    column(12, style = glue("color: {color_destacado}; margin-top: 24px;"),
           HTML("by <a href='http://instagram.com/natogatotattoo'>@natogatotattoo</a>")
    ),
    column(12, style = glue("color: {color_destacado}; margin-top: 6px; font-size: 75%;"),
           HTML("programado en R, <a href='http://instagram.com/natogatotattoo'>código abierto en GitHub</a>")
    )
  ),
  
  ## gráfico de posibilidades ----
  fluidRow(
    column(12, align = "center", 
           style = "margin-top: 24px; margin-bottom: 24px; margin-left:auto;margin-right:auto;",
           div(style = "max-width: 480px;",
             plotOutput("grafico")
           )
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
  prob_baja = 3
  prob_media = 6
  prob_alta = 9
  
  #retorna true si se eligió la opción tatuaje (default)
  es_tatuaje <- reactive(input$tipo == "tatuaje")
  
  # spicy <- reactive(input$spicy)
  
  if (shiny::isRunning()) {
  spicy <- reactive({
    valor <- as.integer(input$spicy/2)
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
    ideas_animales = c("perrito", "conejo", "gato", "capibara", "ratita", "sapo", "rana", "hámster", "pajarito", 
                       "animalito", "animal doméstico", "animal exótico", "animal salvaje")
    ideas_terror = c("demonio",  "calabaza", "rata", "calavera" |> rep(prob_baja), "cráneo", "bruja", "zombie",
                     "esqueleto", "monstruo", "araña", "insecto", "personaje de halloween", "cosa de halloween")
    
    ideas_aves = c("queltehue", "loica", "tiuque", "gorrión", "chincol")
    ideas_objetos = c("jaula", "llave", "tijera", c("navaja", "cuchilla", "manopla") |> rep(spicy()))
    
    #pool de ideas
    ideas = c("mujer" |> rep(prob_media + spicy()), 
              "personaje de anime" |> rep(prob_baja),
              "personaje de videojuegos",
              ideas_animales,
              ideas_terror |> rep(spicy()),
              ideas_aves,
              ideas_objetos,
              "escarabajo", "flor", "honguito", "personaje de tu serie favorita")
    
    idea <- elegir(ideas) #elegir idea
    idea_base <- idea
    message("idea elegida: ", idea, ", nivel de spicy: ", spicy())
    
    ### género ----
    # de todas las ideas, especificar acá cuales son de género femenino, para modificar el resto de opciones
    ideas_femeninas <- c("calabaza", "bruja", "flor", "mujer", "ratita", "rana", "araña", "calavera", "cosa de halloween",
                         "loica", "jaula", "llave", "tijera", "navaja", "cuchilla", "manopla")
    es_femenino <- idea %in% ideas_femeninas
    articulo <- ifelse(es_femenino, "una", "un") # género del artículo de la idea
    
    
    ## conjuntos ----
    #grupos de palabras o frases que constituyen las oraciones. las intro son textos que van antes de la palabra
    
    intros = c(rep("tienes que dibujar", prob_alta), rep("podrías dibujar", prob_media), 
               "hoy te toca dibujar", "qué tal si dibujas", 
               "hay que dibujar", "te parece si dibujas", "dibujemos", "en volá podrías dibujar")
    
    intro_estilos <- c("en estilo", "siguiendo un estilo", "en base a un estilo")
    estilos = c( solo_tatuaje("tradi"),  "cute",  "aesthetic", "oriental", 
                solo_tatuaje("neo oriental"), "misterioso",
                "pastel", "tierno", "lineal", "art noveau", "retro", "8 bits",
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
    extras <- c("estrellitas", "calaveritas", "bichitos",  "brillitos", "nubecitas", "lucecitas", "flores", 
                 "mariposas", "humito", "constelaciones", "pastito", "florcitas", "fuego", 
                c("cadenas", "insectos", "cráneos", "huesos","telarañas") |> rep(spicy())
                )
    
    intro_tamaños <- c("para un" |> rep(prob_media), "pensando en un", "idealmente para un")
    tamaños = c("tamaño chico" |> rep(prob_alta), "tamaño mediano" |> rep(prob_alta), "tamaño grande" |> rep(prob_media), 
                "de brazo" |> rep(prob_media), "de antebrazo" |> rep(prob_media), "de muslo" |> rep(prob_media), 
                "de manga" |> rep(prob_baja), "de espalda" |> rep(prob_baja))
    
    ### conjuntos spicy ----
    if (spicy() > 2) {
      intros <- c(intros, "por el amor de dios, dibuja", "ya ql, dibuja", "mira perrita por qué no dibujai", 
                  "ya mierda, dibujemos", "ya ctm dibuja", "dibuja" |> rep(prob_media)) |> 
        rep(spicy()/2)
      paletas <- c(paletas, "vísceras de animal muerto", "interiores de un animal putrefacto", "putrefactos", "vomitivos", 
                   "asquerosos", "extravagantes", "exóticos", "chillones", "bien maracos", "bien putitos", "coquetones") |> 
        rep(spicy()/2)
      estilos <- c(estilos, "erótico", "coqueto", "horny", "violento", "horror", "gore") |> 
        rep(spicy()/2)
      extras <- c(extras, "violencia", "muerte", "sustancias viscosas no especificadas", "destrucción", "cadáveres", "fetos (opcional)") |> 
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
    
    
    ### conjuntos especiales ----
    #se usan igual que los otros conjuntos, pero solo se usan si se dan las condiciones (por ejemplo que salga elegida cierta idea)
    mujer_tipos = c("ruda", "cute", "nerdy", "de la antiguedad", "histórica", "atemorizada",
                    "tierna", "gorda", "suavecita", "triste", "poderosa", "imponente", "fuerte", 
                    "chiquita",  "pensativa", "curiosa", "rellenita", "haciendo muecas", "fumando",
                    "arlequín", "sonriente", "enternecida", "adorable", "pensativa", "vieja", "joven",
                    # spicy
                    c("diabólica", "deprimida", "mutante",  "del futuro", "malvada", "enfurecida", 
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
    mujer_objetos <- c(mujer_objetos, "la piel descubierta", "un seno descubierto", "cometiendo actos pecaminosos", "incurriendo en pecado", 
                       "acariciándose", "sangre por todos lados", "vísceras") |> rep(spicy()/2)
    mujer_tipos <- c(mujer_tipos, "ensangrentada", "sufriendo", "amarrada", "desfigurada", "completamente desnuda", "desnuda") |> rep(spicy()/2)
    }
    
    
    
    animes <- c("Evangelion", "Mikami", "Sanrio", "Sailor Moon", "Studio Ghibli" |> rep(prob_baja), "Jojo's" |> rep(prob_media), 
                "Cowboy Bebop", "Sakura Card Captor", "Avatar", "tu anime favorito", 
                "algún anime antiguo", "algún anime nuevo")
    
    jojos <- c("Phantom Blood", "Battle Tendency", "Stardust Crusaders", "Diamond Is Unbreakable", "Golden Wind", "Stone Ocean")
    
    videojuegos <- c("Kirby", "Link (Zelda)", "Zelda", "Samus (Metroid)", "Pikmin", "Bayonetta", 
                     "personaje del juego Street Fighter", "personaje del juego Darkstalkers", 
                     "personaje de Super Mario", "personaje de Pokemon") 
    
    mujer_tipo_base <- mujer_tipo <- elegir(mujer_tipos)
    mujer_objeto_base <- mujer_objeto <- elegir(mujer_objetos)
    anime <- elegir(animes)
    jojo <- elegir(jojos)
    personaje_juego <- elegir(videojuegos)
    
    
    
    
    ### adjetivos ----
    #solo las ideas de los conjuntos especificados llevan adjetivos (palabras inmediatamente después de la palabra, por ejemplo, conejo tierno)
    animales_adjetivos <- c(ifelse(es_femenino, "hermosa", "hermoso"), ifelse(es_femenino, "tierna", "tierno"), "alegre", "descansando", "bebé" |> rep(3), 
                            ifelse(es_femenino, "chiquita", "chiquito"), ifelse(es_femenino, "pensativa", "pensativo"),
                            "durmiendo", ifelse(es_femenino, "pequeña", "pequeño"), "grande", "adorable", ifelse(es_femenino, "curiosa", "curioso"),
                            #spicy
                            c("horrible", ifelse(es_femenino, "abandonado", "abandonado"), ifelse(es_femenino, "enojada", "enojado"), ifelse(es_femenino, "rabiosa", "rabioso"), 
                              ifelse(es_femenino, "roñosa", "roñoso"), ifelse(es_femenino, "malvada", "malvado"), ifelse(es_femenino, "enfermita", "enfermo"), 
                              ifelse(es_femenino, "enfurecida", "enfurecido"), "zombie", "mutante", "esqueleto", ifelse(es_femenino, "ensangrentada", "ensangrentado"), 
                              ifelse(es_femenino, "herida", "herido")) |> rep(spicy())
                            )
    
    terror_adjetivos <- c("mutante", ifelse(es_femenino, "mágica", "mágico"), "fantasmal", ifelse(es_femenino, "malévola", "malévolo"), "espectral",
                          "alimentándose de un cadáver", "luchando", "amenazante", "envuelto en oscuridad", "en metamorfosis", ifelse(es_femenino, "embrujada", "embrujado"),
                          ifelse(es_femenino, "ensangrentada", "ensangrentado"), ifelse(es_femenino, "herida", "herido"), ifelse(es_femenino, "corrompida", "corrompido"))
    
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
      idea <- paste(idea, animales_adjetivo)
    }
    
    if (idea_base %in% ideas_terror & 
        probabilidad_alta()) {
      idea <- paste(idea, terror_adjetivo)
    }
    
    # #segunda opción de extras
    # if (probabilidad_media()) {
    #   mujer_objetos_filtrados <- mujer_objetos[mujer_objetos != mujer_objeto]
    #   mujer_objeto <- paste(mujer_objeto, "y", elegir(mujer_objetos_filtrados))
    # }
    
    #segundo objeto para mujeres
    if (probabilidad_media()) {
      mujer_objetos_filtrados <- mujer_objetos[mujer_objetos != mujer_objeto]
      mujer_objeto <- paste(mujer_objeto, "y", elegir(mujer_objetos_filtrados))
    }
    
    if (probabilidad_baja()) {
      idea <- paste(idea, "(con lettering o un texto)")
    }
    
    
    if (probabilidad_baja()) {
      if (probabilidad_media()) {
        idea <- paste(idea, "(en menos de 4 minutos)")
      } else {
        if (probabilidad_media()) {
          idea <- paste(idea, "(en menos de 1 minuto!)")
        } else {
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
    resultados <- namedList(animales_adjetivos, anime, animes, articulo, 
         estilo, estilos, extra, extras, idea, ideas, ideas_animales, 
         ideas_aves, ideas_femeninas, ideas_objetos, ideas_terror, 
         intro, intro_estilo, intro_estilos, intro_extra, intro_extras, 
         intro_paleta, intro_paletas, intro_tamaño, intro_tamaños, 
         intros, jojo, jojos, mujer_objeto_base, mujer_objeto, mujer_objetos, 
         mujer_tipo_base, mujer_tipo, mujer_tipos, paleta, paletas, personaje_juego, 
         tamaño, tamaños, terror_adjetivos, animales_adjetivo, terror_adjetivo, videojuegos, idea_base)
    
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
    if (elegido()$idea  == "mujer") {
      mensaje <- glue("una <b>mujer {elegido()$mujer_tipo} con {elegido()$mujer_objeto}</b>, adornada con {elegido()$extra}")
    }

    #redacción anime
    if (elegido()$idea == "personaje de anime") {
      if (elegido()$anime == "jojos") {
        mensaje <- glue::glue("un <b>personaje de Jojo's</b> de la temporada {elegido()$jojo}")
      } else {
        mensaje <- glue::glue("un <b>personaje de anime de {elegido()$anime}</b>, con {elegido()$extra}")
      }
    }

    #redacción videojuegos
    if (elegido()$idea == "personaje de videojuegos") {
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
    # terror_adjetivos
    
    # elegido()$animales_adjetivos
    # elegido()$animales_adjetivo
    # elegido()$mujer_objeto_base
    # elegido()$mujer_objeto
    # elegido()$mujer_objetos
    
    # unir todos los datos compilados en un solo dataframe
    elegidos <- bind_rows(ideas, paletas, 
                          mujer_tipos, mujer_objetos,
                          animales_adjetivos, terror_adjetivos) |>
      mutate(conjunto = forcats::fct_reorder(conjunto, paso))
    
    # browser()
    # dev.new()
    
    elegidos |>
      ggplot(aes(x = conjunto, y = termino)) +
      geom_tile(aes(fill = prob),
                color = color_fondo,
                linewidth = 2) +
      geom_tile(aes(color = elegido), fill = NA,
                linewidth = 2, linejoin = "round") +
      scale_color_discrete(type = c("transparent", color_destacado)) +
      scale_fill_gradient(low = color_secundario, high = color_texto) +
      facet_wrap(~paso, nrow = 1, scales = "free") +
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
