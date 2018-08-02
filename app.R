##
## Histogramas de algunas distribuciones discretas.
## 
## TO DO: 
##  - arreglar la dependencia de parámetro de la hipergeométrica o la BN
##  - agregar la esperanza
##  - embellecer los graficos (fijar la escala, cambiar colores y agregar abajo el rango)
##

library(rmarkdown)

ui <- fluidPage(
    titlePanel('Algunas distribuciones discretas'),
    sidebarLayout(
        sidebarPanel(
            fluidRow(
                ## Menu de distribuciones
                selectInput("dist", label = h3("Distribución:"), 
                            choices = list("Bernoulli" = "bernoulli",
                                           "Binomial" = "binom",
                                           "Poisson" = "pois",
                                           "Geométrica" = "geom",
                                           "Hipergeométrica" = "hipergeom",
                                           "Binomial negativa" = "binomneg"
                                           ## "Uniforme" = "unif",
                                           ## "Normal" = "norm",
                                           ## "Exponencial" = "exp",
                                           ## "Log-normal" = "logn",
                                           ## "T-Student" = "student",
                                           ## "Cauchy" = "cauchy"
                                           ),
                            selected = 1),

                ## Input: cantidad de observaciones
                ## sliderInput("nobs",
                ##             "Cantidad de observaciones",
                ##             value = 500,
                ##             min = 1,
                ##             max = 2000),
              
                h3("Parámetros"),

                ## Bernoulli parameters
                conditionalPanel(
                    condition = "input.dist == 'bernoulli'",
                    sliderInput('p', 'p',
                                min = 0, max = 1,
                                value = 0.5, step = 0.01),
                    helpText('Probabilidad de éxito')
                ),
                
                ## Binomial parameters
                conditionalPanel(
                    condition = "input.dist == 'binom'",
                    sliderInput('nbinom', 'n',
                                min = 1, max = 100,
                                value = 1, step = 1),
                    sliderInput('pbinom', 'p',
                                min = 0, max = 1,
                                value = 0.5, step = 0.01)
                ),
                
                ## Poisson parameters
                conditionalPanel(
                    condition = "input.dist == 'pois'",
                    sliderInput('lambda', withMathJax('$$\\lambda$$'),
                                min = 0, max = 100,
                                value = 10, step = 1),
                    sliderInput('limpois', 'lim',
                                min = 0, max = 50,
                                value = 5, step = 1)
                ),

                ## Geometric parameters
                conditionalPanel(
                    condition = "input.dist == 'geom'",
                    sliderInput('pgeom', 'p',
                                min = 0, max = 1,
                                value = 0.5, step = 0.01),
                    sliderInput('limgeom', 'lim',
                                min = 0, max = 50,
                                value = 5, step = 1)
                ),
                    
                ## Negative binomial parameters
                conditionalPanel(
                    condition = "input.dist == 'binomneg'",
                    sliderInput('rBN', 'r',
                                min = 1, max = 100,
                                value = 1, step = 1),
                    sliderInput('pBN', 'p',
                                min = 0, max = 1,
                                value = 0.5, step = 0.01)
                ),

                ## Hypergeometric parameters ## ARREGLAR PARAMETROS
                conditionalPanel(
                    condition = "input.dist == 'hipergeom'",
                    sliderInput('mhyp', 'm',
                                min = 1, max = 500,
                                value = 100, step = 1),
                    sliderInput('nhyp', 'n',
                                min = 0, max = 100,
                                value = 10, step = 1),
                    sliderInput('khyp', 'k',
                                min = 0, max = 100,
                                value =  10, step = 1),
                    sliderInput('limBN', 'lim',
                                min = 0, max = 500,
                                value = 5, step = 1)
                )
            )
        ),
        
        mainPanel(
            tabsetPanel(
                tabPanel('Histograma',
                         plotOutput('histograma'),
                         h4('Función de densidad'),
                         uiOutput('funcion_de_probabilidad_puntual')),
                tabPanel('Acumulada')
                ##          withMathJax('$$P(X \\leq x) = F(x) = \\int_{-\\infty}^x \\frac{1}{\\sqrt{2\\pi\\sigma^2}} \\cdot \\exp\\left[-\\frac{1}{2}\\left (\\frac{s-\\mu}{\\sigma}\\right )^2\\right] \\,ds$$'))
                ## tabPanel('Propiedades', includeMarkdown('propiedades.md')
            )
            #app.R## tabPanel('Tabla', tableOutput('tabla')
        )
    )
)

server <- function(input, output, session) {
    output$funcion_de_probabilidad_puntual <- renderUI({
        if (input$dist == 'bernoulli')
            {withMathJax('$$f(x) = \\frac{1}{\\sqrt{2\\pi\\sigma^2}} \\cdot
\\exp\\left[-\\frac{1}{2}\\left (\\frac{x-\\mu}{\\sigma}\\right )^2\\right]$$')}
    })
    
    output$histograma <- renderPlot({
        if (input$dist == 'bernoulli') {barplot(dbinom(0:1,1,input$p))}
        if (input$dist == 'binom') {barplot(dbinom(0:input$nbinom, input$nbinom, input$pbinom))}
        if (input$dist == 'pois') {barplot(dpois(0:input$limpois, lambda = input$lambda))}
        if (input$dist == 'geom') {barplot(dgeom(0:input$limgeom, input$pgeom))}
        if (input$dist == 'binomneg') {barplot(dnbinom(0:input$limBN, input$r, input$pBN))}
        if (input$dist == 'hipergeom') {barplot(dhyper(0:input$mhyp, input$mhyp, input$nhyp, input$khyp))}
    })
    
    observeEvent(input$mu, {
        updateSliderInput(session = session, inputId = "x", value = input$mu)
    })
    
    output$densidad <- renderPlot({
	mu <- input$mu
        sd <- input$sd
        a  <- input$a
        xx <- input$x
        par(bty = 'l')
        curve(dnorm(x ,mu, sd), -a, a,
              xlab =  '', ylab = '', lwd = 3, col = 'blue')
        lines(c(mu, mu),c(0, dnorm(mu, mu, sd)), lty = 2, lwd = 3, col = 'orange')
        xxx <- seq(-a,xx,0.01)
        polygon(c(xxx,xx), c(dnorm(xxx ,mu, sd), pnorm(-a, mu, sd)),
                density = 15, angle = 75,
                col = 'orange')
    })

    output$acumulada <- renderPlot({
	mu <- input$mu
        sd <- input$sd
        xx <- input$x
        a  <- input$a
        par(bty = 'l')
        curve(pnorm(x ,mu, sd), -a, a,
              xlab='',ylab='', lwd = 3, col = 'blue')
        lines(c(xx, xx), c(0,pnorm(xx, mu, sd)), lty = 1, lwd = 3, col = 'black')
    })

    output$tabla <- renderTable({
        columnas <- seq(0, 0.09, 0.01)
        filas    <- seq(0, 4, 0.1)
        tabla    <- as.data.frame(matrix(nrow = length(filas), ncol = length(columnas)))
        tabla    <- t(outer(columnas,filas, function(x,y) pnorm(x+y)))
        colnames(tabla) <- as.character(columnas)
        rownames(tabla) <- as.character(filas)
        tabla}, striped = TRUE, hover = TRUE, bordered = TRUE, spacing = 'xs', align = 'r',
        rownames = TRUE, colnames = TRUE, digits = 5)
}

shinyApp(ui = ui, server = server)
