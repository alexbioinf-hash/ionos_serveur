library(shiny)
library(ggplot2)
library(rmarkdown)
library(knitr)
library(shinyjs)
library(pdftools)
library(DT)
library(shinyWidgets)
library(gridExtra)
library(shinyFiles)
library(fs)

# Function to calculate the traditional FIC index
calculate_fic <- function(mic_a, mic_b, mic_comb) {
  fic_value <- (mic_comb / mic_a) + (mic_comb / mic_b)

  interpretation <- ifelse(fic_value <= 0.5, "Synergy",
                           ifelse(fic_value > 0.5 & fic_value <= 1, "Additive",
                                  ifelse(fic_value > 1 & fic_value <= 4, "Indifference",
                                         "Antagonism")))
  return(list(value = fic_value, interpretation = interpretation))
}

# Small helper to keep the "manage cookies" link consistent everywhere it appears
manage_cookies_link <- function(label = "Manage cookie preferences") {
  tags$a(
    href = "#",
    onclick = "tarteaucitron.userInterface.openPanel(); return false;",
    label
  )
}

# UI
ui <- navbarPage(
  title = "FIC Calculator",
  id = "main_nav",
  windowTitle = "FIC Calculator – Synergy Testing for Antibiotics",
  header = tagList(
    useShinyjs(),
    withMathJax(),
    tags$head(
      tags$meta(name = "description", content = "Free online FIC calculator to assess antibiotic synergy using MIC values. Understand interaction types: synergy, additive, indifference or antagonism."),
      tags$meta(name = "keywords", content = "FIC calculator, antibiotic synergy, MIC calculator, antimicrobial interaction, calculate FIC online, synergy testing tool, checkerboard assay, microbiology tool"),
      tags$meta(name = "robots", content = "index, follow"),
      tags$script(HTML("MathJax.Hub.Config({tex2jax: {inlineMath: [['$','$'], ['\\(','\\)']]}});")),

      # --- Consent management (tarteaucitron.js) ---
      # Loaded from jsDelivr (mirrors the official AmauriC/tarteaucitron.js GitHub release).
      # Blocks Google Analytics and Google AdSense until the visitor explicitly consents.
      # tarteaucitron picks its language pack from `tarteaucitronForceLanguage` (not from init()'s
      # "lang" option), so it must be set before the library script loads.
      tags$script(HTML("var tarteaucitronForceLanguage = 'en';")),
      tags$script(src = "https://cdn.jsdelivr.net/gh/AmauriC/tarteaucitron.js@1.34.0/tarteaucitron.min.js"),
      tags$script(HTML("
        tarteaucitron.init({
          privacyUrl: '',
          bodyPosition: 'bottom',
          hashtag: '#tarteaucitron',
          cookieName: 'tarteaucitron',
          orientation: 'bottom',
          groupServices: true,
          showDetailsOnClick: true,
          serviceDefaultState: 'wait',
          showAlertSmall: false,
          showTitleBanner: true,
          closePopup: false,
          showIcon: true,
          iconPosition: 'BottomRight',
          DenyAllCta: true,
          AcceptAllCta: true,
          highPrivacy: true,
          removeCredit: false,
          moreInfoLink: false,
          mandatory: true,
          googleConsentMode: true,
          lang: 'en'
        });

        tarteaucitron.user.gtagUa = 'G-R5JMLHE591';
        (tarteaucitron.job = tarteaucitron.job || []).push('gtag');

        tarteaucitron.user.adsensecapub = 'ca-pub-1957659388099318';
        (tarteaucitron.job = tarteaucitron.job || []).push('adsenseauto');
      "))
    ),
    tags$div(
      style = "text-align: center; margin-top: 10px;",
      tags$img(src = "https://raw.githubusercontent.com/agodmer/Logos/3a2a79632da0a858d3e3b28099249680eea4e577/fic_logo.png",
               height = "100px")
    )
  ),

  tabPanel("Calculator",
    h1("FIC Calculator"),

    sidebarLayout(
      sidebarPanel(
        h3("Input Data"),
        textInput("agent_comb", "Antibiotic Combination", ""),
        numericInput("mic_a", "MIC (Alone) of A", value = NULL, min = 0.01, step = 0.01),
        numericInput("mic_b", "MIC (Alone) of B", value = NULL, min = 0.01, step = 0.01),
        numericInput("mic_comb", "MIC (Combination A + B)", value = NULL, min = 0.01, step = 0.01),
        actionButton("calculate", "Calculate FIC", class = "btn btn-primary btn-lg btn-block"),
        downloadButton("downloadReport", "Download Report", class = "btn btn-success btn-lg btn-block")
      ),

      mainPanel(
        h3(strong("Methods Explanation")),
        br(),
        strong(
          style = "color:red;",
          "Disclaimer: this application is for educational and scientific use only. It should not be used for medical diagnosis or therapeutic decisions."
        ),
        br(),
        br(),
        h4(strong("Please read:")),
        p("The Fractional Inhibitory Concentration (FIC) Index is a tool used to evaluate the interaction between two antibiotics."),
        p("A lower FIC value suggests stronger synergy, while a higher value indicates antagonism."),
        br(),
        br(),
        h4(strong("Formula:")),
        uiOutput("fic_formula"),
        br(),
        h4(strong("Interpretation:")),
        tags$ul(
          tags$li("FIC ≤ 0.5: strong synergy, meaning the antibiotics work significantly better together."),
          tags$li("0.5 < FIC ≤ 1: additive effect, meaning the combination is similar to the sum of their individual effects."),
          tags$li("1 < FIC ≤ 4: indifference, meaning the combination does not significantly alter efficacy."),
          tags$li("FIC > 4: antagonism, meaning the combination reduces the effectiveness of the antibiotics.")
        ),
        br(),
        DTOutput("traditional_table"),

        h4(strong("For more explanations in the literature:")),
        p("According to Doern (2014), Journal of Clinical Microbiology, 'When Does 2 Plus 2 Equal 5? A Review of Antimicrobial Synergy Testing', the FIC index is commonly used in synergy testing methods to determine whether antibiotic combinations act additively, synergistically, or antagonistically. The checkerboard method, which employs serial twofold dilutions, is one of the most widely used approaches for calculating FIC values and is frequently applied in research and clinical microbiology.", style = "font-size: 12px;"),

        h3(strong("Results")),
        htmlOutput("fic_result"),
        htmlOutput("fic_interpretation"),

        h4(strong("Interpretation Table")),
        tableOutput("interpretation_table")
      )
    ),

    tags$div(
      style = "margin-top: 40px; padding: 20px; background-color: #f8f9fa; border-radius: 10px;",
      h3("About this FIC Calculator Tool", style = "margin-bottom: 15px;"),
      p("This free online FIC calculator helps microbiologists, researchers, and students calculate the ",
        strong("Fractional Inhibitory Concentration (FIC) index"),
        " from ",
        strong("Minimum Inhibitory Concentrations (MIC)"),
        " values."),
      p("Based on the ",
        strong("checkerboard assay method"),
        ", this tool evaluates ",
        strong("antibiotic synergy"),
        ", ",
        strong("additivity"),
        ", ",
        strong("indifference"),
        ", or ",
        strong("antagonism"),
        " between two antimicrobial agents."),
      p("Designed for speed and clarity, this calculator is ideal for labs and individuals looking to avoid manual calculations or Excel templates.")
    ),

    tags$div(
      style = "margin-top: 20px; padding: 10px 20px; text-align: center; font-size: 12px; color: #777;",
      "Privacy Policy · Legal Notice — see the tabs above. ", manage_cookies_link()
    )
  ),

  tabPanel("Privacy Policy",
    tags$div(
      style = "max-width: 800px; margin: 20px auto; font-size: 14px; line-height: 1.6;",
      h2("Privacy Policy"),
      p(em("Last updated: 2026-07-28")),

      h4("Data controller"),
      p("This website is published by A. Sedlakii. For any question about your personal data or to exercise your rights, contact: ",
        tags$a(href = "mailto:alex.bioinf@gmail.com", "alex.bioinf@gmail.com"), "."),

      h4("Data you enter in the calculator"),
      p("The antibiotic name and MIC values you type into the calculator are processed only in your browser session to compute the FIC index and the downloadable report. They are not stored on any server or database, and are not linked to your identity."),

      h4("Cookies and trackers"),
      p("Cookies and similar trackers are only placed on your device after you give consent through the cookie banner (or the floating icon in the bottom-right corner, which lets you change your choice at any time)."),
      tags$table(
        style = "width:100%; border-collapse: collapse;",
        tags$thead(
          tags$tr(
            tags$th(style = "text-align:left; border-bottom:1px solid #ccc; padding:6px;", "Service"),
            tags$th(style = "text-align:left; border-bottom:1px solid #ccc; padding:6px;", "Purpose"),
            tags$th(style = "text-align:left; border-bottom:1px solid #ccc; padding:6px;", "Retention")
          )
        ),
        tags$tbody(
          tags$tr(
            tags$td(style = "padding:6px; border-bottom:1px solid #eee;", "tarteaucitron (consent cookie)"),
            tags$td(style = "padding:6px; border-bottom:1px solid #eee;", "Remembers your cookie choice"),
            tags$td(style = "padding:6px; border-bottom:1px solid #eee;", "6 months")
          ),
          tags$tr(
            tags$td(style = "padding:6px; border-bottom:1px solid #eee;", "Google Analytics (GA4)"),
            tags$td(style = "padding:6px; border-bottom:1px solid #eee;", "Audience measurement"),
            tags$td(style = "padding:6px; border-bottom:1px solid #eee;", "Up to 14 months (Google default)")
          ),
          tags$tr(
            tags$td(style = "padding:6px;", "Google AdSense"),
            tags$td(style = "padding:6px;", "Displaying advertising"),
            tags$td(style = "padding:6px;", "Up to 13 months (Google default)")
          )
        )
      ),
      br(),
      p("Legal basis: your consent (Article 6.1.a GDPR). You can withdraw it at any time via ", manage_cookies_link(), "."),
      p("Recipient: Google LLC. Data collected by Google Analytics and Google AdSense may be transferred outside the European Union (United States), under Google's standard contractual clauses. See ",
        tags$a(href = "https://policies.google.com/technologies/partner-sites", target = "_blank", "Google's policy on partner sites"), "."),

      h4("Your rights"),
      p("Under the GDPR, you have the right to access, rectify, erase, restrict or object to the processing of your data, and the right to data portability. You may exercise these rights by contacting ",
        tags$a(href = "mailto:alex.bioinf@gmail.com", "alex.bioinf@gmail.com"), ". You also have the right to lodge a complaint with a supervisory authority — in France, the ",
        tags$a(href = "https://www.cnil.fr", target = "_blank", "CNIL"), ".")
    )
  ),

  tabPanel("Legal Notice",
    tags$div(
      style = "max-width: 800px; margin: 20px auto; font-size: 14px; line-height: 1.6;",
      h2("Legal Notice"),

      h4("Website editor"),
      p("A. Sedlakii — contact: ", tags$a(href = "mailto:alex.bioinf@gmail.com", "alex.bioinf@gmail.com")),

      h4("Hosting"),
      p("This application is hosted by Posit Software, PBC on shinyapps.io — ",
        tags$a(href = "https://posit.co", target = "_blank", "https://posit.co"), "."),

      h4("Purpose"),
      p("This site provides a free educational and scientific tool for calculating the Fractional Inhibitory Concentration (FIC) index. It is not intended for medical diagnosis or therapeutic decision-making — see the disclaimer on the Calculator tab.")
    )
  )
)

# Server
server <- function(input, output) {
  observeEvent(input$calculate, {
    fic <- calculate_fic(input$mic_a, input$mic_b, input$mic_comb)

    output$fic_result <- renderText({
      sprintf("<div style='font-size:30px; font-weight:bold;'>Global FIC = %s</div>", round(fic$value, 3))
    })
    output$fic_interpretation <- renderText({
      sprintf("<div style='font-size:30px; font-weight:bold;'>Interpretation: %s</div>", fic$interpretation)
    })

    output$interpretation_table <- renderTable({
      data.frame(
        "FIC Value" = c("≤ 0.5", "0.5 - 1", "1 - 4", "> 4"),
        "Interpretation" = c("Synergy", "Additive", "Indifference", "Antagonism")
      )
    })
  })

  output$fic_formula <- renderUI({
    withMathJax(HTML("$$ FIC = \\left( \\frac{MIC_{comb A+B}}{MIC_A} \\right) + \\left( \\frac{MIC_{comb A+B}}{MIC_B} \\right) $$"))
  })

  output$downloadReport <- downloadHandler(
    filename = function() { "FIC_Report.txt" },
    content = function(file) {
      report_content <- paste(
        "FIC Calculation Report\n\n",
        "Antibiotic Combination: ", input$agent_comb, "\n",
        "MIC (Alone) A: ", input$mic_a, "\n",
        "MIC (Alone) B: ", input$mic_b, "\n",
        "MIC (Combination A + B): ", input$mic_comb, "\n\n",
        "Global FIC: ", round(calculate_fic(input$mic_a, input$mic_b, input$mic_comb)$value, 3), "\n",
        "Interpretation: ", calculate_fic(input$mic_a, input$mic_b, input$mic_comb)$interpretation, "\n"
      )
      writeLines(report_content, file)
    }
  )
}

shinyApp(ui = ui, server = server)
