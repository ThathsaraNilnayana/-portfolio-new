# ==============================================================================
# Neural Network Learning Lab — Interactive R Shiny Dashboard
# Course: Neural Networks / Data Science
# Architecture: 3 → 5 → 4 → 2 Feedforward Neural Network
# Mode: Multi-label classification (overheat_alert, fault_alert)
# Optimization: Stochastic Gradient Descent (SGD) with Binary Cross-Entropy
# ==============================================================================

if (!requireNamespace("shiny", quietly = TRUE)) {
  stop("The 'shiny' package is required. Please install it in RStudio using: install.packages('shiny')")
}
library(shiny)

# ------------------------------------------------------------------------------
# 1. DATA PREPARATION & LEAKAGE-FREE STANDARDIZATION (Panel A)
# ------------------------------------------------------------------------------
FEATURE_COLS <- c("temperature_c", "vibration_mm_s", "pressure_kpa")
TARGET_COLS  <- c("overheat_alert", "fault_alert")

candidates <- c(
  "sensor_dataset.csv",
  file.path(getwd(), "sensor_dataset.csv"),
  "E:/3rd year 2nd semester/Project/sensor_dataset.csv",
  "C:/Users/User/.gemini/antigravity/scratch/NeuralNetLab/sensor_dataset.csv"
)

dataset_path <- ""
for (cand in candidates) {
  if (file.exists(cand)) {
    dataset_path <- cand
    break
  }
}

if (dataset_path == "") {
  stop("Could not locate 'sensor_dataset.csv'. Please ensure it is in the project folder.")
}

df_raw <- read.csv(dataset_path)

# Fixed seed 42 split: 70% train (126), 15% validation (27), 15% test (27)
set.seed(42)
n_total <- nrow(df_raw)
shuffled_idx <- sample(n_total)

train_idx <- shuffled_idx[1:126]
val_idx   <- shuffled_idx[127:153]
test_idx  <- shuffled_idx[154:180]

X_tr_raw <- as.matrix(df_raw[train_idx, FEATURE_COLS])
y_tr     <- as.matrix(df_raw[train_idx, TARGET_COLS])
X_va_raw <- as.matrix(df_raw[val_idx,   FEATURE_COLS])
y_va     <- as.matrix(df_raw[val_idx,   TARGET_COLS])
X_te_raw <- as.matrix(df_raw[test_idx,  FEATURE_COLS])
y_te     <- as.matrix(df_raw[test_idx,  TARGET_COLS])

# Standardization parameters strictly calculated from training data
train_mu  <- colMeans(X_tr_raw)
train_sig <- apply(X_tr_raw, 2, sd) + 1e-8

scale_data <- function(mat, mu, sig) {
  sweep(sweep(mat, 2, mu, "-"), 2, sig, "/")
}

X_tr <- scale_data(X_tr_raw, train_mu, train_sig)
X_va <- scale_data(X_va_raw, train_mu, train_sig)
X_te <- scale_data(X_te_raw, train_mu, train_sig)

# ------------------------------------------------------------------------------
# 2. NEURAL NETWORK MATH: FORWARD & BACKWARD PROPAGATION (Panel B, C, D)
# ------------------------------------------------------------------------------
sigmoid <- function(x) 1 / (1 + exp(-pmax(pmin(x, 15), -15)))
relu    <- function(x) x * (x > 0)
relu_d  <- function(x) 1 * (x > 0)
sigm_d  <- function(a) a * (1 - a)

# Initialize network parameters with Xavier / He normal distributions
init_network <- function(seed = 42) {
  set.seed(seed)
  list(
    W1 = matrix(rnorm(3 * 5, 0, sqrt(2/3)), nrow = 3, ncol = 5),
    b1 = numeric(5),
    W2 = matrix(rnorm(5 * 4, 0, sqrt(2/5)), nrow = 5, ncol = 4),
    b2 = numeric(4),
    W3 = matrix(rnorm(4 * 2, 0, sqrt(2/4)), nrow = 4, ncol = 2),
    b3 = numeric(2)
  )
}

# Forward pass computing all intermediate layer values
forward_pass <- function(X, net, act_fn = "relu") {
  Z1 <- sweep(X %*% net$W1, 2, net$b1, "+")
  A1 <- if (act_fn == "relu") relu(Z1) else sigmoid(Z1)
  
  Z2 <- sweep(A1 %*% net$W2, 2, net$b2, "+")
  A2 <- if (act_fn == "relu") relu(Z2) else sigmoid(Z2)
  
  Z3 <- sweep(A2 %*% net$W3, 2, net$b3, "+")
  Yhat <- sigmoid(Z3) # Output layer always uses sigmoid for independent probabilities
  
  list(Z1 = Z1, A1 = A1, Z2 = Z2, A2 = A2, Z3 = Z3, Yhat = Yhat)
}

# Training loop using SGD and Binary Cross-Entropy
train_network <- function(X_train, y_train, X_val, y_val,
                          lr = 0.01, epochs = 100, act_fn = "relu", seed = 42) {
  net <- init_network(seed)
  m <- nrow(X_train)
  eps <- 1e-12
  
  train_losses <- numeric(epochs)
  val_losses   <- numeric(epochs)
  
  for (ep in 1:epochs) {
    # Forward pass on train
    fwd <- forward_pass(X_train, net, act_fn)
    loss_tr <- -mean(y_train * log(fwd$Yhat + eps) + (1 - y_train) * log(1 - fwd$Yhat + eps))
    train_losses[ep] <- loss_tr
    
    # Forward pass on validation
    fwd_val <- forward_pass(X_val, net, act_fn)
    loss_va <- -mean(y_val * log(fwd_val$Yhat + eps) + (1 - y_val) * log(1 - fwd_val$Yhat + eps))
    val_losses[ep] <- loss_va
    
    # Backward pass: Compute exact analytical gradients
    dZ3 <- (fwd$Yhat - y_train) / m
    dW3 <- t(fwd$A2) %*% dZ3
    db3 <- colSums(dZ3)
    
    dA2 <- dZ3 %*% t(net$W3)
    dZ2 <- dA2 * (if (act_fn == "relu") relu_d(fwd$Z2) else sigm_d(fwd$A2))
    dW2 <- t(fwd$A1) %*% dZ2
    db2 <- colSums(dZ2)
    
    dA1 <- dZ2 %*% t(net$W2)
    dZ1 <- dA1 * (if (act_fn == "relu") relu_d(fwd$Z1) else sigm_d(fwd$A1))
    dW1 <- t(X_train) %*% dZ1
    db1 <- colSums(dZ1)
    
    # Update parameters via Gradient Descent
    net$W3 <- net$W3 - lr * dW3
    net$b3 <- net$b3 - lr * db3
    net$W2 <- net$W2 - lr * dW2
    net$b2 <- net$b2 - lr * db2
    net$W1 <- net$W1 - lr * dW1
    net$b1 <- net$b1 - lr * db1
  }
  
  list(net = net, train_losses = train_losses, val_losses = val_losses)
}

# ------------------------------------------------------------------------------
# 3. USER INTERFACE (UI)
# ------------------------------------------------------------------------------
ui <- fluidPage(
  titlePanel(
    div(
      span("🧠 Neural Network Learning Lab", style = "color: #1f4e79; font-weight: bold; font-size: 26px;"),
      h5("How Does a Neural Network Learn? — 3-5-4-2 Factory Sensor Multilabel Classification",
         style = "color: #555; margin-top: 4px;")
    )
  ),
  hr(),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      h4("⚙️ Training Controls", style = "color: #1f4e79; font-weight: bold;"),
      p(em("Panel C: Hyperparameters")),
      
      selectInput("lr", "Learning Rate (LR):",
                  choices = c(0.001, 0.005, 0.01, 0.05, 0.1),
                  selected = 0.01),
      
      sliderInput("epochs", "Number of Epochs:",
                  min = 20, max = 500, value = 100, step = 10),
      
      radioButtons("activation", "Hidden Activation:",
                   choices = c("ReLU" = "relu", "Sigmoid" = "sigmoid"),
                   selected = "relu", inline = TRUE),
      
      sliderInput("threshold", "Decision Threshold:",
                  min = 0.1, max = 0.9, value = 0.5, step = 0.05),
      
      hr(),
      actionButton("btn_train", "▶ Train Model", class = "btn-primary", style = "width: 100%; font-weight: bold;"),
      br(), br(),
      actionButton("btn_reset", "↺ Reset Model", class = "btn-default", style = "width: 100%;"),
      
      hr(),
      h5("Architecture Specs:", style = "font-weight: bold;"),
      tags$ul(
        tags$li("Topology: 3 → 5 → 4 → 2"),
        tags$li("Optimizer: SGD"),
        tags$li("Loss: Binary Cross-Entropy"),
        tags$li("Seed: 42 (Reproducible)")
      ),
      uiOutput("sidebar_status")
    ),
    
    mainPanel(
      width = 9,
      tabsetPanel(
        id = "main_tabs",
        
        # ── TAB A: Data & Preprocessing ──────────────────────────────────────
        tabPanel("📊 A: Data & Preprocessing",
          br(),
          h4("Panel A: Input Data Exploration & Reproducible Preprocessing", style = "color: #2e75b6;"),
          fluidRow(
            column(7,
              h5("Dataset Preview (First 10 Observations)"),
              tableOutput("table_preview")
            ),
            column(5,
              h5("Reproducible 70 / 15 / 15 Split (Seed = 42)"),
              tableOutput("table_splits"),
              div(class = "alert alert-info", style = "font-size: 0.9em;",
                strong("Standardization Note: "),
                "Means and standard deviations are computed solely on the 126 training observations ",
                "to prevent data leakage, and applied across validation, test, and live inputs."
              )
            )
          ),
          hr(),
          h5("Feature Distributions & Label Combination Frequencies"),
          fluidRow(
            column(6, plotOutput("plot_features", height = "280px")),
            column(6, plotOutput("plot_labels", height = "280px"))
          ),
          hr(),
          h5("Training Feature Standardization Statistics"),
          tableOutput("table_stats")
        ),
        
        # ── TAB B: Network Diagram & Trace ───────────────────────────────────
        tabPanel("🕸️ B: Network Diagram",
          br(),
          h4("Panel B: Interactive 3-5-4-2 Network Diagram & Forward Propagation", style = "color: #2e75b6;"),
          fluidRow(
            column(4,
              numericInput("sample_idx", "Select Sample Index (1 to 180):", value = 1, min = 1, max = 180, step = 1),
              h5("Observation Details:"),
              tableOutput("table_sample_info")
            ),
            column(8,
              plotOutput("plot_network", height = "360px"),
              p(style = "font-size: 0.85em; color: #666; text-align: center;",
                "Red connections = positive weight | Blue = negative | Thickness proportional to magnitude")
            )
          ),
          hr(),
          h5("Forward Propagation Step-by-Step Trace for Selected Sample"),
          fluidRow(
            column(4, h6("Hidden Layer 1 (5 Neurons)"), tableOutput("trace_layer1")),
            column(4, h6("Hidden Layer 2 (4 Neurons)"), tableOutput("trace_layer2")),
            column(4, h6("Output Layer (2 Neurons)"), tableOutput("trace_layer3"))
          )
        ),
        
        # ── TAB C: Learning Curves ───────────────────────────────────────────
        tabPanel("📈 C: Learning Curves",
          br(),
          h4("Panel C & D: Training and Validation Loss Curves", style = "color: #2e75b6;"),
          plotOutput("plot_loss_curves", height = "380px"),
          hr(),
          fluidRow(
            column(3, wellPanel(h6("Final Epoch"), h4(textOutput("metric_epochs")))),
            column(3, wellPanel(h6("Final Train Loss"), h4(textOutput("metric_train_loss")))),
            column(3, wellPanel(h6("Final Val Loss"), h4(textOutput("metric_val_loss")))),
            column(3, wellPanel(h6("Best Val Loss"), h4(textOutput("metric_best_val"))))
          ),
          uiOutput("loss_interpretation")
        ),
        
        # ── TAB D: Live Predictions ──────────────────────────────────────────
        tabPanel("🔮 D: Predictions",
          br(),
          h4("Panel E: Live Prediction Panel & Test Set Performance", style = "color: #2e75b6;"),
          fluidRow(
            column(6,
              wellPanel(
                h5("Manual Sensor Inputs"),
                sliderInput("pred_temp", "Temperature (°C):",
                            min = min(df_raw$temperature_c), max = max(df_raw$temperature_c),
                            value = round(mean(df_raw$temperature_c), 1), step = 0.1),
                sliderInput("pred_vib", "Vibration (mm/s):",
                            min = min(df_raw$vibration_mm_s), max = max(df_raw$vibration_mm_s),
                            value = round(mean(df_raw$vibration_mm_s), 2), step = 0.01),
                sliderInput("pred_pres", "Pressure (kPa):",
                            min = min(df_raw$pressure_kpa), max = max(df_raw$pressure_kpa),
                            value = round(mean(df_raw$pressure_kpa), 1), step = 0.1)
              )
            ),
            column(6,
              wellPanel(
                h5("Predicted Probabilities & Decisions"),
                plotOutput("plot_gauges", height = "180px"),
                h4(textOutput("pred_decision_oh")),
                h4(textOutput("pred_decision_fa"))
              )
            )
          ),
          hr(),
          h5("Held-Out Test Set Performance (27 Observations)"),
          fluidRow(
            column(4, wellPanel(h6("Overall Test Accuracy"), h4(textOutput("test_acc_avg")))),
            column(4, wellPanel(h6("Overheat Alert Accuracy"), h4(textOutput("test_acc_oh")))),
            column(4, wellPanel(h6("Fault Alert Accuracy"), h4(textOutput("test_acc_fa"))))
          )
        ),
        
        # ── TAB E: Controlled Experiments ────────────────────────────────────
        tabPanel("🧪 E: Experiments",
          br(),
          h4("Panel F: Controlled Experiments (Rubric Requirements)", style = "color: #2e75b6;"),
          p("Execute the three mandatory controlled experiment suites. Each run starts from fixed seed 42:"),
          fluidRow(
            column(4, actionButton("run_exp1", "▶ Run Exp 1: LR (0.001 vs 0.01 vs 0.1)", class = "btn-info", style = "width: 100%; font-weight: bold;")),
            column(4, actionButton("run_exp2", "▶ Run Exp 2: Activations (ReLU vs Sigmoid)", class = "btn-info", style = "width: 100%; font-weight: bold;")),
            column(4, actionButton("run_exp3", "▶ Run Exp 3: Epochs (20 vs 100 vs 300)", class = "btn-info", style = "width: 100%; font-weight: bold;"))
          ),
          br(),
          tableOutput("table_experiments"),
          plotOutput("plot_exp_comparison", height = "320px")
        )
      )
    )
  )
)

# ------------------------------------------------------------------------------
# 4. SERVER LOGIC
# ------------------------------------------------------------------------------
server <- function(input, output, session) {
  
  # Reactive application state
  model_state <- reactiveValues(
    net = NULL,
    train_losses = NULL,
    val_losses = NULL,
    trained = FALSE,
    experiments = data.frame()
  )
  
  # Default initialization on app start
  observe({
    if (!model_state$trained) {
      res <- train_network(X_tr, y_tr, X_va, y_va, lr = 0.01, epochs = 100, act_fn = "relu", seed = 42)
      model_state$net <- res$net
      model_state$train_losses <- res$train_losses
      model_state$val_losses <- res$val_losses
      model_state$trained <- TRUE
    }
  })
  
  # Train Button Click Handler
  observeEvent(input$btn_train, {
    res <- train_network(X_tr, y_tr, X_va, y_va,
                         lr = as.numeric(input$lr),
                         epochs = as.numeric(input$epochs),
                         act_fn = input$activation,
                         seed = 42)
    model_state$net <- res$net
    model_state$train_losses <- res$train_losses
    model_state$val_losses <- res$val_losses
    model_state$trained <- TRUE
  })
  
  # Reset Button Click Handler
  observeEvent(input$btn_reset, {
    model_state$net <- init_network(42)
    model_state$train_losses <- NULL
    model_state$val_losses <- NULL
    model_state$trained <- FALSE
  })
  
  # Status feedback in sidebar
  output$sidebar_status <- renderUI({
    if (model_state$trained) {
      div(class = "alert alert-success", style = "margin-top: 15px;",
          strong("Status: "), "Model Trained Successfully!")
    } else {
      div(class = "alert alert-warning", style = "margin-top: 15px;",
          strong("Status: "), "Model Reset / Untrained")
    }
  })
  
  # ── Panel A Outputs ────────────────────────────────────────────────────────
  output$table_preview <- renderTable({
    head(df_raw, 10)
  }, digits = 2)
  
  output$table_splits <- renderTable({
    data.frame(
      Split = c("Training", "Validation", "Testing"),
      Observations = c(nrow(X_tr), nrow(X_va), nrow(X_te)),
      Percentage = c("70%", "15%", "15%")
    )
  })
  
  output$table_stats <- renderTable({
    data.frame(
      Feature = FEATURE_COLS,
      Training_Mean = round(train_mu, 3),
      Training_Std = round(train_sig, 3)
    )
  })
  
  output$plot_features <- renderPlot({
    par(mfrow = c(1, 3), mar = c(4, 4, 2, 1))
    hist(df_raw$temperature_c, main = "Temperature (°C)", xlab = "Temp", col = "#2196F3", border = "white")
    hist(df_raw$vibration_mm_s, main = "Vibration (mm/s)", xlab = "Vib", col = "#4CAF50", border = "white")
    hist(df_raw$pressure_kpa, main = "Pressure (kPa)", xlab = "Pressure", col = "#FF9800", border = "white")
  })
  
  output$plot_labels <- renderPlot({
    oh <- df_raw$overheat_alert
    fa <- df_raw$fault_alert
    counts <- c(
      "No Alert (0,0)" = sum(oh == 0 & fa == 0),
      "Only Overheat (1,0)" = sum(oh == 1 & fa == 0),
      "Only Fault (0,1)" = sum(oh == 0 & fa == 1),
      "Both (1,1)" = sum(oh == 1 & fa == 1)
    )
    barplot(counts, col = c("#4CAF50", "#FF9800", "#F44336", "#9C27B0"),
            main = "Target Combinations", las = 2, cex.names = 0.8)
  })
  
  # ── Panel B Outputs: Network Diagram ───────────────────────────────────────
  output$table_sample_info <- renderTable({
    s_idx <- as.integer(input$sample_idx)
    s_idx <- max(1, min(nrow(df_raw), s_idx))
    row_val <- df_raw[s_idx, ]
    data.frame(
      Item = c("Machine ID", "Temperature (°C)", "Vibration (mm/s)", "Pressure (kPa)", "True Overheat", "True Fault"),
      Value = c(row_val$machine_id, row_val$temperature_c, row_val$vibration_mm_s, row_val$pressure_kpa,
                row_val$overheat_alert, row_val$fault_alert)
    )
  })
  
  output$plot_network <- renderPlot({
    par(mar = c(1, 1, 3, 1))
    plot(0, 0, type = "n", xlim = c(0.5, 4.5), ylim = c(-2.5, 2.5),
         axes = FALSE, xlab = "", ylab = "", main = "3 → 5 → 4 → 2 Architecture")
    
    layers <- c(3, 5, 4, 2)
    coords <- list()
    for (l in 1:4) {
      n_nodes <- layers[l]
      y_pos <- seq((n_nodes - 1) / 2, -(n_nodes - 1) / 2, length.out = n_nodes)
      coords[[l]] <- data.frame(x = rep(l, n_nodes), y = y_pos)
    }
    
    # Draw connections with weight-based colors
    net <- model_state$net
    if (is.null(net)) net <- init_network(42)
    weights_list <- list(net$W1, net$W2, net$W3)
    
    for (l in 1:3) {
      W <- weights_list[[l]]
      w_max <- max(abs(W)) + 1e-6
      for (i in 1:layers[l]) {
        for (j in 1:layers[l+1]) {
          w <- W[i, j]
          col <- if (w >= 0) rgb(0.8, 0.2, 0.2, alpha = min(0.9, 0.2 + 0.7 * abs(w) / w_max))
                 else rgb(0.2, 0.2, 0.8, alpha = min(0.9, 0.2 + 0.7 * abs(w) / w_max))
          lines(c(coords[[l]]$x[i], coords[[l+1]]$x[j]),
                c(coords[[l]]$y[i], coords[[l+1]]$y[j]),
                col = col, lwd = 1.2)
        }
      }
    }
    
    # Draw nodes
    node_colors <- c("#2196F3", "#4CAF50", "#FF9800", "#F44336")
    for (l in 1:4) {
      points(coords[[l]]$x, coords[[l]]$y, pch = 21, bg = node_colors[l], col = "white", cex = 2.8, lwd = 2)
    }
    
    text(1:4, rep(-2.3, 4), c("Input (3)", "Hidden 1 (5)", "Hidden 2 (4)", "Output (2)"), font = 2)
  })
  
  # Forward Pass Trace calculations
  sample_fwd <- reactive({
    req(model_state$net)
    s_idx <- max(1, min(nrow(df_raw), as.integer(input$sample_idx)))
    x_sample <- as.matrix(df_raw[s_idx, FEATURE_COLS])
    x_scaled <- scale_data(x_sample, train_mu, train_sig)
    forward_pass(x_scaled, model_state$net, input$activation)
  })
  
  output$trace_layer1 <- renderTable({
    fwd <- sample_fwd()
    data.frame(Neuron = paste0("H1-", 1:5),
               Weighted_Sum_Z = round(fwd$Z1[1, ], 4),
               Activation_A = round(fwd$A1[1, ], 4))
  })
  
  output$trace_layer2 <- renderTable({
    fwd <- sample_fwd()
    data.frame(Neuron = paste0("H2-", 1:4),
               Weighted_Sum_Z = round(fwd$Z2[1, ], 4),
               Activation_A = round(fwd$A2[1, ], 4))
  })
  
  output$trace_layer3 <- renderTable({
    fwd <- sample_fwd()
    thr <- input$threshold
    data.frame(Target = TARGET_COLS,
               Z = round(fwd$Z3[1, ], 4),
               Probability = round(fwd$Yhat[1, ], 4),
               Decision = ifelse(fwd$Yhat[1, ] >= thr, "ALERT (1)", "NORMAL (0)"))
  })
  
  # ── Panel C & D Outputs: Learning Curves ────────────────────────────────────
  output$plot_loss_curves <- renderPlot({
    req(model_state$train_losses)
    tl <- model_state$train_losses
    vl <- model_state$val_losses
    eps <- 1:length(tl)
    
    plot(eps, tl, type = "l", col = "#1565C0", lwd = 2.5,
         xlab = "Epoch", ylab = "Binary Cross-Entropy Loss",
         main = "Training & Validation Loss vs Epoch",
         ylim = c(min(c(tl, vl)) * 0.9, max(c(tl, vl)) * 1.05))
    lines(eps, vl, col = "#C62828", lwd = 2.5, lty = 2)
    legend("topright", legend = c("Training Loss", "Validation Loss"),
           col = c("#1565C0", "#C62828"), lwd = 2.5, lty = c(1, 2))
    grid()
  })
  
  output$metric_epochs <- renderText({
    if (!is.null(model_state$train_losses)) length(model_state$train_losses) else "-"
  })
  output$metric_train_loss <- renderText({
    if (!is.null(model_state$train_losses)) round(tail(model_state$train_losses, 1), 4) else "-"
  })
  output$metric_val_loss <- renderText({
    if (!is.null(model_state$val_losses)) round(tail(model_state$val_losses, 1), 4) else "-"
  })
  output$metric_best_val <- renderText({
    if (!is.null(model_state$val_losses)) {
      best_val <- min(model_state$val_losses)
      best_ep  <- which.min(model_state$val_losses)
      paste0(round(best_val, 4), " (ep ", best_ep, ")")
    } else "-"
  })
  
  output$loss_interpretation <- renderUI({
    req(model_state$val_losses)
    tl_last <- tail(model_state$train_losses, 1)
    vl_last <- tail(model_state$val_losses, 1)
    gap <- vl_last - tl_last
    
    interp <- if (gap > 0.05) {
      div(class = "alert alert-danger", strong("Interpretation: "),
          "Overfitting detected. Validation loss is notably higher than training loss. Consider early stopping or reducing epochs.")
    } else if (gap < 0.01) {
      div(class = "alert alert-success", strong("Interpretation: "),
          "Generalizing smoothly. Training and validation losses track closely without severe divergence.")
    } else {
      div(class = "alert alert-warning", strong("Interpretation: "),
          "Moderate gap between training and validation loss. Model is stabilizing.")
    }
    interp
  })
  
  # ── Panel E Outputs: Predictions & Metrics ──────────────────────────────────
  live_pred <- reactive({
    req(model_state$net)
    x_in <- matrix(c(input$pred_temp, input$pred_vib, input$pred_pres), nrow = 1)
    x_sc <- scale_data(x_in, train_mu, train_sig)
    forward_pass(x_sc, model_state$net, input$activation)$Yhat[1, ]
  })
  
  output$plot_gauges <- renderPlot({
    req(live_pred())
    probs <- live_pred()
    par(mar = c(3, 8, 2, 2))
    bp <- barplot(probs, horiz = TRUE, xlim = c(0, 1),
                  names.arg = c("Overheat Alert", "Fault Alert"),
                  col = c("#FF9800", "#9C27B0"),
                  main = "Live Predicted Alert Probabilities", las = 1)
    abline(v = input$threshold, col = "red", lwd = 2, lty = 2)
    text(probs + 0.05, bp, labels = round(probs, 3), font = 2)
  })
  
  output$pred_decision_oh <- renderText({
    p <- live_pred()[1]
    thr <- input$threshold
    if (p >= thr) paste0("🔴 OVERHEAT ALERT TRIGGERED (P = ", round(p, 4), ")")
    else paste0("🟢 Normal Operating Temperature (P = ", round(p, 4), ")")
  })
  
  output$pred_decision_fa <- renderText({
    p <- live_pred()[2]
    thr <- input$threshold
    if (p >= thr) paste0("🔴 FAULT ALERT TRIGGERED (P = ", round(p, 4), ")")
    else paste0("🟢 Machine Status Normal (P = ", round(p, 4), ")")
  })
  
  # Test Set Performance Metrics
  test_eval <- reactive({
    req(model_state$net)
    fwd_te <- forward_pass(X_te, model_state$net, input$activation)
    preds_bin <- ifelse(fwd_te$Yhat >= input$threshold, 1, 0)
    acc_oh <- mean(preds_bin[, 1] == y_te[, 1])
    acc_fa <- mean(preds_bin[, 2] == y_te[, 2])
    list(oh = acc_oh, fa = acc_fa, avg = mean(c(acc_oh, acc_fa)))
  })
  
  output$test_acc_avg <- renderText({ paste0(round(test_eval()$avg * 100, 1), "%") })
  output$test_acc_oh  <- renderText({ paste0(round(test_eval()$oh * 100, 1), "%") })
  output$test_acc_fa  <- renderText({ paste0(round(test_eval()$fa * 100, 1), "%") })
  
  # ── Panel F Outputs: Controlled Experiments ────────────────────────────────
  observeEvent(input$run_exp1, {
    res_list <- list()
    for (lr_v in c(0.001, 0.01, 0.1)) {
      m_res <- train_network(X_tr, y_tr, X_va, y_va, lr = lr_v, epochs = 100, act_fn = "relu", seed = 42)
      fwd_te <- forward_pass(X_te, m_res$net, "relu")
      acc <- mean(ifelse(fwd_te$Yhat >= 0.5, 1, 0) == y_te)
      res_list[[length(res_list) + 1]] <- data.frame(
        Experiment = paste0("LR = ", lr_v),
        LR = lr_v, Epochs = 100, Activation = "RELU",
        Final_Train_Loss = round(tail(m_res$train_losses, 1), 4),
        Final_Val_Loss = round(tail(m_res$val_losses, 1), 4),
        Best_Val_Loss = round(min(m_res$val_losses), 4),
        Test_Accuracy = paste0(round(acc * 100, 1), "%")
      )
    }
    model_state$experiments <- do.call(rbind, res_list)
  })
  
  observeEvent(input$run_exp2, {
    res_list <- list()
    for (act in c("relu", "sigmoid")) {
      m_res <- train_network(X_tr, y_tr, X_va, y_va, lr = 0.01, epochs = 100, act_fn = act, seed = 42)
      fwd_te <- forward_pass(X_te, m_res$net, act)
      acc <- mean(ifelse(fwd_te$Yhat >= 0.5, 1, 0) == y_te)
      res_list[[length(res_list) + 1]] <- data.frame(
        Experiment = paste0("Act = ", toupper(act)),
        LR = 0.01, Epochs = 100, Activation = toupper(act),
        Final_Train_Loss = round(tail(m_res$train_losses, 1), 4),
        Final_Val_Loss = round(tail(m_res$val_losses, 1), 4),
        Best_Val_Loss = round(min(m_res$val_losses), 4),
        Test_Accuracy = paste0(round(acc * 100, 1), "%")
      )
    }
    model_state$experiments <- do.call(rbind, res_list)
  })
  
  observeEvent(input$run_exp3, {
    res_list <- list()
    for (ep in c(20, 100, 300)) {
      m_res <- train_network(X_tr, y_tr, X_va, y_va, lr = 0.01, epochs = ep, act_fn = "relu", seed = 42)
      fwd_te <- forward_pass(X_te, m_res$net, "relu")
      acc <- mean(ifelse(fwd_te$Yhat >= 0.5, 1, 0) == y_te)
      res_list[[length(res_list) + 1]] <- data.frame(
        Experiment = paste0("Epochs = ", ep),
        LR = 0.01, Epochs = ep, Activation = "RELU",
        Final_Train_Loss = round(tail(m_res$train_losses, 1), 4),
        Final_Val_Loss = round(tail(m_res$val_losses, 1), 4),
        Best_Val_Loss = round(min(m_res$val_losses), 4),
        Test_Accuracy = paste0(round(acc * 100, 1), "%")
      )
    }
    model_state$experiments <- do.call(rbind, res_list)
  })
  
  output$table_experiments <- renderTable({
    req(nrow(model_state$experiments) > 0)
    model_state$experiments
  })
  
  output$plot_exp_comparison <- renderPlot({
    req(nrow(model_state$experiments) > 0)
    df_exp <- model_state$experiments
    losses_mat <- rbind(df_exp$Final_Train_Loss, df_exp$Final_Val_Loss, df_exp$Best_Val_Loss)
    barplot(losses_mat, beside = TRUE, col = c("#1565C0", "#C62828", "#2E7D32"),
            names.arg = df_exp$Experiment, main = "Loss Comparison Across Configurations",
            ylab = "Loss", ylim = c(0, max(losses_mat) * 1.2))
    legend("topright", legend = c("Final Train Loss", "Final Val Loss", "Best Val Loss"),
           fill = c("#1565C0", "#C62828", "#2E7D32"))
    grid()
  })
}

# ------------------------------------------------------------------------------
# 5. EXECUTION
# ------------------------------------------------------------------------------
shinyApp(ui = ui, server = server)
