trainSaveModel <- function(
        trainX,
        trainY,
        valX,
        valY,
        testX,
        testY,
        N,
        classWeight = NULL,
        baseModel = "model_ALL_OF_US_8_17_26_221.keras"
) {
    keras <- reticulate::import("keras")
    pd <- reticulate::import("pandas")
    np <- reticulate::import("numpy", convert = FALSE)

    baseModelPath <- system.file(paste0('models/',baseModel), package = 'GlaucomaPrescreeningPrediction')

    message("Loading model from ", baseModelPath)
    model <- keras::load_model_hdf5(baseModelPath, compile = FALSE)
    #model <- keras$models$load_model(baseModelPath, compile = FALSE)

    message("All layers frozen except for last ", N)
    layer_count <- length(model$layers)
    layers <- reticulate::py_get_attr(model, "layers")
    if (N == 0L) {
        for (i in 1:layer_count) {
          lyr <- reticulate::py_get_item(layers, as.integer(i - 1))
          lyr$trainable <- FALSE
        }
    } else {
        freeze_to <- max(layer_count - as.integer(N), 0L)
        if (freeze_to > 0L) {
            for (i in seq_len(freeze_to)) {
              lyr <- reticulate::py_get_item(layers, as.integer(i - 1))   # 0-based Python indexing
              lyr$trainable <- FALSE
            }
        }
    }

    early_stopping <- keras$callbacks$EarlyStopping(
        monitor = "val_auc",
        verbose = 1L,
        patience = 10L,
        mode = "max",
        restore_best_weights = TRUE
    )

    metrics_list <- reticulate::tuple(
        keras$metrics$BinaryAccuracy(name = "accuracy"),
        keras$metrics$AUC(name = "auc"),
        keras$metrics$AUC(name = "prc", curve = "PR"),
        keras$metrics$Precision(name = "precision"),
        keras$metrics$Recall(name = "recall")
    )

    model$compile(
        optimizer = keras$optimizers$Adam(learning_rate = 1e-3),
        loss = keras$losses$BinaryCrossentropy(),
        metrics = metrics_list
    )


    my_matrix <- as.matrix(trainX)
    storage.mode(my_matrix ) <- "double"
    x_train <-  np$array(my_matrix, dtype = 'float32')
    y_train <-  np$array(as.integer(trainY), dtype = 'int32')

    my_matrix <- as.matrix(valX)
    storage.mode(my_matrix ) <- "double"
    x_val <-  np$array(my_matrix, dtype = 'float32')
    y_val <-  np$array(as.integer(valY), dtype = 'int32')

    model$fit(
      x = x_train,
      y =  y_train,
      epochs=as.integer(150),
      shuffle=TRUE,
      batch_size=as.integer(64),
      class_weight=classWeight,
      callbacks=early_stopping,
      validation_data=reticulate::tuple(x_val, y_val)
      )

    my_matrix <- as.matrix(testX)
    storage.mode(my_matrix ) <- "double"
    x_test <-  np$array(my_matrix, dtype = 'float32')
    y_test <-  np$array(as.integer(testY), dtype = 'int32')

    testResult <- model$evaluate(x_test, y_test, batch_size = 64L, verbose = 0L)
    metric_names <- as.character(model$metrics_names)
    metric_values <- as.numeric(reticulate::py_to_r(testResult))
    for (i in seq_along(metric_names)) {
        message(metric_names[[i]], ": ", metric_values[[i]])
    }

    test_predictions <- model$predict(x_test, batch_size = 64L)
    df_test_predictions <- pd$DataFrame(test_predictions, columns = reticulate::tuple("Prediction"))
    df_test_predictions$truth <- as.integer(testY)

    val_predictions <- model$predict(x_val, batch_size = 64L)
    df_val_predictions <- pd$DataFrame(val_predictions, columns = reticulate::tuple("Prediction"))
    df_val_predictions$truth <- as.integer(valY)

    return(invisible(list(
      outputModel = model,
      testPred = df_test_predictions,
      valPred = df_val_predictions
    )))
}

sweepFineTuneRuns <- function(
        results, # output of model validation
        aeSource = "frozen",
        nList = c(20, 40, 60, 80, 100),
        numUnfrozenLayers = c(0, 1, 3, 6, 7, 8, 9, 11, 12, 15, 16, 18),
        classWeight = NULL,
        baseModel = "model_ALL_OF_US_8_17_26_221.keras",
        seed = 124
) {

  # training data is 70% random sample (seed for repro) for plpData
  sparseData <- PatientLevelPrediction::toSparseM(
    cohort = results$prediction,
    plpData = results$plpData,
    map = covariateMap()
    )

  set.seed(seed)
  trainInd <- sample(1:nrow(sparseData$dataMatrix), size = floor(nrow(sparseData$dataMatrix)*0.7))
  tempInd <- (1:nrow(sparseData$dataMatrix))[!(1:nrow(sparseData$dataMatrix)) %in% trainInd]
  testInd <- sample(tempInd, floor(length(tempInd)*2/3))
  valInd <- tempInd[!tempInd %in% testInd]

  trainX <- as.matrix(sparseData$dataMatrix[trainInd,])
  testX <- as.matrix(sparseData$dataMatrix[testInd,])
  valX <- as.matrix(sparseData$dataMatrix[valInd,])
  trainY <- sparseData$labels$outcomeCount[trainInd]
  testY <- sparseData$labels$outcomeCount[testInd]
  valY <- sparseData$labels$outcomeCount[valInd]

    run_tag <- if (identical(aeSource, "retrained")) "retrain_autoencoders_" else ""

    total_rows <- nrow(trainX)
    if (is.na(total_rows) || total_rows < 1L) {
        stop("X_train appears to be empty or invalid.", call. = FALSE)
    }

    result <- list()
    for (pct in nList) {
        n_train <- as.integer(total_rows * (pct / 100))
        n_train <- max(1L, min(n_train, total_rows))

        subX <- trainX[1:(n_train),]
        subY <- trainY[1:(n_train)]

        for (n_unfrozen in numUnfrozenLayers) {

            message("Running fine-tune with ", pct, "% training data and ", n_unfrozen, " unfrozen layers")
            result[[length(result) + 1]] <- trainSaveModel(
                trainX = subX,
                trainY = subY,
                valX = valX,
                valY = valY,
                testX = testX,
                testY = testY,
                N = as.integer(n_unfrozen),
                classWeight = classWeight,
                baseModel = baseModel
            )
        }
    }

    # add name for result

    return(invisible(result))
}



