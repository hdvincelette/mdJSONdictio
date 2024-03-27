#' Validate Tabular Data Dictionaries
#'
#' Compares a tabular data dictionary to a tabular dataset and summarizes discrepancies in a data frame. The input data dictionary must be formatted to a \href{https://github.com/hdvincelette/mdJSONdictio/blob/master/inst/templates/mdJSONdictio_Dictionary_Template_v2.xlsx?raw=true}{template}.
#' @param x  Data frame of a tabular data dictionary.
#' @param y Data frame of a dataset.
#' @return Returns a data frame comprised of warning messages about the tabular data dictionary.
#' @keywords mdEditor, mdJSON, json, dictionary, metadata
#' @seealso ```validate.table()```
#' @export
#' @examples
#' # Import tabular data dictionary as data frame
#' path<-system.file("extdata", "e.g.dictionary.xlsx", package = "mdJSONdictio")
#' input.dxnry<-readxl::read_excel(path = path)
#'
#' # Import tabular dataset as data frame
#' path<-system.file("extdata", "e.g.dataset.csv", package = "mdJSONdictio")
#' input.data<-read.csv(file = path, na.strings = "", stringsAsFactors = FALSE)
#'
#' # Validate data frame against data frame
#' all.warnings<- validate.table(x = input.dxnry, y = input.data)
#'
#' # Export table to disk
#' write.csv(x = all.warnings, file = "e.g.warnings2.csv")



validate.table <- function(x, y) {
  suppressWarnings({
    options(scipen = 999)

    `%>%` <- magrittr::`%>%`

    # data(sysdata, envir=environment())

    input.dxnry <- x
    input.data <- y

    # Check for tabular data dictionary template v1
    if ("yes" %in% unique(input.dxnry$allowNull) |
        "no" %in% unique(input.dxnry$allowNull)) {
      stop(
        "'allowNull' and 'isCaseSensitive' only accept logical values (TRUE/FALSE).\nCorrect these fields in the data frame input before continuing.\nUse the latest version (v2) of the tabular data dictionary template to ensure the functon runs smoothly.\n  Print `help(package = 'mdJSONdictio')` for Help Pages.'"
      )
    }

    # Check for multibyte strings in data file
    for (a in 1:ncol(input.data)) {
      for (b in 1:nrow(input.data)) {
        if (validEnc(as.character(input.data[b, a])) == FALSE)
          stop(
            'Data contains multibyte strings.',
            '\nColumn: ',
            colnames(input.data[a]),
            '\nRow: ',
            b + 1,
            '\nString: ',
            input.data[b, a],
            '\n\nPrint `help(package = "mdJSONdictio")` for Help Pages.'
          )
      }
    }


    # Prep files
    input.data <-
      input.data %>% dplyr::mutate_if(is.character, ~ dplyr::na_if(., ''))

    input.dxnry <-
      input.dxnry %>% dplyr::mutate_if(is.character, ~ dplyr::na_if(., ''))

    datatype.rules <-
      datatype.rules %>% dplyr::mutate_if(is.character, ~ dplyr::na_if(., ''))

    # for (a in 1:nrow(datatype_rules)) {
    #   if (is.na(datatype_rules$RdataType[a]) == FALSE) {
    #     datatype_rules$RdataType[a] <-
    #       paste0('is.', datatype_rules$RdataType[a])
    #   }
    # }


    # Create dictionary references
    dict.vars <- unique(input.dxnry$codeName)

    dict.domain <- input.dxnry %>%
      dplyr::filter(domainItem_value != "dataField") %>%
      dplyr::select(codeName, domainItem_value) %>%
      dplyr::group_by(codeName) %>%
      dplyr::group_map(~ .x)

    names(dict.domain) <- input.dxnry %>%
      dplyr::filter(domainItem_value != "dataField") %>%
      dplyr::group_by(codeName) %>%
      dplyr::group_map(~ .y) %>%
      unlist()

    dict.datafield <- input.dxnry %>%
      dplyr::filter(domainItem_value == "dataField")


    # Create data frame for warnings
    warnings.df <- data.frame(
      Num = numeric(),
      Variable = character(),
      Category = character(),
      Message = character(),
      stringsAsFactors = FALSE
    )



    # Change missingValue to NA
    #!# missingValue must be correct

    dict.miss <- dict.datafield %>%
      dplyr::filter(!is.na(missingValue))

    data.NA <- input.data

    if (nrow(dict.miss) != 0) {
      for (a in 1:ncol(data.NA)) {
        for (bb in 1:nrow(dict.miss)) {
          if (colnames(data.NA[a]) == dict.miss$codeName[bb] &
              dict.miss$missingValue[bb] %in% unique(data.NA[, a])) {
            data.NA[a] <- utils::type.convert(data.NA[a], as.is = TRUE)
          }
        }
      }
    }



    #### Required fields: codeName, domainItem_value, allowNull ####


    # Check codeName
    for (a in 1:ncol(data.NA)) {
      if (!colnames(data.NA[a]) %in% dict.vars) {
        warnings.df <- warnings.df %>%
          dplyr::add_row(
            Num = nrow(warnings.df) + 1,
            Variable = colnames(data.NA[a]),
            Category = "codeName",
            Message =  paste0(
              'Dataset variable not listed under "codeName" in dictionary'
            )
          )
      }

    }



    # Check domainItem_value
    if (length(unique(input.dxnry$domainItem_value)) != 1) {
      for (a in 1:ncol(data.NA)) {
        for (b in 1:length(dict.domain)) {
          if (colnames(data.NA[a]) == names(dict.domain[b]) &
              length(setdiff(na.omit(data.NA[, a]), dict.domain[[b]][["domainItem_value"]])) !=
              0) {
            warnings.df <- warnings.df %>%
              dplyr::add_row(
                Num = nrow(warnings.df) + 1,
                Variable = colnames(data.NA[a]),
                Category = "domainItem_value",
                Message =  paste0(
                  'Dataset variable contains entry value(s) not listed under "domainItem_Value" in dictionary: ',
                  paste(setdiff(
                    na.omit(data.NA[, a]), dict.domain[[b]][["domainItem_value"]]
                  ),
                  collapse = ", ")
                )
              )
          }

        }
      }
    }

    # Check allowNull
    for (a in 1:ncol(data.NA)) {
      for (bb in 1:nrow(dict.datafield))
        if (colnames(data.NA[a]) == dict.datafield$codeName[bb] &
            dict.datafield$allowNull[bb] == FALSE &
            NA %in% (unique(data.NA[, a]))) {
          warnings.df <- warnings.df %>%
            dplyr::add_row(
              Num = nrow(warnings.df) + 1,
              Variable = colnames(data.NA[a]),
              Category = "allowNull",
              Message =  paste0(
                'Dataset variable contains blank values, which is inconsistent with "allowNull" in dictionary '
              )
            )
        }
    }


    #### Required fields: dataType ####

    # RdataType: all
    #!# Excluding date, time, datetime, xml

    for (a in 1:ncol(data.NA)) {
      for (bb in 1:nrow(dict.datafield)) {
        for (cc in 1:nrow(datatype.rules)) {
          RdataType <- NULL

          if (colnames(data.NA[a]) == dict.datafield$codeName[bb] &
              dict.datafield$dataType[bb] == datatype.rules$value[cc]) {
            if (is.na(datatype.rules$RdataType[cc]) == FALSE) {
              RdataType <- match.fun(datatype.rules$RdataType[cc])
            }

            if (is.null(RdataType) == TRUE) {
              next
            }
            else if (RdataType(data.NA[[a]]) == FALSE &
                     purrr::map(data.NA[a], class) != "logical") {
              warnings.df <- warnings.df %>%
                dplyr::add_row(
                  Num = nrow(warnings.df) + 1,
                  Variable = colnames(data.NA[a]),
                  Category = "dataType_RdataType",
                  Message =  paste0(
                    'Dataset variable is detected as a different datatype (',
                    purrr::map(data.NA[a], class),
                    ') than indicated in dictionary (',
                    dict.datafield$dataType[bb],
                    ')'
                  )
                )
            }

          }
        }
      }
    }


    # RdataType: date, datetime

    ISO.datetime <- function(x,
                             datetime.format = c("%Y-%m-%d",
                                                 "%Y-W%U-%u",
                                                 "%Y-%j",
                                                 "%Y%m%d",
                                                 "%YW%U%u",
                                                 "%YW%U",
                                                 "%Y%j")) {
      tryCatch(
        !is.na(as.Date(x, datetime.format)),
        error = function(err) {
          FALSE
        }
      )
    }

    is.ISO.datetime <- function(x) {
      TRUE %in% ISO.datetime(x)
    }


    other.datetime <- function(x,
                               datetime.format = c("%d-%m-%Y",
                                                   "%m-%d-%Y",
                                                   "%d%m%Y",
                                                   "%m%d%Y")) {
      tryCatch(
        !is.na(as.Date(x, datetime.format)),
        error = function(err) {
          FALSE
        }
      )
    }

    is.other.datetime <- function(x) {
      TRUE %in% other.datetime(x)
    }

    for (a in 1:ncol(data.NA)) {
      datetime <- NA
      for (bb in 1:nrow(dict.datafield)) {
        if (colnames(data.NA[a]) == dict.datafield$codeName[bb] &
            dict.datafield$dataType[bb] %in%  c("datetime", "date")) {
          # print(paste0("data.NA: ", colnames(data.NA[a])))
          # print(paste0("dict.datafield: ", dict.datafield$codeName[bb]))
          # print(paste0("datatype: ", dict.datafield$dataType[bb]))

          if (!NA %in% unique(data.NA[, a]) |
              NA %in% unique(data.NA[, a]) &
              length(unique(data.NA[, a])) > 1) {
            # print(unique(data.NA[, a]))

            datetime <- na.omit(data.NA[, a])

            if (FALSE %in% sapply(datetime, is.ISO.datetime) |
                TRUE %in% sapply(datetime, is.other.datetime) |
                TRUE %in% sapply("/", grepl, datetime) |
                TRUE %in% sapply("AM", grepl, datetime) |
                TRUE %in% sapply("PM", grepl, datetime)) {
              warnings.df <- warnings.df %>%
                dplyr::add_row(
                  Num = nrow(warnings.df) + 1,
                  Variable = colnames(data.NA[a]),
                  Category = "dataType_datetime",
                  Message =  paste0(
                    'Dataset variable has entry value(s) not in standard ISO 1806 datetime format'
                  )
                )
            }

          }
        }
      }
    }


    # RdataType: time

    for (a in 1:ncol(data.NA)) {
      time <- NA
      for (bb in 1:nrow(dict.datafield)) {
        if (colnames(data.NA[a]) == dict.datafield$codeName[bb] &
            dict.datafield$dataType[bb] %in%  c("time")) {
          # print(paste0("data.NA: ", colnames(data.NA[a])))
          # print(paste0("dict.datafield: ", dict.datafield$codeName[bb]))
          # print(paste0("datatype: ", dict.datafield$dataType[bb]))

          if (!NA %in% unique(data.NA[, a]) |
              NA %in% unique(data.NA[, a]) &
              length(unique(data.NA[, a])) > 1) {
            # print(unique(data.NA[, a]))

            time <- na.omit(data.NA[, a])

            if (TRUE %in% sapply("AM", grepl, time) |
                TRUE %in% sapply("PM", grepl, time)) {
              warnings.df <- warnings.df %>%
                dplyr::add_row(
                  Num = nrow(warnings.df) + 1,
                  Variable = colnames(data.NA[a]),
                  Category = "dataType_datetime",
                  Message =  paste0(
                    'Dataset variable has entry value(s) not in standard ISO 1806 time format'
                  )
                )
            }

          }
        }
      }
    }


    ## maxLength
    #!# R only stores up to 32,767 characters


    for (a in 1:ncol(data.NA)) {
      data.nchar <- NA
      maxLength <- NA

      for (bb in 1:nrow(dict.datafield)) {
        for (cc in 1:nrow(datatype.rules)) {
          if (colnames(data.NA[a]) == dict.datafield$codeName[bb] &
              dict.datafield$dataType[bb] == datatype.rules$value[cc] &
              is.na(datatype.rules$maxLength[cc]) == FALSE) {
            # print(paste0("data.NA: ", colnames(data.NA[a])))
            # print(paste0("dict.datafield: ", dict.datafield$codeName[bb]))
            # print(paste0("datatype.rules: ", datatype.rules$value[cc]))

            if (!NA %in% unique(data.NA[, a]) |
                NA %in% unique(data.NA[, a]) &
                length(unique(data.NA[, a])) > 1) {
              maxLength <- max(nchar(as.character(data.NA[, a])), na.rm = TRUE)

              # print(maxLength)

              if (is.na(maxLength) == FALSE &
                  maxLength > datatype.rules$maxLength[cc])  {
                warnings.df <- warnings.df %>%
                  dplyr::add_row(
                    Num = nrow(warnings.df) + 1,
                    Variable = colnames(data.NA[a]),
                    Category = "dataType_maxLength",
                    Message =  paste0(
                      'Dataset variable has entry value(s) with a greater length (',
                      maxLength,
                      ') than allowed for the datatype (',
                      datatype.rules$maxLength[cc],
                      ')'
                    )
                  )
              }

            }
          }
        }
      }
    }


    ## maxPrecision
    #!# Need to test; Excel doesn't store > 15 sigfigs

    for (a in 1:ncol(data.NA)) {
      for (bb in 1:nrow(dict.datafield)) {
        for (cc in 1:nrow(datatype.rules)) {
          if (colnames(data.NA[a]) == dict.datafield$codeName[bb] &
              dict.datafield$dataType[bb] == datatype.rules$value[cc] &
              is.na(datatype.rules$maxPrecision[cc]) == FALSE &
              is.character(data.NA[, a]) == FALSE) {
            # print(paste0("data.NA: ", colnames(data.NA[a])))
            # print(paste0("dict.datafield: ", dict.datafield$codeName[bb]))
            # print(paste0("datatype.rules: ", datatype.rules$value[cc]))

            if (!NA %in% unique(data.NA[, a]) |
                NA %in% unique(data.NA[, a]) &
                length(unique(data.NA[, a])) > 1) {
              maxPrecision <-
                max(nchar(sub("^0+", "", as.character(data.NA[, a]))), na.rm = TRUE)


              # print(maxPrecision)

              if (is.na(maxPrecision) == FALSE &
                  maxPrecision > datatype.rules$maxPrecision[cc]) {
                warnings.df <- warnings.df %>%
                  dplyr::add_row(
                    Num = nrow(warnings.df) + 1,
                    Variable = colnames(data.NA[a]),
                    Category = "dataType_maxPrecision",
                    Message =  paste0(
                      'Dataset variable has entry value(s) with greater precision (',
                      maxPrecision,
                      ') than allowed for the datatype (',
                      datatype.rules$maxPrecision[cc],
                      ')'
                    )
                  )
              }

            }
          }
        }
      }
    }

    ## minValue

    for (a in 1:ncol(data.NA)) {
      minValue <- NA

      for (bb in 1:nrow(dict.datafield)) {
        for (cc in 1:nrow(datatype.rules)) {
          if (colnames(data.NA[a]) == dict.datafield$codeName[bb] &
              dict.datafield$dataType[bb] == datatype.rules$value[cc] &
              is.na(datatype.rules$minValue_unsigned[cc]) == FALSE &
              is.character(data.NA[, a]) == FALSE) {
            # print(paste0("data.NA: ", colnames(data.NA[a])))
            # print(paste0("dict.datafield: ", dict.datafield$codeName[bb]))
            # print(paste0("datatype.rules: ", datatype.rules$value[cc]))

            if (!NA %in% unique(data.NA[, a]) |
                NA %in% unique(data.NA[, a]) &
                length(unique(data.NA[, a])) > 1) {
              minValue <- min(unique(data.NA[, a]), na.rm = TRUE)

              # print(minValue)

              if (is.na(minValue) == FALSE &
                  minValue < datatype.rules$minValue_unsigned[cc]) {
                warnings.df <- warnings.df %>%
                  dplyr::add_row(
                    Num = nrow(warnings.df) + 1,
                    Variable = colnames(data.NA[a]),
                    Category = "dataType_minValue",
                    Message =  paste0(
                      'Dataset variable has entry value(s) with a smaller value (',
                      minValue,
                      ') than allowed for the datatype (',
                      datatype.rules$minValue_unsigned[cc],
                      ')'
                    )
                  )
              }

            }
          }
        }
      }
    }

    ## maxValue

    for (a in 1:ncol(data.NA)) {
      maxValue <- NA

      for (bb in 1:nrow(dict.datafield)) {
        for (cc in 1:nrow(datatype.rules)) {
          if (colnames(data.NA[a]) == dict.datafield$codeName[bb] &
              dict.datafield$dataType[bb] == datatype.rules$value[cc] &
              is.na(datatype.rules$maxValue_unsigned[cc]) == FALSE &
              is.character(data.NA[, a]) == FALSE) {
            # print(paste0("data.NA: ", colnames(data.NA[a])))
            # print(paste0("dict.datafield: ", dict.datafield$codeName[bb]))
            # print(paste0("datatype.rules: ", datatype.rules$value[cc]))

            if (!NA %in% unique(data.NA[, a]) |
                NA %in% unique(data.NA[, a]) &
                length(unique(data.NA[, a])) > 1) {
              maxValue <- max(unique(data.NA[, a]), na.rm = TRUE)

              # print(maxValue)

              if (is.na(maxValue) == FALSE &
                  maxValue > datatype.rules$maxValue_unsigned[cc]) {
                warnings.df <- warnings.df %>%
                  dplyr::add_row(
                    Num = nrow(warnings.df) + 1,
                    Variable = colnames(data.NA[a]),
                    Category = "dataType_maxValue",
                    Message =  paste0(
                      'Dataset variable has entry value(s) with a greater value (',
                      maxValue,
                      ') than allowed for the datatype (',
                      datatype.rules$maxValue_unsigned[cc],
                      ')'
                    )
                  )
              }

            }
          }
        }
      }
    }


    ## distinctValue

    for (a in 1:ncol(data.NA)) {
      distinctValue <- NA

      for (bb in 1:nrow(dict.datafield)) {
        for (cc in 1:nrow(datatype.rules)) {
          if (colnames(data.NA[a]) == dict.datafield$codeName[bb] &
              dict.datafield$dataType[bb] == datatype.rules$value[cc] &
              is.na(datatype.rules$distinctValue[cc]) == FALSE) {
            # print(paste0("data.NA: ", colnames(data.NA[a])))
            # print(paste0("dict.datafield: ", dict.datafield$codeName[bb]))
            # print(paste0("datatype.rules: ", datatype.rules$value[cc]))

            if (!NA %in% unique(data.NA[, a]) |
                NA %in% unique(data.NA[, a]) &
                length(unique(data.NA[, a])) > 1) {
              distinctValue <- length(unique(na.omit(data.NA[, a])))

              # print(distinctValue)

              if (is.na(distinctValue) == FALSE &
                  distinctValue > datatype.rules$distinctValue[cc]) {
                warnings.df <- warnings.df %>%
                  dplyr::add_row(
                    Num = nrow(warnings.df) + 1,
                    Variable = colnames(data.NA[a]),
                    Category = "dataType_distinctValue",
                    Message =  paste0(
                      'Dataset variable has a greater number of distinct values (',
                      distinctValue,
                      ') than allowed for the datatype (',
                      datatype.rules$distinctValue[cc],
                      ')'
                    )
                  )
              }

            }
          }
        }
      }
    }


    ## distinctLength

    for (a in 1:ncol(data.NA)) {
      distinctLength <- NA

      for (bb in 1:nrow(dict.datafield)) {
        for (cc in 1:nrow(datatype.rules)) {
          if (colnames(data.NA[a]) == dict.datafield$codeName[bb] &
              dict.datafield$dataType[bb] == datatype.rules$value[cc] &
              is.na(datatype.rules$distinctLength[cc]) == FALSE) {
            # print(paste0("data.NA: ", colnames(data.NA[a])))
            # print(paste0("dict.datafield: ", dict.datafield$codeName[bb]))
            # print(paste0("datatype.rules: ", datatype.rules$value[cc]))

            if (!NA %in% unique(data.NA[, a]) |
                NA %in% unique(data.NA[, a]) &
                length(unique(data.NA[, a])) > 1) {
              distinctLength <- length(unique(nchar(na.omit(
                data.NA[, a]
              ))))

              # print(distinctLength)

              if (is.na(distinctLength) == FALSE &
                  distinctLength > datatype.rules$distinctLength[cc]) {
                warnings.df <- warnings.df %>%
                  dplyr::add_row(
                    Num = nrow(warnings.df) + 1,
                    Variable = colnames(data.NA[a]),
                    Category = "dataType_distinctLength",
                    Message =  paste0(
                      'Dataset variable has entry values with more than one length (',
                      distinctLength,
                      ') while the datatype indicates values should be "fixed length"'
                    )
                  )
              }

            }
          }
        }
      }
    }


    #### Optional fields ####

    # Check unitsResolution

    # dict.res <- input.dxnry %>%
    #   dplyr::filter(!is.na(unitsResolution))
    #
    # data.res <- input.data %>%
    #   dplyr::select(any_of(dict.res$codeName))


    ### Remove trailing zeros
    #purrr::map_df(stringr::str_remove, pattern = "^0+")



    for (a in 1:ncol(data.NA)) {
      # print(colnames(data.NA[a]))
      data.ndecimal <- NULL
      dict.ndecimal <- NULL

      for (bb in 1:nrow(input.dxnry)) {
        if (colnames(data.NA[a]) == input.dxnry$codeName[bb] &
            is.na(input.dxnry$unitsResolution[bb]) == FALSE) {
          data.ndecimal <-
            nchar(gsub(".*\\.|^[^.]+$", "", as.character(data.NA[, a])))

          dict.ndecimal <-
            nchar(gsub(
              ".*\\.",
              "",
              as.character(input.dxnry$unitsResolution[bb])
            ))

          # print(paste0(min(data.ndecimal, na.rm = TRUE), " (min)"))
          # print(paste0(max(data.ndecimal, na.rm = TRUE), " (max)"))
          # print(paste0(dict.ndecimal, " (dictionary)"))

          if (is.null(dict.ndecimal) == FALSE) {
            min.data.ndecimal <- min(data.ndecimal, na.rm = TRUE)
            max.data.ndecimal <- max(data.ndecimal, na.rm = TRUE)

            if (is.null(min.data.ndecimal) == FALSE &
                min.data.ndecimal < dict.ndecimal) {
              warnings.df <- warnings.df %>%
                dplyr::add_row(
                  Num = nrow(warnings.df) + 1,
                  Variable = colnames(data.NA[a]),
                  Category = "unitsResolution",
                  Message =  paste0(
                    'Dataset variable contains entry value(s) with lower resolution than "unitsResolution" in dictionary'
                  )
                )


              if (is.null(max.data.ndecimal) == FALSE &
                  max.data.ndecimal  > dict.ndecimal) {
                warnings.df <- warnings.df %>%
                  dplyr::add_row(
                    Num = nrow(warnings.df) + 1,
                    Variable = colnames(data.NA[a]),
                    Category = "unitsResolution",
                    Message =  paste0(
                      'Dataset variable contains entry value(s) with higher resolution than "unitsResolution" in dictionary'
                    )
                  )
              }

            }
          }
        }
      }
    }


    # Check fieldWidth

    for (a in 1:ncol(data.NA)) {
      # print(colnames(data.NA[a]))
      data.nchar <- NULL
      dict.nchar <- NULL

      for (bb in 1:nrow(input.dxnry)) {
        if (colnames(data.NA[a]) == input.dxnry$codeName[bb] &
            is.na(input.dxnry$fieldWidth[bb]) == FALSE &
            NA %in% unique(data.NA[, a]) &
            length(unique(data.NA[, a]))!=1) {
          data.nchar <-  nchar(as.character(data.NA[, a]))

          dict.nchar <- input.dxnry$fieldWidth[bb]

          if (is.null(data.nchar) == FALSE) {
            # print(paste0(max(data.nchar, na.rm = TRUE), " (max)"))
            # print(paste0((dict.nchar), " (dictionary)"))


            if (is.null(data.nchar) == FALSE &
                max(data.nchar, na.rm = TRUE) > dict.nchar) {
              warnings.df <- warnings.df %>%
                dplyr::add_row(
                  Num = nrow(warnings.df) + 1,
                  Variable = colnames(data.NA[a]),
                  Category = "fieldWidth",
                  Message =  paste0(
                    'Dataset variable contains entry value(s) that exceeds "fieldWidth" in dictionary'
                  )
                )
            }

          }

        }
      }
    }


    # Check missingValue

    for (a in 1:ncol(input.data)) {
      for (bb in 1:nrow(input.dxnry))
        if (colnames(input.data[a]) == input.dxnry$codeName[bb] &
            is.na(input.dxnry$missingValue[bb]) == FALSE &
            NA %in% (unique(input.data[, a]))) {
          # print(paste0(colnames(input.data[a]) ))
          # print(paste0(input.dxnry$missingValue[bb]))

          warnings.df <- warnings.df %>%
            dplyr::add_row(
              Num = nrow(warnings.df) + 1,
              Variable = colnames(input.data[a]),
              Category = "missingValue",
              Message =  paste0(
                'Dataset variable contains blank values rather than "missingValue" in dictionary: ',
                input.dxnry$missingValue[bb]
              )
            )
        }
    }


    # Check minValue

    for (a in 1:ncol(data.NA)) {
      for (bb in 1:nrow(input.dxnry))
        if (colnames(data.NA[a]) == input.dxnry$codeName[bb] &
            is.na(input.dxnry$minValue[bb]) == FALSE) {
          if (!NA %in% unique(data.NA[, a]) |
              NA %in% unique(data.NA[, a]) &
              length(unique(data.NA[, a])) > 1) {
            if (min(unique(data.NA[, a]), na.rm = TRUE) < as.numeric(input.dxnry$minValue[bb])) {
              warnings.df <- warnings.df %>%
                dplyr::add_row(
                  Num = nrow(warnings.df) + 1,
                  Variable = colnames(data.NA[a]),
                  Category = "minValue",
                  Message =  paste0(
                    'Dataset variable contains entry value(s) less than "minValue" in dictionary'
                  )
                )
            }

          }
        }
    }


    # Check maxValue

    for (a in 1:ncol(data.NA)) {
      for (bb in 1:nrow(input.dxnry))
        if (colnames(data.NA[a]) == input.dxnry$codeName[bb] &
            is.na(input.dxnry$maxValue[bb]) == FALSE) {
          if (!NA %in% unique(data.NA[, a]) |
              NA %in% unique(data.NA[, a]) &
              length(unique(data.NA[, a])) > 1) {
            if (max(unique(data.NA[, a]), na.rm = TRUE) > as.numeric(input.dxnry$maxValue[bb])) {
              warnings.df <- warnings.df %>%
                dplyr::add_row(
                  Num = nrow(warnings.df) + 1,
                  Variable = colnames(data.NA[a]),
                  Category = "maxValue",
                  Message =  paste0(
                    'Dataset variable contains entry value(s) greater than "maxValue" in dictionary'
                  )
                )
            }

          }
        }
    }

    assign("all.warnings", warnings.df)


  })
}
