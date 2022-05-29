#' Validate Tabular Data Dictionaries
#'
#'
#' @param x  A data frame of the tabular data dictionary.
#' @param y A data frame of the dataset.
#' @return
#' @keywords mdEditor, mdJSON, json, dictionary, metadata
#' @seealso ```validate.table()```
#' @export
#' @examples
#' # Import tabular data dictionary as data frame
#' path<-system.file("extdata", "e.g.dictionary.xlsx", package = "mdJSONdictio")
#' input.dict<-readxl::read_excel(path = path)
#'
#' # Import dataset as data frame
#' path<-system.file("extdata", "e.g.dataset.csv", package = "mdJSONdictio")
#' input.data<-read.csv(path, na.strings = "", stringsAsFactors = FALSE)
#'
#' # Validate dataset against dictionary
#' all.warnings<- validate.table(input.dict, input.data)
#'
#' # Export table to disk
#' write(x = all.warnings, file = "e.g.validate.table.json")



validate.table <- function(x, y) {
  options(scipen = 999)

  `%>%` <- magrittr::`%>%`

  input.dict <- x
  input.data <- y


  # Prep files
  input.data <- input.data %>% dplyr::mutate_all(dplyr::na_if, "")
  input.dict <- input.dict %>% dplyr::mutate_all(dplyr::na_if, "")
  datatype.rules <-
    datatype.rules %>% dplyr::mutate_all(dplyr::na_if, "")


  # Create dictionary references
  dict.vars <- unique(input.dict$codeName)

  dict.domain <- input.dict %>%
    dplyr::filter(domainItem_value != "dataField") %>%
    dplyr::select(codeName, domainItem_value) %>%
    dplyr::group_by(codeName) %>%
    dplyr::group_map(~ .x)

  names(dict.domain) <- input.dict %>%
    dplyr::filter(domainItem_value != "dataField") %>%
    dplyr::group_by(codeName) %>%
    dplyr::group_map(~ .y) %>%
    unlist()

  dict.datafield <- input.dict %>%
    dplyr::filter(domainItem_value == "dataField")

  # Create data frame for warnings
  warnings.df <- data.frame(
    Num = numeric(),
    Variable = character(),
    Category = character(),
    Message = character(),
    stringsAsFactors = FALSE
  )

  # Check codeName
  for (a in 1:ncol(input.data)) {
    if (!colnames(input.data[a]) %in% dict.vars) {
      warnings.df <- warnings.df %>%
        dplyr::add_row(
          Num = nrow(warnings.df) + 1,
          Variable = colnames(input.data[a]),
          Category = "domainItem_value",
          Message =  paste0(
            'Dataset variable not listed under "CodeName" in dictionary'
          )
        )
    }

  }




  # Check domainItem_value
  for (a in 1:ncol(input.data)) {
    for (b in 1:length(dict.domain)) {
      if (colnames(input.data[a]) == names(dict.domain[b]) &
          length(setdiff(na.omit(input.data[, a]), dict.domain[[b]][["domainItem_value"]])) !=
          0) {
        warnings.df <- warnings.df %>%
          dplyr::add_row(
            Num = nrow(warnings.df) + 1,
            Variable = colnames(input.data[a]),
            Category = "domainItem_value",
            Message =  paste0(
              'Dataset variable contains entry value not listed under "domainItem_Value" in dictionary: ',
              paste(setdiff(
                na.omit(input.data[, a]), dict.domain[[b]][["domainItem_value"]]
              ),
              collapse = ", ")
            )
          )
      }

    }
  }







  # Check allowNull
  for (a in 1:ncol(input.data)) {
    for (bb in 1:nrow(dict.datafield))
      if (colnames(input.data[a]) == dict.datafield$codeName[bb] &
          dict.datafield$allowNull[bb] == "no" &
          NA %in% (unique(input.data[, a]))) {
        warnings.df <- warnings.df %>%
          dplyr::add_row(
            Num = nrow(warnings.df) + 1,
            Variable = colnames(input.data[a]),
            Category = "allowNull",
            Message =  paste0(
              'Dataset variable contains blank values, which is not consistent with "allowNull" in dictionary '
            )
          )
      }
  }




  ##############################################################################



  # Check dataType

  ## Change missingValue to NA
  #!# missingValue must be correct

  dict.miss <- dict.datafield %>%
    dplyr::filter(!is.na(missingValue))

  data.NA <- input.data

  for (a in 1:ncol(data.NA)) {
    for (bb in 1:nrow(dict.miss)) {
      if (colnames(data.NA[a]) == dict.miss$codeName[bb] &
          dict.miss$missingValue[bb] %in% unique(data.NA[, a])) {
        data.NA[a] <- utils::type.convert(data.NA[a], as.is = TRUE)
      }
    }
  }




  ## RDatatype
  #!# Excluding date, time, datetime, xml

  for (a in 1:ncol(data.NA)) {
    for (bb in 1:nrow(dict.datafield)) {
      for (cc in 1:nrow(datatype.rules)) {
        if (colnames(data.NA[a]) == dict.datafield$codeName[bb] &
            dict.datafield$dataType[bb] == datatype.rules$Value[cc]) {
          RDatatype <- NULL

          if (is.na(datatype.rules$RDatatype[cc]) == FALSE) {
            RDatatype <- match.fun(datatype.rules$RDatatype[cc])
          }

          if (is.null(RDatatype) == TRUE) {
            next
          }
          else if (RDatatype(data.NA[[a]]) == FALSE &
                   purrr::map(data.NA[a], class) != "logical") {
            warnings.df <- warnings.df %>%
              dplyr::add_row(
                Num = nrow(warnings.df) + 1,
                Variable = colnames(input.data[a]),
                Category = "dataType",
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

  ## MaxLength
  #!# R only stores up to 32,767 characters


  for (a in 1:ncol(data.NA)) {
    data.nchar <- NA
    MaxLength <- NA

    for (bb in 1:nrow(dict.datafield)) {
      for (cc in 1:nrow(datatype.rules)) {
        if (colnames(data.NA[a]) == dict.datafield$codeName[bb] &
            dict.datafield$dataType[bb] == datatype.rules$Value[cc] &
            is.na(datatype.rules$MaxLength[cc]) == FALSE) {
          # print(paste0("data.NA: ", colnames(data.NA[a])))
          # print(paste0("dict.datafield: ", dict.datafield$codeName[bb]))
          # print(paste0("datatype.rules: ", datatype.rules$Value[cc]))

          if (!NA %in% unique(data.NA[, a]) |
              NA %in% unique(data.NA[, a]) &
              length(unique(data.NA[, a])) > 1) {
            MaxLength <- max(nchar(as.character(data.NA[, a])), na.rm = TRUE)

            # print(MaxLength)

            if (is.na(MaxLength) == FALSE &
                MaxLength > datatype.rules$MaxLength[cc])  {
              warnings.df <- warnings.df %>%
                dplyr::add_row(
                  Num = nrow(warnings.df) + 1,
                  Variable = colnames(input.data[a]),
                  Category = "dataType",
                  Message =  paste0(
                    'Dataset variable has a greater max length (',
                    MaxLength,
                    ') than allowed for the datatype (',
                    datatype.rules$MaxLength[cc],
                    ')'
                  )
                )
            }

          }
        }
      }
    }
  }


  ## MaxPrecision
  #!# Need to test; Excel doesn't store > 15 sigfigs

  for (a in 1:ncol(data.NA)) {
    for (bb in 1:nrow(dict.datafield)) {
      for (cc in 1:nrow(datatype.rules)) {
        if (colnames(data.NA[a]) == dict.datafield$codeName[bb] &
            dict.datafield$dataType[bb] == datatype.rules$Value[cc] &
            is.na(datatype.rules$MaxPrecision[cc]) == FALSE &
            is.character(data.NA[, a]) == FALSE) {
          # print(paste0("data.NA: ", colnames(data.NA[a])))
          # print(paste0("dict.datafield: ", dict.datafield$codeName[bb]))
          # print(paste0("datatype.rules: ", datatype.rules$Value[cc]))

          if (!NA %in% unique(data.NA[, a]) |
              NA %in% unique(data.NA[, a]) &
              length(unique(data.NA[, a])) > 1) {
            MaxPrecision <-
              max(nchar(gsub(
                ".*\\.|^[^.]+$", "", as.character(data.NA[, a])
              )), na.rm = TRUE)

            # print(MaxPrecision)

            if (is.na(MaxPrecision) == FALSE &
                MaxPrecision > datatype.rules$MaxPrecision[cc]) {
              warnings.df <- warnings.df %>%
                dplyr::add_row(
                  Num = nrow(warnings.df) + 1,
                  Variable = colnames(input.data[a]),
                  Category = "dataType",
                  Message =  paste0(
                    'Dataset variable has a greater max precision (',
                    MaxPrecision,
                    ') than allowed for the datatype (',
                    datatype.rules$MaxPrecision[cc],
                    ')'
                  )
                )
            }

          }
        }
      }
    }
  }

  ## MinValue

  for (a in 1:ncol(data.NA)) {
    MinValue <- NA

    for (bb in 1:nrow(dict.datafield)) {
      for (cc in 1:nrow(datatype.rules)) {
        if (colnames(data.NA[a]) == dict.datafield$codeName[bb] &
            dict.datafield$dataType[bb] == datatype.rules$Value[cc] &
            is.na(datatype.rules$MinValue_unsigned[cc]) == FALSE &
            is.character(data.NA[, a]) == FALSE) {
          # print(paste0("data.NA: ", colnames(data.NA[a])))
          # print(paste0("dict.datafield: ", dict.datafield$codeName[bb]))
          # print(paste0("datatype.rules: ", datatype.rules$Value[cc]))

          if (!NA %in% unique(data.NA[, a]) |
              NA %in% unique(data.NA[, a]) &
              length(unique(data.NA[, a])) > 1) {
            MinValue <- min(unique(data.NA[, a]), na.rm = TRUE)

            # print(MinValue)

            if (is.na(MinValue) == FALSE &
                MinValue < datatype.rules$MinValue_unsigned[cc]) {
              warnings.df <- warnings.df %>%
                dplyr::add_row(
                  Num = nrow(warnings.df) + 1,
                  Variable = colnames(input.data[a]),
                  Category = "dataType",
                  Message =  paste0(
                    'Dataset variable has a smaller min value (',
                    MinValue,
                    ') than allowed for the datatype (',
                    datatype.rules$MinValue_unsigned[cc],
                    ')'
                  )
                )
            }

          }
        }
      }
    }
  }

  ## MaxValue

  for (a in 1:ncol(data.NA)) {
    MaxValue <- NA

    for (bb in 1:nrow(dict.datafield)) {
      for (cc in 1:nrow(datatype.rules)) {
        if (colnames(data.NA[a]) == dict.datafield$codeName[bb] &
            dict.datafield$dataType[bb] == datatype.rules$Value[cc] &
            is.na(datatype.rules$MaxValue_unsigned[cc]) == FALSE &
            is.character(data.NA[, a]) == FALSE) {
          # print(paste0("data.NA: ", colnames(data.NA[a])))
          # print(paste0("dict.datafield: ", dict.datafield$codeName[bb]))
          # print(paste0("datatype.rules: ", datatype.rules$Value[cc]))

          if (!NA %in% unique(data.NA[, a]) |
              NA %in% unique(data.NA[, a]) &
              length(unique(data.NA[, a])) > 1) {
            MaxValue <- max(unique(data.NA[, a]), na.rm = TRUE)

            # print(MaxValue)

            if (is.na(MaxValue) == FALSE &
                MaxValue > datatype.rules$MaxValue_unsigned[cc]) {
              warnings.df <- warnings.df %>%
                dplyr::add_row(
                  Num = nrow(warnings.df) + 1,
                  Variable = colnames(input.data[a]),
                  Category = "dataType",
                  Message =  paste0(
                    'Dataset variable has a greater max value (',
                    MaxValue,
                    ') than allowed for the datatype (',
                    datatype.rules$MaxValue_unsigned[cc],
                    ')'
                  )
                )
            }

          }
        }
      }
    }
  }


  ## DistinctValue

  for (a in 1:ncol(data.NA)) {
    DistinctValue <- NA

    for (bb in 1:nrow(dict.datafield)) {
      for (cc in 1:nrow(datatype.rules)) {
        if (colnames(data.NA[a]) == dict.datafield$codeName[bb] &
            dict.datafield$dataType[bb] == datatype.rules$Value[cc] &
            is.na(datatype.rules$DistinctValue[cc]) == FALSE) {
          # print(paste0("data.NA: ", colnames(data.NA[a])))
          # print(paste0("dict.datafield: ", dict.datafield$codeName[bb]))
          # print(paste0("datatype.rules: ", datatype.rules$Value[cc]))

          if (!NA %in% unique(data.NA[, a]) |
              NA %in% unique(data.NA[, a]) &
              length(unique(data.NA[, a])) > 1) {
            DistinctValue <- length(unique(na.omit(data.NA[, a])))

            # print(DistinctValue)

            if (is.na(DistinctValue) == FALSE &
                DistinctValue > datatype.rules$DistinctValue[cc]) {
              warnings.df <- warnings.df %>%
                dplyr::add_row(
                  Num = nrow(warnings.df) + 1,
                  Variable = colnames(input.data[a]),
                  Category = "dataType",
                  Message =  paste0(
                    'Dataset variable has a greater number of distinct values (',
                    DistinctValue,
                    ') than allowed for the datatype (',
                    datatype.rules$DistinctValue[cc],
                    ')'
                  )
                )
            }

          }
        }
      }
    }
  }


  ## DistinctLength

  for (a in 1:ncol(data.NA)) {
    DistinctLength <- NA

    for (bb in 1:nrow(dict.datafield)) {
      for (cc in 1:nrow(datatype.rules)) {
        if (colnames(data.NA[a]) == dict.datafield$codeName[bb] &
            dict.datafield$dataType[bb] == datatype.rules$Value[cc] &
            is.na(datatype.rules$DistinctLength[cc]) == FALSE) {
          # print(paste0("data.NA: ", colnames(data.NA[a])))
          # print(paste0("dict.datafield: ", dict.datafield$codeName[bb]))
          # print(paste0("datatype.rules: ", datatype.rules$Value[cc]))

          if (!NA %in% unique(data.NA[, a]) |
              NA %in% unique(data.NA[, a]) &
              length(unique(data.NA[, a])) > 1) {
            DistinctLength <- length(unique(nchar(na.omit(
              data.NA[, a]
            ))))

            # print(DistinctLength)

            if (is.na(DistinctLength) == FALSE &
                DistinctLength > datatype.rules$DistinctLength[cc]) {
              warnings.df <- warnings.df %>%
                dplyr::add_row(
                  Num = nrow(warnings.df) + 1,
                  Variable = colnames(input.data[a]),
                  Category = "dataType",
                  Message =  paste0(
                    'Dataset variable has values with more than one distinct length (',
                    DistinctLength,
                    ') while the datatype indicates values should be "fixed length"'
                  )
                )
            }

          }
        }
      }
    }
  }


  ##############################################################################

  # Check unitsResolution
  dict.res <- input.dict %>%
    dplyr::filter(!is.na(unitsResolution))

  data.res <- input.data %>%
    dplyr::select(any_of(dict.res$codeName))


  ### Remove trailing zeros
  #purrr::map_df(stringr::str_remove, pattern = "^0+")


  data.ndecimal <- NULL
  dict.ndecimal <- NULL


  for (a in 1:ncol(data.res)) {
    # print(colnames(data.res[a]))
    data.ndecimal <- NULL

    for (bb in 1:nrow(dict.res)) {
      if (colnames(data.res[a]) == dict.res$codeName[bb]) {
        data.ndecimal <-
          nchar(gsub(".*\\.|^[^.]+$", "", as.character(data.res[, a])))

        dict.ndecimal <-
          nchar(gsub(".*\\.",
                     "",
                     as.character(dict.res$unitsResolution[bb])))
      }
    }

    # print(paste0(min(data.ndecimal, na.rm = TRUE), " (min)"))
    # print(paste0(max(data.ndecimal, na.rm = TRUE), " (max)"))
    # print(paste0(dict.ndecimal, " (dictionary)"))

    if (is.null(data.ndecimal) == FALSE &
        min(data.ndecimal, na.rm = TRUE) < dict.ndecimal) {
      warnings.df <- warnings.df %>%
        dplyr::add_row(
          Num = nrow(warnings.df) + 1,
          Variable = colnames(input.data[a]),
          Category = "unitsResolution",
          Message =  paste0(
            'Dataset variable contains values with lower resolution than "unitsResolution" in dictionary'
          )
        )
    }

    if (is.null(data.ndecimal) == FALSE &
        max(data.ndecimal, na.rm = TRUE) > dict.ndecimal) {
      warnings.df <- warnings.df %>%
        dplyr::add_row(
          Num = nrow(warnings.df) + 1,
          Variable = colnames(input.data[a]),
          Category = "unitsResolution",
          Message =  paste0(
            'Dataset variable contains values with higher resolution than "unitsResolution" in dictionary'
          )
        )
    }

  }



  # Check fieldWidth
  dict.width <- input.dict %>%
    dplyr::filter(!is.na(fieldWidth))

  data.width <- input.data %>%
    dplyr::select(any_of(dict.width$codeName))


  data.nchar <- NULL
  dict.nchar <- NULL



  for (a in 1:ncol(data.width)) {
    # print(colnames(data.width[a]))
    data.nchar <- NULL

    for (bb in 1:nrow(dict.width)) {
      for (aa in 1:nrow(data.width)) {
        if (colnames(data.width[a]) == dict.width$codeName[bb]) {
          data.nchar <- c(data.nchar, nchar(as.character(data.width[aa, a])))
        }
      }
      if (dict.width$codeName[bb] == colnames(data.width[a])) {
        dict.nchar <- dict.width$fieldWidth[bb]
      }
    }

    # print(paste0(max(data.nchar, na.rm = TRUE)," (max)"))
    # print(paste0((dict.nchar)," (dictionary)"))


    if (is.null(data.nchar) == FALSE &
        max(data.nchar, na.rm = TRUE) > dict.nchar) {
      warnings.df <- warnings.df %>%
        dplyr::add_row(
          Num = nrow(warnings.df) + 1,
          Variable = colnames(input.data[a]),
          Category = "fieldWidth",
          Message =  paste0(
            'Dataset variable contains value that exceeds "fieldWidth" in dictionary'
          )
        )
    }

  }



  # Check missingValue
  dict.miss <- dict.datafield %>%
    dplyr::filter(!is.na(missingValue))

  data.miss <- input.data %>%
    dplyr::select(any_of(dict.miss$codeName))

  for (a in 1:ncol(data.miss)) {
    for (bb in 1:nrow(dict.miss))
      if (colnames(data.miss[a]) == dict.miss$codeName[bb] &
          NA %in% (unique(data.miss[, a]))) {
        warnings.df <- warnings.df %>%
          dplyr::add_row(
            Num = nrow(warnings.df) + 1,
            Variable = colnames(input.data[a]),
            Category = "missingValue",
            Message =  paste0(
              'Dataset variable contains blank values rather than "missingValue" in dictionary: ',
              dict.miss$missingValue[bb]
            )
          )
      }
  }


  # Check minValue
  dict.min <- input.dict %>%
    dplyr::filter(!is.na(minValue))

  data.min <- input.data %>%
    dplyr::select(any_of(dict.min$codeName))


  for (a in 1:ncol(data.min)) {
    for (bb in 1:nrow(dict.min))
      if (colnames(data.min[a]) == dict.min$codeName[bb] &
          min(unique(data.min[a]), na.rm = TRUE) < dict.min$minValue[bb]) {
        warnings.df <- warnings.df %>%
          dplyr::add_row(
            Num = nrow(warnings.df) + 1,
            Variable = colnames(input.data[a]),
            Category = "minValue",
            Message =  paste0(
              'Dataset variable contains value less than "minValue" in dictionary'
            )
          )
      }

  }



  # Check maxValue
  dict.max <- input.dict %>%
    dplyr::filter(!is.na(minValue))

  data.max <- input.data %>%
    dplyr::select(any_of(dict.max$codeName))


  for (a in 1:ncol(data.max)) {
    for (bb in 1:nrow(dict.max))
      if (colnames(data.max[a]) == dict.max$codeName[bb] &
          max(unique(data.max[a]), na.rm = TRUE) > dict.max$maxValue[bb]) {
        warnings.df <- warnings.df %>%
          dplyr::add_row(
            Num = nrow(warnings.df) + 1,
            Variable = colnames(input.data[a]),
            Category = "maxValue",
            Message =  paste0(
              'Dataset variable contains value greater than "maxValue" in dictionary'
            )
          )
      }

  }

  assign("all.warnings", warnings.df)

}
