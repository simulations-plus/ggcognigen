# Copyright 2020-$date Cognigen Corporation, a Simulations Plus Company

#' @rdname geom_barcount
#' @inheritParams ggplot2::stat_identity
#' @export

stat_barcount <- function(
  mapping = NULL,
  data = NULL,
  geom = "barcount",
  position = "stack",
  ...,
  overall.stack = TRUE,
  width = NULL,
  na.rm = FALSE,
  orientation = NA,
  show.legend = NA,
  inherit.aes = TRUE) {

  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = StatBarcount,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      overall.stack = overall.stack,
      na.rm = na.rm,
      orientation = orientation,
      position = position,
      width = width,
      ...
    )
  )
}

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export

StatBarcount <- ggplot2::ggproto(
  "StatBarcount",
  StatCount2,
  required_aes = "x|y",

  setup_params = function(data, params) {

    params$flipped_aes <- has_flipped_aes(data, params, range_is_orthogonal = FALSE)

    has_x <- !(is.null(data$x) && is.null(params$x))
    has_y <- !(is.null(data$y) && is.null(params$y))

    if (!has_x && !has_y) {
      rlang::abort("stat_count() requires an x or y aesthetic.")
    }
    params

  },
  compute_panel = function(data, scales, position, overall.stack = TRUE, width = NULL, flipped_aes = FALSE) {

    position <- ggplot2:::check_subclass(position, 'Position', env = parent.frame())
    position_class <- class(position)[1]
    reverse <- position$reverse

    data <- flip_data(data, flipped_aes)
    data$flipped_aes <- flipped_aes

    data <- data[order(data$x, data$group), ]

    if ( is.null(data$y) ){
      data$y <- data$weight %||% rep(1, length(data$x))
      tmp <- data[!duplicated(data[, c('x', 'group')]), ]
      tmp.y <- as.numeric(unlist(tapply(data$y, list(data$x, data$group), sum)))
      tmp$y <- tmp.y[!is.na(tmp.y)]
      data <- tmp
    }
    data <- data[!is.na(data$x) & !is.na(data$y), ]
    x <- data$x
    y <- data$y
    y[which(is.na(data$y))] <- 0
    data$width <- width %||% (resolution(x) * 0.9)

    if ( all(data$group < 0) ){
      has_group <- FALSE
      data$group_unique <- 1:nrow(data)
    } else {
      has_group <- TRUE
      data$group_unique <- data$group
    }

    if ( position_class == 'PositionFill' | position_class == 'PositionFillpercent' |
         ( position_class == 'PositionStack' & overall.stack == FALSE)){
      mid_cumsum <- function(index, count, group, reverse = FALSE){
        if ( all(data$group < 0) ){
          x <- count[index]
        } else {
          x <- count[index]
          group <- group[index]
          if ( reverse ){
            x <- x[ do.call(order, list(-group, -seq_along(x))) ]
            sort_back <- seq_along(x)[ do.call(order, list(-group, -seq_along(x))) ]
          } else {
            x <- x[ do.call(order, list(-group, seq_along(x))) ]
            sort_back <- seq_along(x)[ do.call(order, list(-group, seq_along(x))) ]
          }
        }
        if ( reverse ){
          x <- rev(x)
        }
        xx <- cumsum(x)
        xx <- 0.5 * (xx + c(0, xx[1:(length(xx) - 1)]))
        xx <- c(xx[1], diff(xx))
        if ( reverse ){
          xx <- rev(xx)
        }
        if (has_group){
          xx [ order(sort_back) ]
        } else{
          xx
        }
      }
      counts <- data
      counts$SORT_counts <- 1:nrow(counts)
      ymax <- data.frame(
        x = unique(data$x),
        y_max = as.numeric(unlist(tapply(y, x, sum)))
      )
      counts <- merge(counts, ymax, by = 'x')
      counts <- counts[order(counts$SORT_counts), ]
      if ( position_class == 'PositionStack' ){
        count <- counts$y
      } else {
        count <- counts$y / counts$y_max
      }
      counts$y <- as.numeric(
        unlist(
          tapply(seq_along(count), x, mid_cumsum, count, data$group_unique, reverse)
        )
      )
      adjustment <- counts[!duplicated(counts$x), ]
      adjustment$SORT_counts <- seq(
        from = 1 + max(counts$SORT_counts),
        length.out = length(unique(data$x))
      )
      if ( reverse ){
        adjustment$group <- +Inf
      }
      if ( position_class == 'PositionStack' ){
        adjustment$y <- as.numeric(unlist(tapply(y, x, sum))) - as.numeric(unlist(tapply(counts$y, x, sum)))
      } else {
        adjustment$y <- 1 - as.numeric(unlist(tapply(counts$y, x, sum)))
      }
      counts <- rbind(counts, adjustment)
      if ( position_class == 'PositionFill' ){
        count <- c(
          ifelse(
            count == 0,
            rep('', length(count)),
            signif(count, 3)
          ),
          rep('', nrow(adjustment))
        )
      } else if ( position_class == 'PositionFillpercent' ){
        count <- c(
          ifelse(
            count == 0,
            rep('', length(count)),
            sprintf('%s %%', signif(100*count, 3))
          ),
          rep('', nrow(adjustment))
        )
      } else {
        count <- c(
          ifelse(
            count == 0,
            rep('', length(count)),
            count
          ),
          rep('', nrow(adjustment))
        )
      }
      sort_index <- do.call(order, list(counts$x, counts$SORT_counts))
      counts <- counts[sort_index,]
      count <- count[sort_index]

    } else if ( position_class == 'PositionStack' ){
      counts <- data[!duplicated(data$x), ]
      count <- as.numeric(unlist(tapply(y, x, sum)))
      counts$y <- count + 0.05 * max(count)
    } else if ( position_class %in% c('PositionDodge', 'PositionDodge2') ) {
      counts <- data[!duplicated(data[, c('x', 'group_unique')]), ]
      count <- aggregate(y, list(x, data$group_unique), sum, na.rm = TRUE)
      count <- as.numeric(count[order(count[, 1], count[, 2]), 3])
      counts$y <- count + 0.05 * max(count)
    } else {
      counts$y <- NA
      count <- NA
    }

    counts$SORT_counts <- counts$group_unique <- counts$y_max <- NULL
    counts$count <- counts$y # Required when y is not in data (provide placement of label)
    counts$label <- count
    flip_data(counts, flipped_aes)
  }

)

