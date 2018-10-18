#' word cloud text geoms
#'
#' \code{geom_text_wordcloud} adds text to the plot using a variation of the
#' wordcloud2.js algorithm. The texts are layered around a spiral centered on
#' the original position. This geom is based on
#' \code{\link[ggrepel]{geom_text_repel}} which in turn is based on
#' \code{\link[ggplot2]{geom_text}}. See the documentation for those functions
#' for more details. By default, the font size is directly related to the size
#' aesthetic. \code{geom_text_wordcloud_area} is an alias, with a different set
#' of default, that chooses a font size so that the area of the text is now
#' related to the size aesthetic.

#' @param mapping Set of aesthetic mappings created by
#'   \code{\link[ggplot2]{aes}} or \code{\link[ggplot2]{aes_}}. If specified and
#'   \code{inherit.aes = TRUE} (the default), is combined with the default
#'   mapping at the top level of the plot. You only need to supply
#'   \code{mapping} if there isn't a mapping defined for the plot. Note that if
#'   not specified both x and y are set to 0.5, i.e. the middle of the default
#'   panel. Two non classic aesthetics are defined \code{angle_group} and
#'   \code{mask_group} which define groups used respectively to use different
#'   angular sector and different masks in the word cloud.
#' @param inherit.aes Inherits aesthetics if TRUE
#' @param na.rm Remove missing values if TRUE
#' @param data A data frame. If specified, overrides the default data frame
#'   defined at the top level of the plot.
#' @param stat The statistical transformation to use on the data for this layer,
#'   as a string.
#' @param position Position adjustment, either as a string, or the result of a
#'   call to a position adjustment function.
#' @param parse If \code{TRUE}, the labels will be parsed into expressions and
#'   displayed as described in ?plotmath
#' @param ... other arguments passed on to \code{\link[ggplot2]{layer}}. There
#'   are three types of arguments you can use here: \itemize{ \item Aesthetics:
#'   to set an aesthetic to a fixed value, like \code{colour = "red"} or
#'   \code{size = 3}. \item Other arguments to the layer, for example you
#'   override the default \code{stat} associated with the layer. \item Other
#'   arguments passed on to the stat. }
#' @param show.legend is set by default to \code{FALSE}
#' @param nudge_x,nudge_y Horizontal and vertical adjustments to nudge the
#'   starting position of each text label.
#' @param xlim,ylim Limits for the x and y axes. Text labels will be constrained
#'   to these limits. By default, text labels are constrained to the entire plot
#'   area.
#' @param eccentricity eccentricity of the spiral. Default to .65
#' @param rstep relative wordclould spiral radius increment after one full
#'   rotation. Default to .01.
#' @param tstep wordclould spiral angle increment at each step. Default to .02.
#' @param perc_step parameter used to define the minimal distance between two
#'   successive candidate positions on the ellipse. Default to .01
#' @param max_steps maximum number of steps avoided thanks to this minimal
#'   criterion. Default to 10. Set to 1 to recover the previous behavior
#' @param grid_size grid size used when creating the text bounding boxes.
#'   Default to 4
#' @param max_grid_size maximum size of the bounding boxes. Default to 128
#' @param grid_margin safety margin around the texts. Default to 1.
#' @param seed Random seed passed to \code{set.seed}. Defaults to \code{NA},
#'   which means that \code{set.seed} will not be called.
#' @param rm_outside Remove the texts that could not be fitted. Default to
#'   \code{FALSE}
#' @param shape select the shape of the clouds among \code{circle},
#'   \code{cardioid}, \code{diamond}, \code{square}, \code{triangle-forward},
#'   \code{triangle-upright}, \code{pentagon}, \code{star}. Default to
#'   \code{circle}
#' @param area_corr Set the font size so that the area is proportional to size
#'   aesthetic raised to a certain power when the scale_size_area is used. As
#'   this is not the classical choice, the default is \code{FALSE} so that, by
#'   default, the length of the text is not taken into account.
#'   \code{geom_text_wordcloud_area} set this to \code{TRUE} by default.
#' @param area_corr_power the power used in the area correction. Default to 1/.7
#'   to match human perception.
#' @param mask a mask (or a list of masks) used to define a zone in which the
#'   text should be placed. Each mask should be coercible to a raster in which
#'   the color "black" defined the text zone. When a list of masks is given, the
#'   mask_group aesthetic defines which mask is going to be used. Default to
#'   \code{NA}, i.e. no mask.
#'
#' @return a ggplot
#'
#' @examples
#' set.seed(42)
#' dat <- mtcars
#' dat$name <- row.names(mtcars)
#' dat$size <- dat$mpg
#' dat$size[1] <- 300
#' ggplot(data = dat, aes(size = size, label = name)) + geom_text_wordcloud() +
#'   theme_minimal()
#' dat$angle <- (-90+180*runif(nrow(dat)))*(runif(nrow(dat)>.6))
#' ggplot(data = dat, aes(size = size, label = name, angle = angle)) +
#'  geom_text_wordcloud() + theme_minimal()
#' @export
geom_text_wordcloud <- function(mapping = NULL, data = NULL,
                                stat = "identity", position = "identity",
                                ...,
                                parse = FALSE,
                                nudge_x = 0,
                                nudge_y = 0,
                                eccentricity = 0.65,
                                rstep = .01,
                                tstep = .02,
                                perc_step = .01,
                                max_steps = 10,
                                grid_size = 4,
                                max_grid_size = 128,
                                grid_margin = 1,
                                xlim = c(NA, NA),
                                ylim = c(NA, NA),
                                seed = NA,
                                rm_outside = FALSE,
                                shape = "circle",
                                mask = NA,
                                area_corr = FALSE,
                                area_corr_power = 1 / .7,
                                na.rm = FALSE,
                                show.legend = FALSE,
                                inherit.aes = TRUE) {
  if (!missing(nudge_x) || !missing(nudge_y)) {
    if (!missing(position)) {
      stop("You must specify either `position` or `nudge_x`/`nudge_y`.", call. = FALSE)
    }

    position <- position_nudge(nudge_x, nudge_y)
  }

  if (is.character(shape)) {
    shape <- which(c(
      "circle", "cardioid", "diamond",
      "square", "triangle-forward", "triangle-upright",
      "pentagon", "star"
    ) == shape)
    if (length(shape) != 1) {
      shape <- NA_integer_
    }
  } else {
    shape <- as.integer(shape)
  }
  if (is.na(shape) || shape < 0 || shape > 8) {
    warning("shape invalid. Using the default circle shape instead.")
    shape <- 1L
  }

  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomTextWordcloud,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      parse = parse,
      eccentricity = eccentricity,
      rstep = rstep,
      tstep = tstep,
      perc_step = perc_step,
      max_steps = max_steps,
      grid_size = grid_size,
      max_grid_size = max_grid_size,
      grid_margin = grid_margin,
      xlim = xlim,
      ylim = ylim,
      seed = seed,
      rm_outside = rm_outside,
      shape = shape,
      mask = mask,
      area_corr = area_corr,
      area_corr_power = area_corr_power,
      ...
    )
  )
}

#' @rdname geom_text_wordcloud
#' @export
geom_text_wordcloud_area <- function(mapping = NULL, data = NULL,
                                     stat = "identity", position = "identity",
                                     ...,
                                     parse = FALSE,
                                     nudge_x = 0,
                                     nudge_y = 0,
                                     eccentricity = 0.65,
                                     rstep = .01,
                                     tstep = .02,
                                     perc_step = .01,
                                     max_steps = 10,
                                     grid_size = 4,
                                     max_grid_size = 128,
                                     grid_margin = 1,
                                     xlim = c(NA, NA),
                                     ylim = c(NA, NA),
                                     seed = NA,
                                     rm_outside = FALSE,
                                     shape = "circle",
                                     mask = NA,
                                     area_corr = TRUE,
                                     area_corr_power = 1 / .7,
                                     na.rm = FALSE,
                                     show.legend = FALSE,
                                     inherit.aes = TRUE) {
  if (!missing(nudge_x) || !missing(nudge_y)) {
    if (!missing(position)) {
      stop("You must specify either `position` or `nudge_x`/`nudge_y`.", call. = FALSE)
    }

    position <- position_nudge(nudge_x, nudge_y)
  }

  if (is.character(shape)) {
    shape <- which(c(
      "circle", "cardioid", "diamond",
      "square", "triangle-forward", "triangle-upright",
      "pentagon", "star"
    ) == shape)
    if (length(shape) != 1) {
      shape <- NA_integer_
    }
  } else {
    shape <- as.integer(shape)
  }
  if (is.na(shape) || shape < 0 || shape > 8) {
    warning("shape invalid. Using the default circle shape instead.")
    shape <- 1L
  }

  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomTextWordcloud,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      parse = parse,
      eccentricity = eccentricity,
      rstep = rstep,
      tstep = tstep,
      perc_step = perc_step,
      max_steps = max_steps,
      grid_size = grid_size,
      max_grid_size = max_grid_size,
      grid_margin = grid_margin,
      xlim = xlim,
      ylim = ylim,
      seed = seed,
      rm_outside = rm_outside,
      shape = shape,
      mask = mask,
      area_corr = area_corr,
      area_corr_power = area_corr_power,
      ...
    )
  )
}


GeomTextWordcloud <- ggproto("GeomTextWordcloud", Geom,
  required_aes = c("label"),

  default_aes = aes(
    x = 0.5, y = 0.5,
    colour = "black", size = 3.88, angle = 0, hjust = 0.5,
    vjust = 0.5, alpha = NA, family = "", fontface = 1, lineheight = 1.2, mask_group = 1L, angle_group = 1L
  ),

  setup_data = function(data, params) {
    if (params$area_corr) {
      dev_inch <- dev.size("in")
      dev_pix <- dev.size("px")
      dev_dpi <- dev_pix[1] / dev_inch[1]
      if (is.null(data$size)) {
        data$size <- 3.88
      }
      newsize <- lapply(
        seq_along(data$label),
        compute_newsize, data, dev_dpi, params$area_corr_power
      )
      data$size <- unlist(newsize)
    }
    if (is.null(data$angle_group)) {
      data$max_angle_group <- 1L
      data$angle_group <- 1L
    } else {
      data$max_angle_group <- length(levels(as.factor(data$angle_group)))
      data$angle_group <- as.numeric(as.factor(data$angle_group))
    }
    if (is.null(data$mask_group)) {
      data$mask_group <- 1L
    } else {
      data$mask_group <- as.numeric(as.factor(data$mask_group))
    }
    data
  },

  draw_panel = function(data, panel_params, coord,
                          parse = FALSE,
                          eccentricity = 0.65,
                          rstep = .01,
                          tstep = .02,
                          perc_step = .01,
                          max_steps = 10,
                          grid_size = 4,
                          max_grid_size = 128,
                          grid_margin = 1,
                          xlim = c(NA, NA),
                          ylim = c(NA, NA),
                          seed = NA,
                          rm_outside = FALSE,
                          shape = "circle",
                          mask = NA,
                          area_corr = FALSE,
                          area_corr_power = 1 / .7) {
    lab <- data$label
    if (parse) {
      lab <- parse_safe(as.character(lab))
    }

    data <- coord$transform(data, panel_params)
    if (is.character(data$vjust)) {
      data$vjust <- compute_just(data$vjust, data$y)
    }
    if (is.character(data$hjust)) {
      data$hjust <- compute_just(data$hjust, data$x)
    }

    # Transform limits to panel scales.
    limits <- data.frame(x = xlim, y = ylim)
    limits <- coord$transform(limits, panel_params)

    # Fill NAs with defaults.
    limits$x[is.na(limits$x)] <- c(0, 1)[is.na(limits$x)]
    limits$y[is.na(limits$y)] <- c(0, 1)[is.na(limits$y)]

    gTree(
      limits = limits,
      data = data,
      lab = lab,
      eccentricity = eccentricity,
      rstep = rstep,
      tstep = tstep,
      perc_step = perc_step,
      max_steps = max_steps,
      grid_size = grid_size,
      max_grid_size = max_grid_size,
      grid_margin = grid_margin,
      seed = seed,
      rm_outside = rm_outside,
      shape = shape,
      mask = mask,
      area_corr = area_corr,
      area_corr_power = area_corr_power,
      cl = "textwordcloudtree",
      name = "geom_text_wordcloud"
    )
  },

  draw_key = draw_key_text
)

#' @export
makeContent.textwordcloudtree <- function(x) {
  # Do not create text labels for empty strings.
  valid_strings <- which(not_empty(x$lab))
  invalid_strings <- which(!not_empty(x$lab))


  # Compute the native/pixel ratio
  dev_inch <- dev.size("in")
  dev_pix <- dev.size("px")
  dev_dpi <- dev_pix[1] / dev_inch[1]
  gw_ratio <- as.numeric(convertWidth(unit(1 / dev_dpi, "inch"), "native"))
  gh_ratio <- as.numeric(convertHeight(unit(1 / dev_dpi, "inch"), "native"))

  grid_size <- max(floor(x$grid_size), 1)
  max_grid_size <- max(floor(x$max_grid_size), grid_size)
  grid_margin <- max(floor(x$grid_margin), 0)

  if ((length(x$mask) <= 1) && is.na(x$mask)) {
    boxes_masks <- list()
  } else {
    if (!is.list(x$mask)) {
      mask <- list(x$mask)
    } else {
      mask <- x$mask
    }
    boxes_masks <- lapply(
      mask, compute_mask_boxes, dev_dpi,
      grid_size, max_grid_size, grid_margin,
      gw_ratio, gh_ratio, dev_inch
    )
  }
  if (length(boxes_masks) > 0) {
    boxes_masks_nb <- sapply(boxes_masks, nrow)
    boxes_masks_start <- cumsum(boxes_masks_nb)
    mask_boxes <- cbind(c(0, boxes_masks_start[-length(boxes_masks_start)]), boxes_masks_start)
    boxes_mask <- rep(0:(length(boxes_masks_nb) - 1), boxes_masks_nb)
    boxes_masks <- do.call(rbind, boxes_masks)

    mask_group <- x$data$mask_group[valid_strings]
    mask_group <- mask_group - 1
    if (max(mask_group) >= nrow(mask_boxes)) {
      warnings("Less masks than groups please check if this is correct")
      mask_group <- mask_group %% nrow(mask_boxes)
    }
  } else {
    boxes_masks_nb <- vector("integer")
    mask_boxes <- array(0, dim = c(0, 2))
    boxes_mask <- vector("integer")
    boxes_masks <- array(0, dim = c(1, 4))
    mask_group <- rep(0L, length(valid_strings))
  }

  angle_group <- x$data$angle_group[valid_strings] - 1L

  boxes <- lapply(
    valid_strings, compute_text_boxes, x, dev_dpi,
    grid_size, max_grid_size, grid_margin, gw_ratio, gh_ratio
  )
  if (length(boxes) > 0) {
    boxes_nb <- sapply(boxes, nrow)
    bigboxes <- lapply(boxes, function(box) {
      c(min(box[, 1]), min(box[, 2]), max(box[, 3]), max(box[, 4]))
    })
    boxes_start <- cumsum(boxes_nb)
    text_boxes <- cbind(c(0, boxes_start[-length(boxes_start)]), boxes_start)
    boxes_text <- rep(0:(length(boxes_nb) - 1), boxes_nb)
    boxes <- do.call(rbind, boxes)
    bigboxes <- do.call(rbind, bigboxes)
  } else {
    boxes_nb <- vector("integer")
    text_boxes <- array(0, dim = c(0, 2))
    boxes_text <- vector("integer")
    boxes <- array(0, dim = c(0, 4))
    bigboxes <- array(0, dim = c(0, 4))
  }

  # Make the result reproducible if desired.
  if (is.null(x$seed) || !is.na(x$seed)) {
    set.seed(x$seed)
  }

  points_valid_first <- cbind(
    c(
      x$data$x[valid_strings],
      x$data$x[invalid_strings]
    ),
    c(
      x$data$y[valid_strings],
      x$data$y[invalid_strings]
    )
  )

  wordcloud <- wordcloud_boxes(
    data_points = points_valid_first,
    boxes = boxes,
    boxes_text = boxes_text,
    text_boxes = text_boxes,
    bigboxes = bigboxes,
    boxes_masks = boxes_masks,
    boxes_mask = boxes_mask,
    mask_boxes = mask_boxes,
    mask_group = mask_group,
    angle_group = angle_group,
    max_angle_group = x$data$max_angle_group[1],
    xlim = range(x$limits$x),
    ylim = range(x$limits$y),
    eccentricity = x$eccentricity,
    rstep = x$rstep,
    tstep = x$tstep,
    perc_step = x$perc_step,
    max_steps = x$max_steps,
    rm_outside = x$rm_outside,
    shape = x$shape
  )

  grobs <- lapply(seq_along(valid_strings), make_textgrob, x, valid_strings, wordcloud)
  class(grobs) <- "gList"

  setChildren(x, grobs)
}

# Copied from ggplot2
compute_just <- function(just, x) {
  inward <- just == "inward"
  just[inward] <- c("left", "middle", "right")[just_dir(x[inward])]
  outward <- just == "outward"
  just[outward] <- c("right", "middle", "left")[just_dir(x[outward])]

  unname(c(
    left = 0, center = 0.5, right = 1,
    bottom = 0, middle = 0.5, top = 1
  )[just])
}

# Copied from ggplot2
just_dir <- function(x, tol = 0.001) {
  out <- rep(2L, length(x))
  out[x < 0.5 - tol] <- 1L
  out[x > 0.5 + tol] <- 3L
  out
}

compute_mask <- function(tg_inch, gw_pix, gh_pix, dev_dpi, f_mask) {
  prev_dev_id <- dev.cur()
  dev_id <- Cairo(width = gw_pix, height = gh_pix, dpi = dev_dpi, units = "px", type = "raster")
  pushViewport(grid::viewport(width = 1, height = 1))
  grid.draw(tg_inch)
  popViewport()
  img <- grid.cap()
  dev.off()
  dev.set(prev_dev_id)
  f_mask(img)
}

compute_newsize <- function(i, data, dev_dpi, area_corr_power) {
  row <- data[i, , drop = FALSE]

  tg_inch <- textGrob(
    row$label,
    0, 0,
    default.units = "inch",
    gp = gpar(
      fontsize = 20 * .pt,
      fontfamily = ifelse(is.null(row$family), "", row$family),
      fontface = ifelse(is.null(row$fontface), 1, row$fontface),
      lineheight = ifelse(is.null(row$lineheight), 1.2, row$lineheight)
    )
  )

  gw_inch <- convertWidth(grobWidth(tg_inch), "inch", TRUE) * 1.2
  gh_inch <- convertHeight(grobHeight(tg_inch), "inch", TRUE) * 1.2 + 2 * convertHeight(grobDescent(tg_inch), "inch", TRUE)

  gw_pix <- max(1, ceiling(gw_inch * dev_dpi))
  gh_pix <- max(1, ceiling(gh_inch * dev_dpi))

  tg_inch <- textGrob(
    row$label,
    gw_inch / 2, gh_inch / 2,
    default.units = "inch",
    gp = gpar(
      fontsize = 20 * .pt,
      fontfamily = ifelse(is.null(row$family), "", row$family),
      fontface = ifelse(is.null(row$fontface), 1, row$fontface),
      lineheight = ifelse(is.null(row$lineheight), 1.2, row$lineheight)
    )
  )

  # Compute the text mask
  mask <- compute_mask(tg_inch, gw_pix, gh_pix, dev_dpi, function(img) (img != "transparent"))
  area <- sum(mask)
  if (area > 0) {
    row$size^(area_corr_power) / sqrt(area)
  } else {
    NA_real_
  }
}

compute_mask_boxes <- function(mask_matrix, dev_dpi, grid_size, max_grid_size, grid_margin,
                               gw_ratio, gh_ratio, dev_inch) {
  mask_raster <- rasterGrob(mask_matrix,
    x = unit(0.5, "native"), y = unit(0.5, "native")
  )

  gw_inch <- convertWidth(grobWidth(mask_raster), "inch", TRUE)
  gh_inch <- convertHeight(grobHeight(mask_raster), "inch", TRUE)

  gw_native <- convertWidth(grobWidth(mask_raster), "native", TRUE)
  gh_native <- convertHeight(grobHeight(mask_raster), "native", TRUE)


  gw_pix <- max(1, ceiling(gw_inch / gw_native * dev_dpi / grid_size)) * grid_size
  gh_pix <- max(1, ceiling(gh_inch / gh_native * dev_dpi / grid_size)) * grid_size

  # Compute the mask mask
  mask <- compute_mask(
    mask_raster, gw_pix, gh_pix, dev_dpi,
    function(img) {
      img != "black"
    }
  )

  compute_boxes_from_mask(
    mask, gw_pix, gh_pix, gw_ratio, gh_ratio,
    grid_size, max_grid_size,
    0, 0, 0
  )
}

compute_text_boxes <- function(i, x, dev_dpi, grid_size, max_grid_size, grid_margin,
                               gw_ratio, gh_ratio) {
  row <- x$data[i, , drop = FALSE]
  hj <- x$data$hjust[i]
  vj <- x$data$vjust[i]

  tg_inch <- textGrob(
    x$lab[i],
    0, 0,
    default.units = "inch",
    rot = row$angle,
    just = c(hj, vj),
    gp = gpar(
      fontsize = row$size * .pt,
      fontfamily = row$family,
      fontface = row$fontface,
      lineheight = row$lineheight
    )
  )

  gw_inch <- convertWidth(grobWidth(tg_inch), "inch", TRUE) * 1.2
  gh_inch <- convertHeight(grobHeight(tg_inch), "inch", TRUE) * 1.2 + 2 * convertHeight(grobDescent(tg_inch), "inch", TRUE)

  gw_pix <- max(1, ceiling(gw_inch * dev_dpi / grid_size)) * grid_size
  gh_pix <- max(1, ceiling(gh_inch * dev_dpi / grid_size)) * grid_size

  tg_inch <- textGrob(
    x$lab[i],
    gw_inch / 2, gh_inch / 2,
    default.units = "inch",
    rot = row$angle,
    just = c(hj, vj),
    gp = gpar(
      fontsize = row$size * .pt,
      fontfamily = row$family,
      fontface = row$fontface,
      lineheight = row$lineheight
    )
  )

  # Compute the text mask
  mask <- compute_mask(
    tg_inch, gw_pix, gh_pix, dev_dpi,
    function(img) {
      img != "transparent"
    }
  )

  compute_boxes_from_mask(
    mask, gw_pix, gh_pix, gw_ratio, gh_ratio,
    grid_size, max_grid_size,
    grid_margin, gw_pix / 2, gh_pix / 2
  )
}

compute_boxes_from_mask <- function(mask, gw_pix, gh_pix, gw_ratio, gh_ratio, grid_size, max_grid_size, grid_margin, delta_w, delta_h) {
  max_grid_w <- ceiling(gw_pix / grid_size) * grid_size
  max_grid_h <- ceiling(gh_pix / grid_size) * grid_size
  seq_grid_w <- seq.int(1, max_grid_w, grid_size)
  seq_grid_h <- max_grid_h - seq.int(1, max_grid_h, grid_size) + 1

  mask_lists <- array(0, c(0, 4))

  mask_s <- mask[seq_grid_h, seq_grid_w, drop = FALSE]

  for (j in (-grid_margin):(grid_size + grid_margin - 1)) {
    for (i in (-grid_margin):(grid_size + grid_margin - 1)) {
      mask_s <- mask_s | mask[
        pmin(pmax(1, -j + seq_grid_h), gh_pix),
        pmin(pmax(1, i + seq_grid_w), gw_pix),
        drop = FALSE
      ]
    }
  }
  cur_mask <- mask_s

  step <- 2^c(0:max(0, floor(log2(max_grid_size / grid_size))))

  for (st in step) {
    if (st != max(step)) {
      next_mask <- cur_mask[seq(1, nrow(cur_mask), 2), seq(1, ncol(cur_mask), 2), drop = FALSE]
      for (j in 0:1) {
        for (i in 0:1) {
          next_mask <- next_mask &
            cur_mask[pmin(pmax(1, i + seq(1, nrow(cur_mask), 2)), nrow(cur_mask)),
              pmin(pmax(1, j + seq(1, ncol(cur_mask), 2)), ncol(cur_mask)),
              drop = FALSE
            ]
        }
      }

      mask_ind <- which(next_mask, arr.ind = TRUE)
      if (length(mask_ind) > 0) {
        for (ind in 1:nrow(mask_ind)) {
          cur_mask[
            pmin(pmax(1, 2 * (mask_ind[ind, 1] - 1) + (1:2)), nrow(cur_mask)),
            pmin(pmax(1, 2 * (mask_ind[ind, 2] - 1) + (1:2)), ncol(cur_mask))
          ] <- FALSE
        }
      }
    }

    mask_ind <- which(cur_mask, arr.ind = TRUE)
    if (length(mask_ind) > 0) {
      mask_list <- array(0, dim = c(nrow(mask_ind), 4))
      mask_list[, 1] <- (st * (mask_ind[, 2] - 1) * grid_size - delta_w) * gw_ratio
      mask_list[, 2] <- (st * (mask_ind[, 1] - 1) * grid_size - delta_h) * gh_ratio
      mask_list[, 3] <- pmin(mask_list[, 1] + st * grid_size * gw_ratio, (gw_pix - delta_w) * gw_ratio)
      mask_list[, 4] <- pmin(mask_list[, 2] + st * grid_size * gh_ratio, (gh_pix - delta_h) * gh_ratio)
      mask_lists <- rbind(mask_lists, mask_list)
    }

    cur_mask <- next_mask
  }

  mask_lists
}

make_textgrob <- function(i, x, valid_strings, wordcloud) {
  xi <- valid_strings[i]
  row <- x$data[xi, , drop = FALSE]
  # browser()
  textGrob(
    x$lab[xi],
    # Position of text bounding boxes.
    x = unit(wordcloud$x[i], "native"),
    y = unit(wordcloud$y[i], "native"),
    rot = row$angle,
    gp = gpar(
      col = alpha(row$colour, row$alpha),
      fontsize = row$size * .pt,
      fontfamily = row$family,
      fontface = row$fontface,
      lineheight = row$lineheight
    ),
    hjust = x$data$hjust[i],
    vjust = x$data$vjust[i]
  )
}
