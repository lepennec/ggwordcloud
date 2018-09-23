#' Wordcloud text geom.
#'
#' \code{geom_text_wordcloud} adds text to the plot using a variation of the wordcloud2.js algorithm.
#'
#' This geom is based on \code{\link[ggrepel]{geom_text_repel}} which in turn is
#' based on \code{\link[ggplot2]{geom_text}}. See the documentation for those
#' functions for more details.
#' @format NULL
#' @usage NULL
#' @export
geom_text_wordcloud <- function(mapping = NULL, data = NULL,
                      stat = "identity", position = "identity",
                      ...,
                      parse = FALSE,
                      nudge_x = 0,
                      nudge_y = 0,
                      eccentricity = 0.65,
                      rstep = .05,
                      tstep = .05,
                      grid_size = 8,
                      xlim = c(NA, NA),
                      ylim = c(NA, NA),
                      seed = NA,
                      na.rm = FALSE,
                      show.legend = NA,
                      inherit.aes = TRUE)
{
  if (!missing(nudge_x) || !missing(nudge_y)) {
    if (!missing(position)) {
      stop("You must specify either `position` or `nudge_x`/`nudge_y`.", call. = FALSE)
    }

    position <- position_nudge(nudge_x, nudge_y)
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
      grid_size = grid_size,
      xlim = xlim,
      ylim = ylim,
      seed = seed,
      ...
    )
  )
}

#' @export
GeomTextWordcloud <- ggproto("GeomTextWordcloud", Geom,
                             required_aes = c("x", "y", "label"),

                             default_aes = aes(
                               colour = "black", size = 3.88, angle = 0, hjust = 0.5,
                               vjust = 0.5, alpha = NA, family = "", fontface = 1, lineheight = 1.2
                             ),

                             draw_panel = function(data, panel_params, coord,
                                                   parse = FALSE,
                                                   eccentricity = 0.65,
                                                   rstep = .05,
                                                   tstep = .1,
                                                   grid_size = 4,
                                                   xlim = c(NA, NA),
                                                   ylim = c(NA, NA),
                                                   seed = NA
                             ) {
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
                                 grid_size = grid_size,
                                 seed = seed,
                                 cl = "textwordcloudtree",
                                 name = "geom_text_wordcloud"
                               )
                             },

                             draw_key = draw_key_text
)

#' grid::makeContent function for the grobTree of textRepelGrob objects
#' @param x A grid grobTree.
#' @export
#' @noRd
makeContent.textwordcloudtree <- function(x) {
  # Do not create text labels for empty strings.
  valid_strings <- which(not_empty(x$lab))
  invalid_strings <- which(!not_empty(x$lab))

  # Compute the native/pixel ratio
  dev_inch <- dev.size("in")
  dev_pix <- dev.size("px")
  dev_dpi <- dev_pix[1]/dev_inch[1]
  gw_ratio <- as.numeric(convertWidth(unit(1/dev_dpi, "inch"), "native"))
  gh_ratio <- as.numeric(convertHeight(unit(1/dev_dpi, "inch"), "native"))

  grid_size <- max(floor(x$grid_size),1)
  max_grid_w <- floor(dev_pix[1]/grid_size)*grid_size
  max_grid_h <- floor(dev_pix[2]/grid_size)*grid_size
  seq_grid_w <- seq.int(1, max_grid_w, grid_size)
  seq_grid_h <- seq.int(1, max_grid_h, grid_size)

  boxes <- lapply(valid_strings, function(i) {
    row <- x$data[i, , drop = FALSE]
    hj <- x$data$hjust[i]
    vj <- x$data$vjust[i]

    tg_inch <- textGrob(
      x$lab[i],
      dev_inch[1]/2, dev_inch[2]/2, default.units = "inch",
      rot = row$angle,
      just = c(hj,vj),
      gp = gpar(
        fontsize = row$size * .pt,
        fontfamily = row$family,
        fontface = row$fontface,
        lineheight = row$lineheight
      )
    )

    # Compute the text mask
    prev_dev_id <- dev.cur()
    dev_id <- Cairo(width=dev_pix[1], height = dev_pix[2], dpi = dev_dpi, units = "px", type = "raster")
    pushViewport(grid::viewport(width=1, height=1))
    grid.draw(tg_inch)
    popViewport()
    img <- grid.cap()
    dev.off()
    dev.set(prev_dev_id)
    mask <- img!="transparent"
    mask <- t(mask[dim(mask)[1]:1,])


    mask_s <- mask[seq_grid_w,seq_grid_h]
    for (i in 0:(grid_size-1)) {
      for (j in 0:(grid_size-1)) {
        mask_s <- mask_s | mask[i + seq_grid_w,
                                j + seq_grid_h]
      }
    }

    mask_ind <- which(mask_s, arr.ind = TRUE)
    mask_list <- array(0, dim = c(nrow(mask_ind), 4))
    mask_list[,2] = ((mask_ind[,2]-1)*grid_size - dev_pix[2]/2 ) * gh_ratio
    mask_list[,1] = ((mask_ind[,1]-1)*grid_size - dev_pix[1]/2) * gw_ratio
    mask_list[,3] = mask_list[,1] + grid_size * gw_ratio
    mask_list[,4] = mask_list[,2] + grid_size * gh_ratio
    mask_list
  })
  boxes_nb <- sapply(boxes, nrow)
  boxes_start <- cumsum(boxes_nb)
  text_boxes <- cbind(c(0, boxes_start[-length(boxes_start)]), boxes_start)
  boxes_text <- rep(0:(length(boxes_nb)-1), boxes_nb)
  boxes <- do.call(rbind, boxes)

  # Make the repulsion reproducible if desired.
  if (is.null(x$seed) || !is.na(x$seed)) {
    set.seed(x$seed)
  }

  points_valid_first <- cbind(c(x$data$x[valid_strings],
                                x$data$x[invalid_strings]),
                              c(x$data$y[valid_strings],
                                x$data$y[invalid_strings]))

  wordcloud <- wordcloud_boxes(
    data_points = points_valid_first,
    boxes = boxes,
    boxes_text = boxes_text,
    text_boxes = text_boxes,
    xlim = range(x$limits$x),
    ylim = range(x$limits$y),
    eccentricity = x$eccentricity,
    rstep = x$rstep,
    tstep = x$tstep
  )

  grobs <- lapply(seq_along(valid_strings), function(i) {
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
  })
  class(grobs) <- "gList"

  setChildren(x, grobs)
}

# Copied from ggplot2
compute_just <- function(just, x) {
  inward <- just == "inward"
  just[inward] <- c("left", "middle", "right")[just_dir(x[inward])]
  outward <- just == "outward"
  just[outward] <- c("right", "middle", "left")[just_dir(x[outward])]

  unname(c(left = 0, center = 0.5, right = 1,
           bottom = 0, middle = 0.5, top = 1)[just])
}

# Copied from ggplot2
just_dir <- function(x, tol = 0.001) {
  out <- rep(2L, length(x))
  out[x < 0.5 - tol] <- 1L
  out[x > 0.5 + tol] <- 3L
  out
}
