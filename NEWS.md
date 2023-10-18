# ggwordcloud 0.6.0
* Novelties:
   * Use of gridtext extended richtext_grob by default so that one can use markdown/html syntax in label
   * Remove the area_corr_power factor and provide a power_trans transform to obtain the same effect
   * Add a label_content aesthetic that overrides label_content after scaling factor correction
   * Use transparency instead of white/black for masks
* Bug fixes:
  * remove a warning when a png dimension was smaller than 20 pixels
  * replace msg by proper warning in the C++ code
  * fix a failed test in Windows R-older due to utf8 related warnings
  * remove non-latin characters from the examples and simplify them to pass CRAN check.

# ggwordcloud 0.5.0
* Novelties:
   * x2 speedup
* Bug fix:
   * fix a out of bounds issue

# ggwordcloud 0.4.0
* Novelties:
    * New thank you word list thanks to Enrico Spinielli
    * Use png instead of Cairo
    * New show_boxes option to visualize the bounding boxes used in the placement algorithm
* Bug fixes
    * add missing parse_safe function
    * fix a bug when using only one box size

# ggwordcloud 0.3.0
* Novelties:
    * documentation with a lovely example
    * new mask and shape parameters
    * better replacement by using the right scale
* Bug fixes:
    * workaround for a Cairo issue with some utf8 strings
    * geom_wordcloud_area works even if no sizes are given

# ggwordcloud 0.2.0
* add the rm_outside option
* wordcloud layout acceleration thanks to better word boxing
* wordcloud and wordcloud2 approximate replacements

# ggwordcloud 0.1.0
* Initial version
