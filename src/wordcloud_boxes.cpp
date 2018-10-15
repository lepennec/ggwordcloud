#include <Rcpp.h>
using namespace Rcpp;

// Main code for text label placement -----------------------------------------

typedef struct {
  double x, y;
} Point;

Point operator -(const Point& a, const Point& b) {
  Point p = {a.x - b.x, a.y - b.y};
  return p;
}

Point operator +(const Point& a, const Point& b) {
  Point p = {a.x + b.x, a.y + b.y};
  return p;
}

Point operator /(const Point& a, const double& b) {
  Point p = {a.x / b, a.y / b};
  return p;
}

Point operator *(const double& b, const Point& a) {
  Point p = {a.x * b, a.y * b};
  return p;
}

Point operator *(const Point& a, const double& b) {
  Point p = {a.x * b, a.y * b};
  return p;
}

typedef struct {
  double x1, y1, x2, y2;
} Box;

Box operator +(const Box& b, const Point& p) {
  Box c = {b.x1 + p.x, b.y1 + p.y, b.x2 + p.x, b.y2 + p.y};
  return c;
}

bool overlaps(Box a, Box b) {
  return
  b.x1 <= a.x2 &&
    b.y1 <= a.y2 &&
    b.x2 >= a.x1 &&
    b.y2 >= a.y1;
}

double r_circle(double theta) {
  return 1.0;
}

double r_cardioid(double theta) {
  return (1.0 - std::sin(theta))/2;
}

double r_diamond(double theta) {
  double thetaprime = std::fmod(theta, 2.0 * M_PI / 4.0);
  return (1.0 / (std::cos(thetaprime) + std::sin(thetaprime)));
}

double r_square(double theta) {
  return std::min(1.0/ std::max(1e-6, std::abs(std::cos(theta))),
                  1.0/ std::max(1e-6,std::abs(std::sin(theta))))/std::sqrt(2.0);
}

double r_triangle_forward(double theta) {
  double thetaprime = std::fmod(theta, 2.0 * M_PI /3.0);
  return 1.0 / (std::cos(thetaprime) +
              std::sqrt(3.0) * std::sin(thetaprime));
}

double r_triangle_upright(double theta) {
  return r_triangle_forward(theta + M_PI * 3.0 /2.0);
}

double r_pentagon(double theta) {
  double  thetaprime = std::fmod(theta + .955, 2.0 * M_PI / 5.0);
  return 1.0 / (std::cos(thetaprime) + .726543 * std::sin(thetaprime));
}

double r_star(double theta) {
  double thetaprime = std::fmod(theta + .955, 2.0 * M_PI / 10.0);
  if (std::fmod(theta + .955, 2.0 * M_PI / 5.0) >= (2.0 * M_PI / 10.0)) {
    thetaprime = (2.0 * M_PI / 10.0) - thetaprime;
  }
  return 1.0 / (std::cos(thetaprime) +
                     3.07768 * std::sin(thetaprime));
}

// [[Rcpp::export]]
DataFrame wordcloud_boxes(
    NumericMatrix data_points,
    NumericMatrix boxes,
    IntegerVector boxes_text,
    IntegerMatrix text_boxes,
    NumericMatrix bigboxes,
    NumericVector xlim, NumericVector ylim,
    const double eccentricity = 0.65,
    const double rstep = 0.1, const double tstep = 0.05,
    const double perc_step = 0.01, const int max_steps = 10,
    const bool rm_outside = false,
    const int shape = 1) {

  int n_texts = text_boxes.nrow();
  int n_boxes = boxes.nrow();

  std::vector<bool> text_inside(n_texts);

  int iter = 0;
  bool i_overlaps = true;

  double (* r_mult)(double);
  switch (shape)
  {
  case 2:
    r_mult = & r_cardioid;
    break;
  case 3:
    r_mult = & r_diamond;
    break;
  case 4:
    r_mult = & r_square;
    break;
  case 5:
    r_mult = & r_triangle_forward;
    break;
  case 6:
    r_mult = & r_triangle_upright;
    break;
  case 7:
    r_mult = & r_pentagon;
    break;
  case 8:
    r_mult = & r_star;
    break;
  default:
    r_mult = &r_circle;
  }

  Point xbounds, ybounds;
  xbounds.x = xlim[0];
  xbounds.y = xlim[1];
  ybounds.x = ylim[0];
  ybounds.y = ylim[1];

  Box inside;
  inside.x1 = xlim[0];
  inside.y1 = ylim[0];
  inside.x2 = xlim[1];
  inside.y2 = ylim[1];


  std::vector<Point> current_centroids(n_texts);
  for (int i = 0; i < n_texts; i++) {
    current_centroids[i].x = data_points(i, 0);
    current_centroids[i].y = data_points(i, 1);
  }

  std::vector<Box> TextBoxes(n_boxes);
  std::vector<Box> BigBoxes(n_texts);

  Point d;
  double r;
  double rprime;
  const double rscale = ((xlim[1]-xlim[0])*(xlim[1]-xlim[0])+
                       (ylim[1]-ylim[0])*(ylim[1]-ylim[0])/(eccentricity * eccentricity));
  double theta;
  const double rstepratio = rscale * rstep * perc_step / tstep * 2 * M_PI;

  for (int i = 0; i < n_texts; i++) {
    Rcpp::checkUserInterrupt();
    i_overlaps = true;
    iter       = 0;
    r          = 0;
    theta      = R::runif(0, 2.0 * M_PI);
    d.x        = 0;
    d.y        = 0;
    Point PosOri = current_centroids[i];
    Point CurPos;
    Point corr;
    text_inside[i] = false;

    // Try to position the current text box
    while (i_overlaps && r < rscale) {
      iter += 1;
      i_overlaps = false;

      CurPos = PosOri + d;
      bool all_inside = true;
      BigBoxes[i].x1 = CurPos.x + bigboxes(i, 0);
      BigBoxes[i].x2 = CurPos.x + bigboxes(i, 2);
      BigBoxes[i].y1 = CurPos.y + bigboxes(i, 1);
      BigBoxes[i].y2 = CurPos.y + bigboxes(i, 3);
      if (!overlaps(BigBoxes[i], inside)) {
        all_inside = false;
      }
      for (int ii = text_boxes(i,0); all_inside&&(ii < text_boxes(i,1)); ii++) {
        TextBoxes[ii].x1 = CurPos.x + boxes(ii, 0);
        TextBoxes[ii].x2 = CurPos.x + boxes(ii, 2);
        TextBoxes[ii].y1 = CurPos.y + boxes(ii, 1);
        TextBoxes[ii].y2 = CurPos.y + boxes(ii, 3);
        all_inside = all_inside && overlaps(TextBoxes[ii], inside);
      }

      if (all_inside) {
        corr.x = 0;
        corr.y = 0;
        for (int ii = text_boxes(i,0); ii < text_boxes(i,1); ii++){
          if (TextBoxes[ii].x1 < xbounds.x) {
            corr.x = std::max(xbounds.x-TextBoxes[ii].x1,corr.x);
          }
          if (TextBoxes[ii].x2 > xbounds.y) {
            corr.x = std::min(xbounds.y-TextBoxes[ii].x2,corr.x);
          }
          if (TextBoxes[ii].y1 < ybounds.x) {
            corr.y = std::max(ybounds.x-TextBoxes[ii].y1,corr.y);
          }
          if (TextBoxes[ii].y2 > ybounds.y) {
            corr.y = std::min(ybounds.y-TextBoxes[ii].y2,corr.y);
          }
        }
        BigBoxes[i] = BigBoxes[i] + corr;
        for (int ii = text_boxes(i,0); ii < text_boxes(i,1); ii++){
          TextBoxes[ii] = TextBoxes[ii] + corr;
        }
        CurPos = CurPos + corr;

        for (int j = 0; (!i_overlaps) && (j < i); j++) {
          if (overlaps(BigBoxes[i], BigBoxes[j])) {
            for (int ii = text_boxes(i,0); (!i_overlaps) && (ii < text_boxes(i,1)); ii++){
              if (overlaps(TextBoxes[ii], BigBoxes[j])) {
                for (int jj = text_boxes(j,0); (!i_overlaps) && (jj < text_boxes(j,1)); jj++){
                  if (overlaps(TextBoxes[ii], TextBoxes[jj])) {
                    i_overlaps = true;
                  }
                }
              }
            }
          }
        }
      } else {
        i_overlaps = true;
      }

      if (i_overlaps) {
        int nstep;
        if (r > 0) {
          nstep = std::floor(rstepratio / r);
        } else {
          nstep = 1;
        }
        nstep = std::max(1,std::min(nstep, max_steps));
        theta += tstep * (2.0 * M_PI) * nstep;
        r     += rscale * rstep * tstep * nstep;
        rprime = r * r_mult(theta);
        d.x    = rprime * cos(theta);
        d.y    = rprime * sin(theta)*eccentricity;
      } else {
        current_centroids[i] = CurPos;
        text_inside[i] = true;
      }
    } // loop over already positioned boxes

  } // loop over texts

  NumericVector xs(n_texts);
  NumericVector ys(n_texts);

  int nb_bad = 0;
  for (int i = 0; i < n_texts; i++) {
    if (!text_inside[i]) { nb_bad++; }
    if (text_inside[i]||!rm_outside) {
      xs[i] = current_centroids[i].x;
      ys[i] = current_centroids[i].y;
    } else {
      xs[i] = NA_REAL;
      ys[i] = NA_REAL;
    }
  }

  if (nb_bad > 0) {
    if (nb_bad == 1) {
      if (rm_outside) {
        Rcpp::warning("One word could not fit on page. It has been removed.");
      } else {
        Rcpp::warning("One word could not fit on page. It has been placed at its original position.");
      }
    } else {
      if (rm_outside) {
        Rcpp::warning("Some words could not fit on page. They have been removed.");
      } else {
        Rcpp::warning("Some words could not fit on page. They have been placed at their original positions.");
      }
    }
  }


  return Rcpp::DataFrame::create(
    Rcpp::Named("x") = xs,
    Rcpp::Named("y") = ys
  );
}
