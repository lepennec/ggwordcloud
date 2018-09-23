#include <Rcpp.h>
#include <deque>
#include "AABB.h"
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

// [[Rcpp::export]]
DataFrame wordcloud_boxes(
    NumericMatrix data_points,
    NumericMatrix boxes,
    IntegerVector boxes_text,
    IntegerMatrix text_boxes,
    NumericVector xlim, NumericVector ylim,
    double eccentricity = 0.65,
    double rstep = 0.1, double tstep = 0.05) {

  if (NumericVector::is_na(rstep)) {
    rstep = 0.1;
  }
  if (NumericVector::is_na(tstep)) {
    tstep = 0.05;
  }

  int n_texts = text_boxes.nrow();
  int n_boxes = boxes.nrow();

  int iter = 0;
  bool i_overlaps = true;


  Point xbounds, ybounds;
  xbounds.x = xlim[0];
  xbounds.y = xlim[1];
  ybounds.x = ylim[0];
  ybounds.y = ylim[1];


  std::vector<Point> current_centroids(n_texts);
  for (int i = 0; i < n_texts; i++) {
    current_centroids[i].x = data_points(i, 0);
    current_centroids[i].y = data_points(i, 1);
  }

  std::vector<Box> TextBoxes(n_boxes);

  aabb::Tree tree;
  std::vector<unsigned int> indices;
  std::vector<double> position(2);
  std::vector<double> lowerBound(2);
  std::vector<double> upperBound(2);

  lowerBound[0] = xlim[1]+2*(xlim[1]-xlim[0]);
  lowerBound[1] = ylim[1]+2*(ylim[1]-ylim[0]);
  upperBound[0] = lowerBound[0]+1;
  upperBound[1] = lowerBound[1]+1;

  // Make a AABB trees for the data points and text labels.
  tree = aabb::Tree(2, 0, n_boxes);

  Point d;
  double r;
  double rscale = sqrt((xlim[1]-xlim[0])*(xlim[1]-xlim[0])+
                       eccentricity*eccentricity*(ylim[1]-ylim[0])*(ylim[1]-ylim[0]));
  double theta;

  for (int i = 0; i < n_texts; i++) {
    i_overlaps = true;
    iter       = 0;
    r          = 0;
    theta      = R::runif(0, 2 * M_PI);
    d.x        = 0;
    d.y        = 0;
    Point PosOri = current_centroids[i];
    Point CurPos;
    Point corr;

    // Try to position the current text box
    while (i_overlaps && r < rscale) {
      iter += 1;
      i_overlaps = false;

      CurPos = PosOri + d;
      for (int ii = text_boxes(i,0); ii < text_boxes(i,1); ii++) {
        TextBoxes[ii].x1 = CurPos.x + boxes(ii, 0);
        TextBoxes[ii].x2 = CurPos.x + boxes(ii, 2);
        TextBoxes[ii].y1 = CurPos.y + boxes(ii, 1);
        TextBoxes[ii].y2 = CurPos.y + boxes(ii, 3);
      }
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
      for (int ii = text_boxes(i,0); ii < text_boxes(i,1); ii++){
        TextBoxes[ii] = TextBoxes[ii] + corr;
      }
      CurPos = CurPos + corr;


      for (int ii = text_boxes(i,0); (!i_overlaps) && ii < text_boxes(i,1); ii++){
        lowerBound[0] = TextBoxes[ii].x1;
        lowerBound[1] = TextBoxes[ii].y1;
        upperBound[0] = TextBoxes[ii].x2;
        upperBound[1] = TextBoxes[ii].y2;
        aabb::AABB box;
        box = aabb::AABB(lowerBound, upperBound);
        indices = tree.query(box);
        if (indices.size()>0) {
          i_overlaps = true;
        }
      }

      if (i_overlaps) {
        theta += tstep * (2 * M_PI);
        r     += rscale * rstep * tstep;
        d.x    = r * cos(theta);
        d.y    = r * sin(theta)*eccentricity;
      } else {
        current_centroids[i] = CurPos;
        for (int ii = text_boxes(i,0); ii < text_boxes(i,1); ii++) {
          lowerBound[0] = TextBoxes[ii].x1;
          lowerBound[1] = TextBoxes[ii].y1;
          upperBound[0] = TextBoxes[ii].x2;
          upperBound[1] = TextBoxes[ii].y2;
          tree.insertParticle(ii, lowerBound, upperBound);
        }
      }
    } // loop over already positioned boxes

  } // loop over texts

  NumericVector xs(n_texts);
  NumericVector ys(n_texts);

  for (int i = 0; i < n_texts; i++) {
    xs[i] = current_centroids[i].x;
    ys[i] = current_centroids[i].y;
  }

  return Rcpp::DataFrame::create(
    Rcpp::Named("x") = xs,
    Rcpp::Named("y") = ys
  );
}
