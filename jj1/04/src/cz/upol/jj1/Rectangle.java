package cz.upol.jj1;

import java.util.ArrayList;
import java.util.List;

/**
 * This class defines the geometric shape called a rectangle.
 *
 * <p>The rectangle is stored as it's origin (it's bottom left corner), it's width, and it's height.
 *
 * <p>The use of the two-point or other better representations is advised if your program is
 * distance-computation intensive.
 */
public class Rectangle implements GeometricEntity2D {

  /** The bottom left corner of this rectangle */
  private Point origin;
  /** The width of this rectangle */
  private int width;
  /** The height of this rectangle */
  private int height;

  public Rectangle(Point origin, int width, int height) {
    this.origin = origin;
    this.width = width;
    this.height = height;
  }

  public Rectangle(Point a, Point b) {
    if (a == null || b == null) {
      this.origin = (a == null) ? b : a;
      this.width = 0;
      this.height = 0;
    } else {
      this.origin = new Point(Math.min(a.getX(), b.getX()), Math.min(a.getY(), b.getY()));
      this.width = Math.abs(a.getX() - b.getX());
      this.height = Math.abs(a.getY() - b.getY());
    }
  }

  public Point getOrigin() {
    return origin;
  }

  public void setOrigin(Point origin) {
    this.origin = origin;
  }

  public int getWidth() {
    return width;
  }

  public void setWidth(int width) {
    this.width = width;
  }

  public int getHeight() {
    return height;
  }

  public void setHeight(int height) {
    this.height = height;
  }

  /**
   * @return the area of this rectangle
   */
  public double getArea() {
    return this.width * this.height;
  }

  /**
   * Returns the distance of this rectangle from a given point.
   *
   * @param p point from which the distance should be computed
   * @return {@code -1} if given point {@code p} is {@code null}, otherwise the computed distance
   */
  public double distance(Point p) {
    if (p == null) {
      return -1;
    }
    double distance = Double.MAX_VALUE;

    // Create points
    Point lb = this.origin;
    Point rb = new Point(this.origin.getX() + this.width, this.origin.getY());
    Point rt = new Point(this.origin.getX() + this.width, this.origin.getY() + this.height);
    Point lt = new Point(this.origin.getX(), this.origin.getY() + this.height);

    // Create a list of 4 sides
    List<Line> sides = new ArrayList<Line>();
    sides.add(new Line(lb, rb));
    sides.add(new Line(rb, rt));
    sides.add(new Line(rt, lt));
    sides.add(new Line(lt, lb));

    for (Line side : sides) {
      distance = Math.min(distance, side.distance(p));
    }

    return distance;
  }
}
