package cz.upol.jj1;

import java.util.ArrayList;
import java.util.List;

public class Rectangle implements GeometricEntity, GeometricShape {

  public Point origin;  // Bottom-left corner
  public int width;
  public int height;

  public Rectangle(Point origin, int width, int height) {
    this.origin = origin;
    this.width = width;
    this.height = height;
  }

  public Rectangle(Point a, Point b) {
    this(new Point(Math.min(a.x, b.x), Math.min(a.y, b.y)),
        Math.abs(a.x - b.x),
        Math.abs(a.y - b.y));
  }

  public double getArea() {
    return this.width * this.height;
  }

  public double distance(Point p) {
    double distance = Double.MAX_VALUE;

    // Create points for readability
    Point lb = this.origin;
    Point rb = new Point(this.origin.x + this.width, this.origin.y);
    Point rt = new Point(this.origin.x + this.width, this.origin.y + this.height);
    Point lt = new Point(this.origin.x, this.origin.y + this.height);

    // Create list of 4 sides
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
