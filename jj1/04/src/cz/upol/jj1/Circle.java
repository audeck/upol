package cz.upol.jj1;

/** A simple circle class. */
public class Circle implements GeometricEntity2D {

  /** The center of this circle */
  public Point center;

  public int radius;

  public Circle(Point center, int radius) {
    this.center = center;
    this.radius = radius;
  }

  /**
   * @return the area of this circle
   */
  public double getArea() {
    return Math.PI * Math.pow(this.radius, 2);
  }

  /**
   * Returns the distance of this circle from a given point.
   *
   * @param p point from which the distance should be computed
   * @return {@code -1} if given point {@code p} is {@code null}, otherwise the computed distance
   */
  public double distance(Point p) {
    if (p == null) {
      return -1;
    }
    double originDistance = this.center.distance(p);
    return (originDistance > this.radius) ? originDistance : this.radius - originDistance;
  }
}
