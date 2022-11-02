package cz.upol.jj1;

/** A simple line class. */
public class Line implements GeometricEntity1D {

  /** The "a" end of the line */
  private Point pointA;
  /** The "b" end of the line */
  private Point pointB;

  public Line(Point pointA, Point pointB) {
    this.pointA = pointA;
    this.pointB = pointB;
  }

  public Point getPointA() {
    return pointA;
  }

  public void setPointA(Point pointA) {
    this.pointA = pointA;
  }

  public Point getPointB() {
    return pointB;
  }

  public void setPointB(Point pointB) {
    this.pointB = pointB;
  }

  /** @return this line's length */
  public double getLength() {
    return pointA.distance(pointB);
  }

  /**
   * Returns the distance of this line from a given point.
   *
   * @param p point from which the distance should be computed
   * @return {@code -1} if given point {@code p} is {@code null}, otherwise the computed distance
   */
  public double distance(Point p) {
    if (p == null) {
      return -1;
    }
    return Math.abs(
            (pointB.getX() - pointA.getX()) * (pointA.getY() - p.getY())
                - (pointA.getX() - p.getX()) * (pointB.getY() - pointA.getY()))
        / pointA.distance(pointB);
  }
}
