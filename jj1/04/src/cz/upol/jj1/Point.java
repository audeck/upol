package cz.upol.jj1;

/** A simple point class. */
public class Point implements GeometricEntity1D {

  /** The point's x-axis coordinate */
  private int x;
  /** The point's y-axis coordinate */
  private int y;

  public Point(int x, int y) {
    this.x = x;
    this.y = y;
  }

  public int getX() {
    return x;
  }

  public void setX(int x) {
    this.x = x;
  }

  public int getY() {
    return y;
  }

  public void setY(int y) {
    this.y = y;
  }

  /**
   * Returns the distance of this point from a given point.
   *
   * @param p point from which the distance should be computed
   * @return {@code -1} if given point {@code p} is {@code null}, otherwise the computed distance
   */
  public double distance(Point p) {
    if (p == null) {
      return -1;
    }
    return Math.sqrt(Math.pow(p.x - this.x, 2) + Math.pow(p.y - this.y, 2));
  }
}
