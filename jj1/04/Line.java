package cz.upol.jj1;

public class Line implements GeometricEntity {

  public Point pointA;
  public Point pointB;

  public Line(Point pointA, Point pointB) {
    this.pointA = pointA;
    this.pointB = pointB;
  }

  public double getLength() {
    return pointA.distance(pointB);
  }

  public double distance(Point p) {
    return Math.abs(
        (pointB.x - pointA.x) * (pointA.y - p.y) - (pointA.x - p.x) * (pointB.y - pointA.y))
        / pointA.distance(pointB);
  }
}
