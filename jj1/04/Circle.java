package cz.upol.jj1;

public class Circle implements GeometricEntity, GeometricShape {

  public Point origin;
  public int radius;

  public Circle(Point origin, int radius) {
    this.origin = origin;
    this.radius = radius;
  }

  public double getArea() {
    return Math.PI * Math.pow(this.radius, 2);
  }

  public double distance(Point p) {
    double originDistance = this.origin.distance(p);
    return (originDistance > this.radius) ? originDistance : this.radius - originDistance;
  }
}
