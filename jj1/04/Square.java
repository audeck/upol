package cz.upol.jj1;

public class Square extends Rectangle implements GeometricShape {  // Inherit GeometricShape?

  public Point origin;
  public int sideLength;

  public Square(Point origin, int sideLength) {
    super(origin, sideLength, sideLength);
  }
}
