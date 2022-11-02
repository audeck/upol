package cz.upol.jj1;

/** This class implements the same-sided rectangle (also called a square). */
public class Square extends Rectangle {

  /** The side length of this square */
  public int sideLength;

  public Square(Point origin, int sideLength) {
    super(origin, sideLength, sideLength);
    this.sideLength = sideLength;
  }

  public Square(Point a, Point b) {
    super(a, b);
    if (super.getWidth() != super.getHeight()) {
      // Raise hell!
      throw new Error("This rectangle is not a square!");
    }
  }
}
