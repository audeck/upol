package cz.upol.jj1;

/** A simple point class. Coordinates are limited to integers. */
public class PointFixed extends Point {

  public PointFixed(int x, int y) {
    super(x, y);
  }

  @Override
  public boolean equals(Object obj) {
    if (obj instanceof PointFixed) {
      return ((PointFixed) obj).getX() == this.getX() && ((PointFixed) obj).getY() == this.getY();
    }

    return super.equals(obj);
  }

  @Override
  public int hashCode() {
    long bits = this.getX() ^ ((long) this.getY() * 31);
    return ((int) bits) ^ ((int) (bits >> 32));
  }
}
