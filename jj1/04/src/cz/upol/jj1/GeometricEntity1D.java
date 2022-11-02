package cz.upol.jj1;

/** A geometric entity described in a single dimension (i.e. points and lines). */
public interface GeometricEntity1D {

  /**
   * Returns the distance of this entity to a given point.
   *
   * @param p point from which the distance should be computed
   * @return {@code -1} if given point {@code p} is {@code null}, otherwise the computed distance
   */
  double distance(Point p);
}
