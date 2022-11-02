package cz.upol.jj1;

/** A geometric entity described in two dimensions (i.e. 2D shapes). */
public interface GeometricEntity2D extends GeometricEntity1D {

  /**
   * @return the area of this entity
   */
  double getArea();
}
