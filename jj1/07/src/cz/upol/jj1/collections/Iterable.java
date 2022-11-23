package cz.upol.jj1.collections;

public interface Iterable<T> {

  /** @return the iterable's iterator */
  Iterator<T> iterator();
}
