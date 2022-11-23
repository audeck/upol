package cz.upol.jj1.collections;

import java.util.ArrayList;

/**
 * This class implements a set - a list (in this implementation an ArrayList) without duplicates -
 * and basic set functionality - intersection, union, difference.
 */
public class Set<T extends Comparable<T>> extends ArrayList<T> implements Comparable<Set<T>> {

  @SafeVarargs
  public Set(T... items) {
    for (T item : items) {
      if (!this.contains(item)) {
        this.add(item);
      }
    }
  }

  /**
   * Inserts new value to the set if it doesn't already contain it
   *
   * @param value Value to be added
   * @return true if value was added, otherwise false
   */
  @Override
  public boolean add(T value) {
    if (!this.contains(value)) {
      return super.add(value);
    }

    return false;
  }

  /**
   * Returns the intersection of this set and a given set.
   *
   * @param set a given set
   * @return the intersection of the two sets
   */
  public Set<T> intersection(Set<T> set) {
    Set<T> result = new Set<>();

    for (T item : set) {
      if (this.contains(item)) {
        result.add(item);
      }
    }

    return result;
  }

  /**
   * Returns the union of this set and a given set.
   *
   * @param set a given set
   * @return the union of the two sets
   */
  public Set<T> union(Set<T> set) {
    Set<T> result = new Set<>();

    for (T item : set) {
      result.add(item);
    }

    for (T item : this) {
      result.add(item);
    }

    return result;
  }

  /**
   * Returns the difference of this set and a given set.
   *
   * @param set a given set
   * @return the difference (this - set)
   */
  public Set<T> difference(Set<T> set) {
    Set<T> result = new Set<>();

    for (T item : this) {
      if (!set.contains(item)) {
        result.add(item);
      }
    }

    return result;
  }

  /**
   * Compares this set to a given set. Returns 0 if the sets are equal or a non-zero integer if
   * otherwise.
   *
   * @param set a given set
   * @return 0 if sets are equal, non-zero integer otherwise
   */
  public int compareTo(Set<T> set) {
    for (T item : this) {
      if (!set.contains(item)) {
        return -1;
      }
    }

    return 0;
  }

  @Override
  public String toString() {
    StringBuilder description = new StringBuilder("[");
    java.util.Iterator<T> itr = this.iterator();
    boolean isFirst = true;

    while (itr.hasNext()) {
      if (!isFirst) {
        description.append(", ");
      }
      description.append(itr.next());
      isFirst = false;
    }

    description.append("]");
    return description.toString();
  }
}
