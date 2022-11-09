package cz.upol.jj1.collections;

import java.util.NoSuchElementException;

/**
 * This class implements basic set functionality using an integer's bits for representing said set.
 */
public class BitSet implements Sequence, Iterable {
  /** The bits representing a set */
  private int bits;
  /** A value limit (values can range from 0-31 using 32 bits) */
  private final int LIMIT = 31;

  public BitSet(int... values) {
    this.bits = 0;
    for (int value : values) {
      this.insert(value);
    }
  }

  public int getBits() {
    return this.bits;
  }

  /** @return the size of this bitset */
  public int size() {
    return Integer.bitCount(this.bits);
  }

  /**
   * Inserts a value into this bitset. Throws an {@code IllegalArgumentException} if the value is not
   * in range.
   *
   * @param value value to be added
   */
  public void insert(int value) {
    if (0 > value || value > this.LIMIT) {
      throw new IllegalArgumentException("Bitset value too big!");
    }

    int mask = 1 << value;
    this.bits = this.bits | mask;
  }

  /**
   * Checks if this bitset contains a given value.
   * @param value checked value
   * @return {@code true} if set contains value, {@code false} otherwise
   */
  public boolean contains(int value) {
    if (0 > value || value > this.LIMIT) {
      throw new IllegalArgumentException("Bitset value too big!");
    }

    int bitsCopy = this.bits;
    bitsCopy >>>= value;
    bitsCopy <<= this.LIMIT - 1;
    return bitsCopy > 0;
  }

  /**
   * Removes a value from this bitset.
   *
   * @param value value to be deleted
   * @return {@code true} if a value was deleted, {@code false} otherwise
   */
  public boolean delete(int value) {
    if (this.contains(value)) {
      int mask = ~(1 << value);
      this.bits = this.bits & mask;
      return true;
    } else {
      return false;
    }
  }

  /**
   * Returns a basic integer iterator for this bitset. Iterator doesn't check for concurrent
   * modification.
   *
   * @return this bitset's iterator
   */
  public Iterator iterator() {
    return new BitSetItr(this);
  }

  private static class BitSetItr implements Iterator {
    private BitSet set;

    BitSetItr(BitSet set) {
      this.set = set;
    }

    public boolean hasNext() {
      return this.set.getBits() > 0;
    }

    public int next() {
      if (!hasNext()) {
        throw new NoSuchElementException();
      }

      int nextValue = (int) (Math.log(Integer.lowestOneBit(this.set.getBits())) / Math.log(2));
      this.set.delete(nextValue);
      return nextValue;
    }
  }

  @Override
  public String toString() {
    StringBuilder output = new StringBuilder("[");
    Iterator itr = this.iterator();
    boolean isFirst = true;

    while (itr.hasNext()) {
      if (!isFirst) {
        output.append(", ");
      }
      output.append(itr.next());
      isFirst = false;
    }

    output.append("]");
    return output.toString();
  }
}
