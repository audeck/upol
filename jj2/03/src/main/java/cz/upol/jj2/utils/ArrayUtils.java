package cz.upol.jj2.utils;

public class ArrayUtils {

  /**
   * Copies the sequence in an array referenced by `source` to the array referenced by `destination`,
   * starting at `startIndex` in `destination`. If the entirety of `source` doesn't fit into `destination`,
   * the maximum subsequence that fits is copied over instead (starting at `source[0]`). If `startIndex`
   * is out of range for `destination`, nothing gets copied.
   *
   * @param source the source array
   * @param destination the destination array
   * @param startIndex a starting index in the destination array
   */
  public static <T> void copyInto(T[] source, T[] destination, int startIndex) {
    int copyAmount = Math.min(source.length, destination.length - startIndex);
    if (copyAmount >= 0) System.arraycopy(source, 0, destination, startIndex, copyAmount);
  }
}
