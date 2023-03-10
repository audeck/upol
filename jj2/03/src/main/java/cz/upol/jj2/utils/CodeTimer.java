package cz.upol.jj2.utils;

public class CodeTimer {
  /**
   * Times how long `code` takes to finish and returns the value in nanoseconds.
   * @param code code to be run
   * @return time taken to run `code` in nanoseconds
   */
  public static long timeCode(Runnable code) {
    long startTime = System.nanoTime();
    code.run();
    long endTime = System.nanoTime();
    return endTime - startTime;
  }
}
