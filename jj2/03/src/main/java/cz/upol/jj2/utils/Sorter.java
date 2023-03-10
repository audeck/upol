package cz.upol.jj2.utils;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Comparator;import java.util.List;
import java.util.concurrent.*;

/*
 * TODO:
 *  JavaDoc "implementation notes"
 *  Ask ChatGPT for a ForkJoin implementation
 */
public class Sorter {

  /**
   * Sorts `array` into ascending order using a parallel sort using `nThreads` threads.
   *
   * @param array array to be sorted
   * @param nThreads number of threads to be used
   * @see Sorter#parallelMergeSort( Comparable[], Comparator, int )
   */
  public static <T extends Comparable<T>> void parallelMergeSort(final T[] array, int nThreads) {
    parallelMergeSort(array, Comparator.naturalOrder(), nThreads);
  }

  /**
   * Sorts `array` using a parallel sort using `nThreads` threads. Order is provided by `comparator`
   *
   * @param array array to be sorted
   * @param nThreads number of threads used during the sort
   * @param comparator comparator implementing some order of array elements
   * @implNote "Splits" the array into `n` sub-arrays, where `n` is the largest power of 2 lower than `array.length`,
   *     or the smallest power of 2 higher than `nThreads`. The function then delegates these sub-arrays to an
   *     ExecutorService, which sorts them using `Arrays.sort`, and "merges" them afterwards. The merging is delegated
   *     to the same ExecutorService, again sorted using `Arrays.sort`, as we can expect (near-)best-case time
   *     complexity identical to TimSort. The `comparator` is used when sorting. Note that this implementation doesn't
   *     use any minimum granularity, which makes this slower for smaller granularities due to parallel overhead,
   *     memory contention, and other factors.
   */
  public static <T extends Comparable<T>> void parallelMergeSort(
      final T[] array, Comparator<T> comparator, int nThreads) {
    final ExecutorService threadPool = Executors.newFixedThreadPool(nThreads);
    final List<Future<?>> futures = new ArrayList<>();

    // Get maximum number of partitions passed off to threads
    int nPartitions = 1;
    while (nPartitions < nThreads && nPartitions * 2 < array.length) {
      nPartitions *= 2;
    }

    while (nPartitions >= 1) {
      // Get partition size
      int partitionSize = Math.ceilDiv(array.length, nPartitions);

      // Sort all `nPartitions` of partitions
      for (int i = 0; i < nPartitions; i += 1) {
        int index = i;
        T[] partition = Arrays.copyOfRange(
                array,
                i * partitionSize,
                Math.min((i + 1) * partitionSize, array.length)
        );

        futures.add(
            threadPool.submit(() -> {
              // Since Arrays.sort is based on TimSort, it is used to merge as well
              Arrays.sort(partition, comparator);
              synchronized (array) {
               ArrayUtils.copyInto(partition, array, index * partitionSize);
             }
          })
        );
      }

      // Wait for `threadPool` to finish its queue and decrease number of partitions
      awaitCompletion(futures);
      nPartitions /= 2;
    }

    threadPool.close();
  }

  /**
   * Awaits the completion of some ExecutorService's computations (represented by their return Futures) by blocking
   * the current thread while waiting for all such Futures to resolve. Note that such Futures need not belong
   * to the same ExecutorService.
   *
   * @param futures list of Futures corresponding to some asynchronous computations
   */
  private static void awaitCompletion(List<Future<?>> futures) {
    for (Future<?> future : futures) {
      try {
        future.get();
      } catch (InterruptedException | ExecutionException ignored) {}
    }
    futures.clear();
  }

  /**
   * Checks if `array` is in ascending order. Returns true if so, false otherwise.
   *
   * @param array array to be checked
   * @return true if array is in ascending order, otherwise false
   */
  public static <T extends Comparable<T>> boolean checkSorted(T[] array) {
    return checkSorted(array, Comparator.naturalOrder());
  }

  /**
   * Checks if `array` is in order provided by `comparator`. Returns true if so, false otherwise.
   *
   * @param array array to be checked.
   * @param comparator comparator implementing some order of `array` elements
   * @return true if array's order is in accordance with `comparator`, false otherwise
   */
  public static <T extends Comparable<T>> boolean checkSorted(T[] array, Comparator<T> comparator) {
    for (int i = 0; i < array.length - 1; i += 1) {
      if (comparator.compare(array[i], array[i + 1]) > 0) {
        return false;
      }
    }
    return true;
  }
}
