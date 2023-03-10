package cz.upol.jj2;

import java.util.Random;
import cz.upol.jj2.utils.Sorter;
import cz.upol.jj2.utils.CodeTimer;

public class Main {
  public static void main(String[] args) {
    //testSort();
  }

  private static void testSort() {
    Integer[] randomArray = new Integer[10_000_000];
    Random random = new Random();

    for (int i = 0; i < randomArray.length; i += 1) {
      randomArray[i] = random.nextInt(100);
    }

    //System.out.println("Randomized array: " + Arrays.toString(randomArray));

    long time = CodeTimer.timeCode(() -> Sorter.parallelMergeSort(randomArray, 4));
    //long time = FunctionTimer.timeFunction(() -> Arrays.sort(randomArray));
    //long time = FunctionTimer.timeFunction(() -> Arrays.parallelSort(randomArray));

    if (Sorter.checkSorted(randomArray)) {
      System.out.println("Sorted correctly!");
    } else {
      System.out.println("Sorted incorrectly!");
    }

    System.out.println("Sorting took " + time/1_000_000 + "ms.");

    //System.out.println("Sorted array:     " + Arrays.toString(randomArray));
  }
}
