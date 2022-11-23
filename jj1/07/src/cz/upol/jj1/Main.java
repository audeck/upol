package cz.upol.jj1;

import cz.upol.jj1.collections.BitSet;
import cz.upol.jj1.collections.UPLinkedList;
import java.util.ArrayList;
import java.util.List;

public class Main {

  public static void main(String[] args) { }

  private static void arrays() {
    /*** Arrays ***/

    // Array of integers of size 10
    int[] numbers = new int[20];

    // Set value at index 2
    numbers[2] = 22;

    // Get value at index 2
    var number = numbers[2];
    System.out.printf("Value at index 2 is %d%n", number);

    /*** Multidimensional Arrays ***/
    int[][] matrix = new int[5][5];

    int[][] matrixWithValues = {
      {1, 1, 1, 1, 1},
      {2, 2, 2, 2, 2},
      {3, 3, 3, 3, 3},
      {4, 4, 4, 4, 4},
      {5, 5, 5, 5, 5}
    };

    // Set value at indexes 1 & 1
    matrix[1][1] = 22;

    // Get value at indexes 1 & 1
    var matrixValue = matrix[1][1];
    System.out.printf("Value at [1, 1] is %d%n", matrixValue);
  }
}
