package cz.upol.jj1;
import java.lang.Math;

public class Main {
    
    public static void main(String[] args) {
        primes(16);

        System.out.println(multiply(5 ,6));
        System.out.println(multiply(-5 ,6));
        System.out.println(multiply(5 ,-6));
        System.out.println(multiply(-5 ,-6));

        System.out.println(nameNumber(3));

        printTriangle(5);
    }

    /**
     * Prints out all prime numbers less than {@code upperBound} on a single line separated with a
     * space.
     *
     * @param upperBound upper bound of primes
     */
    static void primes(int upperBound) {
        boolean isPrime;
        for (int i = 2; i < upperBound; i++) {
            isPrime = true;
            for (int j = 2; j <= Math.sqrt(i); j++) {
                if (i % j == 0) {
                    isPrime = false;
                    break;
                }
            }
            if (isPrime) {
                System.out.print(i);
                System.out.print(", ");
            }
        }
        System.out.print('\n');
    }

    /**
     * Multiplies two numbers together manually.
     *
     * @param left first number to be multiplied
     * @param right second number to be multiplied
     * @return result of multiplication
     */
    static int multiply(int left, int right) {
        boolean rightIsNegative = (right < 0);

        int result = 0;

        for (int i = 0; i < Math.abs(right); i++) {
            result += (rightIsNegative) ? -left : left;
        }

        return result;
    }

    /**
     * Returns a string with {@code number}'s name in Czech. Goes up to 10.
     *
     * @param number number to be named
     * @return the czech name of {@code number}
     */
    static String nameNumber(int number) {
        return switch (number) {
            case 1 -> "Jedna";
            case 2 -> "Dva";
            case 3 -> "Tři";
            case 4 -> "Čtyři";
            case 5 -> "Pět";
            case 6 -> "Šest";
            case 7 -> "Sedm";
            case 8 -> "Osm";
            case 9 -> "Děvět";
            case 10 -> "Deset";
            default -> "neznám";
        };
    }

    /**
     * Prints out a triangle of format   *
     *                                  **
     *                                 * *
     *                                ****
     *
     * @param baseLength side length of the triangle
     */
    static void printTriangle(int baseLength) {
        for (int i = 0; i < baseLength - 1; i++) {
            for (int j = 1; j <= baseLength; j++) {
                if (j == baseLength || j == baseLength - i) {
                    System.out.print('*');
                } else {
                    System.out.print(' ');
                }
            }
            System.out.print('\n');
        }

        // Last row
        for (int i = 0; i < baseLength; i++) {
            System.out.print('*');
        }

        System.out.print('\n');
    }
}
