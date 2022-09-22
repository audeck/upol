package cz.upol.jj1;
import java.awt.*;
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

    static int multiply(int left, int right) {
        boolean rightIsNegative = (right < 0);

        int result = 0;

        for (int i = 0; i < Math.abs(right); i++) {
            result += (rightIsNegative) ? -left : left;
        }

        return result;
    }

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
