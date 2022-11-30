package cz.upol.jj1;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

public class Main {

  public static void main(String[] args) {
    comparePoints(); // HashSet size = 4
    comparePointsFixed(); // HashSet size = 3

    System.out.println(freq("hello2puck;flip.how elk hello/Hello"));
    System.out.println(freqIgnoreCase("hello2puck;flip.how elk hello/Hello"));

    System.out.println(rpnCalc("1 -2 3 + +"));  // = 2
    System.out.println(rpnCalc("1 32 + 42 * 5 + 66 -"));  // = 1325

    Map<String, Integer> bindings = new HashMap<>();
    bindings.put("foo", 6);
    bindings.put("bar", 9);

    System.out.println(rpnCalc("foo bar + 3 /", bindings));  // = 5
  }

  private static void comparePoints() {
    Set<Point> points = new HashSet<Point>();
    Point point0 = new Point(3, 2);
    Point point1 = new Point(-1, 4);
    Point point2 = new Point(1, -2);
    Point point3 = new Point(3, 2);

    points.add(point0);
    points.add(point1);
    points.add(point2);
    points.add(point3);

    System.out.println(points);
  }

  private static void comparePointsFixed() {
    Set<PointFixed> points = new HashSet<PointFixed>();
    PointFixed point0 = new PointFixed(3, 2);
    PointFixed point1 = new PointFixed(-1, 4);
    PointFixed point2 = new PointFixed(1, -2);
    PointFixed point3 = new PointFixed(3, 2);

    points.add(point0);
    points.add(point1);
    points.add(point2);
    points.add(point3);

    System.out.println(points);
  }

  /**
   * Counts substring occurences in a given string and returns a map of the data. Whitespace,
   * digits, and punctuation are taken as delimiters.
   *
   * @param s a given string
   * @return a map of (substring, numberOfSubstringOccurences)
   */
  static Map<String, Integer> freq(String s) {
    if (s == null) {
      return null;
    }
    String[] words = s.split("\\d|\\s|\\W+");
    Map<String, Integer> output = new HashMap<String, Integer>();

    for (String word : words) {
      if (output.containsKey(word)) {
        output.replace(word, output.get(word) + 1);
      } else {
        output.put(word, 1);
      }
    }

    return output;
  }

  /**
   * Counts substring occurences in a given string and returns a map of the data. Whitespace,
   * digits, and punctuation are taken as delimiters. Ignores substring case!
   *
   * @param s a given string
   * @return a map of (substring, numberOfSubstringOccurences)
   */
  static Map<String, Integer> freqIgnoreCase(String s) {
    if (s == null) {
      return null;
    }
    return freq(s.toLowerCase());
  }

  /**
   * Checks if a given string is composed of only base 10 digits. Specially, returns false for empty
   * string and null.
   *
   * @param s a given string
   * @return true if s is a non-empty string containing only base 10 digits, otherwise false
   */
  private static boolean isNumber(String s) {
    if (s == null || s.length() == 0) {
      return false;
    }
    char[] sChars = s.toCharArray();
    int start = 0;

    if (sChars[start] == '-') {
      start += 1;
    }

    for (int i = start; i < s.length(); i += 1) {
      if ('0' > sChars[i] || sChars[i] > '9') {
        return false;
      }
    }

    return true;
  }

  /**
   * Checks if a given string is composed of one of the four operators: +, -, *, /.
   *
   * @param s a given string
   * @return true if s is one of "+", "-", "*" or "/", otherwise false
   */
  private static boolean isOperator(String s) {
    if (s == null || s.length() == 0) {
      return false;
    }
    return s.equals("+") || s.equals("-") || s.equals("*") || s.equals("/");
  }

  /**
   * Applies an operation to a given stack, depending on the operator given.
   *
   * @param stack a given stack
   * @param operator a given operator (one of "+", "-", "*" or "/")
   */
  private static void applyOperationToStack(List<Integer> stack, String operator) {
    int size = stack.size();

    if (size < 2) {
      throw new IllegalArgumentException("Stack doesn't have enough numbers to apply an operation");
    }

    switch (operator) {
      case "+" -> {
        stack.set(size - 2, stack.get(size - 2) + stack.get(size - 1));
        stack.remove(size - 1);
      }
      case "-" -> {
        stack.set(size - 2, stack.get(size - 2) - stack.get(size - 1));
        stack.remove(size - 1);
      }
      case "*" -> {
        stack.set(size - 2, stack.get(size - 2) * stack.get(size - 1));
        stack.remove(size - 1);
      }
      case "/" -> {
        stack.set(size - 2, stack.get(size - 2) / stack.get(size - 1));
        stack.remove(size - 1);
      }
    }
  }

  /**
   * A simple rpn stack calculator.
   *
   * @see <a href="https://en.wikipedia.org/wiki/Reverse_Polish_notation">Reverse Polish
   *     Notation</a>
   * @param expr a string representing an order of operations on a stack
   * @return the top-most value left on the stack
   */
  static int rpnCalc(String expr) {
    if (expr == null || expr.length() == 0) {
      return 0;
    }
    List<String> atoms = new ArrayList<String>(Arrays.asList(expr.split("\\s")));
    List<Integer> stack = new ArrayList<Integer>();

    // Filter for accepted strings
    atoms = atoms.stream().filter(atom -> {
      return isNumber(atom)
          || atom.equals("+")
          || atom.equals("-")
          || atom.equals("*")
          || atom.equals("/");
    }).toList();

    if (atoms.size() == 0) {
      return 0;
    }

    for (String atom : atoms) {
      if (isOperator(atom)) {
        applyOperationToStack(stack, atom);
      } else {
        stack.add(Integer.parseInt(atom));
      }
    }

    return stack.get(stack.size() - 1);
  }

  /**
   * A simple rpn stack calculator. Variables should be a map of tuples (var_name, var_value).
   *
   * @see <a href="https://en.wikipedia.org/wiki/Reverse_Polish_notation">Reverse Polish
   *     Notation</a>
   * @param expr a string representing an order of operations on a stack
   * @return the top-most value left on the stack
   */
  static int rpnCalc(String expr, Map<String, Integer> variables) {
    if (expr == null || expr.length() == 0) {
      return 0;
    }
    if (variables == null || variables.size() == 0) {
      return rpnCalc(expr);
    }

    List<String> atoms = new ArrayList<String>(Arrays.asList(expr.split("\\s")));

    for (String atom : atoms) {
      if (variables.containsKey(atom)) {
        atoms.set(atoms.indexOf(atom), variables.get(atom).toString());
      }
    }

    return rpnCalc(String.join(" ", atoms));
  }
}
