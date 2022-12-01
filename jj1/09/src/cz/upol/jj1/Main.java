package cz.upol.jj1;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;

public class Main {

  public static void main(String[] args) {
    // code here
  }

  /**
   * Counts substring occurrences in a given string and returns a map of the data. Whitespace,
   * digits, and punctuation are taken as delimiters.
   *
   * @param s a given string
   * @return a map of (substring, numberOfSubstringOccurrences)
   */
  public static Map<String, Integer> freq(String s) {
    if (s == null) {
      return null;
    }
    String[] words = s.split("\\d|\\s|\\W+");
    Map<String, Integer> output = new HashMap<String, Integer>();

    // O(N) :o
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
   * Counts substring occurrences in a given string and returns a map of the data. Whitespace,
   * digits, and punctuation are taken as delimiters. Ignores substring case!
   *
   * @param s a given string
   * @return a map of (substring, numberOfSubstringOccurrences)
   */
  public static Map<String, Integer> freqIgnoreCase(String s) {
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
  public static int rpnCalc(String expr) {
    if (expr == null || expr.length() == 0) {
      return 0;
    }
    List<String> atoms = new ArrayList<String>(Arrays.asList(expr.split("\\s")));
    List<Integer> stack = new ArrayList<Integer>();

    // Filter for accepted strings & handle instructions
    atoms.stream().filter(atom -> {
      return isNumber(atom) || isOperator(atom);
    }).forEach(atom -> {
      if (isOperator(atom)) {
        applyOperationToStack(stack, atom);
      } else {
        stack.add(Integer.parseInt(atom));
      }
    });

    return (stack.size() > 0) ? stack.get(stack.size() - 1) : 0;
  }

  /**
   * A simple rpn stack calculator. Variables should be a map of tuples (varName, varValue).
   *
   * @see <a href="https://en.wikipedia.org/wiki/Reverse_Polish_notation">Reverse Polish
   *     Notation</a>
   * @param expr a string representing an order of operations on a stack
   * @param variables a map of pairs (variableName, variableValue)
   * @return the top-most value left on the stack
   */
  public static int rpnCalc(String expr, Map<String, Integer> variables) {
    if (expr == null || expr.length() == 0) {
      return 0;
    }
    if (variables == null || variables.size() == 0) {
      return rpnCalc(expr);
    }

    List<String> atoms = new ArrayList<String>(Arrays.asList(expr.split("\\s")));

    // Replace all variable names with values
    for (String atom : atoms) {
      if (variables.containsKey(atom)) {
        atoms.set(atoms.indexOf(atom), variables.get(atom).toString());
      }
    }

    return rpnCalc(String.join(" ", atoms));
  }

  /**
   * Parses an integer given in string form.
   *
   * @param s a given string form
   * @return an integer represented by s
   */
  public static Optional<Integer> parseInt(String s) {
    char[] cs = s.toCharArray();
    int index = 0;
    boolean isNegative = false;
    int output = 0;
    int bound = 0;

    // First character can be sign
    if (cs[0] == '-' || cs[0] == '+') {
      bound += 1;
      isNegative = cs[0] == '-';
    }


    while (cs.length - index > bound) {
      double nextValue = Math.pow(10, index);
      if (Integer.MAX_VALUE - output < nextValue) {
        // RAISE HELL
      }
      index -= 1;
    }

    return Optional.of(output);
  }
}
