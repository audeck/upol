package cz.upol.jj1;

import java.math.BigDecimal;
import java.text.DecimalFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;

public class Main {
  /** The offset of digits in the ASCII table (for chars) */
  private static final int ASCII_DIGIT_OFFSET = 48;

  public static void main(String[] args) {
    // code here
    System.out.println(parseInt("-00000000694317289"));
    System.out.println(parseDouble("-0004535.1239"));
  }

  /**
   * Counts substring occurences in a given string and returns a map of the data. Whitespace,
   * digits, and punctuation are taken as delimiters.
   *
   * @param s a given string
   * @return a map of (substring, numberOfSubstringOccurences)
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
   * Counts substring occurences in a given string and returns a map of the data. Whitespace,
   * digits, and punctuation are taken as delimiters. Ignores substring case!
   *
   * @param s a given string
   * @return a map of (substring, numberOfSubstringOccurences)
   */
  public static Map<String, Integer> freqIgnoreCase(String s) {
    if (s == null) {
      return null;
    }
    return freq(s.toLowerCase());
  }

  /**
   * A simple rpn stack calculator. Supports both integer and double (float) format numbers, as well
   * as two truth values (#t for true; #f for false). Doesn't generalize truth values (numbers are
   * incomparable with truth values). Supports all 4 basic arithmetic operations (+, -, *, /),
   * as well as all the basic boolean arithmetic operations (<, >, =, <=, >=), plus the ternary
   * operator ?.
   *
   * @see <a href="https://en.wikipedia.org/wiki/Reverse_Polish_notation">Reverse Polish
   *     Notation</a>
   */
  public static class RPNCalculator {
    /** Stack representation */
    static List<String> stack = new ArrayList<String>();

    /**
     * Checks if a given string is a base 10 float. Specially, returns false for empty
     * string and null.
     *
     * @param s a given string
     * @return true if s is a non-empty string containing only base 10 digits + '.', otherwise false
     */
    private static boolean isFloat(String s) {
      if (s == null || s.length() == 0) {
        return false;
      }
      char[] sChars = s.toCharArray();
      boolean isFloat = false;
      int start = 0;

      if (sChars[start] == '-') {
        start += 1;
      }

      for (int i = start; i < s.length(); i += 1) {
        if (sChars[i] == '.' && !isFloat) {
          isFloat = true;
        } else if ('0' > sChars[i] || sChars[i] > '9') {
          return false;
        }
      }

      return isFloat;
    }

    /**
     * Checks if a given string is a base 10 integer. Specially, returns false for empty
     * string and null.
     *
     * @param s a given string
     * @return true if s is a non-empty string containing only base 10 digits, otherwise false
     */
    private static boolean isInteger(String s) {
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
     * Checks if a given string is a truth value (boolean) of format #t or #f.
     *
     * @param s a given string
     * @return true if s is either #t or #f, otherwise false
     */
    private static boolean isBoolean(String s) {
      return s.charAt(0) == '#' && (s.charAt(1) == 't' || s.charAt(1) == 'f');
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
      return s.equals("+") || s.equals("-") || s.equals("*") || s.equals("/") || s.equals("<")
          || s.equals(">") || s.equals("=") || s.equals("<=") || s.equals(">=") || s.equals("?");
    }

    /** Unified check for all valid atom types */
    private static boolean isValidAtom(String s) {
      if (!isInteger(s) && !isFloat(s) && !isBoolean(s) && !isOperator(s)) {
        throw new IllegalArgumentException("Expression contains an invalid atom: " + s);
      }
      return true;
    }

    /** Exception function */
    private static void stackSizeException(String operator) {
      throw new IllegalArgumentException("Stack contains too few atoms to perform operation '"
          + operator
          + "'"
      );
    }

    /** Returns the result of term1 + term2 */
    private static String addition(String term1, String term2) {
      boolean returnsFloat = isFloat(term1) || isFloat(term2);

      if (returnsFloat) {
        return Double.toString(Double.parseDouble(term1) + Double.parseDouble(term2));
      } else {
        return Integer.toString(Integer.parseInt(term1) + Integer.parseInt(term2));
      }
    }

    /** Returns the result of term1 - term2 */
    private static String subtraction(String term1, String term2) {
      return addition(term1, "-" + term2);
    }

    /** Returns the result of term1 * term2 */
    private static String multiplication(String term1, String term2) {
      boolean returnsFloat = isFloat(term1) || isFloat(term2);

      if (returnsFloat) {
        return Double.toString(Double.parseDouble(term1) * Double.parseDouble(term2));
      } else {
        return Integer.toString(Integer.parseInt(term1) * Integer.parseInt(term2));
      }
    }

    /** Returns the result of term1 / term2 (always a float) */
    private static String division(String term1, String term2) {
      if (Double.parseDouble(term2) == 0d) {
        throw new ArithmeticException("Tried to divide by zero (big bad!)");
      }

      return Double.toString(Double.parseDouble(term1) / Double.parseDouble(term2));
    }

    /** Returns the result of term1 < term2 */
    private static String lessThan(String term1, String term2) {
      boolean result;

      if (isInteger(term1) && isInteger(term2)) {
        result = (Integer.parseInt(term1) < Integer.parseInt(term2));
      }
      else if (isFloat(term1) && isFloat(term2)) {
        result = (Double.parseDouble(term1) < Double.parseDouble(term2));
      }
      else if (isBoolean(term1) && isBoolean(term2)) {
        result = (term1.equals("#f") && term2.equals("#t"));
      }
      else {
        throw new IllegalArgumentException("Uncomparable values: " + term1 + " and " + term2);
      }

      return result ? "#t" : "#f";
    }

    /** Returns the result of term1 > term2 */
    private static String greaterThan(String term1, String term2) {
      return lessThan(term2, term1);
    }

    /** Returns the result of term1 = term2 */
    private static String compare(String term1, String term2) {
      boolean result;

      if (isInteger(term1) && isInteger(term2)) {
        result = (Integer.parseInt(term1) == Integer.parseInt(term2));
      }
      else if (isFloat(term1) && isFloat(term2)) {
        result = (Double.parseDouble(term1) == Double.parseDouble(term2));
      }
      else if (isBoolean(term1) && isBoolean(term2)) {
        result = (term1.equals(term2));
      }
      else {
        throw new IllegalArgumentException("Uncomparable values: " + term1 + " and " + term2);
      }

      return result ? "#t" : "#f";
    }

    /** Returns the result of term1 <= term2 */
    private static String lessThanEqual(String term1, String term2) {
      return (lessThan(term1, term2).equals("#t") || compare(term1, term2).equals("#t"))
          ? "#t" : "#f";
    }

    /** Returns the result of term1 >= term2 */
    private static String greaterThanEqual(String term1, String term2) {
      return (greaterThan(term1, term2).equals("#t") || compare(term1, term2).equals("#t"))
          ? "#t" : "#f";
    }

    /** Returns the result of term1 ? term2 : term3 */
    private static String ternary(String term1, String term2, String term3) {
      return (compare(term1, "#t").equals("#t")) ? term2 : term3;
    }

    /**
     * Applies an operation to the internal stack, depending on the operator given.
     *
     * @param operator a given operator (one of "+", "-", "*" or "/")
     */
    private static void applyOperationToStack(String operator) {
      int size = stack.size();
      String operationResult = "";
      String firstArg, secondArg, thirdArg;

      // Binary operation size check
      if (size < 2) {
        stackSizeException(operator);
      }

      // Binary operation application
      firstArg = stack.get(size - 1);
      secondArg = stack.get(size - 2);

      switch (operator) {
        case "+" -> {
          operationResult = addition(secondArg, firstArg);
        }
        case "-" -> {
          operationResult = subtraction(secondArg, firstArg);
        }
        case "*" -> {
          operationResult = multiplication(secondArg, firstArg);
        }
        case "/" -> {
          operationResult = division(secondArg, firstArg);
        }
        case "<" -> {
          operationResult = lessThan(secondArg, firstArg);
        }
        case ">" -> {
          operationResult = greaterThan(secondArg, firstArg);
        }
        case "=" -> {
          operationResult = compare(secondArg, firstArg);
        }
        case "<=" -> {
          operationResult = lessThanEqual(secondArg, firstArg);
        }
        case ">=" -> {
          operationResult = greaterThanEqual(secondArg, firstArg);
        }
      }

      // Update stack and return if able
      if (!operationResult.equals("")) {
        stack.set(size - 2, operationResult);
        stack.remove(size - 1);
        return;
      }

      // Ternary operation size check...
      if (size < 3) {
        stackSizeException(operator);
      }

      thirdArg = stack.get(size - 3);

      switch (operator) {
        case "?" -> {
          operationResult = ternary(firstArg, secondArg, thirdArg);
        }
      }

      if (!operationResult.equals("")) {
        stack.set(size - 3, operationResult);
        stack.remove(size - 2);
        stack.remove(size - 1);
        return;
      }

      // Unknown operator
      throw new IllegalArgumentException("Unknown operator: '" + operator + "'");
    }

    /**
     * Calculates the value of expression (it's right-most value).
     *
     * @param expression a string representing an order of operations on an RPN stack
     * @return the right-most value left on the stack (adding left to right)
     */
    public static String calculate(String expression) {
      return calculate(expression, null);
    }

    /**
     * Calculates the value of expression (it's right-most value). Variables should be a map of
     * tuples (variableName, variableValue).
     *
     * @param expression a string representing an order of operations on an RPN stack
     * @return the right-most value left on the stack (adding left to right)
     */
    public static String calculate(String expression, Map<String, Object> variables) {
      if (expression == null || expression.length() == 0) {
        return expression;
      }

      // Regex split on whitespace
      List<String> atoms = new ArrayList<String>(Arrays.asList(expression.split("\\s")));

      // Replace variables with their mapped values
      if (variables != null && variables.size() > 0) {
        for (String atom : atoms) {
          if (variables.containsKey(atom)) {
            atoms.set(atoms.indexOf(atom), variables.get(atom).toString());
          }
        }
      }

      // Filter out invalid atoms and handle each atom (add numbers/booleans; apply operations)
      atoms.stream().filter(RPNCalculator::isValidAtom).forEach(atom -> {
        if (isOperator(atom)) {
          applyOperationToStack(atom);
        } else {
          stack.add(atom);
        }
      });

      return (stack.size() > 0) ? stack.get(stack.size() - 1) : "";
    }
  }

  public static String rpnCalc(String expr) {
    return RPNCalculator.calculate(expr);
  }

  public static String rpnCalc(String expr, Map<String, Object> variables) {
    return RPNCalculator.calculate(expr, variables);
  }

  /** Removes all leading zeros from a given string */
  private static String removeLeadingZeros(String string) {
    while (string.charAt(0) == '0') {
      string = string.substring(1);
    }

    return string;
  }

  /** Checks if a string contains only digit characters; throws a NumberFormatException otherwise */
  private static void checkNumberFormat(String number) {
    for (int i = 0; i < number.length(); i += 1) {
      if (number.charAt(i) < '0' || '9' < number.charAt(i)) {
        throw new NumberFormatException("Cannot parse number from a string containing non-digits "
            + "(apart from leading sign)");
      }
    }
  }

  /**
   * Parses a base (= radix) 10 integer from a given string. If the absolute value of the integer is
   * greater than Integer.MAX_VALUE, returns Optional.of(Integer.MAX_VALUE) or
   * Optional.of(-Integer.MAX_VALUE), depending on the string's leading sign.
   *
   * @param string the given string
   * @return Optional containing the integer, or Integer.MAX_VALUE if abs(integer) is greater
   */
  public static Optional<Integer> parseInt(String string) {
    if (string == null) {
      throw new NumberFormatException("Cannot parse integer from a null string");
    }
    boolean isNegative = false;
    int start = 0;
    int number = 0;

    // Check leading sign (if present)
    if (string.charAt(0) == '+' || string.charAt(0) == '-') {
      start += 1;
      isNegative = string.charAt(0) == '-';
    }

    // Remove leading zeros and check if string contains only digits
    string = removeLeadingZeros(string.substring(start));
    checkNumberFormat(string);

    // Reverse string
    String gnirts = new StringBuilder(string).reverse().toString();

    // "Build" parsed number
    for (int i = 0; i < gnirts.length(); i += 1) {
      number += (gnirts.charAt(i) - ASCII_DIGIT_OFFSET) * Math.pow(10, i);
    }

    return isNegative ? Optional.of(-number) : Optional.of(number);
  }

  /** Finds and return the index of the first decimal point ('.') in a string */
  private static int indexOfDecimalPoint(String number) {
    for (int i = 0; i < number.length(); i += 1) {
      if (number.charAt(i) == '.') {
        return i;
      }
    }

    return -1;
  }

  /**
   * Parses a base (= radix) 10 double from a given string. If the double's value is higher than
   * Double.MAX_VALUE, returns an Optional of Double.POSITIVE_INFINITY or Double.NEGATIVE_INFINITY
   * respectively. If the double's value is closer to zero than Double.MIN_VALUE, returns an
   * Optional of 0.
   * @param string the given string
   * @return Optional containing Double.POSITIVE_INFINITY or Double.NEGATIVE_INFINITY respectively
   *         if the parsed double's absolute value is greater than Double.MAX_VALUE, 0 if the parsed
   *         double's absolute value is lower than Double.MIN_VALUE, or just the double's value
   *         otherwise
   */
  public static Optional<Double> parseDouble(String string) {
    if (string == null) {
      throw new NumberFormatException("Cannot parse double from a null string");
    }
    boolean isNegative = false;
    int start = 0;
    double number = 0;
    int decimalIndex = indexOfDecimalPoint(string);

    // If there is no decimal point, just parse integer and cast to double
    if (decimalIndex == -1) {
      return Optional.of((double) parseInt(string).get());
    }

    // Check leading sign (if present)
    if (string.charAt(0) == '+' || string.charAt(0) == '-') {
      start += 1;
      isNegative = string.charAt(0) == '-';
    }

    // Remove sign and leading zeros; update decimal index
    string = removeLeadingZeros(string.substring(start));
    decimalIndex = indexOfDecimalPoint(string);

    // Get, reverse and check the format of the number's integer part
    String integerPart = new StringBuilder(string.substring(0, decimalIndex)).reverse().toString();
    checkNumberFormat(integerPart);

    // Get and check the format of the number's decimal part
    String decimalPart = string.substring(decimalIndex + 1);
    checkNumberFormat(decimalPart);

    // "Build" the number's integer part
    for (int i = 0; i < integerPart.length(); i += 1) {
      number += (integerPart.charAt(i) - ASCII_DIGIT_OFFSET) * Math.pow(10, i);
    }

    // "Build" the number's decimal part
    for (int i = 0; i < decimalPart.length(); i += 1) {
      number += (decimalPart.charAt(i) - ASCII_DIGIT_OFFSET) * Math.pow(10, -(i + 1));
    }

    // Truncate, because computers can't do math (precisely) sometimes
    double scale = Math.pow(10, decimalPart.length());
    number = Math.floor(number * scale)/scale;

    return isNegative ? Optional.of(-number) : Optional.of(number);
  }
}
