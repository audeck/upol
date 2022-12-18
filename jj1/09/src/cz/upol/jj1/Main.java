package cz.upol.jj1;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.PrintWriter;
import java.nio.file.FileSystemException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.EmptyStackException;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;

public class Main {
  /** The offset of digits in the ASCII table (for chars) */
  private static final int ASCII_DIGIT_OFFSET = 48;
  private static final int DOUBLE_MIN_DECIMAL_EXPONENT = 324;

  public static void main(String[] args) throws IOException {
    rpnCalc(
        Path.of("C:\\Users\\suchlu05\\IdeaProjects\\rpnexpr.txt"),
        Path.of("C:\\Users\\suchlu05\\IdeaProjects\\rpnout.txt")
    );
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

    // O(N) B)
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
   * as well as all the basic comparison operations (<, >, =, <=, >=), plus the ternary
   * operator (?).
   *
   * @see <a href="https://en.wikipedia.org/wiki/Reverse_Polish_notation">Reverse Polish
   *     Notation</a>
   */
  public static class RPNCalculator {
    /** Stack representation */
    static List<String> stack = new ArrayList<String>();

    private static String pop() {
      if (stack.size() < 1) {
        throw new EmptyStackException();
      }

      return stack.remove(stack.size() - 1);
    }

    private static void push(String s) {
      stack.add(s);
    }

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

    /** Pops 2 atoms from the stack and pushes their sum */
    private static void applyAddition() {
      // Get terms
      String term1 = pop();
      String term2 = pop();

      boolean returnsFloat = isFloat(term1) || isFloat(term2);
      String result;

      if (returnsFloat) {
        result = Double.toString(Double.parseDouble(term1) + Double.parseDouble(term2));
      } else {
        result = Integer.toString(Integer.parseInt(term1) + Integer.parseInt(term2));
      }

      push(result);
    }

    /** Pops 2 atoms from the stack and pushes their difference */
    private static void applySubtraction() {
      push("-" + pop());  // Change the sign of term2
      applyAddition();
    }

    /** Pops 2 atoms from the stack and pushes their product */
    private static void applyMultiplication() {
      String term1 = pop();
      String term2 = pop();

      boolean returnsFloat = isFloat(term1) || isFloat(term2);
      String result;

      if (returnsFloat) {
        result = Double.toString(Double.parseDouble(term1) * Double.parseDouble(term2));
      } else {
        result = Integer.toString(Integer.parseInt(term1) * Integer.parseInt(term2));
      }

      push(result);
    }

    /** Pops 2 atoms from the stack and pushes the result of secondPop / firstPop (always float!) */
    private static void applyDivision() {
      String denominator = pop();
      String numerator = pop();

      String result;

      if (Double.parseDouble(denominator) == 0d) {
        throw new ArithmeticException("Tried to divide by zero (big bad!)");
      }

      result = Double.toString(Double.parseDouble(numerator) / Double.parseDouble(denominator));
      push(result);
    }

    /** Pops 2 atoms from the stack and pushes the result of secondPop < firstPop */
    private static void applyLessThan() {
      String term2 = pop();
      String term1 = pop();
      boolean result;

      if (isFloat(term1) || isInteger(term1) && isFloat(term2) || isInteger(term2)) {
        result = (Double.parseDouble(term1) < Double.parseDouble(term2));
      }
      else if (isBoolean(term1) && isBoolean(term2)) {
        result = (term1.equals("#f") && term2.equals("#t"));
      }
      else {
        throw new IllegalArgumentException("Uncomparable values: " + term1 + " and " + term2);
      }

      push(result ? "#t" : "#f");
    }

    /** Pops 2 atoms from the stack and pushes the result of secondPop > firstPop */
    private static void applyGreaterThan() {
      // Swap top 2 atoms
      String term1 = pop();
      String term2 = pop();
      push(term1);
      push(term2);

      // Apply less than
      applyLessThan();
    }

    /** Pops 2 atoms from the stack and pushes #t if they're equal, #f otherwise */
    private static void applyCompare() {
      String term1 = pop();
      String term2 = pop();
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
        throw new IllegalArgumentException("Incomparable values: " + term1 + " and " + term2);
      }

      push(result ? "#t" : "#f");
    }

    private static void applyOr() {
      String term1 = pop();
      String term2 = pop();

      if (!isBoolean(term2) || !isBoolean(term1)) {
        throw new IllegalArgumentException("Illegal atoms for application of or: "
            + term1 + " " + term2);
      }

      if (term1.equals("#t") || term2.equals("#t")) {
        push("#t");
      } else {
        push("#f");
      }
    }

    private static void applyLessThanEqual() {
      String term1 = pop();
      String term2 = pop();

      push(term1);
      push(term2);
      applyLessThan();

      push(term1);
      push(term2);
      applyCompare();

      applyOr();
    }

    private static void applyGreaterThanEqual() {
      // Swap top 2 atoms
      String term1 = pop();
      String term2 = pop();
      push(term1);
      push(term2);

      // Apply less than or equal
      applyLessThanEqual();
    }

    private static void applyTernary() {
      String term1 = pop();
      String term2 = pop();
      String term3 = pop();

      if (!isBoolean(term1)) {
        throw new IllegalArgumentException("Can't decide ternary on invalid atom: " + term1);
      }

      push(term1.equals("#t") ? term2 : term3);
    }

    /**
     * Applies an operation to the internal stack, depending on the operator given.
     *
     * @param operator a valid operator
     * @see RPNCalculator#isOperator(String)
     */
    private static void applyOperation(String operator) {
      switch (operator) {
        case "+" -> {
          applyAddition();
        }
        case "-" -> {
          applySubtraction();
        }
        case "*" -> {
          applyMultiplication();
        }
        case "/" -> {
          applyDivision();
        }
        case "<" -> {
          applyLessThan();
        }
        case ">" -> {
          applyGreaterThan();
        }
        case "=" -> {
          applyCompare();
        }
        case "<=" -> {
          applyLessThanEqual();
        }
        case ">=" -> {
          applyGreaterThanEqual();
        }
        case "?" -> {
          applyTernary();
        }
        default -> {
          // Unknown operator
          throw new IllegalArgumentException("Unknown operator: '" + operator + "'");
        }
      }
    }

    private static void replaceVariables(List<String> atoms, Map<String, Object> variables) {
      if (variables != null && variables.size() > 0) {
        for (String atom : atoms) {
          if (variables.containsKey(atom)) {
            atoms.set(atoms.indexOf(atom), variables.get(atom).toString());
          }
        }
      }
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

      replaceVariables(atoms, variables);

      // Filter out invalid atoms and handle each atom (add numbers/booleans; apply operations)
      atoms.stream().filter(RPNCalculator::isValidAtom).forEach(atom -> {
        if (isOperator(atom)) {
          applyOperation(atom);
        } else {
          push(atom);
        }
      });

      return (stack.isEmpty()) ? "" : pop();
    }
  }

  /** @see RPNCalculator#calculate(String) */
  public static String rpnCalc(String expr) {
    return RPNCalculator.calculate(expr);
  }

  /** @see RPNCalculator#calculate(String, Map) */
  public static String rpnCalc(String expr, Map<String, Object> variables) {
    return RPNCalculator.calculate(expr, variables);
  }

  /**
   * Reads all lines from file input and writes their rpnCalc results to file output.
   *
   * @param input file containing rpn expressions
   * @param output file
   * @throws IOException
   * @see RPNCalculator
   */
  public static void rpnCalc(Path input, Path output) throws IOException {
    if (!Files.exists(input) || Files.isDirectory(input)) {
      throw new IOException("Invalid input file: " + input.toString());
    }

    StringBuilder outputString = new StringBuilder("");

    Files.lines(input).forEach((line) -> {
      outputString.append(RPNCalculator.calculate(line)).append("\n");
    });

    if (!Files.exists(output)) {
      Files.createFile(output);
    }
    Files.writeString(output, outputString);
  }

  /**
   * Reads all lines from file input and writes their rpnCalc result into the same file.
   *
   * @param input
   * @throws IOException
   * @see RPNCalculator
   */
  public static void rpnCalc(Path input) throws IOException {
    rpnCalc(input, input);
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
        throw new NumberFormatException("Cannot parse number from a string: " + number);
      }
    }
  }

  /**
   * Parses a base (= radix) 10 integer from a given string. If the absolute value of the integer is
   * greater than Integer.MAX_VALUE, returns Optional.of(Integer.MAX_VALUE) or
   * Optional.of(-Integer.MAX_VALUE), depending on the string's leading sign.
   *
   * @param string the given string
   * @return Optional containing the integer or Integer.MAX_VALUE if abs(integer) is greater
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

  /** Return the index of the first decimal point ('.') in a string, or -1 if not found. */
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
   *
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

    // Get, reverse and check the format of the number's integer part (should be an integer)
    String integerPart = new StringBuilder(string.substring(0, decimalIndex)).reverse().toString();
    checkNumberFormat(integerPart);

    // Get and check the format of the number's decimal part (should be an integer)
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
    // Only when decimalPart isn't full; otherwise scale = Infinity
    if (decimalPart.length() < DOUBLE_MIN_DECIMAL_EXPONENT) {
      double scale = Math.pow(10, decimalPart.length());
      number = Math.floor(number * scale)/scale;
    }

    return isNegative ? Optional.of(-number) : Optional.of(number);
  }
}
