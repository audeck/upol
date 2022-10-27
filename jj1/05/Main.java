package cz.upol.jj1;

import java.util.List;
import java.util.regex.Pattern;

public class Main {

  public static void main(String[] args) {
    System.out.println(formatStr("A: %0; B: %001; C: %0", 1, 1.6));
    System.out.println(formatStr("A: %0; B: %2", "Hello!", 1.6));

    Animal a = new Animal("Alík", AnimalSpecies.DOG, Gender.MALE);
    Animal b = new Animal("Bobík", AnimalSpecies.DUCK, Gender.FEMALE);
    Animal c = new Animal("Chubaka", AnimalSpecies.DOG, Gender.FEMALE);
    Animal d = new Animal("Donald", AnimalSpecies.DUCK, Gender.MALE);

    AnimalFarm farm = new AnimalFarm(a, b, c);
    //farm.list();
    farm.add(d);
    farm.list();
  }

  /**
   * Replaces all valid specifiers with args (arg.toString()). A valid specifier is of form #number,
   * such that number is a positive integer less than the number of args (leading zeros are
   * omitted). Invalid specifiers are left ignored.
   *
   * @param format The string to be formatted
   * @param args   The objects to be replaced with specifiers
   * @return The formatted string
   */
  public static String formatStr(String format, Object... args) {
    // Regex pattern matching
    Pattern pattern = Pattern.compile("(%\\d+)");  // Escape the escape \\
    List<String> specifiers = pattern.matcher(format)
        .results()
        .map(matchResult -> matchResult.group(1))
        .toList();

    for (String specifier : specifiers) {
      int specifierValue = Integer.parseInt(specifier.substring(1));
      if (specifierValue < specifiers.size()) {
        format = format.replaceFirst(specifier, args[specifierValue].toString());
      }
    }

    return format;
  }
}
