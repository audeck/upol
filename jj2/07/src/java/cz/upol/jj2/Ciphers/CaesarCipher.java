package cz.upol.jj2.Ciphers;

import java.util.stream.Collectors;

public class CaesarCipher {
  public static final String VALID_CHARACTERS = "ABCDEFGHIJKLMNOPQRSTUVXWYZ .";

  private int leftShift;

  public CaesarCipher(int initialShift) {
    this.leftShift = initialShift;
  }

  public CaesarCipher() {
    this(0);
  }

  public char encipher(char character) {
    if ('A' <= character && character <= 'Z') {
      return (char) (Math.floorMod((character - 'A') - leftShift, 'Z' - 'A' + 1) + 'A');
    } else {
      return character;
    }
  }

  public String encipher(String string) {
    return string
        .chars()
        .mapToObj((ch) -> String.valueOf(encipher((char) ch)))
        .collect(Collectors.joining());
  }

  public boolean isValidText(String text) {
    // Check of invalid characters
    for (char c : text.toCharArray()) {
      if (VALID_CHARACTERS.indexOf(c) == -1) {
        return false;
      }
    }

    return true;
  }

  public void setLeftShift(int leftShift) {
    this.leftShift = leftShift;
  }

  public int getLeftShift() {
    return leftShift;
  }
}
