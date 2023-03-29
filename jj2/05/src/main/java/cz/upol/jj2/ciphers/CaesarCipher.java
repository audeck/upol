package cz.upol.jj2.ciphers;

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

    public char decipher(char character) {
        if ('A' <= character && character <= 'Z') {
            return (char) (Math.floorMod((character - 'A') + leftShift, 'Z' - 'A' + 1) + 'A');
        } else {
            return character;
        }
    }

    public String decipher(String string) {
        return string
                .chars()
                .mapToObj((ch) -> String.valueOf(decipher((char) ch)))
                .collect(Collectors.joining());
    }

    public void setLeftShift(int leftShift) {
        // Could do mod 26 here, but that's up for debate; topic for a hypothetical client follow-up :^)
        this.leftShift = leftShift;
    }

    public int getLeftShift() {
        return leftShift;
    }
}
