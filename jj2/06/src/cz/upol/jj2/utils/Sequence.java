package cz.upol.jj2.utils;

import java.util.Arrays;

/**
 * Defines valid (in the scope of this application) generalized Fibonacci number sequences.
 */
public enum Sequence {
    FIBONACCI("Fibonacci"),
    TRIBONACCI("tribonacci"),
    TETRANACCI("tetranacci"),
    PENTANACCI("pentanacci"),
    HEXANACCI("hexanacci"),
    OCTANACCI("octanacci");

    private final String name;

    Sequence(String name) {
        this.name = name;
    }

    public String getName() {
        return name;
    }

    /**
     * @param name name of sequence
     * @return sequence named `name`
     */
    public static Sequence fromName(String name) {
        for (Sequence sequence : values()) {
            if (sequence.getName().equalsIgnoreCase(name)) {
                return sequence;
            }
        }
        throw new IllegalArgumentException("Invalid sequence name: " + name);
    }

    /**
     * @return a `String` array of all sequence names
     */
    public static String[] getAllNames() {
        return Arrays.stream(values()).map(Sequence::getName).toArray(String[]::new);
    }
}
