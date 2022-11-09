package cz.upol.jj1.collections;

public interface Iterator {

    /**
     * Check if a sequence has another value.
     * @return True if next value is available.
     */
    boolean hasNext();

    /**
     * Get next value in a sequence
     * @return Next value
     */
    int next();
}
