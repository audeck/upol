package cz.upol.jj1.collections;

public interface Sequence {

    /**
     * Count num,ber of elements i the sequence
     * @return Number of elements
     */
     int size();

    /**
     * Inserts new value to the sequence
     * @param value Value to be added
     */
    void insert(int value);

    /**
     * Check if a value is in a sequence
     * @param value Checked value
     * @return True if the value is in a sequence otherwise false
     */
    boolean contains(int value);

    /**
     * Delete value from a sequence
     * @param value Value to be deleted
     * @return True if value was deleted, otherwise false
     */
    boolean delete(int value);
}
