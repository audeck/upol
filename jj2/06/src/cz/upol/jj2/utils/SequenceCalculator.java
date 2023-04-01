package cz.upol.jj2.utils;

import javax.swing.*;
import java.math.BigInteger;
import java.util.ArrayList;
import java.util.List;

/**
 * This class calculates generalized Fibonacci sequence (as defined in the `Sequence` enum) numbers.
 * Uses single branch recursion with memoization.
 *
 * @see Sequence
 */
public class SequenceCalculator {
    private final List<BigInteger>[] memo;

    public SequenceCalculator() {
        // Initialize memo
        this.memo = new ArrayList[Sequence.values().length];

        for (int i = 0; i < Sequence.values().length; i += 1) {
            this.memo[i] = new ArrayList<>();

            // Add first numbers
            for (int j = 0; j < i + 1; j += 1) {
                this.memo[i].add(BigInteger.valueOf(0));
            }
            this.memo[i].add(BigInteger.valueOf(1));
        }
    }

    /**
     * Returns the number at `index` in (a valid) `sequence`, or -1 if `worker` has been cancelled.
     * NOTE: Might be too fast to show the "cancel button" functionality. Oops.
     *
     * @param worker worker executing this task
     * @param sequence a generalized Fibonacci sequence
     * @param index index of number
     * @return number at `index` in `sequence`
     */
    public BigInteger getNumberAtIndex(SwingWorker<BigInteger, Void> worker, Sequence sequence, int index) {
        List<BigInteger> sequenceMemo = memo[sequence.ordinal()];

        // Calculate new numbers if needed
        while (!worker.isCancelled() && sequenceMemo.size() <= index) {
            BigInteger nextNumber = BigInteger.valueOf(0);

            for (int i = 1; i <= sequence.ordinal() + 2; i += 1) {
                nextNumber = nextNumber.add(sequenceMemo.get(sequenceMemo.size() - i));
            }

            sequenceMemo.add(nextNumber);
        }

        return worker.isCancelled() ? BigInteger.valueOf(-1) : sequenceMemo.get(index);
    }
}
