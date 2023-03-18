package cz.upol.jj2.receipts;

/**
 * This class is a DAO exception class, mainly meant to wrap SQLException occurrences.
 */
public class ReceiptDAOException extends Exception {
  public ReceiptDAOException(String errorMessage, Exception e) {
    super(errorMessage, e);
  }
}
