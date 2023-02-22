package cz.upol.jj2.ReaderWriters;

import cz.upol.jj2.Receipts.Receipt;
import java.io.InputStream;
import java.io.OutputStream;

/**
 * This interface standardizes reading and writing Receipts from and to the XML format.
 *
 * @see Receipt
 */
public interface ReceiptReaderWriter {
  /**
   * Nacte ze streamu XML soubor a dle nej vytvori prislusny objekt reprezentujici uctenku
   */
  public Receipt loadReceipt(InputStream input) throws Exception;

  /**
   * Ulozi do prislusneho streamu XML soubor predstavujici danou uctenku
   */
  public void storeReceipt(OutputStream output, Receipt receipt) throws Exception;
}
