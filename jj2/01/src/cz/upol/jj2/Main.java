package cz.upol.jj2;

import cz.upol.jj2.ReaderWriters.DOMReceiptReaderWriter;
import cz.upol.jj2.ReaderWriters.SAXReceiptReaderWriter;
import cz.upol.jj2.ReaderWriters.StAXReceiptReaderWriter;
import cz.upol.jj2.Receipts.Receipt;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;

public class Main {

  public static void main(String[] args) {
    //testReaderWriters();
  }

  public static void testReaderWriters() {
    try {
      Receipt receipt;
      Path inputPath = Paths.get("xml/receipt.xml");
      Path outputPath;

      // DOM API
      DOMReceiptReaderWriter dom = new DOMReceiptReaderWriter();
      receipt = dom.loadReceipt(Files.newInputStream(inputPath));
      System.out.println(receipt);

      outputPath = Paths.get("xml/dom.xml");
      dom.storeReceipt(Files.newOutputStream(outputPath), receipt);

      // SAX
      SAXReceiptReaderWriter sax = new SAXReceiptReaderWriter();
      receipt = sax.loadReceipt(Files.newInputStream(inputPath));
      System.out.println(receipt);

      // StAX
      StAXReceiptReaderWriter stax = new StAXReceiptReaderWriter();
      receipt = stax.loadReceipt(Files.newInputStream(inputPath));
      System.out.println(receipt);

      outputPath = Paths.get("xml/stax.xml");
      stax.storeReceipt(Files.newOutputStream(outputPath), receipt);

    } catch (Exception e) {
      e.printStackTrace();
    }
  }
}
