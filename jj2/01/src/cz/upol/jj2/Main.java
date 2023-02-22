package cz.upol.jj2;

import cz.upol.jj2.ReaderWriters.DOMReceiptReaderWriter;
import cz.upol.jj2.ReaderWriters.SAXReceiptReaderWriter;
import cz.upol.jj2.ReaderWriters.StAXReceiptReaderWriter;
import cz.upol.jj2.Receipts.Receipt;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.InputStream;
import java.io.OutputStream;

public class Main {

  public static void main(String[] args) {
    //testReaderWriters();
  }

  public static void testReaderWriters() {
    try {
      InputStream input;
      OutputStream output;

      // DOM API
      DOMReceiptReaderWriter dom = new DOMReceiptReaderWriter();
      input = new FileInputStream("E:\\Github\\upol\\jj2\\01\\test.xml");

      Receipt receipt1 = dom.loadReceipt(input);
      System.out.println(receipt1);

      output = new FileOutputStream("E:\\Github\\upol\\jj2\\01\\dom.xml");
      dom.storeReceipt(output, receipt1);

      // SAX
      SAXReceiptReaderWriter sax = new SAXReceiptReaderWriter();
      input = new FileInputStream("E:\\Github\\upol\\jj2\\01\\test.xml");

      Receipt receipt2 = sax.loadReceipt(input);
      System.out.println(receipt2);

      // StAX
      StAXReceiptReaderWriter stax = new StAXReceiptReaderWriter();
      input = new FileInputStream("E:\\Github\\upol\\jj2\\01\\test.xml");

      Receipt receipt3 = stax.loadReceipt(input);
      System.out.println(receipt3);

      output = new FileOutputStream("E:\\Github\\upol\\jj2\\01\\stax.xml");
      dom.storeReceipt(output, receipt3);
    } catch (Exception e) {
      e.printStackTrace();
    }
  }
}
