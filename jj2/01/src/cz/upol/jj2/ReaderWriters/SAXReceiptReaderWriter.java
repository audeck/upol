package cz.upol.jj2.ReaderWriters;

import cz.upol.jj2.Receipts.Receipt;
import cz.upol.jj2.Receipts.ReceiptItem;
import java.io.InputStream;
import java.io.OutputStream;
import javax.xml.parsers.SAXParser;
import javax.xml.parsers.SAXParserFactory;
import org.xml.sax.Attributes;
import org.xml.sax.SAXException;
import org.xml.sax.helpers.DefaultHandler;

/**
 * This class implement reading receipts from XML using the Simple API for XML
 */
public class SAXReceiptReaderWriter implements ReceiptReaderWriter {

  @Override
  public Receipt loadReceipt(InputStream input) throws Exception {
    SAXParserFactory parserFactory = SAXParserFactory.newInstance();
    SAXParser parser = parserFactory.newSAXParser();
    final Receipt receipt = new Receipt();

    parser.parse(
        input,
        new DefaultHandler() {
          boolean isName = false;
          boolean isItin = false;
          boolean isItem = false;
          ReceiptItem currentItem;

          @Override
          public void startElement(
              String uri, String localName, String qName, Attributes attributes)
              throws SAXException {
            if (qName.equalsIgnoreCase("name")) {
              isName = true;
            } else if (qName.equalsIgnoreCase("itin")) {
              isItin = true;
            } else if (qName.equalsIgnoreCase("item")) {
              isItem = true;

              currentItem = new ReceiptItem();
              currentItem.setUnitPrice(Integer.parseInt(attributes.getValue("unitPrice")));
              currentItem.setAmount(Integer.parseInt(attributes.getValue("amount")));
            }
          }

          @Override
          public void characters(char ch[], int start, int length) throws SAXException {
            String text = new String(ch, start, length);
            if (isName) {
              receipt.setName(text);
            } else if (isItin) {
              receipt.setItin(text);
            } else if (isItem) {
              currentItem.setName(text.replaceAll("^\\s+|\\s+$", ""));
            }
          }

          @Override
          public void endElement(String uri, String localName, String qName) throws SAXException {
            if (qName.equalsIgnoreCase("name")) {
              isName = false;
            } else if (qName.equalsIgnoreCase("itin")) {
              isItin = false;
            } else if (qName.equalsIgnoreCase("item")) {
              isItem = false;
              receipt.addItem(currentItem);
            }
          }
        });

    return receipt;
  }

  @Override
  public void storeReceipt(OutputStream output, Receipt receipt) throws Exception {
    throw new NoSuchMethodException("The SAX API doesn't support storing receipts");
  }
}
