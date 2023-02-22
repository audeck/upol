package cz.upol.jj2.ReaderWriters;

import cz.upol.jj2.Receipts.Receipt;
import cz.upol.jj2.Receipts.ReceiptItem;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.StringWriter;
import javax.xml.stream.XMLInputFactory;
import javax.xml.stream.XMLOutputFactory;
import javax.xml.stream.XMLStreamReader;
import javax.xml.stream.XMLStreamWriter;

/**
 * This class implements reading and writing receipt to and from XML using the StAX API
 */
public class StAXReceiptReaderWriter implements ReceiptReaderWriter {

  @Override
  public Receipt loadReceipt(InputStream input) throws Exception {
    XMLInputFactory xmlInputFactory = XMLInputFactory.newInstance();
    XMLStreamReader reader = xmlInputFactory.createXMLStreamReader(input);
    Receipt receipt = new Receipt();

    while (reader.hasNext()) {
      if (reader.getEventType() == XMLStreamReader.START_ELEMENT) {
        String element = reader.getName().toString();

        // Name
        if (element.equalsIgnoreCase("name")) {
          receipt.setName(reader.getElementText());
        // Itin
        } else if (element.equalsIgnoreCase("itin")) {
          receipt.setItin(reader.getElementText());
        // Item
        } else if (element.equalsIgnoreCase("item")) {
          ReceiptItem item = new ReceiptItem();
          String amount = reader.getAttributeValue(null, "amount");
          String unitPrice = reader.getAttributeValue(null, "unitPrice");

          item.setName(reader.getElementText());
          item.setAmount(Integer.parseInt(amount));
          item.setUnitPrice(Integer.parseInt(unitPrice));

          receipt.addItem(item);
        }
      }

      // Iterate reader
      reader.next();
    }

    return receipt;
  }

  @Override
  public void storeReceipt(OutputStream output, Receipt receipt) throws Exception {
    XMLOutputFactory xmlOutputFactory = XMLOutputFactory.newInstance();
    XMLStreamWriter xmlStreamWriter = xmlOutputFactory.createXMLStreamWriter(output);

    // XML creation (follows usual XML structure)
    xmlStreamWriter.writeStartDocument();

    xmlStreamWriter.writeStartElement("receipt");
    xmlStreamWriter.writeAttribute("total", String.valueOf(receipt.getTotal()));

    xmlStreamWriter.writeStartElement("name");
    xmlStreamWriter.writeCharacters(receipt.getName());
    xmlStreamWriter.writeEndElement();

    xmlStreamWriter.writeStartElement("itin");
    xmlStreamWriter.writeCharacters(receipt.getItin());
    xmlStreamWriter.writeEndElement();

    xmlStreamWriter.writeStartElement("items");

    for (ReceiptItem item : receipt.getItems()) {
      xmlStreamWriter.writeStartElement("item");
      xmlStreamWriter.writeAttribute("amount", String.valueOf(item.getAmount()));
      xmlStreamWriter.writeAttribute("unitPrice", String.valueOf(item.getUnitPrice()));
      xmlStreamWriter.writeCharacters(item.getName());
      xmlStreamWriter.writeEndElement();
    }

    xmlStreamWriter.writeEndElement(); // items
    xmlStreamWriter.writeEndElement(); // receipt

    xmlStreamWriter.writeEndDocument();
  }
}
