package cz.upol.jj2.ReaderWriters;

import cz.upol.jj2.Receipts.Receipt;
import cz.upol.jj2.Receipts.ReceiptItem;
import java.io.InputStream;
import java.io.OutputStream;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.transform.OutputKeys;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.Text;
import org.w3c.dom.bootstrap.DOMImplementationRegistry;
import org.w3c.dom.ls.DOMImplementationLS;
import org.w3c.dom.ls.LSSerializer;

/**
 * This class implements reading and writing receipts to and from XML using the DOM API
 */
public class DOMReceiptReaderWriter implements ReceiptReaderWriter {

  @Override
  public Receipt loadReceipt(InputStream input) throws Exception {
    // Load XML into a doc
    DocumentBuilderFactory documentBuilderFactory = DocumentBuilderFactory.newInstance();
    DocumentBuilder documentBuilder = documentBuilderFactory.newDocumentBuilder();
    Document document = documentBuilder.parse(input);

    // Create receipt
    Receipt receipt =
        new Receipt(
            document.getElementsByTagName("name").item(0).getTextContent(),
            document.getElementsByTagName("itin").item(0).getTextContent(),
            new ArrayList<ReceiptItem>());

    // Add items
    Node items = document.getElementsByTagName("items").item(0);
    for (int i = 0; i < items.getChildNodes().getLength(); i += 1) {
      Node item = items.getChildNodes().item(i);

      if (item.hasAttributes()) {
        ReceiptItem receiptItem =
            new ReceiptItem(
                item.getTextContent().replaceAll("^\\s+|\\s+$", ""),
                Integer.parseInt(item.getAttributes().getNamedItem("unitPrice").getTextContent()),
                Integer.parseInt(item.getAttributes().getNamedItem("amount").getTextContent()));

        receipt.addItem(receiptItem);
      }
    }

    return receipt;
  }

  @Override
  public void storeReceipt(OutputStream output, Receipt receipt) throws Exception {
    // Create XML doc
    DocumentBuilderFactory documentBuilderFactory = DocumentBuilderFactory.newInstance();
    DocumentBuilder documentBuilder = documentBuilderFactory.newDocumentBuilder();
    Document document = documentBuilder.newDocument();

    // Create static elements
    Element root = document.createElement("receipt");
    root.setAttribute("total", String.valueOf(receipt.getTotal()));

    Element name = document.createElement("name");
    Text nameText = document.createTextNode(receipt.getName());

    Element itin = document.createElement("itin");
    Text itinText = document.createTextNode(receipt.getItin());

    Element items = document.createElement("items");

    // Structure the elements
    document.appendChild(root);

    root.appendChild(name);
    name.appendChild(nameText);

    root.appendChild(itin);
    itin.appendChild(itinText);

    root.appendChild(items);

    // Add individual items
    for (ReceiptItem rItem : receipt.getItems()) {
      // Create item element
      Element item = document.createElement("item");
      item.setAttribute("amount", String.valueOf(rItem.getAmount()));
      item.setAttribute("unitPrice", String.valueOf(rItem.getUnitPrice()));
      Text itemText = document.createTextNode(rItem.getName());

      // Structure the item
      items.appendChild(item);
      item.appendChild(itemText);
    }

    // Serialize document and write into output
    TransformerFactory transformerFactory = TransformerFactory.newInstance();
    Transformer transformer = transformerFactory.newTransformer();

    //transformer.setOutputProperty(OutputKeys.ENCODING, "UTF-8");
    transformer.transform(new DOMSource(document), new StreamResult(output));
  }
}
