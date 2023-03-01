package cz.upol.jj2.XMLReaderWriters;

import java.io.InputStream;
import java.io.OutputStream;
import java.util.ArrayList;
import java.util.List;
import java.util.Vector;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.w3c.dom.Text;
import org.xml.sax.SAXException;

/**
 * This class implements saving and loading of messages data to and from XML.
 * Interface-less since this isn't the right storage solution anyway.
 */
public class XMLMessagesReaderWriter {

  /** Vector string order */
  public enum MessageVector {
    SENDER,
    RECIPIENT,
    MESSAGE
  }

  /**
   * Loads message data into a list of (String, String, String) vectors from an XML file.
   */
  public List<Vector<String>> loadMessages(InputStream input) throws Exception {
    DocumentBuilderFactory documentBuilderFactory = DocumentBuilderFactory.newInstance();
    DocumentBuilder documentBuilder = documentBuilderFactory.newDocumentBuilder();
    Document document;
    try {
      document = documentBuilder.parse(input);
    } catch (SAXException e) {
      // No valid XML file
      return new ArrayList<>();
    }

    NodeList messages = document.getElementsByTagName("message");
    List<Vector<String>> result = new ArrayList<>();

    for (int i = 0; i < messages.getLength(); i += 1) {
      Node message = messages.item(i);
      Vector<String> messageVec = new Vector<>();

      messageVec.add(message.getAttributes().getNamedItem("sender").getTextContent());
      messageVec.add(message.getAttributes().getNamedItem("recipient").getTextContent());
      messageVec.add(message.getTextContent());

      result.add(messageVec);
    }

    return result;
  }

  /**
   * Saves message data into an XML file.
   */
  public void saveMessages(OutputStream output, List<Vector<String>> messages) throws Exception {
    DocumentBuilderFactory documentBuilderFactory = DocumentBuilderFactory.newInstance();
    DocumentBuilder documentBuilder = documentBuilderFactory.newDocumentBuilder();
    Document document = documentBuilder.newDocument();

    Element root = document.createElement("messages");
    document.appendChild(root);

    for (Vector<String> message : messages) {
      Element messageElement = document.createElement("message");

      messageElement.setAttribute("sender", message.get(MessageVector.SENDER.ordinal()));
      messageElement.setAttribute("recipient", message.get(MessageVector.RECIPIENT.ordinal()));
      Text messageText = document.createTextNode(message.get(MessageVector.MESSAGE.ordinal()));

      root.appendChild(messageElement);
      messageElement.appendChild(messageText);
    }

    TransformerFactory transformerFactory = TransformerFactory.newInstance();
    Transformer transformer = transformerFactory.newTransformer();

    transformer.transform(new DOMSource(document), new StreamResult(output));
  }
}
