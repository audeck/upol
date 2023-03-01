package cz.upol.jj2.XMLReaderWriters;

import java.io.InputStream;
import java.io.OutputStream;
import java.util.Map;
import java.util.TreeMap;
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
import org.xml.sax.SAXException;

/**
 * This class implements saving and loading of user data to and from XML.
 * Interface-less since this isn't the right storage solution anyway.
 */
public class XMLUserDataReaderWriter {

  /**
   *  Loads user data into a map of (String, String) pairs from an XML file. Map used is TreeMap.
   */
  public Map<String, String> loadUserData(InputStream input) throws Exception {
    DocumentBuilderFactory documentBuilderFactory = DocumentBuilderFactory.newInstance();
    DocumentBuilder documentBuilder = documentBuilderFactory.newDocumentBuilder();
    Document document;
    try {
      document = documentBuilder.parse(input);
    } catch (SAXException e) {
      // No valid XML file
      return new TreeMap<>();
    }

    NodeList users = document.getElementsByTagName("user");
    Map<String, String> result = new TreeMap<>();

    for (int i = 0; i < users.getLength(); i += 1) {
      Node user = users.item(i);
      String userName = user.getAttributes().getNamedItem("name").getTextContent();
      String userPass = user.getAttributes().getNamedItem("password").getTextContent();
      result.put(userName, userPass);
    }

    return result;
  }

  /**
   *  Saves user data into an XML file.
   */
  public void saveUserData(OutputStream output, Map<String, String> userData) throws Exception {
    DocumentBuilderFactory documentBuilderFactory = DocumentBuilderFactory.newInstance();
    DocumentBuilder documentBuilder = documentBuilderFactory.newDocumentBuilder();
    Document document = documentBuilder.newDocument();

    Element users = document.createElement("users");
    document.appendChild(users);

    for (var user : userData.entrySet()) {
      Element userElement = document.createElement("user");
      userElement.setAttribute("name", user.getKey());
      userElement.setAttribute("password", user.getValue());
      users.appendChild(userElement);
    }

    TransformerFactory transformerFactory = TransformerFactory.newInstance();
    Transformer transformer = transformerFactory.newTransformer();

    transformer.transform(new DOMSource(document), new StreamResult(output));
  }
}
