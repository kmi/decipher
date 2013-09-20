package uk.ac.open.kmi.decipher.util;

import java.io.IOException;
import java.io.StringReader;

import javax.xml.namespace.QName;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerConfigurationException;
import javax.xml.transform.TransformerFactory;
import javax.xml.xpath.XPath;
import javax.xml.xpath.XPathConstants;
import javax.xml.xpath.XPathExpressionException;
import javax.xml.xpath.XPathFactory;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;

public class XMLDocHandler {

	private DocumentBuilderFactory myDocumentBuilderFactory = DocumentBuilderFactory.newInstance();
	private DocumentBuilder myDocumentBuilder = null;
	private TransformerFactory myTranformerFactory = TransformerFactory.newInstance();
	private Transformer myTransformer = myTranformerFactory.newTransformer();
	
	private static XMLDocHandler instance = null;
	private XMLDocHandler() throws ParserConfigurationException, TransformerConfigurationException {
		this.myDocumentBuilderFactory.setNamespaceAware(true);
		this.myDocumentBuilder = this.myDocumentBuilderFactory.newDocumentBuilder();
	}
	
	public static XMLDocHandler getInstance() throws Exception {
		if(instance == null) {
			try {
				instance = new XMLDocHandler();
			} catch (ParserConfigurationException e) {
				throw new Exception("configration error for parsing XML file.");
			} catch (TransformerConfigurationException e) {
				throw new Exception("configration error for parsing XML file.");
			}
		}
		return instance;
	}
	
	public Document fromString(String xml) throws SAXException, IOException {
		return this.myDocumentBuilder.parse(new InputSource(new StringReader(xml)));
	}
    
	public Object query(Document dom, String xpathQuery, QName qn){
		try
	    {
	      XPath xpath = XPathFactory.newInstance().newXPath();
	      Object e = xpath.evaluate(xpathQuery, dom, qn);
	      return e;
	    }
	    catch (XPathExpressionException e)
	    {
	      e.printStackTrace();
	    }
      return null;
	}
	
}
