package uk.ac.open.kmi.decipher.util;

import java.io.IOException;
import java.io.InputStream;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.HashMap;
import java.util.Map;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.Result;
import javax.xml.transform.Source;
import javax.xml.transform.Templates;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerConfigurationException;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;
import javax.xml.transform.stream.StreamSource;

import org.w3c.dom.Document;
import org.xml.sax.SAXException;

public class XSLTLiLo {

	private void XSLTlowering(String input, String loweringSchema){
        // Using native xslt	 		
      URL url;
	try {
			url = new URL(loweringSchema);
			InputStream q_stream = url.openStream();

			   DocumentBuilderFactory docBuildFactory = DocumentBuilderFactory.newInstance();					     
			   DocumentBuilder parser = docBuildFactory.newDocumentBuilder();
			   Document document = parser.parse(input);  
			   
		      TransformerFactory myTranformerFactory = TransformerFactory.newInstance();
			  Templates template;
			
				template = myTranformerFactory.newTemplates(new StreamSource(q_stream));
				Transformer xformer = template.newTransformer(); 
	            String output = null;
				Source source = new DOMSource(document); 
				Result result = new StreamResult(output); 
				xformer.transform(source, result); 
				System.out.println(output);

	 	 } catch (MalformedURLException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
	  	} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (TransformerConfigurationException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (TransformerException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (ParserConfigurationException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (SAXException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}				
/*
// Invoke XSLT Lowering Service	     
 XSLTLiLoEngine xsltlilo= new XSLTLiLoEngine();
 String lowered_input=xsltlilo.doLowering(loweringSchema, input);
 Map<String,String> sParam =new HashMap<String,String>();
 sParam.put("body-value", lowered_input);
 */
}
}
