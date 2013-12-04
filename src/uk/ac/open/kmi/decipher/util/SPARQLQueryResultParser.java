package uk.ac.open.kmi.decipher.util;

import java.io.IOException;
import java.io.StringReader;
import java.io.UnsupportedEncodingException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;

import org.apache.commons.lang.StringEscapeUtils;
import org.w3c.dom.Document;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;

public class SPARQLQueryResultParser {

		private static List<String> parseVariables(Node headNode) {
			List<String> varList = new ArrayList<String>();
			NodeList nodeList= headNode.getChildNodes();
			for ( int i = 0; i < nodeList.getLength(); i++ ) {
				Node node = nodeList.item(i);
				if ( node.getNodeName().equalsIgnoreCase("variable") ) {
					String variable = node.getAttributes().getNamedItem("name").getTextContent();
					varList.add(variable);
				}
			}
			return varList;
		}

		private static QueryRow parseResult(Node resultNode) {
			QueryRow result = new QueryRowImpl();
			Map<String, String> rowData = new HashMap<String, String>();
			Map<String, String> rowDatatype = new HashMap<String, String>();
			NodeList nodeList= resultNode.getChildNodes();
			for ( int i = 0; i < nodeList.getLength(); i++ ) {
				Node node = nodeList.item(i);
				if ( node.getNodeName().equalsIgnoreCase("binding") ) {
					String variable = node.getAttributes().getNamedItem("name").getTextContent();
					NodeList vNodeList = node.getChildNodes();
					for ( int j = 0; j < vNodeList.getLength(); j++ ) {
						Node vNode = vNodeList.item(j);
						if ( !vNode.getNodeName().equalsIgnoreCase("#text") ) {
							rowDatatype.put(variable, vNode.getNodeName());
//							rowData.put(variable, StringEscapeUtils.escapeXml(vNode.getTextContent()));
							rowData.put(variable, vNode.getTextContent());
//							System.out.println(variable + ": " + vNode.getTextContent() + ", type: " + vNode.getNodeName());
						}
					}
				}
			}

			result.setQueryRow(rowDatatype, rowData);
			return result;
		}

		private static List<QueryRow> parseResults(Node resultsNode) {
			List<QueryRow> rowList = new ArrayList<QueryRow>();
			NodeList nodeList= resultsNode.getChildNodes();
			for ( int i = 0; i < nodeList.getLength(); i++ ) {
				Node node = nodeList.item(i);
				if ( node.getNodeName().equalsIgnoreCase("result") ) {
//					System.out.println(node.getNodeName());
					rowList.add(parseResult(node));
//					QueryRow result = new QueryRowImpl();
//					Map<String, String> rowData = new HashMap<String, String>();
//					Map<String, String> rowDatatype = new HashMap<String, String>();
				}
			}
			return rowList;
		}

		public static QueryResult parse(String queryResultString) {
			QueryResult result = new QueryResultImpl();
			try {
	        	DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
				DocumentBuilder builder = factory.newDocumentBuilder();
				InputSource is = new InputSource(new StringReader(queryResultString));
				Document dom = builder.parse(is);
				NodeList nodeList= dom.getDocumentElement().getChildNodes();
				for ( int i = 0; i < nodeList.getLength(); i++ ) {
					Node node = nodeList.item(i);
					if ( node.getNodeName().equalsIgnoreCase("head") ) {
						// variables
						result.setVariables(parseVariables(node));
					} else if ( node.getNodeName().equalsIgnoreCase("results") ) {
						// results
						result.setQueryRows(parseResults(node));
					}
				}
			} catch (UnsupportedEncodingException e) {
				e.printStackTrace();
			} catch (IOException e) {
				e.printStackTrace();
			} catch (ParserConfigurationException e) {
				e.printStackTrace();
			} catch (SAXException e) {
				e.printStackTrace();
			}
			return result;
		}
	
	
}
