package uk.ac.open.kmi.decipher.util;

public class SPARQLUtil {

	public static final String PREFIX= "PREFIX lode:<http://linkedevents.org/ontology/>" 
			+ "PREFIX wordn-sc:<http://www.w3.org/2006/03/wn/wn20/schema/>"
			+ "PREFIX nytimes:<http://data.nytimes.com/>"
			+ "PREFIX geo-pos:<http://www.w3.org/2003/01/geo/wgs84_pos#>"
			+ "PREFIX dbp-prop:<http://dbpedia.org/property/>"
			+ "PREFIX umbel-ac:<http://umbel.org/umbel/ac/>"
			+ "PREFIX geonames:<http://sws.geonames.org/>"
			+ "PREFIX protons:<http://proton.semanticweb.org/2005/04/protons#>"
			+ "PREFIX curate:<http://decipher.open.ac.uk/curate/ontology/>"
			+ "PREFIX protonu:<http://proton.semanticweb.org/2005/04/protonu#>"
			+ "PREFIX protont:<http://proton.semanticweb.org/2005/04/protont#>"
			+ "PREFIX rdfs:<http://www.w3.org/2000/01/rdf-schema#>"
			+ "PREFIX sw-vocab:<http://www.w3.org/2003/06/sw-vocab-status/ns#>"
			+ "PREFIX ff:<http://factforge.net/>"
			+ "PREFIX music-ont:<http://purl.org/ontology/mo/>"
			+ "PREFIX opencyc-en:<http://sw.opencyc.org/2008/06/10/concept/en/>"
			+ "PREFIX om:<http://www.ontotext.com/owlim/>"
			+ "PREFIX dbpedia:<http://dbpedia.org/resource/>"
			+ "PREFIX dc-term:<http://purl.org/dc/terms/>"
			+ "PREFIX crm:<http://www.cidoc-crm.org/rdfs/cidoc_crm_v5.0.2_english_label.rdfs#>"
			+ "PREFIX rdf:<http://www.w3.org/1999/02/22-rdf-syntax-ns#>"
			+ "PREFIX factbook:<http://www.daml.org/2001/12/factbook/factbook-ont#>"
			+ "PREFIX oasis:<http://psi.oasis-open.org/iso/639/#>"
			+ "PREFIX ot:<http://www.ontotext.com/>"
			+ "PREFIX dc:<http://purl.org/dc/elements/1.1/>"
			+ "PREFIX onto:<http://www.ontotext.com/>"
			+ "PREFIX geo-ont:<http://www.geonames.org/ontology#>"
			+ "PREFIX umbel-en:<http://umbel.org/umbel/ne/wikipedia/>"
			+ "PREFIX foaf:<http://xmlns.com/foaf/0.1/>"
			+ "PREFIX bbc-pont:<http://purl.org/ontology/po/>"
			+ "PREFIX lingvoj:<http://www.lingvoj.org/ontology#>"
			+ "PREFIX yago:<http://mpii.de/yago/resource/>"
			+ "PREFIX fb:<http://rdf.freebase.com/ns/>"
			+ "PREFIX dbtune:<http://dbtune.org/bbc/peel/work/>"
			+ "PREFIX umbel:<http://umbel.org/umbel#>"
			+ "PREFIX dul:<http://www.ontologydesignpatterns.org/ont/dul/DUL.owl#>" //for dossier 2727
//			+ "PREFIX dul:<http://ontologydesignpatterns.org/ont/dul/DUL.owl#>"
			+ "PREFIX umbel-sc:<http://umbel.org/umbel/sc/>"
			+ "PREFIX wordnet16:<http://xmlns.com/wordnet/1.6/>"
			+ "PREFIX dbp-ont:<http://dbpedia.org/ontology/>"
			+ "PREFIX ub:<http://www.lehigh.edu/~zhp2/2004/0401/univ-bench.owl#>"
			+ "PREFIX xsd:<http://www.w3.org/2001/XMLSchema#>"
			+ "PREFIX owl:<http://www.w3.org/2002/07/owl#>"
			+ "PREFIX wordnet:<http://www.w3.org/2006/03/wn/wn20/instances/>"
			+ "PREFIX gr:<http://purl.org/goodrelations/v1#>"
			+ "PREFIX protonkm:<http://proton.semanticweb.org/2005/04/protonkm#>"
			+ "PREFIX skos:<http://www.w3.org/2004/02/skos/core#>"
			+ "PREFIX opencyc:<http://sw.opencyc.org/concept/>";	
	
	
	/*	
	private void parseSPARQLResult(String results){DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
	//  	Reader reader=new CharArrayReader(response.toCharArray());
	Document doc=null;
	try {
		doc = factory.newDocumentBuilder().parse(new ByteArrayInputStream(queryResult.getBytes("UTF-8")));
	} catch (UnsupportedEncodingException e) {
		// TODO Auto-generated catch block
		e.printStackTrace();
	} catch (SAXException e) {
		// TODO Auto-generated catch block
		e.printStackTrace();
	} catch (IOException e) {
		// TODO Auto-generated catch block
		e.printStackTrace();
	} catch (ParserConfigurationException e) {
		// TODO Auto-generated catch block
		e.printStackTrace();
	}

	Element root = doc.getDocumentElement();
	Element resultsNode = (Element) root.getElementsByTagName("results").item(0);
	NodeList childNodes = resultsNode.getChildNodes();
    int c=0;
	for (int i = 0; i < childNodes.getLength(); i++) {
		Node nNode = childNodes.item(i);
		if (nNode.getNodeType() == Node.ELEMENT_NODE) { 
			c++;
			Element eElement = (Element) nNode;
			NodeList bindingNodes = eElement.getElementsByTagName("binding");
			for (int j = 0; j < bindingNodes.getLength(); j++) {
				Node bNode = bindingNodes.item(j);
				if (bNode.getNodeType() == Node.ELEMENT_NODE) {
					Element bElement = (Element) bNode;
					String nameAtt = bElement.getAttribute("name"); 
					if(nameAtt.compareTo("req")==0){
						Element uri = (Element) bElement.getElementsByTagName("uri").item(0);
						if(uri!=null){
							String uriValue = uri.getChildNodes().item(0).getNodeValue();
							returnRes +="<request"+ c + ">"+ StringEscapeUtils.escapeXml(uriValue) +"</request"+c+">";                      			
						}
					} else if(nameAtt.compareTo("reqValue")==0){
						Element literal = (Element) bElement.getElementsByTagName("literal").item(0);
						if(literal!=null){
							Node litVal = literal.getChildNodes().item(0);
							if(litVal!=null) {
								String litValue = literal.getChildNodes().item(0).getNodeValue();
								returnRes +="<request"+c+"Body>" + StringEscapeUtils.escapeXml(litValue) + "</request"+c+"Body>"; 
							}		    
						}
					}else if(nameAtt.compareTo("resp")==0){
							Element uri = (Element) bElement.getElementsByTagName("uri").item(0);
							if(uri!=null){
								String uriValue = uri.getChildNodes().item(0).getNodeValue();
								returnRes +="<response"+ c + ">" +StringEscapeUtils.escapeXml(uriValue)+ "</response"+c+">";    
							}
				    } else if(nameAtt.compareTo("respSC")==0){
						Element uri = (Element) bElement.getElementsByTagName("uri").item(0);
						if(uri!=null){
							String uriValue = uri.getChildNodes().item(0).getNodeValue();
							returnRes +="<response"+c+"Status>" +StringEscapeUtils.escapeXml(uriValue)+ "</response"+c+"Status>";    
						}
			        }else if(nameAtt.compareTo("respValue")==0){
							Element literal = (Element) bElement.getElementsByTagName("literal").item(0);
							if(literal!=null){
								Node litVal = literal.getChildNodes().item(0);
								if(litVal!=null) {
									String litValue = literal.getChildNodes().item(0).getNodeValue();
									returnRes +="<response"+c+"Body>" + StringEscapeUtils.escapeXml(litValue) + "</response"+c+"Body>"; 
							}
						}
					}
				  }
				}
			}
		} 
	}
*/	
	
}
