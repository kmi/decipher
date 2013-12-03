package uk.ac.open.kmi.decipher.repository;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import org.apache.commons.httpclient.HttpClient;
import org.apache.commons.httpclient.HttpException;
import org.apache.commons.httpclient.methods.GetMethod;
import org.ontoware.aifbcommons.collection.ClosableIterator;
import org.ontoware.rdf2go.ModelFactory;
import org.ontoware.rdf2go.RDF2Go;
import org.ontoware.rdf2go.model.Model;
import org.ontoware.rdf2go.model.QueryResultTable;
import org.ontoware.rdf2go.model.QueryRow;

public class RDFInputReader {

	public Map<String,String> read(String in){
	List<String> result = new ArrayList<String>();
	HttpClient client = new HttpClient();
//	client.getHostConfiguration().setProxy("wwwcache.open.ac.uk", 80);
	ModelFactory modelFactory = RDF2Go.getModelFactory();
	Model model = modelFactory.createModel();

	String serviceEndPoint = "http://ws.geonames.org/search?q=" + "London" + "&type=rdf";

	GetMethod geoNameSearch = new GetMethod(serviceEndPoint);

	String rdf = "";
	try {
		client.executeMethod(geoNameSearch);
		rdf = geoNameSearch.getResponseBodyAsString();
		String rdf8 = new String(rdf.getBytes(), "UTF-8");

		System.out.println(rdf8);

		String modelQueryString = "SELECT DISTINCT ?name ?country ?location ?map ?lat ?long WHERE { ?location <http://www.geonames.org/ontology#name> ?name ."
				+ "?location <http://www.geonames.org/ontology#locationMap> ?map ."
				+ "?location <http://www.geonames.org/ontology#countryCode> ?country ."
				+ "?location <http://www.w3.org/2003/01/geo/wgs84_pos#lat> ?lat ."
				+ "?location <http://www.w3.org/2003/01/geo/wgs84_pos#long> ?long}";

		model.open();

		model.readFrom(new ByteArrayInputStream(rdf8.getBytes()));

		QueryResultTable results = model.sparqlSelect(modelQueryString);
		System.out.println(results.toString());
		ClosableIterator<QueryRow> iter = results.iterator();
		while (iter.hasNext()) {
			QueryRow row = iter.next();
			String name = row.getValue("name").toString();
			String country = row.getValue("country").toString();
			String location = row.getValue("location").toString();
			String map = row.getValue("location").toString();
			String lat = row.getValue("lat").toString();
			String longatti = row.getValue("long").toString();
			result.add(name + " -> " + country + ";" + location + ";" + lat
					+ ";" + longatti + ";" + name + ";" + name + ":"
					+ country + ":" + location + ":" + map);
		}

	} catch (HttpException e) {
		// TODO Auto-generated catch block
		e.printStackTrace();
	} catch (IOException e) {
		// TODO Auto-generated catch block
		e.printStackTrace();
	}
	model.close();
//	return result;
	return null;
	}
	
	public static void main(String[] args){
		RDFInputReader rdfR= new RDFInputReader();
		rdfR.read(null);
	
	}

}
