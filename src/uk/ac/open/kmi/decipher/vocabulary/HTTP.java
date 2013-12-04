package uk.ac.open.kmi.decipher.vocabulary;

import org.ontoware.rdf2go.model.node.URI;
import org.ontoware.rdf2go.model.node.impl.URIImpl;

//HTTP RDF Vocabulay

public class HTTP {

		public static final String NS_URI = "http://www.w3.org/2006/http#";
		public static final String NS_PREFIX = "http";
		
		public static final String SC_NS_URI = "http://www.w3.org/2008/http-statusCodes#";
		public static final String SC_NS_PREFIX = "sc";
	 
		public static final String CNT_NS_URI = "http://www.w3.org/2008/content#";
		public static final String CNT_NS_PREFIX = "cnt";
		
		public static final String DCT_NS_URI = "	http://purl.org/dc/terms/";
		public static final String DCT_NS_PREFIX = "dct";
	
		// Classes:
		public static final String CONNECTION = NS_URI + "Connection";
		public static final URI Connection = new URIImpl(CONNECTION);
        public static final String REQUEST = NS_URI + "Request";
		public static final URI Request = new URIImpl(REQUEST);
		public static final String RESPONSE = NS_URI + "Response";
		public static final URI Response = new URIImpl(RESPONSE);
		public static final String STATUS_CODE = NS_URI + "StatusCode";
		public static final URI StatusCode = new URIImpl(STATUS_CODE);
		public static final String METHOD = NS_URI + "Method";
		public static final URI Method = new URIImpl(METHOD);
         
		//Properties
		public static final String REQUESTS = NS_URI + "requests";
		public static final URI requests = new URIImpl(REQUESTS);

		public static final String BODY = NS_URI + "body";
		public static final URI body = new URIImpl(BODY);
		public static final String METHOD_NAME = NS_URI + "methodName";
		public static final URI methodName = new URIImpl(METHOD_NAME);
		public static final String MTHD = NS_URI + "mthd";
		public static final URI mthd = new URIImpl(MTHD);
		public static final String STATUS_CODE_NUMBER = NS_URI + "statusCodeNumber";
		public static final URI statusCodeNumber = new URIImpl(STATUS_CODE_NUMBER);
		public static final String RESP = NS_URI + "resp";
		public static final URI resp = new URIImpl(RESP);
		public static final String SC = NS_URI + "sc";
		public static final URI sc = new URIImpl(SC);

	
		
}
