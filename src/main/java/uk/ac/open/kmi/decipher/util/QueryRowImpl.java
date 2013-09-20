package uk.ac.open.kmi.decipher.util;

import java.util.Map;

public class QueryRowImpl implements QueryRow {

	/**
	 * 
	 */
	private static final long serialVersionUID = 6528680539087013048L;

	private Map<String, String> rowData;

	private Map<String, String> rowDatatype;

	public QueryRowImpl() {
		
	}

	public Map<String, String> getQueryRowData() {
		return rowData;
	}

	public Map<String, String> getQueryRowDatatype() {
		return rowDatatype;
	}

	public void setQueryRow(Map<String, String> rowDatatype, Map<String, String> rowData) {
		this.rowDatatype = rowDatatype;
		this.rowData = rowData;
	}

	public String getValue(String var) {
		return rowData.get(var);
	}

	public String getDatatype(String var) {
		String type = rowDatatype.get(var);
		if ( type.startsWith("datatype:") ) {
			return type.substring("datatype:".length());
		} else  if ( type.startsWith("lang:") ) {
			return "literal";
		} else {
			return type;
		}
	}

	public String getLang(String var) {
		String type = rowDatatype.get(var);
		if ( type.startsWith("lang:") ) {
			return type.substring("lang:".length());
		} else {
			return "";
		}
	}

	public boolean isBlankNode(String var) {
		if ( rowDatatype.get(var).equalsIgnoreCase("blank") )
			return true;
		return false;
	}

	public boolean isLiteral(String var) {
		if ( rowDatatype.get(var).startsWith("datatype:") || rowDatatype.get(var).startsWith("lang:")
				|| rowDatatype.get(var).startsWith("literal") )
			return true;
		return false;
	}

	public boolean isUri(String var) {
		if ( rowDatatype.get(var).equalsIgnoreCase("uri") )
			return true;
		return false;
	}

}
