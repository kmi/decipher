package uk.ac.open.kmi.decipher.util;

import java.io.Serializable;
import java.util.Map;

public interface QueryRow extends Serializable {

	public Map<String, String> getQueryRowData();

	public Map<String, String> getQueryRowDatatype();

	public void setQueryRow(Map<String, String> rowDatatype, Map<String, String> rowData);

	public String getValue(String variable);

	public boolean isUri(String variable);

	public boolean isLiteral(String variable);

	public boolean isBlankNode(String variable);

	public String getLang(String variable);

	public String getDatatype(String variable);

}
