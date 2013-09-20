package uk.ac.open.kmi.decipher.util;

import java.io.Serializable;
import java.util.List;

public interface QueryResult extends Serializable {

	public List<String> getVariables();

	public void setVariables(List<String> variables);

	public List<QueryRow> getQueryRows();

	public void setQueryRows(List<QueryRow> queryRows);

}
