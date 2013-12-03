package uk.ac.open.kmi.decipher.util;

import java.util.List;


public class QueryResultImpl implements QueryResult {

	/**
	 * 
	 */
	private static final long serialVersionUID = 8488637623771516054L;

	private List<String> variables;

	private List<QueryRow> queryRows;

	public QueryResultImpl() {
		
	}

	public List<String> getVariables() {
		return variables;
	}

	public void setVariables(List<String> variables) {
		this.variables = variables;
	}

	public List<QueryRow> getQueryRows() {
		return queryRows;
	}

	public void setQueryRows(List<QueryRow> queryRows) {
		this.queryRows = queryRows;
	}

}
