// (c) Copyright 2002 by Deliana Foutekova <unjm@stud.uni-karlsruhe.de>
package docIndex;

import java.util.*;
import java.net.*;

/** 
 * An abstract class that shall have subclasses, which provide the 
 * index data. The applet only knows about this class and its methods, 
 * and gets the name of the subclass via an input parameter. 
 */
public abstract class Index {

    /**
     * Read in index.
     */
    public abstract String[][] getIndexTable();

    /** 
     * Search the table for the keyword and return the String
     * representation of the URL for this keyword as stored in the
     * table.
     */
    public String getUrlAsString(String key) {
	key = makeSuitableKeyword(key);

	String indexTable[][] = getIndexTable();

	// binary search
	String res = null;
	int startIndex = 0;
	int endIndex = indexTable.length - 1;
	int i = 0;

	while (true) {
	    int diff = endIndex - startIndex;
	    String tableKey = indexTable[startIndex][0];
	    
	    if (diff == 0) {
		if (key.compareTo(tableKey) == 0) // found it!
		    res = indexTable[startIndex][1];
		break;
	    } 

	    if (key.compareTo(tableKey) > 0) {
		/* endIndex = endIndex; -- remains the same*/
		// don't add zero to the start index, this will produce an endless loop
		// diff / 2 equals to floor(diff / 2)
		startIndex = startIndex + (int)Math.max(diff / 2, 1);

	    } else if (key.compareTo(tableKey) < 0) {
		endIndex = startIndex;
		// startIndex shall not be lesser than zero.
		// (diff + 1) / 2 equals to ceil(diff / 2),
		// according to ceil(a/b) == (a + b + 1) / b
		startIndex = (int)Math.max(startIndex - ((diff + 1) / 2), 0);

	    } else { // found it!
		res = indexTable[startIndex][1];
		break;
	    }
	}


	return res;
    }

    /** Merge the given String and URL and create a new URL. */
    public URL createURL(String urlAsString, URL documentBase) {
	URL res = null;

	try {
	    urlAsString = makeSuitableUrl(urlAsString);
	    res = new URL(documentBase, urlAsString);
	} catch (Exception e) {
	    e.getMessage();
	    e.printStackTrace();
	} finally {
	    return res;
	}
    }

    public String makeSuitableUrl(String urlAsString) {
	int indexOfSemicolon = urlAsString.indexOf(':');
	String res = "";

	if (indexOfSemicolon < 0) {
	    res = urlAsString;
	} else {
	    res = urlAsString.substring(0, indexOfSemicolon)+"%3A"+urlAsString.substring(indexOfSemicolon+1);
	}
   
	return res;
    }

    public String makeSuitableKeyword(String key) {
	String res = key.toLowerCase();
	int indexOfSemicolon = key.indexOf(':');

	if (indexOfSemicolon > 0) {
	    res = res.substring(indexOfSemicolon + 1);
	} 
	
	return res;
    }

}
