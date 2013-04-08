// (c) Copyright 2002 by Deliana Foutekova <unjm@stud.uni-karlsruhe.de>
package docIndex;

import java.applet.*;
import java.awt.*;
import java.net.*;
import java.util.*;

/** An applet that has an input field in which you can type in
 * a keyword. If the applet knows the keyword, it requests a 
 * URL related to the keyword.
 */
public class DocIndexApplet extends Applet {

    private TextField input = new TextField();
    private String target = "_self";
    private String indexClassName = "docIndex.XlibIndex";
    private URL documentBase = null;
    private String appletName = "docIndex.DocIndexApplet";
    private String version = "1.0";

    public void init() {
	// read the parameters
	String param = getParameter("target");
	if (param != null) 
	    target = param;

	param = getParameter("index");
	if (param != null) 
	    indexClassName = "docIndex."+param+"Index";

	param = getParameter("fgcolor");
	Color fgColor = parseColor("Foreground color", param, Color.black);

	param = getParameter("bgcolor");
	Color bgColor = parseColor("Background color", param, Color.white);

	param = getParameter("width");
	int w = parseInt("Width", param, this.getSize().width);

	param = getParameter("height");
	int h = parseInt("Height", param, this.getSize().height);

	
	// Set size, colors and make text field visible.
	this.setForeground(fgColor);
	this.setBackground(bgColor);
	input.setBackground(Color.white);


	GridBagLayout myLayout = new GridBagLayout();
	GridBagConstraints c = new GridBagConstraints();
	c.fill = GridBagConstraints.HORIZONTAL;
	c.weightx = 1;
	this.setLayout(myLayout);
	myLayout.setConstraints(input, c);
	this.add(input);

	this.setSize(w, h);
    }

    public void start() {
	super.start();
    }

    /** A list of the accepted parameters. */
    public String[][] getParameterInfo() {
	return new String[][] {
	    {"target", "String", "A target window to show the documentation"},
	    {"index", "String", "The name of the package which documentation to browse"},
            {"fgcolor", "hex int", "the foreground color, defaults to #000000 (black)"},
	    {"bgcolor", "hex int", "the background color, defaults to #ffffff (white)"},
	    {"width", "decimal int", "the width in pixel, defaults to 300 or is set via the \"WIDTH\" tag"},
	    {"height", "decimal int", "the height in pixel, defaults to 100 or is set via the \"HEIGHT\" tag"}
	};
    }

    public String getAppletInfo() {
	return appletName+" "+version+"  1.0 (c) Copyright 2002 by Deliana Foutekova <unjm@rz.uni-karlsruhe.de>";
    }



    public Color parseColor(String colorDescription, String param, Color defaultColor) {
	Color res = defaultColor;
	if (param == null) 
	    return res;

	try {
	    if (param.startsWith("#")) {
		res = new Color(Integer.parseInt(param.substring(1), 16));
	    } else if (param.startsWith("0x")) {
		res = new Color(Integer.parseInt(param.substring(2), 16));
	    } else {
		res = new Color(Integer.parseInt(param, 10));
	    }

	} catch (Exception e) {
	    System.out.println(appletName+": "+colorDescription+" not recognized as a hex int. Falling back to default color "+defaultColor);
	} finally {
	    return res;
	}
    }

    public int parseInt(String intDescription, String param, int defaultValue) {
	int res = defaultValue;
	if (param == null) 
	    return res;

	try {
	    res = Integer.parseInt(param);
	} catch (Exception e) {
	    System.out.println(appletName+": "+intDescription+" not recognized as a decimal integer. Falling back to default "+defaultValue+" (usually set by the browser)");
	} finally {
	    return res;
	}
    }

    public URL getURL(String key) throws Exception {
	URL res = null;

	// Read class containing the index. 
	Index index = (Index)Class.forName(indexClassName).newInstance();
	    
	// get index entry
	String urlAsString = index.getUrlAsString(key);
	
	// get URL
	if (urlAsString != null) {
	    if (documentBase == null) {
		try {
		    documentBase = this.getDocumentBase();
		} catch (NullPointerException e) { 
		    System.out.println(appletName+": No document base. Probably applet not embedded in a web page and documentBase not set manually?");
		}
	    }

	    res = index.createURL(urlAsString, documentBase);
	}

	return res;
    }

    public static final int NO_STATUS = -1;
    public static final int OK_STATUS = 0;
    public static final int ITEM_NOT_FOUND_STATUS = 1;
    public static final int INDEX_CLASS_NOT_FOUND_STATUS = 2;
    public static final int EXCEPTION_STATUS = 3;

    public int showURL(URL theUrl) {
	if (theUrl == null) 
	    return ITEM_NOT_FOUND_STATUS;

	this.getAppletContext().showDocument(theUrl, target);
	return OK_STATUS;
    }

    /** Set the document base. When the applet is embedded in a HTML
     * document, the browser sets the document base, which the applet
     * gets through getDocumentBase(). setDocumentBase() is meant for
     * occasions when the applet is not called from within a HTML
     * document and has therefore no documentBase. This can be needed
     * e.g. for testing purposes.
     */
    public URL setDocumentBase(String spec) throws MalformedURLException {
	documentBase = new URL(spec);
	return documentBase;
    }


    // Event handling method
    public boolean handleEvent(Event event) {
	if (event.id == Event.ACTION_EVENT &&  event.target == input) {

	    URL theUrl = null;
	    int status = NO_STATUS;
	    String notFound = "NOT FOUND: ";
	    try {
		String query = input.getText();
		if (query.startsWith(notFound))
		    query = query.substring(notFound.length());
		theUrl = getURL(query);
		status = showURL(theUrl);
	    } catch (ClassNotFoundException e) {
		status = INDEX_CLASS_NOT_FOUND_STATUS;
	    } catch (Exception e) {
		status = EXCEPTION_STATUS;
		e.getMessage();
		e.printStackTrace();
	    }

	    if (status == OK_STATUS) {
		input.setText("");
	    } else if (status == ITEM_NOT_FOUND_STATUS) {
		if (!(input.getText().startsWith(notFound))) 
		    input.setText(notFound+input.getText());
		input.setCaretPosition(notFound.length());
	    } else if (status == INDEX_CLASS_NOT_FOUND_STATUS) {
		input.setText("INDEX NOT FOUND, SO IMPOSSIBLE TO SEARCH.");
	    } else if (status == EXCEPTION_STATUS) {
		input.setText("EXCEPTION OCCURED. You may take a look at your java console/stdout and submit a bug report.");
	    } else {
		input.setText("no idea what's going wrong.");
	    }

	    //System.out.println(appletName+": Searched with status: "+status);
	    return true;
	}
	return super.handleEvent(event);
    }

}
