/* TODO Eclipse Bug??
 * Exception in thread "Thread-9" java.util.ConcurrentModificationException
	at java.util.HashMap$HashIterator.nextEntry(HashMap.java:841)
	at java.util.HashMap$KeyIterator.next(HashMap.java:877)
	at org.eclipse.jface.text.source.AnnotationModel.getAnnotationIterator(AnnotationModel.java:552)
	at org.eclipse.jface.text.source.AnnotationModel.getAnnotationIterator(AnnotationModel.java:530)
	at org.eclipse.jface.text.source.projection.ProjectionSummary.removeSummaries(ProjectionSummary.java:193)
	at org.eclipse.jface.text.source.projection.ProjectionSummary.internalUpdateSummaries(ProjectionSummary.java:166)
	at org.eclipse.jface.text.source.projection.ProjectionSummary.access$3(ProjectionSummary.java:148)
	at org.eclipse.jface.text.source.projection.ProjectionSummary$Summarizer.run(ProjectionSummary.java:70)
 */

//TODO Refactoring: package namen -> de.fh-trier -> auf neue domain


//!!!!!!!!!!!!!!!!!!!!!!!!!! Hohe Prioritaet
//TODO GUI-Dialog Environment anlegen aendern (Parameter fuer Exectuable hinzufuegen, TODO Werte auslesen)
//TODO GUI-Dialog Executable oder Parameterstart ausgewaehlt => automatisch localhost in server und nicht aenderbar!! (Feld enabled = false)
//TODO Aenderung Eval-Server Projekt ermoeglichen
//TODO Aenderung Eval-Server Preferences ermoeglichen (Server aus Plugin, aenderbar???)
//TODO connect/disconnect ermoeglichen, in projekt-kontextmenue, apropos connect-image, ListenerView
//!!!!!!!!!!!!!!!!!!!!!!!!!!

//TODO Save Memimage bei beenden Lisp-Prozess ermoeglichen!!!
//TODO Auschliessen von paketen bei symboluebertragung ermoeglichen
//TODO Prozess-Output Anbindung
//TODO defpackage parser
//TODO Eval aus Outline
//TODO Markierung gleicher Woerter!
//TODO in paketliste nicknames aufnehmen
//TODO Template fuer Doku
//TODO REgex-Suche in Apropos
//TODO Severity-Einstellungen fuer Fehlertypen
//TODO Eval Selektion aus Listener
//TODO Funktionssync
//TODO View fuer Connection Uebersicht
//TODO LispModel zur Verwaltung selbst definierter SExpressions
//TODO Selektion Editor mit Outline abgleichen (ueber offset in dokument)
//TODO Folding Kommentar Toplevel
//TODO todo-Markierung in Quellcode ermoeglichen
//TODO Parsen Effizienter -> Nur veränderte TL-Form neu scannen-> Mitteilung ueber Listener
//TODO Unterstuetzung CLOS
//TODO ProjectionSummary Anzeige mit Syntax-Highlighting Lisp
//TODO Logical-Package-View