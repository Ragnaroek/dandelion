;;;-*- Mode:Common-Lisp; Package:PICTURES; Base:10 -*-
;;;
;;;
;;;
;;;			 TEXAS INSTRUMENTS INCORPORATED
;;;				  P.O. BOX 149149
;;;			       AUSTIN, TEXAS 78714-9149
;;;
;;; Copyright (C)1987,1988,1989,1990 Texas Instruments Incorporated.
;;;
;;; Permission is granted to any individual or institution to use, copy, modify,
;;; and distribute this software, provided that this complete copyright and
;;; permission notice is maintained, intact, in all copies and supporting
;;; documentation.
;;;
;;; Texas Instruments Incorporated provides this software "as is" without
;;; express or implied warranty.
;;;
;;; Authors: Delmar Hager, James Dutton, Teri Crowe
;;; Contributors: Kerry Kimbrough, Patrick Hogan, Eric Mielke

(in-package "PICTURES")

(defgeneric find-font (font-family size)
  (:documentation
 "Returns or (with setf) changes the font from the FONT-FAMILY
  that is closest to the SIZE"))
(defgeneric (setf find-font)(new-value font-family size))

(defgeneric find-family-members ( font-family display)
  (:documentation
   "Returns or (with setf) changes the family-members of the FONT-FAMILY
    for a given DISPLAY"))
(defgeneric (setf find-family-members)(new-value font-family display))

(defgeneric vertex-y (polypoint  position)
  (:documentation
   "Get or setf the y coordinate of a polypoint at the given position"))

(defgeneric (setf vertex-y)(new-value polypoint position))

(defgeneric vertex-y (polypoint  position)
  (:documentation
   "Get or setf the y coordinate of a polypoint at the given position"))
(defgeneric (setf vertex-y)(new-value polypoint position))

(defgeneric vertex-i (sequence index)
  (:documentation
   "Returns or (with setf) changes the ith vertex of the given sequence"))
(defgeneric (setf vertex-i)(new-value sequence index))

(defgeneric gstate-value ( gstate keyword)
  (:documentation
   "Print the value of a gstate keyword. Return nil if not defined"))

(defgeneric remove-gstate-value (gstate keyword))

(defgeneric gstate-foreground (gstate )
 (:documentation
  "Returns or (with setf) changes the FOREGROUND value associated
   with the GRAPHIC or GSTATE.
   This specifies the color that will be displayed when a GRAPHIC
   is drawn on the screen. The type  pixel points to a position in
   the color map. "))
(defgeneric (setf gstate-value) (new-value gstate keyword))

(defgeneric gstate-background (gstate )
 (:documentation
  "Returns or (with setf) changes the BACKGROUND value associated
   with the GRAPHIC or GSTATE.
   This specifies the color that will be displayed when no graphic
   is being displayed on the screen.  It is used with tiles or stipples.
   The type pixel points to a position in the color map."))
(defgeneric (setf gstate-background)(new-value gstate))

(defgeneric gstate-dashes (gstate )
 (:documentation
  "Returns or (with setf) changes the DASH value associated
   with the GRAPHIC or GSTATE.
   This sequence determines the pattern of the DASHES for a dashed line."))
(defgeneric (setf gstate-dashes)(new-value gstate))

(defgeneric gstate-function (gstate )
 (:documentation
  "Returns or (with setf) changes the boolen-constant FUNCTION
   value associated with the GRAPHIC or GSTATE.
   The FUNCTION detemines how pixels on the screen are displayed
   when one pixel is drawn on another."))
(defgeneric (setf gstate-function) (new-value gstate ))

(defgeneric gstate-line-width (gstate )
 (:documentation
  "Returns or (with setf) changes the LINE-WIDTH value associated
   with the GRAPHIC or GSTATE. The LINE-WIDTH is in world-coordinate units."))
(defgeneric (setf gstate-line-width) (new-value gstate))

(defgeneric gstate-line-style (gstate )
 (:documentation
  "Returns or (with setf) changes the line-style value associated
   with the GRAPHIC or GSTATE."))
(defgeneric (setf gstate-line-style)(new-value gstate))

(defgeneric gstate-cap-style (gstate )
 (:documentation
  "Returns or (with setf) changes the CAP-STYLE value associated
   with the GRAPHIC  or GSTATE.
   The CAP STYLE  determines how the end of a line is drawn."))
(defgeneric (setf gstate-cap-style) (new-value gstate))

(defgeneric gstate-join-style (gstate )
 (:documentation
  "Returns or (with setf) changes the JOIN-STYLE value associated
   with the GRAPHIC or GSTATE.
   The JOIN STYLE  defines how two lines will be joined together."))
(defgeneric (setf gstate-join-style)(new-value gstate))

(defgeneric gstate-fill-style (gstate )
 (:documentation
  "Returns or (with setf) changes the FILL-STYLE value associated
   with the GRAPHIC or GSTATE.
   The FILL SYLE will determine the color or pattern that will be
   drawn with a filled GRAPHIC."))
(defgeneric (setf gstate-fill-style)(new-value gstate))

(defgeneric gstate-fill-rule (gstate )
 (:documentation
  "Returns or (with setf) changes the FILL-RULE value associated with
   the GRAPHIC or GSTATE.
   The FILL RULE is used to determine how non-convex filled polygon
   will be filled."))
(defgeneric (setf gstate-fill-rule)(new-value gstate))

(defgeneric gstate-tile (gstate )
 (:documentation
  "Returns or (with setf) changes the TILE value associated with
   the GRAPHIC or GSTATE.
   A image may be chosen to use as a pattern when a filled graphic is drawn."))
(defgeneric (setf gstate-tile) (new-value gstate))

(defgeneric gstate-stipple (gstate )
 (:documentation
  "Returns or (with setf) changes the STIPPLE value associated
   with the GRAPHIC or GSTATE.
   A stipple is a bitmap that has a depth of one.  It can be used
   to create a foreground or background pattern."))
(defgeneric (setf gstate-stipple) (new-value gstate))

(defgeneric move-transform (transform delta-x delta-y)
  (:DOCUMENTATION
   "Destructivly modify the TRANSFORM, translating the previous
    transformation by the given distances DELTA-X, DELTA-Y.
    The new value of the TRANSFORM is returned."))

(defgeneric rotate-transform (transform angle &optional  fixed-x  fixed-y )
 (:documentation
  "Rotate the GRAPHIC or TRANSFORM by the given ANGLE (in radians) around
   the given fixed point.
   Rotation allows the programmer to change the display angle of a GRAPHIC.
   The default value is 0 for FIXED-X and FIXED-Y"))

(defgeneric scale-transform (transform scale-x scale-y &optional fixed-x fixed-y )
  (:documentation
    "Modify the TRANSFORM, scaling the previous transformation by the given
     SCALE-X and SCALE-Y around the given FIXED-X and FIXED-Y.
     Default value for FIXED-X and FIXED-Y is 0"))

(defgeneric scale-point (transform x-distance y-distance))

(defgeneric graphic-transform (graphic)
 (:documentation
  "Returns or (with setf) changes the  TRANSFORM associated with the GRAPHIC.
   A nil transform represents the common case of the identity transform."))
(defgeneric (setf graphic-transform) (new-value graphic))

(defgeneric graphic (graphic) 
  (:documentation
   "compute the extent-rectangel of a GRAPHIC in object coordinates."))

(defgeneric editable-p (graphic))
(defgeneric viewable-p (graphic))
(defgeneric graphic-damage (graphic))

(defgeneric graphic-stack-purge (graphic-stack  &optional graphic)  
  (:documentation
   "Pop the GRAPHIC-STACK until the given GRAPHIC is found and
    then pop that entry as well. "))

(defgeneric graphic-stack-push (graphic-stack graphic))
(defgeneric graphic-stack-fill (graphic-stack graphic))
(defgeneric graphic-stack-pop  (graphic-stack))

(defgeneric graphic-fixed-point (graphic fixed-point &optional world-coordinate )
  (:documentation
   "Return the object coordinates of the given FIXED-POINT on the GRAPHIC
    extent. If WORLD-COORDINATE is true the coordinates are returned
    as world coordinates"))

(defgeneric graphic-combined-gstate (graphic)
 (:documentation 
  "Return the fully combined gstate for the given graphic.
   The method is used when writing draw-graphic methods."))

(defgeneric graphic-combined-edge-gstate (graphic)
 (:documentation
  "Return the fully combined edge gstate for the given graphic.
   The method is used when writing draw-graphic methods."))

(defgeneric combine-into (gstate1 gstate2))

(defgeneric graphic-contains-p (graphic x y &optional arpeture)
  (:documentation
    "Determines whether a given world coordinate (X, Y) lies on the GRAPHIC.
   ARPETURE increasing the area for picking"))

(defgeneric draw-graphic (graphic view &optional min-x min-y width height)
 (:documentation
   "Draw the GRAPHIC object in the given view. If MIN-X, MIN-Y, WIDTH,
    and HEIGHT are given, then only parts of the object that lie within
    the given rectangle need to be drawn."))

(defgeneric draw-graphic-clipped (graphic view min-x min-y width height)
 (:documentation 
  " Draw the GRAPHIC in the given VIEW with the given clipping
   rectangle - MIN-X MIN-Y WIDTH HEIGHT -."   ))

(defgeneric graphic-intersects-p (graphic  min-x min-y width height)
 (:documentation
   "If the given GRAPHIC intersects the rectangle - MIN-X MIN-Y WIDTH HEIGHT
    - given in world coordinates, then return true,  otherwise, return nil."))

(defgeneric graphic-plist (graphic)
 (:documentation
  "Returns a list of properties for the given graphic."))

(defgeneric graphic-parent ( graphic)
 (:documentation
  "Returns or (with setf) changes the parent of the GRAPHIC .
   If a GRAPHIC  is inserted into a scene, the scene becomes
   the graphic-parent."))
(defgeneric (setf graphic-parent)(new-value graphic))

(defgeneric graphic-gstate ( graphic)
 (:documentation
  " Returns or (with setf) changes the graphics state of the given graphic."))
(defgeneric (setf graphic-gstate)(new-value graphic))

(defgeneric graphic-view ( graphic)
 (:documentation
  "Returns or (with setf) changes the first VIEW associated with the GRAPHIC."))
(defgeneric (setf graphic-view)(new-value graphic))

(defgeneric graphic-within-p ( graphic min-x min-y width height)
 (:documentation
  "If the given GRAPHIC lies completely within the rectangle
   - MIN-X MIN-Y WIDTH HEIGHT - given in world coordinates,
   then return true,  otherwise, return nil."))

(defgeneric world-extent (graphic  &optional result-extent)
 (:documentation
  "Returns the EXTENT of the given GRAPHIC in world coordinates.
   The extent is placed in RESULT-EXTENT if provided, otherwise
   a new EXTENT rectangle is returned."))

(defgeneric graphic-world-transform (graphic)
 (:documentation
  "Returns the fully-composed TRANSFORM to compute world coordinates
   for the GRAPHIC."))

(defgeneric graphic-extent (graphic )
 (:documentation
   "Return the extent-rectangle for the GRAPHIC in  object coordinates and caches the extent. "))

(defgeneric extent-compute (graphic )
 (:documentation
   "Return the extent-rectangle for the GRAPHIC in  object coordinates. "))

(defgeneric extent-changed (extent-cache))

(defgeneric scene-elements ( graphic) 
  (:documentation
   " Returns a list of the scene elements in a GRAPHIC.
    A return value of nil could mean the scene
    is empty of the GRAPHIC is not a scene "))
(defgeneric (setf scene-elements)(new-value graphic))

(defgeneric  graphic-sensitivity (graphic ) 
  (:documentation
   " Returns or (with setf) changes the sensitivity of the given GRAPHIC.
    The sensitivity of the GRAPHIC determines both the display and
    the editing states of a GRAPHIC."))
(defgeneric (setf graphic-sensitivity)(new-value graphic))

(defgeneric   scene-insert ( scene   graphic &optional position )
  (:documentation
   " Inserts the GRAPHIC at the given POSITION in the SCENE when
    POSITION is a number.  If POSITION  is a graphic, then GRAPHIC
    is inserted immediately after it.  If POSITION   nil, then
    GRAPHIC is inserted at the end of the elements sequence."))

(defgeneric scene-delete (scene pos)
  (:documentation
   " Removes the graphic at the given POSition from the SCENE. "))

(defgeneric scene-graphic ( scene position)
  (:documentation
   "return the graphic in the scene at the give position"))

(defgeneric   scene-reparent ( scene   new-parent &rest elements)
  (:documentation
   "Move each of the SCENE-elements and ELEMENTS into the NEW-PARENT scene. "))

(defgeneric scene-restack (scene old-position new-position)
  (:documentation
   "For the given SCENE, delete the graphic in OLD-POSITION
    and re-insert it in NEW-POSITION.")
  )



(defgeneric gravity-point (view gravity)
  (:documentation 
   "Return the world coordinates of the given gravity point on
    the VIEW's  world extent"))

(defgeneric repair-view (view)   
  (:documentation
   "Redraws any damaged regions in the VIEW and clears any damages. ")
  )

(defgeneric view-damage (view &rest damaged-region)   
  (:documentation
   " Records a DAMAGED-REGION of the VIEW for later repair.
    The DAMAGED-REGION contains either a single graphic object
    the damaged region being the object's extent) or a world
    coordinate sequence of the form (min-x min-y width height."))

(defgeneric view-gravity (view))
(defgeneric view-pan (view x y &optional gravity))
(defgeneric view-zoom-gravity (view))

(defgeneric view-orientation (view &key x y)
  (:DOCUMENTATION
   "Sets the orienation of the coordinate system to be used by the VIEW.
    The signs of X and Y will determine orientation of the coordinates.
    The 1st quadrant of the cartesian coordinate system is the default"))

(DEFgeneric  view-show-world (view)
 (:documentation
  "A view action method that shows all viewable objects in the graphic world"))

(DEFgeneric  view-show-region (view extent)
 (:documentation
   "A VIEW method that shows all viewable objects in the in the
    specified wcoord EXTENT"))

(DEFgeneric  transform-point (view  window-x window-y )
  (:documentation
   "Convert the given X and Y world coordinates to view
    coordinates for the  given VIEW." ))

(DEFgeneric  transform-x (view  window-x  )
  (:documentation
   "Convert the given X  world coordinate to a view coordinate for
    the  given VIEW." ))

(DEFgeneric  transform-y (view  window-y  )
  (:documentation 
   "Convert the given Y  world coordinate to a view coordinate 
    for the  given VIEW." ))

(DEFgeneric  view-untransform-point (view  window-x window-y )
 (:documentation
  "Convert the given X and Y view coordinates to world coordinates
   for the  given VIEW." ))

(defgeneric view-scale (view)
  (:documentation "Return the present scale factor of a VIEW "))
(defgeneric (setf view-scale) (new-value view))

(defgeneric view-pixel-size (view)
  (:documentation
   "Return the world-coordinate size of a pixel for the given VIEW. "))

(defgeneric view-scale-point (view  x-distance y-distance
				    &optional graphic-world-transform)  
  (:documentation
   " Convert the given X-DISTANCE and Y-DISTANCE to equivalent
    distances in the  VIEW coordinate system. 
    If GRAPHIC-WORLD-TRANSFORM is given, apply it to the
    distances before converting to view coordinates."))

(defgeneric view-transform-vector (view  vertices &optional round)  
  (:documentation
   "This method destructively changes the value of VERTICES by
    applying the VIEW transform to them. ROUND is a boolean value
    signals the use of round function instead of the floor function
    be used on the values being returned."))

(DEFgeneric  untransform-point (view  window-x window-y )
 (:documentation
  "Convert the given X and Y view coordinates to world coordinates
   for the  given VIEW." ))


(DEFgeneric  view-untransform-x (view  window-x  )
 (:documentation
  "   Convert the given X  view coordinate to a world coordinate for the
   given VIEW." ))


(DEFgeneric  view-untransform-y (view  window-y  )
 (:documentation
  "   Convert the given Y  view coordinate to a world coordinate
   for the  given VIEW." ))


(DEFgeneric  view-selection (view)
 (:documentation
  "Return the list of selected graphics in the VIEW"))

(DEFgeneric  view-add-selection (view graphic)
 (:documentation
  "Add the GRAPHIC or SEQUENCE of graphics to the VIEW selection."))

(DEFgeneric  view-remove-selection (view graphic)
 (:documentation
  "Remove the graphic from the view selection"))

(DEFgeneric  view-clear-selection (view )
 (:documentation
  "Unselect all the selected graphics in the view."))

(DEFgeneric  view-select-graphic (view &key add   )
 (:documentation
  "A view action that will replace the view selection with the
   picked graphic or if the keyword :ADD is T then the picked
   graphic is added to the view selection."))


(DEFgeneric  view-unselect-graphic (view )
 (:documentation
  "A view action that will remove the picked graphic from the view selection"))

(DEFgeneric  view-select-region (view  &key add )
 (:documentation
  "A view action method that initiates a user dialog that creates
   a rubberband box to select a group of graphics. All of the
   graphics surrounded by the rubberband box are added the view-selection.
   If ADD is nil the view selection is cleared before the graphics are added."))

(DEFgeneric  view-unselect-region (view )
 (:documentation
  "A view action method that initiates a user dialog that creates a
   rubberband box to unselect a group of graphics. All of the graphics
   surrounded by the rubberband box are removed from the view-selection."))

(DEFgeneric  scale-rubberband ( view  fixed-x fixed-y )
 (:documentation
  "A view action method used to performs a user dialog that displays
   a rubberband box that is modified by moving the pointer until the
   button is release. FIXED-X and FIXED-Y set the corner of the
   rubberband box that is will not move. ")
  )

(DEFGENERIC  view-x-pan (view)
 (:documentation
  "set or get the  horizontal pan value for the view "))

(DEFgeneric  view-y-pan (view)
 (:documentation "set or get the  vertical pan value for the view "))

(DEFgeneric  view-pan-right (view)
 (:documentation "An action method to attach to an event to pan a view right"))

(DEFgeneric  view-pan-left (view)
 (:documentation "An action method to attach to an event to pan a view left"))

(DEFgeneric  view-pan-up (view)
 (:documentation "An action method to attach to an event to pan a view up"))

(DEFgeneric  view-pan-down (view)
 (:documentation "An action method to attach to an event to pan a view down"))

(defgeneric vertex-x (polypoint  position)
  (:documentation
   "Get or setf the x coordinate of a polypoint at the given position"))
(defgeneric (setf vertex-x)(new-value polypoint position))

(defgeneric vertex-y (polypoint  position)
  (:documentation
   "Get or setf the y coordinate of a polypoint at the given position"))
(defgeneric (setf vertex-y)(new-value polypoint position))

(defgeneric rectangle-origin-x (rectangle)
 (:documentation "Return or set the x value for the origin of the rectangle."))
(defgeneric (setf rectangle-origin-x)(new-value rectangle))

(defgeneric rectangle-origin-y (rectangle)
 (:documentation "Return or set the y value for the origin of the rectangle."))
(defgeneric (setf rectangle-origin-y)(new-value rectangle))

(defgeneric rectangle-width (rectangle)
 (:documentation "Return or set the width the rectangle."))
(defgeneric (setf rectangle-width)(new-value rectangle))

(defgeneric rectangle-height (rectangle)
 (:documentation "Return or set the height the rectangle."))
(defgeneric (setf rectangle-height)(new-value rectangle))

(defgeneric rectangle-size (rectangle)
 (:documentation "Return width and height the rectangle."))

(defgeneric ellipse-origin-x (ellipse)
 (:documentation "Return or set the x value for the origin of the ellipse."))
(defgeneric (setf ellipse-origin-x)(new-value ellipse))

(defgeneric ellipse-origin-y (ellipse)
 (:documentation "Return or set the y value for the origin of the ellipse."))
(defgeneric (setf ellipse-origin-y)(new-value ellipse))

(defgeneric ellipse-width (ellipse)
 (:documentation "Return or set the width the ellipse."))
(defgeneric (setf ellipse-width)(new-value ellipse))

(defgeneric ellipse-height (ellipse)
 (:documentation "Return or set the height the ellipse."))
(defgeneric (setf ellipse-height)(new-value ellipse))

(DEFGENERIC  label-reverse-p (label)
 (:documentation "When true reverse the direction the label is printed when the angle of a rotated label is between
  90 and 270 degress "))
(defgeneric (setf label-reverse-p)(new-value label))

(DEFGENERIC  label-angle-extent (label)
 (:documentation "The extent of the rotate label"))
(defgeneric (setf label-angle-extent)(new-value label))

(DEFGENERIC  label-filled-background-p (label)
 (:documentation
  "When true label-fill-background-p, draw the text with the
   designated background color"))
(defgeneric (setf label-filled-background-p)(new-value label))

(defgeneric label-font (label &optional change-extent-p))
(defgeneric (setf label-font)(new-value label &optional change-extent-p))

(defgeneric label-string (label &optional change-extent-p))
(defgeneric view-label-extents (label scale view))

(defgeneric grabber-graphic (grabber-rect)
  (:documentation "Get or set  the graphic associated with a grabber rectangle"))
(defgeneric (setf grabber-graphic)(new-value grabber-rect))

(defgeneric view-transform-graphic (grabber view &key event)
  (:documentation
    "Perform an interactive dialag to scale, or move  a graphic in a VIEW.
     The GRABBER is a rectangle on the the  grabber rectangle that
     causes the dialog initiated when it receives the proper event. "))
 
(defgeneric view-move-graphic (grabber view &key event)
  (:documentation
    "Perform an interactive dialag to move  a graphic in a VIEW.
     The GRABBER is a rectangle on the the  grabber rectangle
     that causes the dialog initiated when it receives the proper event. "))
 
(defgeneric view-scale-graphic (grabber view &key uniform event)
  (:documentation
    "Perform an interactive dialag to scale a graphic in a VIEW.
     The GRABBER is a rectangle on the the  grabber rectangle that
     causes the dialog initiated when it receives the proper event. "))
 
(defgeneric view-rotate-graphic (grabber view &key  event)
  (:documentation
    "Perform an interactive dialag to rotate a graphic in a VIEW.
     The GRABBER is a rectangle on the the  grabber rectangle that
     causes the dialog initiated when it receives the proper event. "))

(DEFgeneric  view-zoom-value (view)
 (:documentation
  "   Set or get the scale factor for zooming in on the view
   and use the inverse for zoom out"))
(defgeneric (setf view-zoom-value)(new-value view))

(DEFgeneric  view-zoom-x (view)
 (:documentation "set or get the world x value to zoom on in the view "))
(defgeneric (setf view-zoom-x)(new-value view))

(DEFgeneric  view-zoom-y (view)
 (:documentation "set or get the world y value to zoom on in the view "))
(defgeneric (setf viewe-zoom-y)(new-value view))

(DEFgeneric  view-zoom-in (view)
 (:documentation  "An action method to attach to an event to zoom in on the view"))
      
(DEFgeneric  view-zoom-out (view)
 (:documentation  "An action method to attach to an event to zoom out in the view"))

(defgeneric view-zoom-gravity (view))
(defgeneric refresh-view (view))

(defgeneric restore-graphic (format &optional stream)
  (:documentation "Reads a given format from a given stream."))

(defgeneric save-graphic (graphic format &optional stream)
  (:documentation "Saves a graphic object in a given format to a given stream"))


