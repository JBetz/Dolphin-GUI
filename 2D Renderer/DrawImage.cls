"Filed out from Dolphin Smalltalk"!

DrawCommand subclass: #DrawImage
	instanceVariableNames: 'rectangle stretch'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

DrawImage guid: (GUID fromString: '{acba7282-96f9-463d-b63e-6226357ea87b}')!

DrawImage comment: ''!

!DrawImage categoriesForClass!Kernel-Objects! !

!DrawImage methodsFor!

applyClip: aRectangle
	rectangle := rectangle intersect: aRectangle!

applyOffset: aPoint
	rectangle := rectangle moveBy: aPoint!

initialize
	super initialize.
	stretch := false.
	rotation := 0.0!

rectangle
	^rectangle!

rectangle: anObject
	rectangle := anObject!

scaledBy: aFloat 
	rectangle := rectangle scaledBy: aFloat center: rectangle center !

stretch
	^stretch!

stretch: anObject
	stretch := anObject! !

!DrawImage categoriesForMethods!
applyClip:!public! !
applyOffset:!public! !
initialize!public! !
rectangle!accessing!public! !
rectangle:!accessing!public! !
scaledBy:!public! !
stretch!accessing!public! !
stretch:!accessing!public! !
!

