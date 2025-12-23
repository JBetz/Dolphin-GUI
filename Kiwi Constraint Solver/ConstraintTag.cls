"Filed out from Dolphin Smalltalk"!

Object subclass: #ConstraintTag
	instanceVariableNames: 'marker other'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

ConstraintTag guid: (GUID fromString: '{d60cc730-02df-43be-85fc-358f816cd803}')!

ConstraintTag comment: ''!

!ConstraintTag categoriesForClass!Kernel-Objects! !

!ConstraintTag methodsFor!

marker
	^marker!

marker: anObject
	marker := anObject!

other
	^other!

other: anObject
	other := anObject! !

!ConstraintTag categoriesForMethods!
marker!accessing!public! !
marker:!accessing!public! !
other!accessing!public! !
other:!accessing!public! !
!

!ConstraintTag class methodsFor!

marker: aSymbol other: anotherSymbol
	^self new
		marker: aSymbol;
		other: anotherSymbol! !

!ConstraintTag class categoriesForMethods!
marker:other:!public! !
!

