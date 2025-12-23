"Filed out from Dolphin Smalltalk"!

Object subclass: #EditInfo
	instanceVariableNames: 'tag constraint constant'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

EditInfo guid: (GUID fromString: '{d21c1596-61b2-4fde-8e0e-c7ec34a30475}')!

EditInfo comment: ''!

!EditInfo categoriesForClass!Unclassified! !

!EditInfo methodsFor!

constant
	^constant!

constant: anObject
	constant := anObject!

constraint
	^constraint!

constraint: anObject
	constraint := anObject!

dumpOn: aReadWriteStream 
	^Error notYetImplemented!

tag
	^tag!

tag: anObject
	tag := anObject! !

!EditInfo categoriesForMethods!
constant!accessing!public! !
constant:!accessing!public! !
constraint!accessing!public! !
constraint:!accessing!public! !
dumpOn:!public! !
tag!accessing!public! !
tag:!accessing!public! !
!

