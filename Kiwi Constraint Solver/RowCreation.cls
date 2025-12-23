"Filed out from Dolphin Smalltalk"!

Object subclass: #RowCreation
	instanceVariableNames: 'row tag'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

RowCreation guid: (GUID fromString: '{281e31ac-7f63-4453-99d1-2dd5e0dff035}')!

RowCreation comment: ''!

!RowCreation categoriesForClass!Unclassified! !

!RowCreation methodsFor!

row
	^row!

row: anObject
	row := anObject!

tag
	^tag!

tag: anObject
	tag := anObject! !

!RowCreation categoriesForMethods!
row!accessing!public! !
row:!accessing!public! !
tag!accessing!public! !
tag:!accessing!public! !
!

!RowCreation class methodsFor!

row: aRow tag: aTag
	^self new
		row: aRow;
		tag: aTag! !

!RowCreation class categoriesForMethods!
row:tag:!public! !
!

