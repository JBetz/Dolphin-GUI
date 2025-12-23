"Filed out from Dolphin Smalltalk"!

ConstraintSpec subclass: #RelativeConstraint
	instanceVariableNames: 'target gap'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

RelativeConstraint guid: (GUID fromString: '{aba0ce4c-36dd-4376-bb99-62224c889d09}')!

RelativeConstraint comment: ''!

!RelativeConstraint categoriesForClass!Unclassified! !

!RelativeConstraint methodsFor!

gap: anInteger
	gap := anInteger!

initialize
	super initialize.
	gap := 0!

target: anObject
	target := anObject! !

!RelativeConstraint categoriesForMethods!
gap:!public! !
initialize!public! !
target:!accessing!public! !
!

