"Filed out from Dolphin Smalltalk"!

RelativeConstraint subclass: #AlignTop
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

AlignTop guid: (GUID fromString: '{34fe8343-9912-41dd-94c2-6bf019e02c07}')!

AlignTop comment: ''!

!AlignTop categoriesForClass!Unclassified! !

!AlignTop methodsFor!

compile
	constraints add: (Constraint
				lhs: frame top
				equalsRhs: target top
				strength: strength)! !

!AlignTop categoriesForMethods!
compile!public! !
!

