"Filed out from Dolphin Smalltalk"!

RelativeConstraint subclass: #AlignRight
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

AlignRight guid: (GUID fromString: '{64b4e42a-4fdc-40e9-90b1-75f5a5523dde}')!

AlignRight comment: ''!

!AlignRight categoriesForClass!Unclassified! !

!AlignRight methodsFor!

compile
	constraints add: (Constraint
				lhs: frame right
				equalsRhs: target right
				strength: strength)! !

!AlignRight categoriesForMethods!
compile!public! !
!

