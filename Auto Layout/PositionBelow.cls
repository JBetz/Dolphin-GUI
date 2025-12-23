"Filed out from Dolphin Smalltalk"!

RelativeConstraint subclass: #PositionBelow
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

PositionBelow guid: (GUID fromString: '{24151270-83d1-4f49-8b2b-9c4804018edc}')!

PositionBelow comment: ''!

!PositionBelow categoriesForClass!Unclassified! !

!PositionBelow methodsFor!

compile
	constraints add: (Constraint
				lhs: frame top
				equalsRhs: target bottom + gap
				strength: strength)! !

!PositionBelow categoriesForMethods!
compile!public! !
!

