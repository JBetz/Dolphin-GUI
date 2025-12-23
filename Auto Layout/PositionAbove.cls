"Filed out from Dolphin Smalltalk"!

RelativeConstraint subclass: #PositionAbove
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

PositionAbove guid: (GUID fromString: '{0fcd097c-c1f2-460e-a9e6-ba741015b805}')!

PositionAbove comment: ''!

!PositionAbove categoriesForClass!Unclassified! !

!PositionAbove methodsFor!

compile
	constraints add: (Constraint
				lhs: frame bottom
				equalsRhs: target top - gap
				strength: strength)! !

!PositionAbove categoriesForMethods!
compile!public! !
!

