"Filed out from Dolphin Smalltalk"!

RelativeConstraint subclass: #PositionBefore
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

PositionBefore guid: (GUID fromString: '{a2d0e062-d93c-4282-94b4-5e0ae4482dd7}')!

PositionBefore comment: ''!

!PositionBefore categoriesForClass!Unclassified! !

!PositionBefore methodsFor!

compile
	constraints add: (Constraint
				lhs: frame right + gap
				equalsRhs: target left
				strength: strength)! !

!PositionBefore categoriesForMethods!
compile!public! !
!

