"Filed out from Dolphin Smalltalk"!

WidthConstraint subclass: #FixedWidth
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

FixedWidth guid: (GUID fromString: '{e272c226-9674-4b46-a8cb-dc3144a65e52}')!

FixedWidth comment: ''!

!FixedWidth categoriesForClass!Unclassified! !

!FixedWidth methodsFor!

compile
	constraints add: (Constraint
				lhs: frame width
				equalsRhs: width
				strength: strength)! !

!FixedWidth categoriesForMethods!
compile!public! !
!

