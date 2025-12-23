"Filed out from Dolphin Smalltalk"!

RelativeConstraint subclass: #AlignBottom
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

AlignBottom guid: (GUID fromString: '{3b2adcbe-0309-4ea1-bf33-6b311c43d31c}')!

AlignBottom comment: ''!

!AlignBottom categoriesForClass!Unclassified! !

!AlignBottom methodsFor!

compile
	constraints add: (Constraint
				lhs: frame bottom
				equalsRhs: target bottom
				strength: strength)! !

!AlignBottom categoriesForMethods!
compile!public! !
!

