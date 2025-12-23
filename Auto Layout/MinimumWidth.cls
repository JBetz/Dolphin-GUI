"Filed out from Dolphin Smalltalk"!

WidthConstraint subclass: #MinimumWidth
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

MinimumWidth guid: (GUID fromString: '{337ce845-ec03-4736-ade8-97ce5efe421c}')!

MinimumWidth comment: ''!

!MinimumWidth categoriesForClass!Unclassified! !

!MinimumWidth methodsFor!

compile
	constraints add: (Constraint
				lhs: frame width
				greaterThanOrEqualRhs: width
				strength: strength)! !

!MinimumWidth categoriesForMethods!
compile!public! !
!

