"Filed out from Dolphin Smalltalk"!

WidthConstraint subclass: #MaximumWidth
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

MaximumWidth guid: (GUID fromString: '{b64859e0-dbcd-4b07-895a-7d945be95c07}')!

MaximumWidth comment: ''!

!MaximumWidth categoriesForClass!Unclassified! !

!MaximumWidth methodsFor!

compile
	constraints add: (Constraint
				lhs: frame width
				lessThanOrEqualRhs: width
				strength: strength)! !

!MaximumWidth categoriesForMethods!
compile!public! !
!

