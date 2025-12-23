"Filed out from Dolphin Smalltalk"!

ConstraintSpec subclass: #EdgeConstraint
	instanceVariableNames: 'target'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

EdgeConstraint guid: (GUID fromString: '{9e562233-b149-48fb-9054-b00c0dcb1ba0}')!

EdgeConstraint comment: ''!

!EdgeConstraint categoriesForClass!Unclassified! !

!EdgeConstraint methodsFor!

compile
	constraints add: (Constraint
				lhs: self variable
				equalsRhs: target
				strength: strength)!

target
	^target!

target: anObject
	target := anObject!

variable
	self subclassResponsibility! !

!EdgeConstraint categoriesForMethods!
compile!public! !
target!accessing!public! !
target:!accessing!public! !
variable!public! !
!

