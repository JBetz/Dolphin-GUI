"Filed out from Dolphin Smalltalk"!

HeightConstraint subclass: #MinimumHeight
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

MinimumHeight guid: (GUID fromString: '{8ef46dc0-ec15-489e-bf45-9d2de929b7df}')!

MinimumHeight comment: ''!

!MinimumHeight categoriesForClass!Unclassified! !

!MinimumHeight methodsFor!

compile
	constraints add: (Constraint
				lhs: frame height
				greaterThanOrEqualRhs: height
				strength: strength)! !

!MinimumHeight categoriesForMethods!
compile!public! !
!

