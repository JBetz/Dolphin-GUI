"Filed out from Dolphin Smalltalk"!

ConstraintSpec subclass: #FixedCorner
	instanceVariableNames: 'point'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

FixedCorner guid: (GUID fromString: '{3afa3040-f52e-4779-9e1e-7eed663f0f71}')!

FixedCorner comment: ''!

!FixedCorner categoriesForClass!Unclassified! !

!FixedCorner methodsFor!

compile
	constraints
		add: (Constraint
					lhs: frame right
					equalsRhs: point x
					strength: strength);
		add: (Constraint
					lhs: frame bottom
					equalsRhs: point y
					strength: strength)!

point: aPoint
	point := aPoint! !

!FixedCorner categoriesForMethods!
compile!public! !
point:!public! !
!

