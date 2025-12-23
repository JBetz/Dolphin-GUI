"Filed out from Dolphin Smalltalk"!

ConstraintSpec subclass: #FixedOrigin
	instanceVariableNames: 'point'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

FixedOrigin guid: (GUID fromString: '{d127e742-bfc5-401f-a7f7-fb40b4a85c45}')!

FixedOrigin comment: ''!

!FixedOrigin categoriesForClass!Unclassified! !

!FixedOrigin methodsFor!

compile
	constraints
		add: (Constraint
					lhs: frame left
					equalsRhs: point x
					strength: strength);
		add: (Constraint
					lhs: frame top
					equalsRhs: point y
					strength: strength)!

point: aPoint
	point := aPoint! !

!FixedOrigin categoriesForMethods!
compile!public! !
point:!public! !
!

