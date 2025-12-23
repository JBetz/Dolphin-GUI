"Filed out from Dolphin Smalltalk"!

ConstraintSpec subclass: #FixedExtent
	instanceVariableNames: 'extent'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

FixedExtent guid: (GUID fromString: '{262d64ca-28ff-43fc-8c6e-73c710f3fa13}')!

FixedExtent comment: ''!

!FixedExtent categoriesForClass!Unclassified! !

!FixedExtent methodsFor!

compile
	constraints
		add: (Constraint
					lhs: frame width
					equalsRhs: extent x
					strength: strength);
		add: (Constraint
					lhs: frame height
					equalsRhs: extent y
					strength: strength)!

extent: aPoint 
	extent := aPoint! !

!FixedExtent categoriesForMethods!
compile!public! !
extent:!public! !
!

