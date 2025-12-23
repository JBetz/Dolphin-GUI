"Filed out from Dolphin Smalltalk"!

RelativeConstraint subclass: #Inside
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

Inside guid: (GUID fromString: '{50694e96-5f22-4db6-907f-f1e7f8fbba83}')!

Inside comment: ''!

!Inside categoriesForClass!Unclassified! !

!Inside methodsFor!

compile
	constraints
		add: (Constraint
					lhs: frame left
					greaterThanOrEqualRhs: target left
					strength: strength);
		add: (Constraint
					lhs: frame top
					greaterThanOrEqualRhs: target top
					strength: strength);
		add: (Constraint
					lhs: frame right
					lessThanOrEqualRhs: target right
					strength: strength);
		add: (Constraint
					lhs: frame bottom
					lessThanOrEqualRhs: target bottom
					strength: strength)! !

!Inside categoriesForMethods!
compile!public! !
!

