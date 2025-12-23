"Filed out from Dolphin Smalltalk"!

RelativeConstraint subclass: #Contains
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

Contains guid: (GUID fromString: '{15048835-8169-41a3-b067-2480159634b4}')!

Contains comment: ''!

!Contains categoriesForClass!Unclassified! !

!Contains methodsFor!

compile
	constraints
		add: (Constraint
					lhs: frame left
					lessThanOrEqualRhs: target left
					strength: strength);
		add: (Constraint
					lhs: frame top
					lessThanOrEqualRhs: target top
					strength: strength);
		add: (Constraint
					lhs: frame right
					greaterThanOrEqualRhs: target right
					strength: strength);
		add: (Constraint
					lhs: frame bottom
					greaterThanOrEqualRhs: target bottom
					strength: strength)! !

!Contains categoriesForMethods!
compile!public! !
!

