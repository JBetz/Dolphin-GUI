"Filed out from Dolphin Smalltalk"!

RelativeConstraint subclass: #CenterHorizontally
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

CenterHorizontally guid: (GUID fromString: '{216444a3-0833-43e0-9800-680d3ff08448}')!

CenterHorizontally comment: ''!

!CenterHorizontally categoriesForClass!Unclassified! !

!CenterHorizontally methodsFor!

compile
	constraints add: (Constraint
				lhs: frame center x
				equalsRhs: target center x
				strength: strength)! !

!CenterHorizontally categoriesForMethods!
compile!public! !
!

