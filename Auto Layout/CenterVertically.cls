"Filed out from Dolphin Smalltalk"!

RelativeConstraint subclass: #CenterVertically
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

CenterVertically guid: (GUID fromString: '{c3043ed8-e7f0-4888-8e22-c5cfbceded87}')!

CenterVertically comment: ''!

!CenterVertically categoriesForClass!Unclassified! !

!CenterVertically methodsFor!

compile
	constraints add: (Constraint
				lhs: frame center y
				equalsRhs: target center y
				strength: strength)! !

!CenterVertically categoriesForMethods!
compile!public! !
!

