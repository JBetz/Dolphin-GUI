"Filed out from Dolphin Smalltalk"!

HeightConstraint subclass: #FixedHeight
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

FixedHeight guid: (GUID fromString: '{faa1d406-63be-48ce-9e51-92e2ff1f0300}')!

FixedHeight comment: ''!

!FixedHeight categoriesForClass!Unclassified! !

!FixedHeight methodsFor!

compile
	constraints add: (Constraint
				lhs: frame height
				equalsRhs: height
				strength: strength)! !

!FixedHeight categoriesForMethods!
compile!public! !
!

