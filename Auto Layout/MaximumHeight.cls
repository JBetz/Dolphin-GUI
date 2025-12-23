"Filed out from Dolphin Smalltalk"!

HeightConstraint subclass: #MaximumHeight
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

MaximumHeight guid: (GUID fromString: '{20e0ca10-c692-415f-8e3d-61cf205aa180}')!

MaximumHeight comment: ''!

!MaximumHeight categoriesForClass!Unclassified! !

!MaximumHeight methodsFor!

compile
	constraints add: (Constraint
				lhs: frame height
				lessThanOrEqualRhs: height
				strength: strength)! !

!MaximumHeight categoriesForMethods!
compile!public! !
!

