"Filed out from Dolphin Smalltalk"!

RelativeConstraint subclass: #AlignLeft
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

AlignLeft guid: (GUID fromString: '{8954055a-b329-4570-9863-b5942fb67b4a}')!

AlignLeft comment: ''!

!AlignLeft categoriesForClass!Unclassified! !

!AlignLeft methodsFor!

compile
	constraints add: (Constraint
				lhs: frame left
				equalsRhs: target left
				strength: strength)! !

!AlignLeft categoriesForMethods!
compile!public! !
!

