"Filed out from Dolphin Smalltalk"!

RelativeConstraint subclass: #PositionAfter
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

PositionAfter guid: (GUID fromString: '{e5e89366-0587-484c-8a9a-73621c6b29bc}')!

PositionAfter comment: ''!

!PositionAfter categoriesForClass!Unclassified! !

!PositionAfter methodsFor!

compile
	constraints add: (Constraint
				lhs: frame left
				equalsRhs: target right + gap
				strength: strength)! !

!PositionAfter categoriesForMethods!
compile!public! !
!

