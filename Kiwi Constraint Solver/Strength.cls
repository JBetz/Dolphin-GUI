"Filed out from Dolphin Smalltalk"!

Object subclass: #Strength
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

Strength guid: (GUID fromString: '{0aa4d2df-37c0-4667-b876-367709d848da}')!

Strength comment: ''!

!Strength categoriesForClass!Kernel-Objects! !

!Strength class methodsFor!

clip: aNumber
	^0.0 max: (self required min: aNumber)!

createStrong: strong medium: medium weak: weak 
	^self createStrong: strong medium: medium weak: weak weight: 1.0!

createStrong: strong medium: medium weak: weak weight: weight
	| result |
	result := 0.0.
	result := result + ((0.0 max: (1000.0 min: strong * weight)) * 1000000.0).
	result := result + ((0.0 max: (1000.0 min: medium * weight)) * 1000.0).
	result := result + (0.0 max: (1000.0 min: weak * weight)).
	^result!

medium
	^self createStrong: 0.0 medium: 1.0 weak: 0.0!

required
	^self createStrong: 1000.0 medium: 1000.0 weak: 1000.0!

strong
	^self createStrong: 1.0 medium: 0.0 weak: 0.0!

weak
	^self createStrong: 0.0 medium: 0.0 weak: 1.0! !

!Strength class categoriesForMethods!
clip:!public! !
createStrong:medium:weak:!public! !
createStrong:medium:weak:weight:!public! !
medium!public! !
required!public! !
strong!public! !
weak!public! !
!

