"Filed out from Dolphin Smalltalk"!

Collection variableSubclass: #OrderedDictionary
	instanceVariableNames: 'dictionary orderedKeys'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

OrderedDictionary guid: (GUID fromString: '{bf555aad-801c-4bd1-8d55-8f0af176345d}')!

OrderedDictionary comment: ''!

!OrderedDictionary categoriesForClass!Kernel-Objects! !

!OrderedDictionary methodsFor!

= anObject
	"Returns true if the receiver and argument are identical, or if they
	are both some kind of order-preserving dictionary and if they have
	the same associations regardless of order."

	self == anObject ifTrue: [^true].
	(anObject isOrderPreservingDictionary
		and: [self isIdentityDictionary = anObject isIdentityDictionary and: [self size = anObject size]])
			ifFalse: [^false].
	dictionary
		associationsDo: [:each | (anObject at: each key ifAbsent: [^false]) = each value ifFalse: [^false]].
	^true!

add: anAssociation
	| oldSize |
	oldSize := dictionary size.
	dictionary add: anAssociation.
	dictionary size > oldSize
		ifTrue: 
			[orderedKeys size > oldSize ifFalse: [self growOrderedKeys].
			orderedKeys at: oldSize + 1 put: anAssociation key].
	^anAssociation!

addAll: anAssociationCollection
	"Since Collection implements #associationsDo:, this method can accept
	any collection of associations including Arrays and OrderedCollections"

	anAssociationCollection associationsDo: [:each | self add: each].
	^anAssociationCollection!

associationAt: aKey
	^ dictionary associationAt: aKey!

associationAt: aKey ifAbsent: aBlock
	^dictionary associationAt: aKey ifAbsent: aBlock!

associationAt: aKey ifPresent: aBlock
	"Squeak and GS do not have #associationAt:ifPresent: so it
	is reimplemented for portability"

	^aBlock cull: (dictionary associationAt: aKey ifAbsent: [^nil])!

associations
	| associations i |
	associations := Array new: self size.
	i := 1.
	self associationsDo: 
			[:each |
			associations at: i put: each.
			i := i + 1].
	^associations!

associationsDo: aBlock
	self keysDo: [:each | aBlock value: (self associationAt: each)]!

associationsSelect: aBlock
	^self speciesNewFrom: (self associations select: aBlock)!

at: aKey
	^dictionary at: aKey!

at: aKey ifAbsent: aBlock
	^dictionary at: aKey ifAbsent: aBlock!

at: aKey ifAbsentPut: aBlock
	^self at: aKey ifAbsent: [self at: aKey put: aBlock value]!

at: aKey ifAbsentPutValue: aValue
	^(self includesKey: aKey) ifTrue: [self at: aKey] ifFalse: [self at: aKey put: aValue]!

at: aKey ifPresent: aBlock
	"Squeak and GS don't use #cull: for the ifPresent:
	block, so it is reimplemented for portability"

	^aBlock cull: (self at: aKey ifAbsent: [^nil])!

at: aKey ifPresent: aPresentBlock ifAbsent: anAbsentBlock
	"Squeak and GS don't use #cull: for the ifPresent:
	block, so it is reimplemented for portability"

	self at: aKey ifPresent: [:value | ^aPresentBlock cull: value].
	^anAbsentBlock value!

at: aKey put: aValue
	| oldSize |
	oldSize := dictionary size.
	dictionary at: aKey put: aValue.
	dictionary size > oldSize
		ifTrue: 
			[orderedKeys size > oldSize ifFalse: [self growOrderedKeys].
			orderedKeys at: oldSize + 1 put: aKey].
	^aValue!

capacity
	^dictionary capacity!

collect: aBlock
	^self speciesNewFrom: (self associations collect: [:each | each key -> (aBlock value: each value)])!

copyEmpty
	"Squeak and GS don't have Collection>>#copyEmpty:, so it is
	reimplemented for portability"

	^self species new!

dictionary
	^dictionary!

dictionaryClass
	^Dictionary!

do: aBlock
	self valuesDo: aBlock!

errorInvalidIndex: anIndex
	"Squeak and GS do not have SubscriptOutOfBounds, so Error is used
	for portability"

	self error: 'Invalid index: ' , anIndex printString!

errorValueNotFound: aValue
	"Squeak and GS do not have ValueNotFound, so Error is used
	for portability"

	self error: 'Value not found'!

growOrderedKeys
	orderedKeys := (Array new: ((orderedKeys size * 1.5) asInteger max: 10))
				replaceFrom: 1
				to: orderedKeys size
				with: orderedKeys
				startingAt: 1!

hash
	^dictionary hash!

identityIndexOfKey: aKey
	^self identityIndexOfKey: aKey ifAbsent: [0]!

identityIndexOfKey: aKey ifAbsent: aBlock
	1 to: self size do: [:i | (orderedKeys at: i) == aKey ifTrue: [^i]].
	^aBlock value!

includes: anObject
	^dictionary includes: anObject!

includesAssociation: anAssociation
	"IndentityDictionary>>includesAssociation: works differently on GS
	testing both key and value identity, so it is reimplemented here
	to behave like Pharo/Squeak"

	^(dictionary at: anAssociation key ifAbsent: [^false]) = anAssociation value!

includesIdentity: anObject
	"GS does not have includesIdentity:"

	self valuesDo: [:each | each == anObject ifTrue: [^true]].
	^false!

includesKey: aKey
	^dictionary includesKey: aKey!

indexOfKey: aKey
	^self indexOfKey: aKey ifAbsent: [0]!

indexOfKey: aKey ifAbsent: aBlock
	1 to: self size do: [:i | (orderedKeys at: i) = aKey ifTrue: [^i]].
	^aBlock value!

initialize
	super initialize.
	dictionary := self dictionaryClass!

initialize: aCapacity
	dictionary := self dictionaryClass new: aCapacity.
	orderedKeys := Array new: aCapacity!

isDictionary
	^ true!

isEmpty
	"Squeak's Collection>>#isEmpty is inefficient"

	^self size = 0!

isIdentityDictionary
	^ false!

isOrderPreservingDictionary
	^ true!

keyAtIdentityValue: aValue
	^self keyAtIdentityValue: aValue ifAbsent: [self errorValueNotFound: aValue]!

keyAtIdentityValue: aValue ifAbsent: aBlock
	"GS does not have keyAtIdentityValue:ifAbsent:"

	self keysAndValuesDo: [:key :value | value == aValue ifTrue: [^key]].
	^aBlock value!

keyAtIndex: anIndex
	^self keyAtIndex: anIndex ifAbsent: [self errorInvalidIndex: anIndex]!

keyAtIndex: anIndex ifAbsent: aBlock
	(anIndex > 0 and: [anIndex <= self size])
		ifTrue: [^orderedKeys at: anIndex]
		ifFalse: [^aBlock value]!

keyAtValue: aValue
	^dictionary keyAtValue: aValue!

keyAtValue: aValue ifAbsent: aBlock
	^dictionary keyAtValue: aValue ifAbsent: aBlock!

keyForIdentity: anObject
	"Reimplemented for portability"

	self keysAndValuesDo: [:key :value | value == anObject ifTrue: [^key]].
	^nil!

keys
	^orderedKeys copyFrom: 1 to: self size!

keysAndValuesDo: aBlock
	self keysDo: [:each | aBlock value: each value: (self at: each)]!

keysAndValuesRemove: aTwoArgumentBlock
	| removedAssociations |
	removedAssociations := OrderedCollection new.
	self associationsDo: 
			[:each |
			(aTwoArgumentBlock value: each key value: each value) ifTrue: [removedAssociations add: each]].
	removedAssociations do: [:each | self removeKey: each key]!

keysDo: aBlock
	"Use to:do: for speed"

	1 to: self size do: [:i | aBlock value: (orderedKeys at: i)]!

keysSortedSafely
	"GS's #keysSortedSafely returns a SortedCollection instead of
	an Array, so this is reimplemented directly for portability, and
	'self keys' is used instead of 'dictionary keys', because GS's
	#keys returns a Set which can't be sorted"

	^self keys sort!

orderedKeys
	^orderedKeys!

orderedKeysIdentityIndexOf: aKey
	"GS does not have #identityIndexOf:"

	1 to: orderedKeys size do: [:i | (orderedKeys at: i) == aKey ifTrue: [^i]].
	^0!

orderedKeysIndexOf: aKey
	^orderedKeys indexOf: aKey!

orderedKeysRemove: aRemovedKey
	| index |
	index := self orderedKeysIndexOf: aRemovedKey.

	"shift every remaining key after to the left by one"
	orderedKeys
		replaceFrom: index
		to: self size
		with: orderedKeys
		startingAt: index + 1.

	"one key was removed and the rest shifted, so nil what was the last
	key slot before removing and shifting"
	orderedKeys at: self size + 1 put: nil!

postCopy
	orderedKeys := orderedKeys copy.
	dictionary := dictionary copy!

printElementsOn: aStream
	aStream nextPut: $(.
	self size > 100
		ifTrue: 
			[aStream nextPutAll: 'size '.
			self size printOn: aStream]
		ifFalse: 
			[self associations withIndexDo: 
					[:each :i |
					aStream
						print: each key;
						nextPutAll: '->';
						print: each value.
					i < self size ifTrue: [aStream space]]].
	aStream nextPut: $)!

remove: anObject ifAbsent: aBlock
	self shouldNotImplement!

removeAll
	1 to: self size do: [:i | orderedKeys at: i put: nil].
	dictionary removeAll!

removeKey: aKey
	| value |
	value := dictionary removeKey: aKey.
	self orderedKeysRemove: aKey.
	^value!

removeKey: aKey ifAbsent: aBlock
	| oldSize value |
	oldSize := dictionary size.
	value := dictionary removeKey: aKey ifAbsent: [nil].
	dictionary size < oldSize ifTrue: [self orderedKeysRemove: aKey].
	^value ifNil: [aBlock value]!

removeKeys: aKeyCollection
	"Fast removal of multiple keys; returns self to avoid
	having to create a removed value collection and does not
	raise errors."

	aKeyCollection size > 1
		ifTrue: 
			[| oldSize newOrderedKeys newOrderedKeysIndex |
			oldSize := self size.
			aKeyCollection do: [:each | dictionary removeKey: each ifAbsent: [nil]].
			newOrderedKeys := Array new: oldSize.
			newOrderedKeysIndex := 0.
			1 to: oldSize
				do: 
					[:i |
					| key |
					(dictionary includesKey: (key := orderedKeys at: i))
						ifTrue: [newOrderedKeys at: (newOrderedKeysIndex := newOrderedKeysIndex + 1) put: key]].
			orderedKeys := newOrderedKeys]
		ifFalse: 
			[aKeyCollection size = 1
				ifTrue: 
					["use #anyOne, because it can be a Set"
					self removeKey: aKeyCollection anyOne ifAbsent: [nil]]]!

select: aBlock
	^self speciesNewFrom: (self associations select: [:each | aBlock value: each value])!

size
	^ dictionary size!

speciesNewFrom: anAssociationCollection
	^self species newFrom: anAssociationCollection!

values
	^self associations collect: [:each | each value]!

valuesDo: aBlock
	self keysDo: [:each | aBlock value: (self at: each)]! !

!OrderedDictionary categoriesForMethods!
=!comparing!public! !
add:!adding!public! !
addAll:!adding!public! !
associationAt:!accessing!public! !
associationAt:ifAbsent:!accessing!public! !
associationAt:ifPresent:!accessing!public! !
associations!accessing!public! !
associationsDo:!enumerating!public! !
associationsSelect:!enumerating!public! !
at:!accessing!public! !
at:ifAbsent:!accessing!public! !
at:ifAbsentPut:!accessing!public! !
at:ifAbsentPutValue:!public! !
at:ifPresent:!accessing!public! !
at:ifPresent:ifAbsent:!accessing!public! !
at:put:!accessing!public! !
capacity!accessing!public! !
collect:!enumerating!public! !
copyEmpty!copying!public! !
dictionary!private! !
dictionaryClass!accessing!public! !
do:!enumerating!public! !
errorInvalidIndex:!private! !
errorValueNotFound:!private! !
growOrderedKeys!private! !
hash!comparing!public! !
identityIndexOfKey:!accessing!public! !
identityIndexOfKey:ifAbsent:!accessing!public! !
includes:!public!testing! !
includesAssociation:!public!testing! !
includesIdentity:!public!testing! !
includesKey:!public!testing! !
indexOfKey:!accessing!public! !
indexOfKey:ifAbsent:!accessing!public! !
initialize!public! !
initialize:!initialization!public! !
isDictionary!public!testing! !
isEmpty!public!testing! !
isIdentityDictionary!public!testing! !
isOrderPreservingDictionary!public!testing! !
keyAtIdentityValue:!accessing!public! !
keyAtIdentityValue:ifAbsent:!accessing!public! !
keyAtIndex:!accessing!public! !
keyAtIndex:ifAbsent:!accessing!public! !
keyAtValue:!accessing!public! !
keyAtValue:ifAbsent:!accessing!public! !
keyForIdentity:!accessing!public! !
keys!accessing!public! !
keysAndValuesDo:!enumerating!public! !
keysAndValuesRemove:!public!removing! !
keysDo:!enumerating!public! !
keysSortedSafely!accessing!public! !
orderedKeys!private! !
orderedKeysIdentityIndexOf:!private! !
orderedKeysIndexOf:!private! !
orderedKeysRemove:!private! !
postCopy!copying!public! !
printElementsOn:!printing!public! !
remove:ifAbsent:!public!removing! !
removeAll!public!removing! !
removeKey:!public!removing! !
removeKey:ifAbsent:!public!removing! !
removeKeys:!public!removing! !
select:!enumerating!public! !
size!accessing!public! !
speciesNewFrom:!private! !
values!private! !
valuesDo:!private! !
!

!OrderedDictionary class methodsFor!

new
	^self new: 10!

new: aCapacity
	^self basicNew initialize: aCapacity!

newFrom: anAssociationCollection
	| newDictionary |
	newDictionary := self new: anAssociationCollection size.
	anAssociationCollection associationsDo: [:each | newDictionary at: each key put: each value].
	^newDictionary!

newFromPairs: aSequenceableCollection
	| newDictionary |
	newDictionary := self new: (aSequenceableCollection size / 2) floor.
	1 to: aSequenceableCollection size - 1
		by: 2
		do: [:i | newDictionary at: (aSequenceableCollection at: i) put: (aSequenceableCollection at: i + 1)].
	^newDictionary! !

!OrderedDictionary class categoriesForMethods!
new!instance creation!public! !
new:!instance creation!public! !
newFrom:!instance creation!public! !
newFromPairs:!instance creation!public! !
!

