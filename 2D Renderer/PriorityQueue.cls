"Filed out from Dolphin Smalltalk"!

Dictionary variableSubclass: #PriorityQueue
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

PriorityQueue guid: (GUID fromString: '{928c2e51-0158-4483-9b47-12f3a526fe5d}')!

PriorityQueue comment: ''!

!PriorityQueue categoriesForClass!Kernel-Objects! !

!PriorityQueue methodsFor!

asOrderedCollection
	| items priorityLevels |
	items := OrderedCollection new.
	priorityLevels := self keys asOrderedCollection
				sortUsing: (DefaultSortAlgorithm newSortBlock: [:a :b | a > b]).
	priorityLevels do: [:anInteger | items addAll: (self at: anInteger)].
	^items!

clear
	self removeAll!

dequeue
	^(self at: self highestPriorityLevel) removeLast!

do: aBlock
	self do: aBlock sortBlock: [:a :b | a > b]!

do: aBlock sortBlock: aSortBlock
	| priorityLevels |
	priorityLevels := self keys asOrderedCollection
				sortUsing: (DefaultSortAlgorithm newSortBlock: aSortBlock).
	priorityLevels do: [:anInteger | (self at: anInteger) do: [:anItem | aBlock value: anItem]]!

enqueue: anObject
	self enqueue: anObject priority: 0!

enqueue: anObject priority: anInteger
	| priorityList |
	priorityList := self at: anInteger ifAbsent: [self at: anInteger put: OrderedCollection new].
	priorityList add: anObject!

highestPriorityLevel
	^(self keys asOrderedCollection sortUsing: (DefaultSortAlgorithm sortBlock: [:a :b | a > b]))
		ifNotNil: [:priorityLevels | priorityLevels first]!

reverseDo: aBlock
	self do: aBlock sortBlock: [:a :b | a < b]! !

!PriorityQueue categoriesForMethods!
asOrderedCollection!public! !
clear!public! !
dequeue!public! !
do:!public! !
do:sortBlock:!private! !
enqueue:!public! !
enqueue:priority:!public! !
highestPriorityLevel!public! !
reverseDo:!public! !
!

