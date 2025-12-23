"Filed out from Dolphin Smalltalk"!

Object subclass: #Frame
	instanceVariableNames: 'solver top left width height hugWidth hugHeight constraintSpecs editVariables'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

Frame guid: (GUID fromString: '{11013d37-9006-461a-8044-fcf74d4e8ab0}')!

Frame comment: ''!

!Frame categoriesForClass!Unclassified! !

!Frame methodsFor!

addConstraintSpec: aConstraintSpec
	constraintSpecs add: aConstraintSpec.
	aConstraintSpec applyTo: solver.
	solver updateVariables!

bottom
	^top + height!

bottomLeft
	^left@(top + height)!

bottomRight
	^(left + width)@(top + height)!

center
	^(left + (width / 2)) @ (top + (height / 2))!

centeredHorizontallyIn: aFrame
	self centeredHorizontallyIn: aFrame strength: Strength required!

centeredHorizontallyIn: aFrame strength: aFloat
	self addConstraintSpec: (CenterHorizontally new
				frame: self;
				target: aFrame;
				strength: aFloat)!

centeredIn: aFrame
	self
		centeredHorizontallyIn: aFrame;
		centeredVerticallyIn: aFrame!

centeredIn: anObject strength: aFloat
	self
		centeredHorizontallyIn: anObject strength: aFloat;
		centeredVerticallyIn: anObject strength: aFloat!

centeredVerticallyIn: aFrame
	self centeredVerticallyIn: aFrame strength: Strength required!

centeredVerticallyIn: aFrame strength: aFloat
	self addConstraintSpec: (CenterVertically new
				frame: self;
				target: aFrame;
				strength: aFloat)!

constraintSpecs
	^constraintSpecs!

contains: anObject
	self addConstraintSpec: (Contains new
				frame: self;
				target: anObject)!

corner
	^self bottomRight!

editExtent: aPoint
	self editExtent: aPoint strength: Strength required - 1!

editExtent: aPoint strength: aFloat
	solver
		tryAddingEditVariable: width strength: aFloat;
		tryAddingEditVariable: height strength: aFloat;
		suggest: width value: aPoint x;
		suggest: height value: aPoint y;
		updateVariables.
	editVariables
		add: width;
		add: height!

editHeight: aNumber
	self editHeight: aNumber strength: Strength strong!

editHeight: aNumber strength: aFloat
	solver
		tryAddingEditVariable: height strength: aFloat;
		suggest: height value: aNumber;
		updateVariables.
	editVariables add: height!

editOrigin: aPoint
	self editOrigin: aPoint strength: Strength strong!

editOrigin: aPoint strength: aFloat
	solver
		tryAddingEditVariable: left strength: aFloat;
		tryAddingEditVariable: top strength: aFloat;
		suggest: top value: aPoint x asFloat;
		suggest: left value: aPoint y asFloat;
		updateVariables.
	editVariables
		add: left;
		add: top!

editWidth: aNumber
	self editWidth: aNumber strength: Strength strong!

editWidth: aNumber strength: aFloat
	solver
		tryAddingEditVariable: width strength: aFloat;
		suggest: width value: aNumber;
		updateVariables.
	editVariables add: width!

extent
	^width @ height!

fixBottom: anObject
	self addConstraintSpec: (FixedBottom new
				frame: self;
				target: anObject)!

fixBottom: anObject strength: aStrength
	self addConstraintSpec: (FixedBottom new
				frame: self;
				target: anObject;
				strength: aStrength)!

fixCorner: aPoint
	self addConstraintSpec: (FixedCorner new
				strength: Strength required;
				frame: self;
				point: aPoint)!

fixCorner: aPoint strength: aFloat
	self addConstraintSpec: (FixedCorner new
				strength: aFloat;
				frame: self;
				point: aPoint)!

fixExtent
	self fixExtent: self contentExtent!

fixExtent: aPoint
	self fixExtent: aPoint strength: Strength required!

fixExtent: aPoint strength: aStrength
	self addConstraintSpec: (FixedExtent new
				frame: self;
				extent: aPoint;
				strength: aStrength)!

fixHeight
	self fixHeight: self height!

fixHeight: anObject
	self addConstraintSpec: (FixedHeight new
				frame: self;
				height: anObject)!

fixHeight: anObject strength: aStrength
	self addConstraintSpec: (FixedHeight new
				frame: self;
				height: anObject;
				strength: aStrength)!

fixLeft: anObject
	self addConstraintSpec: (FixedLeft new
				frame: self;
				target: anObject)!

fixLeft: anObject strength: aStrength
	self addConstraintSpec: (FixedLeft new
				frame: self;
				target: anObject;
				strength: aStrength)!

fixOrigin: aPoint
	self fixOrigin: aPoint strength: Strength required!

fixOrigin: aPoint strength: anInteger
	self addConstraintSpec: (FixedOrigin new
				frame: self;
				point: aPoint;
				strength: anInteger)!

fixRelativeOrigin: aPoint
	^self fixOrigin: aPoint strength: Strength required!

fixRight: anObject
	self addConstraintSpec: (FixedRight new
				frame: self;
				target: anObject)!

fixRight: anObject strength: aStrength
	self addConstraintSpec: (FixedRight new
				frame: self;
				target: anObject;
				strength: aStrength)!

fixTop: anObject
	self addConstraintSpec: (FixedTop new
				frame: self;
				target: anObject)!

fixTop: anObject strength: aStrength
	self addConstraintSpec: (FixedTop new
				frame: self;
				target: anObject;
				strength: aStrength)!

fixWidth: anObject
	self addConstraintSpec: (FixedWidth new
				frame: self;
				width: anObject)!

fixWidth: anObject strength: anInteger
	self addConstraintSpec: (FixedWidth new
				frame: self;
				width: anObject;
				strength: anInteger)!

height
	^height!

hugExtent: aFloat
	hugWidth := aFloat.
	hugHeight := aFloat!

hugHeight
	^hugHeight!

hugHeight: aFloat 
	hugHeight := aFloat!

hugWidth
	^hugWidth!

hugWidth: aFloat
	hugWidth := aFloat!

initialize
	super initialize.
	left := Variable new.
	top := Variable new.
	width := Variable new.
	height := Variable new.
	hugWidth := Strength medium.
	hugHeight := Strength medium.
	constraintSpecs := OrderedCollection new.
	editVariables := Set new!

inside: anObject
	self addConstraintSpec: (Inside new
				frame: self;
				target: anObject)!

inside: anObject strength: anInteger
	self addConstraintSpec: (Inside new
				frame: self;
				target: anObject;
				strength: anInteger)!

left
	^left!

leftAlignWith: aWidget
	self addConstraintSpec: (AlignLeft new
				frame: self;
				target: aWidget)!

maximumHeight: anObject
	self addConstraintSpec: (MaximumHeight new
				frame: self;
				height: anObject)!

maximumWidth: anObject
	self addConstraintSpec: (MaximumWidth new
				frame: self;
				width: anObject)!

minimumHeight: anObject
	self addConstraintSpec: (MinimumHeight new
				frame: self;
				height: anObject)!

minimumWidth: anObject
	self addConstraintSpec: (MinimumWidth new
				frame: self;
				width: anObject)!

nameVariables: aSymbol
	left name: aSymbol , '_left'.
	top name: aSymbol , '_top'.
	width name: aSymbol , '_width'.
	height name: aSymbol , '_height'!

origin
	^self topLeft!

positionAbove: aWidget
	self addConstraintSpec: (PositionAbove new
				frame: self;
				target: aWidget)!

positionAbove: aWidget gap: anInteger
	self addConstraintSpec: (PositionAbove new
				frame: self;
				target: aWidget;
				gap: anInteger)!

positionAfter: aWidget
	self addConstraintSpec: (PositionAfter new
				frame: self;
				target: aWidget)!

positionAfter: aWidget gap: anInteger
	self addConstraintSpec: (PositionAfter new
				frame: self;
				target: aWidget;
				gap: anInteger)!

positionBefore: aWidget
	self addConstraintSpec: (PositionBefore new
				frame: self;
				target: aWidget)!

positionBefore: aWidget gap: anInteger
	self addConstraintSpec: (PositionBefore new
				frame: self;
				target: aWidget;
				gap: anInteger)!

positionBelow: aWidget
	self addConstraintSpec: (PositionBelow new
				frame: self;
				target: aWidget)!

positionBelow: aWidget gap: anInteger
	self addConstraintSpec: (PositionBelow new
				frame: self;
				target: aWidget;
				gap: anInteger)!

positionBelow: aWidget grap: anInteger
	self addConstraintSpec: (PositionBelow new
				frame: self;
				target: aWidget;
				gap: anInteger)!

rectangle
	^Rectangle origin: left value @ top value extent: width value @ height value!

removeConstraints
	constraintSpecs do: [:each | each removeFrom: solver].
	constraintSpecs removeAll.
	editVariables asArray do: [:each | solver removeEditVariable: each].
	editVariables removeAll!

right
	^left + width!

rightAlignWith: aWidget
	self addConstraintSpec: (AlignRight new
				frame: self;
				target: aWidget)!

solver
	^solver!

solver: anObject
	solver := anObject!

top
	^top!

topLeft
	^left @ top!

topRight
	^(left + width) @ top!

width
	^width! !

!Frame categoriesForMethods!
addConstraintSpec:!private! !
bottom!public! !
bottomLeft!public! !
bottomRight!public! !
center!public! !
centeredHorizontallyIn:!constraints!public! !
centeredHorizontallyIn:strength:!constraints!public! !
centeredIn:!constraints!public! !
centeredIn:strength:!public! !
centeredVerticallyIn:!constraints!public! !
centeredVerticallyIn:strength:!constraints!public! !
constraintSpecs!public! !
contains:!constraints!public! !
corner!public! !
editExtent:!public! !
editExtent:strength:!public! !
editHeight:!public! !
editHeight:strength:!public! !
editOrigin:!public! !
editOrigin:strength:!public! !
editWidth:!public! !
editWidth:strength:!public! !
extent!public! !
fixBottom:!constraints!public! !
fixBottom:strength:!constraints!public! !
fixCorner:!constraints!public! !
fixCorner:strength:!constraints!public! !
fixExtent!constraints!public! !
fixExtent:!constraints!public! !
fixExtent:strength:!constraints!public! !
fixHeight!constraints!public! !
fixHeight:!constraints!public! !
fixHeight:strength:!constraints!public! !
fixLeft:!constraints!public! !
fixLeft:strength:!constraints!public! !
fixOrigin:!public! !
fixOrigin:strength:!public! !
fixRelativeOrigin:!public! !
fixRight:!constraints!public! !
fixRight:strength:!constraints!public! !
fixTop:!constraints!public! !
fixTop:strength:!constraints!public! !
fixWidth:!constraints!public! !
fixWidth:strength:!constraints!public! !
height!public! !
hugExtent:!public! !
hugHeight!public! !
hugHeight:!public! !
hugWidth!public! !
hugWidth:!public! !
initialize!public! !
inside:!constraints!public! !
inside:strength:!constraints!public! !
left!public! !
leftAlignWith:!public! !
maximumHeight:!constraints!public! !
maximumWidth:!constraints!public! !
minimumHeight:!constraints!public! !
minimumWidth:!constraints!public! !
nameVariables:!public! !
origin!public! !
positionAbove:!constraints!public! !
positionAbove:gap:!public! !
positionAfter:!constraints!public! !
positionAfter:gap:!public! !
positionBefore:!constraints!public! !
positionBefore:gap:!public! !
positionBelow:!constraints!public! !
positionBelow:gap:!public! !
positionBelow:grap:!public! !
rectangle!public! !
removeConstraints!public! !
right!public! !
rightAlignWith:!public! !
solver!accessing!public! !
solver:!accessing!public! !
top!public! !
topLeft!public! !
topRight!public! !
width!public! !
!

!Frame class methodsFor!

new
	^super new initialize!

new: aString
	^self new nameVariables: aString! !

!Frame class categoriesForMethods!
new!public! !
new:!public! !
!

