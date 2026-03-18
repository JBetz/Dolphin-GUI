"Filed out from Dolphin Smalltalk"!

ExternalStructure subclass: #GpuMemoryInfo
	instanceVariableNames: ''
	classVariableNames: '_OffsetOf_budget_video_memory _OffsetOf_current_usage _OffsetOf_total_video_memory'
	poolDictionaries: ''
	classInstanceVariableNames: ''!

GpuMemoryInfo guid: (GUID fromString: '{211d4147-b52b-44dd-811d-447bd6151be2}')!

GpuMemoryInfo addClassConstant: '_OffsetOf_budget_video_memory' value: 16r8!
GpuMemoryInfo addClassConstant: '_OffsetOf_current_usage' value: 16r10!
GpuMemoryInfo addClassConstant: '_OffsetOf_total_video_memory' value: 16r0!

GpuMemoryInfo comment: ''!

!GpuMemoryInfo categoriesForClass!External-Data-Structured! !

!GpuMemoryInfo methodsFor!

budget_video_memory
	"Answer the <Integer> value of the receiver's 'budget_video_memory' field."

	^bytes qwordAtOffset: _OffsetOf_budget_video_memory!

budget_video_memory: anInteger
	"Set the receiver's 'budget_video_memory' field to the value of the argument, anInteger"

	bytes qwordAtOffset: _OffsetOf_budget_video_memory put: anInteger!

current_usage
	"Answer the <Integer> value of the receiver's 'current_usage' field."

	^bytes qwordAtOffset: _OffsetOf_current_usage!

current_usage: anInteger
	"Set the receiver's 'current_usage' field to the value of the argument, anInteger"

	bytes qwordAtOffset: _OffsetOf_current_usage put: anInteger!

total_video_memory
	"Answer the <Integer> value of the receiver's 'total_video_memory' field."

	^bytes qwordAtOffset: _OffsetOf_total_video_memory!

total_video_memory: anInteger
	"Set the receiver's 'total_video_memory' field to the value of the argument, anInteger"

	bytes qwordAtOffset: _OffsetOf_total_video_memory put: anInteger! !

!GpuMemoryInfo categoriesForMethods!
budget_video_memory!**compiled accessors**!public! !
budget_video_memory:!**compiled accessors**!public! !
current_usage!**compiled accessors**!public! !
current_usage:!**compiled accessors**!public! !
total_video_memory!**compiled accessors**!public! !
total_video_memory:!**compiled accessors**!public! !
!

!GpuMemoryInfo class methodsFor!

defineFields
	"self compileDefinition"

	self
		defineField: #total_video_memory type: QWORDField new;
		defineField: #budget_video_memory type: QWORDField new;
		defineField: #current_usage type: QWORDField new!

getFieldNames
	^#(#total_video_memory #budget_video_memory #current_usage)! !

!GpuMemoryInfo class categoriesForMethods!
defineFields!public! !
getFieldNames!**compiled accessors**!constants!private! !
!

