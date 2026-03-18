"Filed out from Dolphin Smalltalk"!

Renderer2D subclass: #D3D12Renderer2D
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

D3D12Renderer2D guid: (GUID fromString: '{302b4b24-a27a-468c-8809-0bec19d83526}')!

D3D12Renderer2D comment: ''!

!D3D12Renderer2D categoriesForClass!Kernel-Objects! !

!D3D12Renderer2D methodsFor!

direct3DDevice
	^sdlRenderer direct3D12Device!

openSharedTexture: anExternalAddress
	^CInteropLibrary default openSharedD3D12Resource_device: self direct3DDevice
		handle: anExternalAddress!

releaseSharedTexture: anExternalAddress
	CInteropLibrary default releaseD3D12Resource_resource: anExternalAddress!

wrapExternalTexture: anExternalHandle extent: aPoint format: anInteger
	| properties |
	properties := SDL_Properties new.
	properties
		setPointerProperty: SDL_PROP_TEXTURE_CREATE_D3D12_TEXTURE_POINTER value: anExternalHandle;
		setNumberProperty: SDL_PROP_TEXTURE_CREATE_WIDTH_NUMBER value: aPoint x truncated;
		setNumberProperty: SDL_PROP_TEXTURE_CREATE_HEIGHT_NUMBER value: aPoint y truncated;
		setNumberProperty: SDL_PROP_TEXTURE_FORMAT_NUMBER value: anInteger.
	^sdlRenderer createTextureWithProperties: properties! !

!D3D12Renderer2D categoriesForMethods!
direct3DDevice!public! !
openSharedTexture:!public! !
releaseSharedTexture:!public! !
wrapExternalTexture:extent:format:!public! !
!

!D3D12Renderer2D class methodsFor!

driverName
	^'direct3d12'! !

!D3D12Renderer2D class categoriesForMethods!
driverName!public! !
!

