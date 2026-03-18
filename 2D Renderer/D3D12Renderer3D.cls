"Filed out from Dolphin Smalltalk"!

Renderer3D subclass: #D3D12Renderer3D
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

D3D12Renderer3D guid: (GUID fromString: '{e76b806c-d7c3-478a-8c7c-cba81c40117f}')!

D3D12Renderer3D comment: ''!

!D3D12Renderer3D categoriesForClass!Kernel-Objects! !

!D3D12Renderer3D methodsFor!

openSharedTexture: anExternalAddress
	^anExternalAddress!

releaseSharedTexture: anExternalAddress
	!

wrapExternalTexture: anExternalHandle extent: aPoint format: anInteger
	| properties |
	properties := SDL_Properties new.
	properties
		setPointerProperty: SDL_PROP_GPU_TEXTURE_CREATE_DXGI_SHARED_HANDLE_POINTER value: anExternalHandle;
		setNumberProperty: SDL_PROP_TEXTURE_CREATE_WIDTH_NUMBER value: aPoint x truncated;
		setNumberProperty: SDL_PROP_TEXTURE_CREATE_HEIGHT_NUMBER value: aPoint y truncated;
		setNumberProperty: SDL_PROP_TEXTURE_FORMAT_NUMBER value: anInteger.
	^self
		createTexture: aPoint
		format: anInteger
		properties: properties! !

!D3D12Renderer3D categoriesForMethods!
openSharedTexture:!public! !
releaseSharedTexture:!public! !
wrapExternalTexture:extent:format:!public! !
!

!D3D12Renderer3D class methodsFor!

driverName
	^'direct3d12'! !

!D3D12Renderer3D class categoriesForMethods!
driverName!public! !
!

