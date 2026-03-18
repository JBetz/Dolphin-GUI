"Filed out from Dolphin Smalltalk"!

Renderer2D subclass: #D3D11Renderer2D
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

D3D11Renderer2D guid: (GUID fromString: '{2f616b07-b6f2-4b2f-90fa-7ebb6ab8a165}')!

D3D11Renderer2D comment: ''!

!D3D11Renderer2D categoriesForClass!Kernel-Objects! !

!D3D11Renderer2D methodsFor!

direct3DDevice
	^sdlRenderer direct3D11Device!

openSharedTexture: anExternalAddress
	^CInteropLibrary default openSharedD3D11Texture2D_device: self direct3DDevice
		handle: anExternalAddress!

releaseSharedTexture: anExternalAddress
	CInteropLibrary default releaseD3D11Texture2D_texture: anExternalAddress!

wrapExternalTexture: anExternalHandle extent: aPoint format: anInteger
	| properties |
	properties := SDL_Properties new.
	properties
		setPointerProperty: SDL_PROP_TEXTURE_CREATE_D3D11_TEXTURE_POINTER value: anExternalHandle;
		setNumberProperty: SDL_PROP_TEXTURE_CREATE_WIDTH_NUMBER value: aPoint x truncated;
		setNumberProperty: SDL_PROP_TEXTURE_CREATE_HEIGHT_NUMBER value: aPoint y truncated;
		setNumberProperty: SDL_PROP_TEXTURE_FORMAT_NUMBER value: anInteger.
	^sdlRenderer createTextureWithProperties: properties! !

!D3D11Renderer2D categoriesForMethods!
direct3DDevice!public! !
openSharedTexture:!public! !
releaseSharedTexture:!public! !
wrapExternalTexture:extent:format:!public! !
!

!D3D11Renderer2D class methodsFor!

driverName
	^'direct3d11'! !

!D3D11Renderer2D class categoriesForMethods!
driverName!public! !
!

