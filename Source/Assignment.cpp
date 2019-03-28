/*******************************************
	PostProcessPoly.cpp

	Main scene and game functions
********************************************/

#include <Windows.h>
#include <sstream>
#include <string>
using namespace std;

#include <d3d10.h>
#include <d3dx10.h>

#include "Defines.h"
#include "CVector3.h"
#include "CVector4.h"
#include "Camera.h"
#include "Light.h"
#include "EntityManager.h"
#include "Messenger.h"
#include "CParseLevel.h"
#include "Assignment.h" 
#include "HSLColour.h"

namespace gen
{

//*****************************************************************************
// Post-process data
//*****************************************************************************

// Enumeration of different post-processes
enum PostProcesses
{
	Copy,
	Tint,
	Gradient,
	GreyNoise,
	Burn,
	Distort,
	Spiral,
	HeatHaze,
	BoxBlur,
	GaussianBlur,
	UnderWater,
	Negative,
	Retro,
	Bloom,
	DepthOfField,
	MotionBlur,
	Vignette,
	NumPostProcesses,
};

// Currently used post process
vector<PostProcesses> FullScreenFilters = { Copy };
int PostProcessIndex = 0;

// Post-process settings
float BurnLevel = 0.0f;
const float BurnSpeed = 0.2f;
float SpiralTimer = 0.0f;
const float SpiralSpeed = 1.0f;
float HeatHazeTimer = 0.0f;
const float HeatHazeSpeed = 1.0f;
float UnderWaterTimer = 0.0f;
const float UnderWaterSpeed = 1.0f;
float BlurLevel = 1;
float BlurStep = 0.1;
int KernelSize;
float FocalRange = 0.01;
float FocalDistance = 0.9645;
float FocalStep = 0.0001;
float PixelSize = 0.007; // Bond. James Bond.
int NumberOfColours = 16;
bool IsCameraMoving = false;


// Separate effect file for full screen & area post-processes. Not necessary to use a separate file, but convenient given the architecture of this lab
ID3D10Effect* PPEffect;

// Technique name for each post-process
const string PPTechniqueNames[NumPostProcesses] = {	"PPCopy", "PPTint", "PPGradient", "PPGreyNoise", "PPBurn", "PPDistort",
"PPSpiral", "PPHeatHaze", "PPBoxBlur", "PPGaussianBlur", "PPUnderWater", "PPNegative", "PPRetro", "PPBloom", "PPDepthOfField", "PPMotionBlur",
"PPVignette"};

// Technique pointers for each post-process
ID3D10EffectTechnique* PPTechniques[NumPostProcesses];


// Will render the scene to a texture in a first pass, then copy that texture to the back buffer in a second post-processing pass
// So need a texture and two "views": a render target view (to render into the texture - 1st pass) and a shader resource view (use the rendered texture as a normal texture - 2nd pass)
ID3D10Texture2D*          SceneTexture = NULL;
ID3D10RenderTargetView*   SceneRenderTarget = NULL;
ID3D10ShaderResourceView* SceneShaderResource = NULL;

// Additional textures used by post-processes
ID3D10ShaderResourceView* NoiseMap = NULL;
ID3D10ShaderResourceView* BurnMap = NULL;
ID3D10ShaderResourceView* DistortMap = NULL;

// Variables to link C++ post-process textures to HLSL shader variables (for area / full-screen post-processing)
ID3D10EffectShaderResourceVariable* SceneTextureVar = NULL;
ID3D10EffectShaderResourceVariable* PostProcessMapVar = NULL; // Single shader variable used for the three maps above (noise, burn, distort). Only one is needed at a time
ID3D10EffectShaderResourceVariable* DepthMapVar = NULL;

// Variables specifying the area used for post-processing
ID3D10EffectVectorVariable* PPAreaTopLeftVar = NULL;
ID3D10EffectVectorVariable* PPAreaBottomRightVar = NULL;
ID3D10EffectScalarVariable* PPAreaDepthVar = NULL;

// Other variables for individual post-processes
ID3D10EffectVectorVariable* TintColourVar = NULL;
ID3D10EffectVectorVariable* TintColour2Var = NULL;
ID3D10EffectVectorVariable* UnderWaterTintColourVar = NULL;
ID3D10EffectVectorVariable* NoiseScaleVar = NULL;
ID3D10EffectVectorVariable* NoiseOffsetVar = NULL;
ID3D10EffectScalarVariable* DistortLevelVar = NULL;
ID3D10EffectScalarVariable* BurnLevelVar = NULL;
ID3D10EffectScalarVariable* SpiralTimerVar = NULL;
ID3D10EffectScalarVariable* HeatHazeTimerVar = NULL;
ID3D10EffectScalarVariable* UnderWaterTimerVar = NULL;
ID3D10EffectShaderResourceVariable* KernelVar = NULL;

// Gaussian blur
ID3D10ShaderResourceView* KernelRV = NULL;
ID3D10Texture2D* KernelArray = NULL;
ID3D10EffectScalarVariable* KernelSizeVar = NULL;
ID3D10EffectScalarVariable* PixelSizeVar = NULL;
ID3D10EffectScalarVariable* NumberOfColoursVar = NULL;

// Depth of field
ID3D10EffectScalarVariable* FocalDistanceVar = NULL;
ID3D10EffectScalarVariable* FocalRangeVar = NULL;
ID3D10EffectScalarVariable* NearClipVar = NULL;
ID3D10EffectScalarVariable* FarClipVar = NULL;
ID3D10EffectVectorVariable* MousePosVar = NULL;
bool UsingMousePos = false;

// Other
ID3D10ShaderResourceView* BlurredShaderResource = NULL;
ID3D10RenderTargetView* BlurredRenderTarget = NULL;
ID3D10Texture2D* BlurredTexture = NULL;
ID3D10EffectShaderResourceVariable* BlurredMapVar = NULL;

ID3D10Texture2D* IntermediateTexture = NULL;
ID3D10ShaderResourceView* IntermediateShaderResource = NULL;
ID3D10RenderTargetView* IntermediateRenderTarget = NULL;

ID3D10ShaderResourceView* BrightShaderResource = NULL;
ID3D10RenderTargetView* BrightRenderTarget = NULL;
ID3D10Texture2D* BrightTexture = NULL;
ID3D10EffectShaderResourceVariable* BrightMapVar = NULL;

ID3D10ShaderResourceView* LastFrameShaderResource = NULL;
ID3D10RenderTargetView* LastFrameRenderTarget = NULL;
ID3D10Texture2D* LastFrameTexture = NULL;
ID3D10EffectShaderResourceVariable* LastFrameMapVar = NULL;

extern ID3D10EffectScalarVariable* ViewportWidthVar;// = NULL; // Dimensions of the viewport needed to help access the scene texture (see poly post-processing shaders)
extern ID3D10EffectScalarVariable* ViewportHeightVar;// = NULL;

HSLColour GradientTopColour(0, 1, 0.5f);
HSLColour GradientBotColour(240, 1, 0.5f);

//*****************************************************************************

//-----------------------------------------------------------------------------
// Constants
//-----------------------------------------------------------------------------

// Control speed
const float CameraRotSpeed = 2.0f;
float CameraMoveSpeed = 80.0f;

// Amount of time to pass before calculating new average update time
const float UpdateTimePeriod = 0.25f;



//-----------------------------------------------------------------------------
// Global system variables
//-----------------------------------------------------------------------------

// Folders used for meshes/textures and effect files
extern const string MediaFolder;
extern const string ShaderFolder;

// Get reference to global DirectX variables from another source file
extern ID3D10Device*           g_pd3dDevice;
extern IDXGISwapChain*         SwapChain;
extern ID3D10ShaderResourceView* DepthShaderView;
extern ID3D10DepthStencilView* DepthStencilView;
extern ID3D10RenderTargetView* BackBufferRenderTarget;
extern ID3D10RenderTargetView* PostProcessingRenderTargets[];
ID3D10ShaderResourceView* PostProcessShaderResources[2];
ID3D10Texture2D*          PostProcessTextures[2];
extern ID3DX10Font*            OSDFont;

// Actual viewport dimensions (fullscreen or windowed)
extern TUInt32 BackBufferWidth;
extern TUInt32 BackBufferHeight;

// Current mouse position
//extern TUInt32 MouseX;
//extern TUInt32 MouseY;
extern CVector2 MousePixel;

// Messenger class for sending messages to and between entities
extern CMessenger Messenger;


//-----------------------------------------------------------------------------
// Global game/scene variables
//-----------------------------------------------------------------------------

// Entity manager and level parser
CEntityManager EntityManager;
CParseLevel LevelParser( &EntityManager );

// Other scene elements
const int NumLights = 2;
CLight*  Lights[NumLights];
CCamera* MainCamera;

// Sum of recent update times and number of times in the sum - used to calculate
// average over a given time period
float SumUpdateTimes = 0.0f;
int NumUpdateTimes = 0;
float AverageUpdateTime = -1.0f; // Invalid value at first


//-----------------------------------------------------------------------------
// Game Constants
//-----------------------------------------------------------------------------

// Lighting
const SColourRGBA AmbientColour( 0.3f, 0.3f, 0.4f, 1.0f );
CVector3 LightCentre( 0.0f, 30.0f, 50.0f );
const float LightOrbit = 170.0f;
const float LightOrbitSpeed = 0.2f;

//-----------------------------------------------------------------------------
// Scene management
//-----------------------------------------------------------------------------

// Creates the scene geometry
bool SceneSetup()
{
	// Prepare render methods
	InitialiseMethods();
	
	// Read templates and entities from XML file
	if (!LevelParser.ParseFile( "Entities.xml" )) return false;
	
	// Set camera position and clip planes
	MainCamera = new CCamera( CVector3( 25, 30, -115 ), CVector3(ToRadians(8.0f), ToRadians(-35.0f), 0) );
	MainCamera->SetNearFarClip( 2.0f, 300000.0f ); 

	// Sunlight
	Lights[0] = new CLight( CVector3( -10000.0f, 6000.0f, 0000.0f), SColourRGBA(1.0f, 0.8f, 0.6f) * 12000, 20000.0f ); // Colour is multiplied by light brightness

	// Light orbiting area
	Lights[1] = new CLight( LightCentre, SColourRGBA(0.0f, 0.2f, 1.0f) * 50, 100.0f );

	return true;
}


// Release everything in the scene
void SceneShutdown()
{
	// Release render methods
	ReleaseMethods();

	// Release lights
	for (int light = NumLights - 1; light >= 0; --light)
	{
		delete Lights[light];
	}

	// Release camera
	delete MainCamera;

	// Destroy all entities
	EntityManager.DestroyAllEntities();
	EntityManager.DestroyAllTemplates();
}


//*****************************************************************************
// Post Processing Setup
//*****************************************************************************

// Prepare resources required for the post-processing pass
bool PostProcessSetup()
{
	D3D10_TEXTURE2D_DESC arrayDesc;
	arrayDesc.Width = 256;
	arrayDesc.Height = 1;	// Since we're passing an array, we only need one row
	arrayDesc.MipLevels = 1;
	arrayDesc.ArraySize = 1;
	arrayDesc.Format = DXGI_FORMAT_R32_FLOAT; // 32-bit floats
	//arrayDesc.Format = DXGI_FORMAT_R8G8B8A8_UNORM; // RGBA texture (8-bits each)
	arrayDesc.SampleDesc.Count = 1;
	arrayDesc.SampleDesc.Quality = 0;
	arrayDesc.Usage = D3D10_USAGE_DYNAMIC;
	arrayDesc.BindFlags = D3D10_BIND_SHADER_RESOURCE;
	arrayDesc.CPUAccessFlags = D3D10_CPU_ACCESS_WRITE;

	// Create the "scene texture" - the texture into which the scene will be rendered in the first pass
	D3D10_TEXTURE2D_DESC textureDesc;
	textureDesc.Width  = BackBufferWidth;  // Match views to viewport size
	textureDesc.Height = BackBufferHeight;
	textureDesc.MipLevels = 1; // No mip-maps when rendering to textures (or we will have to render every level)
	textureDesc.ArraySize = 1;
	textureDesc.Format = DXGI_FORMAT_R8G8B8A8_UNORM; // RGBA texture (8-bits each)
	textureDesc.SampleDesc.Count = 1;
	textureDesc.SampleDesc.Quality = 0;
	textureDesc.Usage = D3D10_USAGE_DEFAULT;
	textureDesc.BindFlags = D3D10_BIND_RENDER_TARGET | D3D10_BIND_SHADER_RESOURCE; // Indicate we will use texture as render target, and pass it to shaders
	textureDesc.CPUAccessFlags = 0;
	textureDesc.MiscFlags = 0;
	if (FAILED(g_pd3dDevice->CreateTexture2D( &textureDesc, NULL, &SceneTexture ))) return false;
	if (FAILED(g_pd3dDevice->CreateTexture2D(&textureDesc, NULL, &PostProcessTextures[0]))) return false;
	if (FAILED(g_pd3dDevice->CreateTexture2D(&textureDesc, NULL, &PostProcessTextures[1]))) return false;
	if (FAILED(g_pd3dDevice->CreateTexture2D(&arrayDesc, NULL, &KernelArray))) return false;
	if (FAILED(g_pd3dDevice->CreateTexture2D(&textureDesc, NULL, &BlurredTexture))) return false;
	if (FAILED(g_pd3dDevice->CreateTexture2D(&textureDesc, NULL, &IntermediateTexture))) return false;	
	if (FAILED(g_pd3dDevice->CreateTexture2D(&textureDesc, NULL, &BrightTexture))) return false;
	if (FAILED(g_pd3dDevice->CreateTexture2D(&textureDesc, NULL, &LastFrameTexture))) return false;

	// Get a "view" of the texture as a render target - giving us an interface for rendering to the texture
	if (FAILED(g_pd3dDevice->CreateRenderTargetView( SceneTexture, NULL, &SceneRenderTarget ))) return false;
	if (FAILED(g_pd3dDevice->CreateRenderTargetView(PostProcessTextures[0], NULL, &PostProcessingRenderTargets[0]))) return false;
	if (FAILED(g_pd3dDevice->CreateRenderTargetView(PostProcessTextures[1], NULL, &PostProcessingRenderTargets[1]))) return false;
	if (FAILED(g_pd3dDevice->CreateRenderTargetView(BlurredTexture, NULL, &BlurredRenderTarget))) return false;
	if (FAILED(g_pd3dDevice->CreateRenderTargetView(IntermediateTexture, NULL, &IntermediateRenderTarget))) return false;
	if (FAILED(g_pd3dDevice->CreateRenderTargetView(BrightTexture, NULL, &BrightRenderTarget))) return false;
	if (FAILED(g_pd3dDevice->CreateRenderTargetView(LastFrameTexture, NULL, &LastFrameRenderTarget))) return false;

	// And get a shader-resource "view" - giving us an interface for passing the texture to shaders
	D3D10_SHADER_RESOURCE_VIEW_DESC srDesc;
	srDesc.Format = textureDesc.Format;
	srDesc.ViewDimension = D3D10_SRV_DIMENSION_TEXTURE2D;
	srDesc.Texture2D.MostDetailedMip = 0;
	srDesc.Texture2D.MipLevels = 1;

	// Depth buffer desc
	D3D10_SHADER_RESOURCE_VIEW_DESC dbsrDesc;
	dbsrDesc.Format = DXGI_FORMAT_R32_FLOAT;
	dbsrDesc.ViewDimension = D3D10_SRV_DIMENSION_TEXTURE2D;
	dbsrDesc.Texture2D.MostDetailedMip = 0;
	dbsrDesc.Texture2D.MipLevels = 1;

	D3D10_SHADER_RESOURCE_VIEW_DESC saDesc;
	saDesc.Format = arrayDesc.Format;
	saDesc.ViewDimension = D3D10_SRV_DIMENSION_TEXTURE2D;
	saDesc.Texture1D.MostDetailedMip = 0;
	saDesc.Texture1D.MipLevels = 1;
	if (FAILED(g_pd3dDevice->CreateShaderResourceView( SceneTexture, &srDesc, &SceneShaderResource ))) return false;
	if (FAILED(g_pd3dDevice->CreateShaderResourceView(PostProcessTextures[0], &srDesc, &PostProcessShaderResources[0]))) return false;
	if (FAILED(g_pd3dDevice->CreateShaderResourceView(PostProcessTextures[1], &srDesc, &PostProcessShaderResources[1]))) return false;
	if (FAILED(g_pd3dDevice->CreateShaderResourceView(KernelArray, &saDesc, &KernelRV))) return false;
	if (FAILED(g_pd3dDevice->CreateShaderResourceView(BlurredTexture, &srDesc, &BlurredShaderResource))) return false;
	if (FAILED(g_pd3dDevice->CreateShaderResourceView(IntermediateTexture, &srDesc, &IntermediateShaderResource))) return false;
	if (FAILED(g_pd3dDevice->CreateShaderResourceView(BrightTexture, &srDesc, &BrightShaderResource))) return false;
	if (FAILED(g_pd3dDevice->CreateShaderResourceView(LastFrameTexture, &srDesc, &LastFrameShaderResource))) return false;
	
	// Load post-processing support textures
	if (FAILED( D3DX10CreateShaderResourceViewFromFile( g_pd3dDevice, (MediaFolder + "Noise.png").c_str() ,   NULL, NULL, &NoiseMap,   NULL ) )) return false;
	if (FAILED( D3DX10CreateShaderResourceViewFromFile( g_pd3dDevice, (MediaFolder + "Burn.png").c_str() ,    NULL, NULL, &BurnMap,    NULL ) )) return false;
	if (FAILED( D3DX10CreateShaderResourceViewFromFile( g_pd3dDevice, (MediaFolder + "Distort.png").c_str() , NULL, NULL, &DistortMap, NULL ) )) return false;


	// Load and compile a separate effect file for post-processes.
	ID3D10Blob* pErrors;
	DWORD dwShaderFlags = D3D10_SHADER_ENABLE_STRICTNESS; // These "flags" are used to set the compiler options

	string fullFileName = ShaderFolder + "PostProcess.fx";
	if( FAILED( D3DX10CreateEffectFromFile( fullFileName.c_str(), NULL, NULL, "fx_4_0", dwShaderFlags, 0, g_pd3dDevice, NULL, NULL, &PPEffect, &pErrors, NULL ) ))
	{
		if (pErrors != 0)  MessageBox( NULL, reinterpret_cast<char*>(pErrors->GetBufferPointer()), "Error", MB_OK ); // Compiler error: display error message
		else               MessageBox( NULL, "Error loading FX file. Ensure your FX file is in the same folder as this executable.", "Error", MB_OK );  // No error message - probably file not found
		return false;
	}

	// There's an array of post-processing technique names above - get array of post-process techniques matching those names from the compiled effect file
	for (int pp = 0; pp < NumPostProcesses; pp++)
	{
		PPTechniques[pp] = PPEffect->GetTechniqueByName( PPTechniqueNames[pp].c_str() );
	}

	// Link to HLSL variables in post-process shaders
	SceneTextureVar			= PPEffect->GetVariableByName( "SceneTexture" )->AsShaderResource();
	PostProcessMapVar		= PPEffect->GetVariableByName( "PostProcessMap" )->AsShaderResource();
	DepthMapVar				= PPEffect->GetVariableByName( "DepthMap" )->AsShaderResource();
	PPAreaTopLeftVar		= PPEffect->GetVariableByName( "PPAreaTopLeft" )->AsVector();
	PPAreaBottomRightVar	= PPEffect->GetVariableByName( "PPAreaBottomRight" )->AsVector();
	PPAreaDepthVar			= PPEffect->GetVariableByName( "PPAreaDepth" )->AsScalar();
	TintColourVar			= PPEffect->GetVariableByName( "TintColour" )->AsVector();
	TintColour2Var			= PPEffect->GetVariableByName( "TintColour2" )->AsVector();
	UnderWaterTintColourVar = PPEffect->GetVariableByName( "UnderWaterTintColour")->AsVector();
	NoiseScaleVar			= PPEffect->GetVariableByName( "NoiseScale" )->AsVector();
	NoiseOffsetVar			= PPEffect->GetVariableByName( "NoiseOffset" )->AsVector();
	DistortLevelVar			= PPEffect->GetVariableByName( "DistortLevel" )->AsScalar();
	BurnLevelVar			= PPEffect->GetVariableByName( "BurnLevel" )->AsScalar();
	SpiralTimerVar			= PPEffect->GetVariableByName( "SpiralTimer" )->AsScalar();
	HeatHazeTimerVar		= PPEffect->GetVariableByName( "HeatHazeTimer" )->AsScalar();
	UnderWaterTimerVar		= PPEffect->GetVariableByName( "UnderWaterTimer" )->AsScalar();
	KernelVar				= PPEffect->GetVariableByName( "Kernel" )->AsShaderResource();
	KernelSizeVar			= PPEffect->GetVariableByName( "KernelSize" )->AsScalar();
	PixelSizeVar			= PPEffect->GetVariableByName( "PixelSize" )->AsScalar();
	NumberOfColoursVar		= PPEffect->GetVariableByName( "NumberOfColours" )->AsScalar();
	FocalDistanceVar		= PPEffect->GetVariableByName( "FocalDistance" )->AsScalar();
	FocalRangeVar			= PPEffect->GetVariableByName( "FocalRange" )->AsScalar();
	NearClipVar				= PPEffect->GetVariableByName( "NearClip" )->AsScalar();
	FarClipVar				= PPEffect->GetVariableByName( "FarClip" )->AsScalar();
	MousePosVar				= PPEffect->GetVariableByName( "MousePos" )->AsVector();
	BlurredMapVar			= PPEffect->GetVariableByName( "BlurredMap" )->AsShaderResource();
	LastFrameMapVar			= PPEffect->GetVariableByName( "LastFrameMap" )->AsShaderResource();
	BrightMapVar			= PPEffect->GetVariableByName( "BrightMap" )->AsShaderResource();
	ViewportWidthVar		= PPEffect->GetVariableByName( "ViewportWidth" )->AsScalar();
	ViewportHeightVar		= PPEffect->GetVariableByName( "ViewportHeight" )->AsScalar();

	return true;
}

void PostProcessShutdown()
{
	if (PPEffect)            PPEffect->Release();
    if (DistortMap)          DistortMap->Release();
    if (BurnMap)             BurnMap->Release();
    if (NoiseMap)            NoiseMap->Release();
	if (SceneShaderResource) SceneShaderResource->Release();
	if (PostProcessShaderResources[0]) PostProcessShaderResources[0]->Release();
	if (PostProcessShaderResources[1]) PostProcessShaderResources[1]->Release();
	if (SceneRenderTarget)   SceneRenderTarget->Release();
	if (SceneTexture)        SceneTexture->Release();
	if (PostProcessTextures[0]) PostProcessTextures[0]->Release();
	if (PostProcessTextures[1]) PostProcessTextures[1]->Release();
	if (KernelRV) KernelRV->Release();
	if (KernelArray) KernelArray->Release();
	if (BlurredTexture) BlurredTexture->Release();
	if (BlurredRenderTarget) BlurredRenderTarget->Release();
	if (BlurredShaderResource) BlurredShaderResource->Release();
	if (IntermediateShaderResource) IntermediateShaderResource->Release();
	if (IntermediateRenderTarget) IntermediateRenderTarget->Release();
	if (BrightShaderResource) BrightShaderResource->Release();
	if (BrightRenderTarget) BrightRenderTarget->Release();
	if (BrightTexture) BrightTexture->Release();
	if (LastFrameShaderResource) LastFrameShaderResource->Release();
	if (LastFrameRenderTarget) LastFrameRenderTarget->Release();
	if (LastFrameTexture) LastFrameTexture->Release();
}

//*****************************************************************************


//-----------------------------------------------------------------------------
// Post Process Setup / Update
//-----------------------------------------------------------------------------

void PrepareGaussianKernel(float blurLevel)
{
	const double pi = 3.14159265359;
	const double e = 2.71828182846;
	const double sigma = blurLevel;
	const int blurRadius = 3 * sigma;
	KernelSize = blurRadius * 2 + 1;
	double* kernel = new double[KernelSize];
	double sum = 0.0;

	// https://en.wikipedia.org/w/index.php?title=Gaussian_blur#Mathematics
	auto gaussianFunction = [pi, sigma, e](int x)
	{
		// r = 2 * sigma ^ 2
		double r = 2.0 * sigma * sigma;
		return (1.0 / sqrt(r * pi)) * pow(e, -((x * x) / r));
	};

	// https://docs.microsoft.com/en-us/windows/desktop/api/d3d10/nf-d3d10-id3d10texture2d-map
	// https://docs.microsoft.com/en-gb/windows/desktop/direct3d10/d3d10-graphics-programming-guide-resources-creating-textures
	D3D10_MAPPED_TEXTURE2D mappedTex;
	KernelArray->Map(D3D10CalcSubresource(0, 0, 1), D3D10_MAP_WRITE_DISCARD, 0, &mappedTex);
	float* arr = (float*)mappedTex.pData;

	for (int i = 0; i < KernelSize / 2 + 1; ++i)
	{
		// First and last are the same, so are second and second to last, and so on
		// We can store the value once and only go to the middle of the array (which will be an odd number)
		double value = gaussianFunction(i - (KernelSize / 2));
		kernel[i] = value;
		kernel[(KernelSize - 1 - i)] = value;
		sum += (value * ((i < KernelSize / 2) ? 2.0 : 1.0));
	}

	// And populate the array
	for (int i = 0; i < KernelSize; ++i)
	{
		int colStart = i * 4;
		arr[i] = kernel[i] / sum;
	}

	KernelSizeVar->SetInt(KernelSize);
	KernelVar->SetResource(KernelRV);
	KernelArray->Unmap(D3D10CalcSubresource(0, 0, 1));

	// Set the viewport dimensions
	ViewportWidthVar->SetFloat(static_cast<float>(BackBufferWidth));
	ViewportHeightVar->SetFloat(static_cast<float>(BackBufferHeight));
	delete[] kernel;
}

// Set up shaders for given post-processing filter (used for full screen and area processing)
void SelectPostProcess( PostProcesses filter )
{
	switch (filter)
	{
		case Tint:
		{
			// Set the colour used to tint the scene
			D3DXCOLOR TintColour = D3DXCOLOR(1.0f,0.0f,0.0f,1.0f);
			TintColourVar->SetRawValue( &TintColour, 0, 12 );
		}
		break;

		case Gradient:
		{
			// Set the colours used to tint the scene
			TintColourVar->SetRawValue(&GradientTopColour.GetD3DXCOLOR(), 0, 12);
			TintColour2Var->SetRawValue(&GradientBotColour.GetD3DXCOLOR() , 0, 12);
		}
		break;

		case GreyNoise:
		{
			const float GrainSize = 140; // Fineness of the noise grain

			// Set shader constants - scale and offset for noise. Scaling adjusts how fine the noise is.
			CVector2 NoiseScale = CVector2( BackBufferWidth / GrainSize, BackBufferHeight / GrainSize );
			NoiseScaleVar->SetRawValue( &NoiseScale, 0, 8 );

			// The offset is randomised to give a constantly changing noise effect (like tv static)
			CVector2 RandomUVs = CVector2( Random( 0.0f,1.0f ),Random( 0.0f,1.0f ) );
			NoiseOffsetVar->SetRawValue( &RandomUVs, 0, 8 );

			// Set noise texture
			PostProcessMapVar->SetResource( NoiseMap );
		}
		break;

		case Burn:
		{
			// Set the burn level (value from 0 to 1 during animation)
			BurnLevelVar->SetFloat( BurnLevel );

			// Set burn texture
			PostProcessMapVar->SetResource( BurnMap );
		}
		break;

		case Distort:
		{
			// Set the level of distortion
			const float DistortLevel = 0.03f;
			DistortLevelVar->SetFloat( DistortLevel );

			// Set distort texture
			PostProcessMapVar->SetResource( DistortMap );
		}
		break;

		case Spiral:
		{
			// Set the amount of spiral - use a tweaked cos wave to animate
			SpiralTimerVar->SetFloat( (1.0f - Cos(SpiralTimer)) * 4.0f );
		}
		break;

		case HeatHaze:
		{
			// Set the amount of spiral - use a tweaked cos wave to animate
			HeatHazeTimerVar->SetFloat( HeatHazeTimer );
		}
		break;

		case BoxBlur:
		{
			// Set the viewport dimensions
			ViewportWidthVar->SetFloat(static_cast<float>(BackBufferWidth));
			ViewportHeightVar->SetFloat(static_cast<float>(BackBufferHeight));
		}
		break;

		case GaussianBlur:
		{
			PrepareGaussianKernel(BlurLevel);
		}
		break;

		case UnderWater:
		{
			D3DXCOLOR UnderWaterTintColour = D3DXCOLOR(0.2f, 0.5f, 1.0f, 1.0f);
			UnderWaterTintColourVar->SetRawValue(&UnderWaterTintColour, 0, 12);
			UnderWaterTimerVar->SetFloat(UnderWaterTimer);
		}
		break;
		case Negative:
		{

		}
		case Retro:
		{
			PixelSizeVar->SetFloat(PixelSize);
			NumberOfColoursVar->SetInt(NumberOfColours);
		}
		break;
		case Bloom:
		{
			PrepareGaussianKernel(15);
		}
		break;
		case DepthOfField:
		{
			PrepareGaussianKernel(15);
			FocalDistanceVar->SetFloat(FocalDistance);
			FocalRangeVar->SetFloat(FocalRange);
		}
		break;
	}
}

// Update post-processes (those that need updating) during scene update
void UpdatePostProcesses( float updateTime )
{
	// Not all post processes need updating
	BurnLevel = Mod( BurnLevel + BurnSpeed * updateTime, 1.0f );
	SpiralTimer   += SpiralSpeed * updateTime;
	HeatHazeTimer += HeatHazeSpeed * updateTime;

	GradientTopColour.ModifyHue(0.1);
	GradientBotColour.ModifyHue(0.1);
	UnderWaterTimer += UnderWaterSpeed * updateTime;
}

CVector2 CameraPixelFromWorldPoint(CCamera* cam, CVector3 worldPoint)
{
		CVector4 viewportPt = CVector4(worldPoint, 1.0f) * cam->GetViewProjMatrix();
		if (viewportPt.w < cam->GetNearClip())
		{
			return CVector2::kOrigin;
		}

		viewportPt.x /= viewportPt.w;
		viewportPt.y /= viewportPt.w;
		CVector2 camPixel;
		camPixel.x = (viewportPt.x + 1.0)* BackBufferWidth * 0.5f;
		camPixel.y = (1.0f - viewportPt.y) * BackBufferHeight * 0.5f;
		return camPixel;

}

// Sets in the shaders the top-left, bottom-right and depth coordinates of the area post process to work on
// Requires a world point at the centre of the area, the width and height of the area (in world units), an optional depth offset (to pull or push 
// the effect of the post-processing into the scene). Also requires the camera, since we are creating a camera facing quad.
void SetPostProcessArea( CCamera* camera, CVector3 areaCentre, float width, float height, float depthOffset = 0.0f, bool isFacingCamera = true)
{
	// Get the area centre in camera space.
	CVector4 cameraSpaceCentre = CVector4(areaCentre, 1.0f) * camera->GetViewMatrix();

	// Get top-left and bottom-right of camera-facing area of required dimensions 
	cameraSpaceCentre.x -= width / 2;
	cameraSpaceCentre.y += height / 2; // Careful, y axis goes up here
	CVector4 cameraTopLeft = cameraSpaceCentre;
	cameraSpaceCentre.x += width;
	cameraSpaceCentre.y -= height;
	CVector4 cameraBottomRight = cameraSpaceCentre;

	// Get the projection space coordinates of the post process area
	CVector4 projTopLeft = cameraTopLeft * camera->GetProjMatrix();
	CVector4 projBottomRight = cameraBottomRight * camera->GetProjMatrix();

	// Perform perspective divide to get coordinates in normalised viewport space (-1.0 to 1.0 from left->right and bottom->top of the viewport)
	projTopLeft.x /= projTopLeft.w;
	projTopLeft.y /= projTopLeft.w;
	projBottomRight.x /= projBottomRight.w;
	projBottomRight.y /= projBottomRight.w;

	// Also do perspective divide on z to get depth buffer value for the area. Add the required depth offset (using an approximation)
	projTopLeft.z += depthOffset;
	projTopLeft.w += depthOffset;
	projTopLeft.z /= projTopLeft.w;

	// Convert the x & y coordinates to UV space (0 -> 1, y flipped). This extra step makes the shader work simpler
	projTopLeft.x = projTopLeft.x / 2.0f + 0.5f;
	projTopLeft.y = -projTopLeft.y / 2.0f + 0.5f;
	projBottomRight.x = projBottomRight.x / 2.0f + 0.5f;
	projBottomRight.y = -projBottomRight.y / 2.0f + 0.5f;

	// Send the values calculated to the shader. The post-processing vertex shader needs only these values to
	// create the vertex buffer for the quad to render, we don't need to create a vertex buffer for post-processing at all.
	PPAreaTopLeftVar->SetRawValue(&projTopLeft.Vector2(), 0, 8);         // Viewport space x & y for top-left
	PPAreaBottomRightVar->SetRawValue(&projBottomRight.Vector2(), 0, 8); // Same for bottom-right
	PPAreaDepthVar->SetFloat(projTopLeft.z); // Depth buffer value for area

	// ***NOTE*** Most applications you will see doing post-processing would continue here to create a vertex buffer in C++, and would
	// not use the unusual vertex shader that you will see in the .fx file here. That might (or might not) give a tiny performance boost,
	// but very tiny, if any (only a handful of vertices affected). I prefer this method because I find it cleaner and more flexible overall. 
}

// Set the top-left, bottom-right and depth coordinates for the area post process to work on for full-screen processing
// Since all post process code is now area-based, full-screen processing needs to explicitly set up the appropriate full-screen rectangle
void SetFullScreenPostProcessArea()
{
	CVector2 TopLeftUV     = CVector2( 0.0f, 0.0f ); // Top-left and bottom-right in UV space
	CVector2 BottomRightUV = CVector2( 1.0f, 1.0f );

	PPAreaTopLeftVar->SetRawValue( &TopLeftUV, 0, 8 );
	PPAreaBottomRightVar->SetRawValue( &BottomRightUV, 0, 8 );
	PPAreaDepthVar->SetFloat( 0.0f ); // Full screen depth set at 0 - in front of everything
}


//-----------------------------------------------------------------------------
// Game loop functions
//-----------------------------------------------------------------------------

// Draw one frame of the scene
void RenderScene()
{
	// Setup the viewport - defines which part of the back-buffer we will render to (usually all of it)
	D3D10_VIEWPORT vp;
	vp.Width  = BackBufferWidth;
	vp.Height = BackBufferHeight;
	vp.MinDepth = 0.0f;
	vp.MaxDepth = 1.0f;
	vp.TopLeftX = 0;
	vp.TopLeftY = 0;
	g_pd3dDevice->RSSetViewports( 1, &vp );


	//------------------------------------------------
	// SCENE RENDER PASS - rendering to a texture

	// Specify that we will render to the scene texture in this first pass (rather than the backbuffer), will share the depth/stencil buffer with the backbuffer though
	g_pd3dDevice->OMSetRenderTargets( 1, &SceneRenderTarget, DepthStencilView );

	// Clear the texture and the depth buffer
	g_pd3dDevice->ClearRenderTargetView( SceneRenderTarget, &AmbientColour.r );
	g_pd3dDevice->ClearDepthStencilView( DepthStencilView, D3D10_CLEAR_DEPTH, 1.0f, 0 );

	// Prepare camera
	MainCamera->SetAspect( static_cast<TFloat32>(BackBufferWidth) / BackBufferHeight );
	MainCamera->CalculateMatrices();
	MainCamera->CalculateFrustrumPlanes();

	// Set camera and light data in shaders
	SetCamera( MainCamera );
	SetAmbientLight( AmbientColour );
	SetLights( &Lights[0] );

	// Render entities
	EntityManager.RenderAllEntities( MainCamera );

	//------------------------------------------------

	// Perform a quick copy, so that we can write to a rendered texture
	PostProcessMapVar->SetResource(SceneShaderResource);
	// Increase the index only when using the shader resources
	g_pd3dDevice->OMSetRenderTargets(1, &PostProcessingRenderTargets[PostProcessIndex], NULL);
	SelectPostProcess(Copy);
	SetFullScreenPostProcessArea(); // Define the full-screen as the area to affect
	g_pd3dDevice->IASetInputLayout(NULL);
	g_pd3dDevice->IASetPrimitiveTopology(D3D10_PRIMITIVE_TOPOLOGY_TRIANGLESTRIP);
	PPTechniques[Copy]->GetPassByIndex(0)->Apply(0);
	g_pd3dDevice->Draw(4, 0);

	//**|PPPOLY|***************************************
	// POLY POST PROCESS RENDER PASS
	// The scene has been rendered in full into a texture then copied to the back-buffer. However, the post-processed polygons were missed out. Now render the entities
	// again, but only the post-processed materials. These are rendered to the back-buffer in the correct places in the scene, but most importantly their shaders will
	// have the scene texture available to them. So these polygons can distort or affect the scene behind them (e.g. distortion through cut glass). Note that this also
	// means we can do blending (additive, multiplicative etc.) in the shader. The post-processed materials are identified with a boolean (RenderMethod.cpp). This entire
	// approach works even better with "bucket" rendering, where post-process shaders are held in a separate bucket - making it unnecessary to "RenderAllEntities" as 
	// we are doing here.

	// NOTE: Post-processing - need to set the back buffer as a render target. Relying on the fact that the section above already did that
	// Polygon post-processing occurs in the scene rendering code (RenderMethod.cpp) - so pass over the scene texture and viewport dimensions for the scene post-processing materials/shaders
	SetSceneTexture(SceneShaderResource, BackBufferWidth, BackBufferHeight);
	g_pd3dDevice->OMSetRenderTargets(1, &PostProcessingRenderTargets[PostProcessIndex], DepthStencilView);

	// Render all entities again, but flag that we only want the post-processed polygons
	EntityManager.RenderAllEntities(MainCamera, true);

	//************************************************

	//------------------------------------------------

	// AREA POST PROCESS RENDER PASS - Render smaller quad on the back-buffer mapped with a matching area of the scene texture, with different post-processing

	// NOTE: Post-processing - need to render to the back buffer and select scene texture for use in shader. Relying on the fact that the section above already did that

	// Will have post-processed area over the moving cube
	CEntity* cubey = EntityManager.GetEntity("Cubey");

	// Set the area size, 20 units wide and high, 0 depth offset. This sets up a viewport space quad for the post-process to work on
	// Note that the function needs the camera to turn the cube's point into a camera facing rectangular area
	SetPostProcessArea(MainCamera, cubey->Position(), 20, 20, -9);

	// Select one of the post-processing techniques and render the area using it
	SelectPostProcess(Spiral); // Make sure you also update the line below when you change the post-process method here!
	g_pd3dDevice->IASetInputLayout(NULL);
	g_pd3dDevice->IASetPrimitiveTopology(D3D10_PRIMITIVE_TOPOLOGY_TRIANGLESTRIP);
	PPTechniques[Spiral]->GetPassByIndex(0)->Apply(0);
	g_pd3dDevice->Draw(4, 0);

	// Wall windows shader
	/*CEntity* wallWindow = EntityManager.GetEntity("Wall");
	// Need to create a non-camera-facing quad for this
	CVector3 pos = wallWindow->Position();
	pos.y = 12.5f;
	SetPostProcessArea(MainCamera, pos, 12.5f, 12.5f, 2, false);

	SelectPostProcess(Negative);
	g_pd3dDevice->IASetInputLayout(NULL);
	g_pd3dDevice->IASetPrimitiveTopology(D3D10_PRIMITIVE_TOPOLOGY_TRIANGLESTRIP);
	PPTechniques[Negative]->GetPassByIndex(0)->Apply(0);
	g_pd3dDevice->Draw(4, 0);*/

	//------------------------------------------------
	// FULL SCREEN POST PROCESS RENDER PASS - Render full screen quad on the back-buffer mapped with the scene texture, with post-processing

	// Repeat the following process for each Post-Process effect in the array
	for (int i = 0, size = FullScreenFilters.size(); i < size; ++i)
	{

		if (FullScreenFilters[i] == MotionBlur)
		{
			if (IsCameraMoving)
			{
				LastFrameMapVar->SetResource(LastFrameShaderResource);
				g_pd3dDevice->OMSetRenderTargets(1, &PostProcessingRenderTargets[PostProcessIndex], NULL);
				g_pd3dDevice->IASetInputLayout(NULL);
				g_pd3dDevice->IASetPrimitiveTopology(D3D10_PRIMITIVE_TOPOLOGY_TRIANGLESTRIP);
				PPTechniques[FullScreenFilters[i]]->GetPassByIndex(0)->Apply(0);
				g_pd3dDevice->Draw(4, 0);
			}
			continue;
		}
		if (FullScreenFilters[i] == DepthOfField || FullScreenFilters[i] == Bloom)
		{
			if (FullScreenFilters[i] == Bloom)
			{
				// Do a bright filter pass using the last rendered image
				PostProcessMapVar->SetResource(PostProcessShaderResources[PostProcessIndex]);
				g_pd3dDevice->OMSetRenderTargets(1, &BrightRenderTarget, NULL);
				g_pd3dDevice->IASetInputLayout(NULL);
				g_pd3dDevice->IASetPrimitiveTopology(D3D10_PRIMITIVE_TOPOLOGY_TRIANGLESTRIP);
				PPTechniques[Bloom]->GetPassByName("BrightFilter")->Apply(0);
				g_pd3dDevice->Draw(4, 0);

				PostProcessMapVar->SetResource(BrightShaderResource);
			}
			else if (FullScreenFilters[i] == DepthOfField)
			{
				PostProcessMapVar->SetResource(PostProcessShaderResources[PostProcessIndex]);
			}

			// Perform a prepass here, where we do a Gaussian blur		
			// Render to an intermediate texture, since we don't want to mess up the double-buffering order
			g_pd3dDevice->OMSetRenderTargets(1, &IntermediateRenderTarget, NULL);
			g_pd3dDevice->IASetInputLayout(NULL);
			g_pd3dDevice->IASetPrimitiveTopology(D3D10_PRIMITIVE_TOPOLOGY_TRIANGLESTRIP);
			PPTechniques[GaussianBlur]->GetPassByName("Horizontal")->Apply(0);
			g_pd3dDevice->Draw(4, 0);

			// Do the second pass 
			PostProcessMapVar->SetResource(IntermediateShaderResource);
			g_pd3dDevice->OMSetRenderTargets(1, &BlurredRenderTarget, NULL);
			g_pd3dDevice->IASetInputLayout(NULL);
			g_pd3dDevice->IASetPrimitiveTopology(D3D10_PRIMITIVE_TOPOLOGY_TRIANGLESTRIP);
			PPTechniques[GaussianBlur]->GetPassByName("Vertical")->Apply(0);
			g_pd3dDevice->Draw(4, 0);

			// After drawing, store the blurred map
			BlurredMapVar->SetResource(BlurredShaderResource);
		}

		// Do not clear the depth buffer, since we're reading from there
		//g_pd3dDevice->ClearDepthStencilView(DepthStencilView, D3D10_CLEAR_DEPTH, 1.0f, 0);
		PostProcessMapVar->SetResource(PostProcessShaderResources[PostProcessIndex]);
		// Increase the index only when using the shader resources
		PostProcessIndex = (PostProcessIndex + 1) % 2;

		// Select render target (will ignore depth-buffer for full-screen quad) and select scene texture for use in shader
		// Additionally, pass NULL as a depth stencil, because we want to use it as a shader resource
		g_pd3dDevice->OMSetRenderTargets(1, &PostProcessingRenderTargets[PostProcessIndex], NULL);
		DepthMapVar->SetResource(DepthShaderView);
		NearClipVar->SetFloat(MainCamera->GetNearClip());
		FarClipVar->SetFloat(MainCamera->GetFarClip());
		if (UsingMousePos)
		{
			if (MousePixel.Length() > 0)
			{
				auto mousePos = CVector3(MousePixel.x / BackBufferWidth, MousePixel.y / BackBufferHeight, UsingMousePos ? 1 : 0);
				MousePosVar->SetRawValue(&mousePos, 0, 12);
			}
			UsingMousePos = false;
		}


		// Prepare shader settings for the current full screen filter
		SelectPostProcess(FullScreenFilters[i]);
		SetFullScreenPostProcessArea(); // Define the full-screen as the area to affect

		// Using special vertex shader than creates its own data for a full screen quad (see .fx file). No need to set vertex/index buffer, just draw 4 vertices of quad
		// Select technique to match currently selected post-process
		g_pd3dDevice->IASetInputLayout(NULL);
		g_pd3dDevice->IASetPrimitiveTopology(D3D10_PRIMITIVE_TOPOLOGY_TRIANGLESTRIP);
		PPTechniques[FullScreenFilters[i]]->GetPassByIndex(0)->Apply(0);
		g_pd3dDevice->Draw(4, 0);

		if (FullScreenFilters[i] == GaussianBlur)
		{
			// Do the second pass here
			PostProcessMapVar->SetResource(PostProcessShaderResources[PostProcessIndex]);
			// Increase the index only when using the shader resources
			PostProcessIndex = (PostProcessIndex + 1) % 2;
			g_pd3dDevice->OMSetRenderTargets(1, &PostProcessingRenderTargets[PostProcessIndex], NULL);
			g_pd3dDevice->IASetInputLayout(NULL);
			g_pd3dDevice->IASetPrimitiveTopology(D3D10_PRIMITIVE_TOPOLOGY_TRIANGLESTRIP);
			PPTechniques[GaussianBlur]->GetPassByName("Vertical")->Apply(0);
			g_pd3dDevice->Draw(4, 0);
		}
	}

	// Now rerender everything to the Backbuffer
	g_pd3dDevice->OMSetRenderTargets(1, &BackBufferRenderTarget, NULL);
	PostProcessMapVar->SetResource(PostProcessShaderResources[PostProcessIndex]);
	SelectPostProcess(Copy);
	SetFullScreenPostProcessArea();
	g_pd3dDevice->IASetInputLayout(NULL);
	g_pd3dDevice->IASetPrimitiveTopology(D3D10_PRIMITIVE_TOPOLOGY_TRIANGLESTRIP);
	PPTechniques[Copy]->GetPassByIndex(0)->Apply(0);
	g_pd3dDevice->Draw(4, 0);

	g_pd3dDevice->OMSetRenderTargets(1, &LastFrameRenderTarget, NULL);
	PostProcessMapVar->SetResource(PostProcessShaderResources[PostProcessIndex]);
	SelectPostProcess(Copy);
	SetFullScreenPostProcessArea();
	g_pd3dDevice->IASetInputLayout(NULL);
	g_pd3dDevice->IASetPrimitiveTopology(D3D10_PRIMITIVE_TOPOLOGY_TRIANGLESTRIP);
	PPTechniques[Copy]->GetPassByIndex(0)->Apply(0);
	g_pd3dDevice->Draw(4, 0);
	//------------------------------------------------

	// These two lines unbind the scene texture from the shader to stop DirectX issuing a warning when we try to render to it again next frame
	SceneTextureVar->SetResource( 0 );
	PPTechniques[FullScreenFilters[0]]->GetPassByIndex(0)->Apply(0);

	// Render UI elements last - don't want them post-processed
	//RenderSceneText();

	// Present the backbuffer contents to the display
	SwapChain->Present( 0, 0 );
}


// Render a single text string at the given position in the given colour, may optionally centre it
void RenderText( const string& text, int X, int Y, float r, float g, float b, bool centre = false )
{
	RECT rect;
	if (!centre)
	{
		SetRect( &rect, X, Y, 0, 0 );
		OSDFont->DrawText( NULL, text.c_str(), -1, &rect, DT_NOCLIP, D3DXCOLOR( r, g, b, 1.0f ) );
	}
	else
	{
		SetRect( &rect, X - 100, Y, X + 100, 0 );
		OSDFont->DrawText( NULL, text.c_str(), -1, &rect, DT_CENTER | DT_NOCLIP, D3DXCOLOR( r, g, b, 1.0f ) );
	}
}

// Render on-screen text each frame
void RenderSceneText()
{
	// Write FPS text string
	stringstream outText;
	if (AverageUpdateTime >= 0.0f)
	{
		outText << "Frame Time: " << AverageUpdateTime * 1000.0f << "ms" << endl << "FPS:" << 1.0f / AverageUpdateTime;
		RenderText( outText.str(), 2, 2, 0.0f, 0.0f, 0.0f );
		RenderText( outText.str(), 0, 0, 1.0f, 1.0f, 0.0f );
		outText.str("");
	}

	// Output post-process name
	outText << "Fullscreen Post-Process: ";
	switch (FullScreenFilters[0])
	{
	case Copy: 
		outText << "Copy";
		break;
	case Tint: 
		outText << "Tint";
		break;
	case Gradient:
		outText << "Gradient";
		break;
	case GreyNoise: 
		outText << "Grey Noise";
		break;
	case Burn: 
		outText << "Burn";
		break;
	case Distort: 
		outText << "Distort";
		break;
	case Spiral: 
		outText << "Spiral";
		break;
	case HeatHaze: 
		outText << "Heat Haze";
		break;
	case BoxBlur:
		outText << "Blur";
		break;
	case UnderWater:
		outText << "UnderWater";
		break;
	}
	RenderText( outText.str(),  0, 32,  1.0f, 1.0f, 1.0f );
}


// Update the scene between rendering
void UpdateScene( float updateTime )
{
	// Reset bool
	IsCameraMoving = false;

	// Call all entity update functions
	EntityManager.UpdateAllEntities( updateTime );

	// Update any post processes that need updates
	UpdatePostProcesses( updateTime );

	// Set camera speeds
	// Key F1 used for full screen toggle
	if (KeyHit( Key_F2 )) CameraMoveSpeed = 5.0f;
	if (KeyHit( Key_F3 )) CameraMoveSpeed = 40.0f;
	if (KeyHit( Key_F4 )) CameraMoveSpeed = 160.0f;
	if (KeyHit( Key_F5 )) CameraMoveSpeed = 640.0f;

	// Choose post-process
	if (KeyHit(Key_Back))
	{
		if (FullScreenFilters.size() > 1)
		{
			// Only delete last filter if there's at least two
			FullScreenFilters.pop_back();
		}
	}

	if (KeyHit(Key_1))
	{
		FullScreenFilters.push_back(Gradient);
	}
	if (KeyHit(Key_2))
	{
		FullScreenFilters.push_back(GaussianBlur);
	}
	if (KeyHit(Key_3))
	{
		FullScreenFilters.push_back(UnderWater);
	}
	if (KeyHit(Key_4))
	{
		FullScreenFilters.push_back(DepthOfField);
	}
	if (KeyHit(Key_5))
	{
		FullScreenFilters.push_back(Retro);
	}
	if (KeyHit(Key_6))
	{
		FullScreenFilters.push_back(Bloom);
	}
	if (KeyHit(Key_7))
	{
		FullScreenFilters.push_back(Negative);
	}
	if (KeyHit(Key_8))
	{
		FullScreenFilters.push_back(MotionBlur);
	}

	if (KeyHeld(Key_Numpad8))
	{
		// Increase radius
		BlurLevel += BlurStep;
	}
	if (KeyHeld(Key_Numpad2))
	{
		// Decrease radius
		if (BlurLevel > BlurStep)
		{
			BlurLevel -= BlurStep;
		}
	}

	if (KeyHeld(Key_Numpad9))
	{
		FocalRange += FocalStep;
	}
	if (KeyHeld(Key_Numpad7))
	{
		if (FocalRange > FocalStep) FocalRange -= FocalStep;
	}

	if (KeyHeld(Key_Numpad3))
	{
		if (FocalDistance < 1) FocalDistance += FocalStep;
	}
	if (KeyHeld(Key_Numpad1))
	{
		if (FocalDistance > FocalStep) FocalDistance -= FocalStep;
	}

	if (KeyHeld(Key_Add))
	{
		if (PixelSize < 1) PixelSize += 0.0001;
	}
	if (KeyHeld(Key_Subtract))
	{
		if (PixelSize > 0) PixelSize -= 0.0001;
	}

	if (KeyHit(Key_Prior))
	{
		if (NumberOfColours < 256) NumberOfColours++;
	}
	if (KeyHit(Key_Next))
	{
		if (NumberOfColours > 1) NumberOfColours--;
	}

	if (KeyHeld(Mouse_LButton))
	{
		// std::find(FullScreenFilters.begin(), FullScreenFilters.end(), DepthOfField) != FullScreenFilters.end())

		// Set the distance to the depth of the pixel under the mouse
		UsingMousePos = true;
	}

	// Rotate cube and attach light to it
	CEntity* cubey = EntityManager.GetEntity( "Cubey" );
	cubey->Matrix().RotateX( ToRadians(53.0f) * updateTime );
	cubey->Matrix().RotateZ( ToRadians(42.0f) * updateTime );
	cubey->Matrix().RotateWorldY( ToRadians(12.0f) * updateTime );
	Lights[1]->SetPosition( cubey->Position() );
	
	// Rotate polygon post-processed entity
	CEntity* ppEntity = EntityManager.GetEntity( "PostProcessBlock" );
	ppEntity->Matrix().RotateY( ToRadians(30.0f) * updateTime );

	// Move the camera
	MainCamera->Control( Key_Up, Key_Down, Key_Left, Key_Right, Key_W, Key_S, Key_A, Key_D, 
	                     CameraMoveSpeed * updateTime, CameraRotSpeed * updateTime );

	// Accumulate update times to calculate the average over a given period
	SumUpdateTimes += updateTime;
	++NumUpdateTimes;
	if (SumUpdateTimes >= UpdateTimePeriod)
	{
		AverageUpdateTime = SumUpdateTimes / NumUpdateTimes;
		SumUpdateTimes = 0.0f;
		NumUpdateTimes = 0;
	}
}


} // namespace gen
