//--------------------------------------------------------------------------------------
//	File: PostProcess.fx
//
//	Post processing shaders
//--------------------------------------------------------------------------------------

//--------------------------------------------------------------------------------------
// Global Variables
//--------------------------------------------------------------------------------------

// Viewport Dimensions - needed for converting pixel coordinates (0->Width, 0->Height) to UV coordinates (0->1) - used in polygon post-processing
float ViewportWidth;
float ViewportHeight;

// Post Process Area - Dimensions
float2 PPAreaTopLeft;     // Top-left and bottom-right coordinates of area to post process, provided as UVs into the scene texture...
float2 PPAreaBottomRight; // ... i.e. the X and Y coordinates range from 0.0 to 1.0 from left->right and top->bottom of viewport
float  PPAreaDepth;       // Depth buffer value for area (0.0 nearest to 1.0 furthest). Full screen post-processing uses 0.0f

// Other variables used for individual post-processes
float3 TintColour;
float3 TintColour2;
float3 UnderWaterTintColour;
float2 NoiseScale;
float2 NoiseOffset;
float  DistortLevel;
float  BurnLevel;
float  SpiralTimer;
float  HeatHazeTimer;
float UnderWaterTimer;
Texture2D<float> Kernel;
float KernelSize;
float PixelSize;
float NumberOfColours;
float3 MousePos;
float FocalDistance;
float FocalRange;
float NearClip;
float FarClip;
float VignetteSize = 0.0001;

// Texture maps
Texture2D SceneTexture;     // Texture containing the scene to copy to the full screen quad
Texture2D PostProcessMap;   // Second map for special purpose textures used during post-processing
Texture2D DepthMap;         // Depth buffer, used for depth of field calculations
Texture2D BlurredMap;       // Map where the gaussian blur for bloom and depth of field is sent to
Texture2D LastFrameMap;     // Map where last frame is stored
Texture2D BrightTexture;    // PostProcessMap through a bright pass filter

// Samplers to use with the above texture maps. Specifies texture filtering and addressing mode to use when accessing texture pixels
// Usually use point sampling for the scene texture (i.e. no bilinear/trilinear blending) since don't want to blur it in the copy process
SamplerState PointClamp
{
    Filter = MIN_MAG_MIP_POINT;
    AddressU = Clamp;
    AddressV = Clamp;
	MaxLOD = 0.0f;
};

// See comment above. However, screen distortions may benefit slightly from bilinear filtering (not tri-linear because we won't create mip-maps for the scene each frame)
SamplerState BilinearClamp
{
    Filter = MIN_MAG_LINEAR_MIP_POINT;
    AddressU = Clamp;
    AddressV = Clamp;
	MaxLOD = 0.0f;
};

// Use other filtering methods for the special purpose post-processing textures (e.g. the noise map)
SamplerState BilinearWrap
{
    Filter = MIN_MAG_LINEAR_MIP_POINT;
    AddressU = Wrap;
    AddressV = Wrap;
};
SamplerState TrilinearWrap
{
    Filter = MIN_MAG_MIP_LINEAR;
    AddressU = Wrap;
    AddressV = Wrap;
};


//--------------------------------------------------------------------------------------
// Structures
//--------------------------------------------------------------------------------------

// The full-screen and area post-processing vertex shader input uses a special input type, the vertex ID. This value is automatically generated and does
// not come from a vertex buffer. The value starts at 0 and increases by one with each vertex processed.
struct VS_POSTPROCESS_INPUT
{
    uint vertexId : SV_VertexID;
};

// Vertex shader output / pixel shader input for the post processing shaders
// Provides the viewport positions of the quad to be post processed, then *two* UVs. The Scene UVs indicate which part of the 
// scene texture is being post-processed. The Area UVs range from 0->1 within the area only - these UVs can be used to apply a
// second texture to the area itself, or to find the location of a pixel within the area affected (the Scene UVs could be
// used together with the dimensions variables above to calculate this 2nd set of UVs, but this way saves pixel shader work)
struct PS_POSTPROCESS_INPUT
{
    float4 ProjPos : SV_POSITION;
	float2 UVScene : TEXCOORD0;
	float2 UVArea  : TEXCOORD1;
};

struct PS_POSTPROCESS_OUTPUT
{
    float4 Target1 : SV_Target0;
    float4 Target2 : SV_Target1;
};



//--------------------------------------------------------------------------------------
// Vertex Shaders
//--------------------------------------------------------------------------------------

// Post Process Full Screen and Area - Generate Vertices
//
// This rather unusual shader generates its own vertices - the input data is merely the vertex ID - an automatically generated increasing index.
// No vertex or index buffer required, so convenient on the C++ side. Probably not so efficient, but fine for just a few post-processing quads
PS_POSTPROCESS_INPUT PPQuad(VS_POSTPROCESS_INPUT vIn)
{
    PS_POSTPROCESS_INPUT vOut;
	
	// The four points of a full-screen quad - will use post process area dimensions (provided above) to scale these to the correct quad needed
	float2 Quad[4] =  { float2(0.0, 0.0),   // Top-left
	                    float2(1.0, 0.0),   // Top-right
	                    float2(0.0, 1.0),   // Bottom-left
	                    float2(1.0, 1.0) }; // Bottom-right

	// vOut.UVArea contains UVs for the area itself: (0,0) at top-left of area, (1,1) at bottom right. Simply the values stored in the Quad array above.
	vOut.UVArea = Quad[vIn.vertexId]; 

	// vOut.UVScene contains UVs for the section of the scene texture to use. The top-left and bottom-right coordinates are provided in the PPAreaTopLeft and
	// PPAreaBottomRight variables one pages above, use lerp to convert the Quad values above into appopriate coordinates (see AreaPostProcessing lab for detail)
	vOut.UVScene = lerp( PPAreaTopLeft, PPAreaBottomRight, vOut.UVArea ); 
	             
	// vOut.ProjPos contains the vertex positions of the quad to render, measured in viewport space here. The x and y are same as Scene UV coords but in range -1 to 1 (and flip y axis),
	// the z value takes the depth value provided for the area (PPAreaDepth) and a w component of 1 to prevent the perspective divide (already did that in the C++)
    vOut.ProjPos = float4(vOut.UVScene * 2.0f - 1.0f, PPAreaDepth, 1.0f);
	vOut.ProjPos.y = -vOut.ProjPos.y;

    return vOut;
}


//--------------------------------------------------------------------------------------
// Post-processing Pixel Shaders
//--------------------------------------------------------------------------------------

// Post-processing shader that simply outputs the scene texture, i.e. no post-processing. A waste of processing, but illustrative
PS_POSTPROCESS_OUTPUT PPCopyShader( PS_POSTPROCESS_INPUT ppIn ) : SV_Target
{
    PS_POSTPROCESS_OUTPUT ppOut;
	float3 ppColour = PostProcessMap.Sample( PointClamp, ppIn.UVScene ).rgb;

    ppOut.Target1 = float4(ppColour, 1.0f);
    ppOut.Target2 = float4(ppColour, 1.0f);

    return ppOut;
}


// Post-processing shader that tints the scene texture to a given colour
float4 PPTintShader( PS_POSTPROCESS_INPUT ppIn ) : SV_Target
{
	// Sample the texture colour (look at shader above) and multiply it with the tint colour (variables near top)
    float3 ppColour = (float3)SceneTexture.Sample(PointClamp, ppIn.UVScene) * TintColour;
	return float4( ppColour, 1.0f );
}

// Post-processing shader that tints the scene texture to a gradient
float4 PPGradientShader(PS_POSTPROCESS_INPUT ppIn) : SV_Target
{
	float3 heightColour = lerp(TintColour, TintColour2, ppIn.UVScene.y);
    float3 ppColour = (float3)PostProcessMap.Sample(PointClamp, ppIn.UVScene) * heightColour;
	return float4(ppColour, 1.0f);
}


// Post-processing shader that tints the scene texture to a given colour
float4 PPGreyNoiseShader( PS_POSTPROCESS_INPUT ppIn ) : SV_Target
{
	const float NoiseStrength = 0.5f; // How noticable the noise is

	// Get texture colour, and average r, g & b to get a single grey value
    float3 texColour = SceneTexture.Sample( PointClamp, ppIn.UVScene ).rgb;
    float grey = (texColour.r + texColour.g + texColour.b) / 3.0f;
    
    // Get noise UV by scaling and offseting texture UV. Scaling adjusts how fine the noise is.
    // The offset is randomised to give a constantly changing noise effect (like tv static)
    float2 noiseUV = ppIn.UVArea * NoiseScale + NoiseOffset;
    grey += NoiseStrength * (PostProcessMap.Sample( BilinearWrap, noiseUV ).r - 0.5f); // Noise can increase or decrease grey value
    float3 ppColour = grey;

	// Calculate alpha to display the effect in a softened circle, could use a texture rather than calculations for the same task.
	// Uses the second set of area texture coordinates, which range from (0,0) to (1,1) over the area being processed
	float softEdge = 0.05f; // Softness of the edge of the circle - range 0.001 (hard edge) to 0.25 (very soft)
	float2 centreVector = ppIn.UVArea - float2(0.5f, 0.5f);
	float centreLengthSq = dot(centreVector, centreVector);
	float ppAlpha = 1.0f - saturate( (centreLengthSq - 0.25f + softEdge) / softEdge ); // Soft circle calculation based on fact that this circle has a radius of 0.5 (as area UVs go from 0->1)

    // Output final colour
	return float4( ppColour, ppAlpha );
}


// Post-processing shader that "burns" the image away
float4 PPBurnShader( PS_POSTPROCESS_INPUT ppIn ) : SV_Target
{
	const float4 White = 1.0f;
	
	// Pixels are burnt with these colours at the edges
	const float4 BurnColour = float4(0.8f, 0.4f, 0.0f, 1.0f);
	const float4 GlowColour = float4(1.0f, 0.8f, 0.0f, 1.0f);
	const float GlowAmount = 0.15f; // Thickness of glowing area
	const float Crinkle = 0.1f; // Amount of texture crinkle at the edges 

	// Get burn texture colour
    float4 burnTexture = PostProcessMap.Sample( TrilinearWrap, ppIn.UVArea );
    
    // The range of burning colours are from BurnLevel  to BurnLevelMax
	float BurnLevelMax = BurnLevel + GlowAmount; 

    // Output black when current burn texture value below burning range
    if (burnTexture.r <= BurnLevel)
    {
		return float4( 0.0f, 0.0f, 0.0f, 1.0f );
	}
    
    // Output scene texture untouched when current burnTexture texture value above burning range
	else if (burnTexture.r >= BurnLevelMax)
    {
		float3 ppColour = SceneTexture.Sample( PointClamp, ppIn.UVScene ).rgb;
		return float4( ppColour, 1.0f );
	}
	
	else // Draw burning edges
	{
		float3 ppColour;

		// Get level of glow (0 = none, 1 = max)
		float GlowLevel = 1.0f - (burnTexture.r - BurnLevel) / GlowAmount;

		// Extract direction to crinkle (2D vector) from the g & b components of the burn texture sampled above (converting from 0->1 range to -0.5->0.5 range)
		float2 CrinkleVector = burnTexture.rg - float2(0.5f, 0.5f);
		
		// Get main texture colour using crinkle offset
	    float4 texColour =  SceneTexture.Sample( PointClamp, ppIn.UVScene - GlowLevel * Crinkle * CrinkleVector );

		// Split glow into two regions - the very edge and the inner section
		GlowLevel *= 2.0f;
		if (GlowLevel < 1.0f)
		{		
			// Blend from main texture colour on inside to burn tint in middle of burning area
			ppColour = lerp( texColour, BurnColour * texColour, GlowLevel ).rgb;
		}
		else
		{
			// Blend from burn tint in middle of burning area to bright glow at the burning edges
			ppColour = lerp( BurnColour * texColour, GlowColour, GlowLevel - 1.0f ).rgb;
		}
		return float4( ppColour, 1.0f );
	}
}


// Post-processing shader that distorts the scene as though viewed through cut glass
float4 PPDistortShader( PS_POSTPROCESS_INPUT ppIn ) : SV_Target
{
	const float LightStrength = 0.025f;
	
	// Get distort texture colour
    float4 distortTexture = PostProcessMap.Sample( TrilinearWrap, ppIn.UVArea );

	// Get direction (2D vector) to distort UVs from the g & b components of the distort texture (converting from 0->1 range to -0.5->0.5 range)
	float2 DistortVector = distortTexture.rg - float2(0.5f, 0.5f);
			
	// Simple fake diffuse lighting formula based on 2D vector, light coming from top-left
	float light = dot( normalize(DistortVector), float2(0.707f, 0.707f) ) * LightStrength;
	
	// Get final colour by adding fake light colour plus scene texture sampled with distort texture offset
	float3 ppColour = light + SceneTexture.Sample( BilinearClamp, ppIn.UVScene + DistortLevel * DistortVector ).rgb;

    return float4( ppColour, 1.0f );
}


// Post-processing shader that spins the area in a vortex
float4 PPSpiralShader( PS_POSTPROCESS_INPUT ppIn ) : SV_Target
{
	// Get vector from UV at centre of post-processing area to UV at pixel
	const float2 centreUV = (PPAreaBottomRight.xy + PPAreaTopLeft.xy) / 2.0f;
	float2 centreOffsetUV = ppIn.UVScene - centreUV;
	float centreDistance = length( centreOffsetUV ); // Distance of pixel from UV (i.e. screen) centre
	
	// Get sin and cos of spiral amount, increasing with distance from centre
	float s, c;
	sincos( centreDistance * SpiralTimer * SpiralTimer, s, c );
	
	// Create a (2D) rotation matrix and apply to the vector - i.e. rotate the
	// vector around the centre by the spiral amount
	matrix<float,2,2> rot2D = { c, s,
	                           -s, c };
	float2 rotOffsetUV = mul( centreOffsetUV, rot2D );

	// Sample texture at new position (centre UV + rotated UV offset)
    float3 ppColour = PostProcessMap.Sample( BilinearClamp, centreUV + rotOffsetUV ).rgb;

	// Calculate alpha to display the effect in a softened circle, could use a texture rather than calculations for the same task.
	// Uses the second set of area texture coordinates, which range from (0,0) to (1,1) over the area being processed
	const float softEdge = 0.05f; // Softness of the edge of the circle - range 0.001 (hard edge) to 0.25 (very soft)
	float2 centreVector = ppIn.UVArea - float2(0.5f, 0.5f);
	float centreLengthSq = dot(centreVector, centreVector);
	float ppAlpha = 1.0f - saturate( (centreLengthSq - 0.25f + softEdge) / softEdge ); // Soft circle calculation based on fact that this circle has a radius of 0.5 (as area UVs go from 0->1)

    return float4( ppColour, ppAlpha );
}


// Post-processing shader that gives a semi-transparent wiggling heat haze effect
float4 PPHeatHazeShader( PS_POSTPROCESS_INPUT ppIn ) : SV_Target
{
	const float EffectStrength = 0.02f;
	
	// Calculate alpha to display the effect in a softened circle, could use a texture rather than calculations for the same task.
	// Uses the second set of area texture coordinates, which range from (0,0) to (1,1) over the area being processed
	const float softEdge = 0.15f; // Softness of the edge of the circle - range 0.001 (hard edge) to 0.25 (very soft)
	float2 centreVector = ppIn.UVArea - float2(0.5f, 0.5f);
	float centreLengthSq = dot(centreVector, centreVector);
	float ppAlpha = 1.0f - saturate( (centreLengthSq - 0.25f + softEdge) / softEdge ); // Soft circle calculation based on fact that this circle has a radius of 0.5 (as area UVs go from 0->1)

	// Haze is a combination of sine waves in x and y dimensions
	float SinX = sin(ppIn.UVArea.x * radians(1440.0f) + HeatHazeTimer);
	float SinY = sin(ppIn.UVArea.y * radians(3600.0f) + HeatHazeTimer * 0.7f);
	
	// Offset for scene texture UV based on haze effect
	// Adjust size of UV offset based on the constant EffectStrength, the overall size of area being processed, and the alpha value calculated above
	float2 hazeOffset = float2(SinY, SinX) * EffectStrength * ppAlpha * (PPAreaBottomRight.xy - PPAreaTopLeft.xy);

	// Get pixel from scene texture, offset using haze
    float3 ppColour = SceneTexture.Sample( BilinearClamp, ppIn.UVScene + hazeOffset ).rgb;

	// Adjust alpha on a sine wave - better to have it nearer to 1.0 (but don't allow it to exceed 1.0)
    ppAlpha *= saturate(SinX * SinY * 0.33f + 0.55f);

	return float4( ppColour, ppAlpha );
}

// Post-processing shader that applies a box blur
float4 PPBoxBlurShader(PS_POSTPROCESS_INPUT ppIn) : SV_Target
{
    float3 ppColour = float3(0, 0, 0);
    // Average the value of all neighbouring pixels
    float x, y;

    float BoxBlurSize = 20;

    for (uint i = 0; i < BoxBlurSize; ++i)
    {
        x = ppIn.UVScene.x + ((i - BoxBlurSize / 2) / ViewportWidth) + (1 / ViewportWidth);
        for (uint j = 0; j < BoxBlurSize; ++j)
        {
            y = ppIn.UVScene.y + ((j - BoxBlurSize / 2) / ViewportHeight) + (1 / ViewportHeight);
            ppColour += PostProcessMap.Sample(PointClamp, float2(x, y)).rgb;
        }
    }

    ppColour /= BoxBlurSize * BoxBlurSize;

    return float4(ppColour, 1.0);   
}

// Post-processing shader that applies a gaussian blur in X
float4 PPGaussianBlurHorizontalShader(PS_POSTPROCESS_INPUT ppIn) : SV_Target
{
    float3 ppColour = float3(0, 0, 0);
	// Needs to add the weighted value of all neighbouring pixels in X
    float x;
    float y = ppIn.UVScene.y;
    for (uint i = 0; i < KernelSize; i++)
	{    
        // Add half a pixel compensation
        x = ppIn.UVScene.x + ((i - KernelSize / 2) / ViewportWidth) + (0.5f / ViewportWidth);
        ppColour += PostProcessMap.Sample(PointClamp, float2(x, y)).rgb * Kernel[float2(i, 0)];
    }
	
    return float4(ppColour, 1.0);
}

// Post-processing shader that applies a gaussian blur in Y
float4 PPGaussianBlurVerticalShader(PS_POSTPROCESS_INPUT ppIn) : SV_Target
{
    float3 ppColour = float3(0, 0, 0);
	// Needs to add the weighted value of all neighbouring pixels in Y
    float x = ppIn.UVScene.x;
    float y;
    for (uint i = 0; i < KernelSize; i++)
    {
        // Add half a pixel compensation
        y = ppIn.UVScene.y + ((i - KernelSize / 2) / ViewportHeight) + (0.5f / ViewportHeight);
        ppColour += PostProcessMap.Sample(PointClamp, float2(x, y)).rgb * Kernel[float2(i, 0)];
    }
	
    return float4(ppColour, 1.0);
}

// Post-processing shader that gives the scene an underwater feeling
float4 PPUnderWaterShader(PS_POSTPROCESS_INPUT ppIn) : SV_Target
{
    float2 offset = float2(0, sin(ppIn.UVArea.x * radians(200.0f) + UnderWaterTimer) / 50);
    float2 coords = ppIn.UVScene + offset;
    float3 ppColour = (float3) PostProcessMap.Sample(PointClamp, coords) * UnderWaterTintColour;
    // Wanted to use depth to affect how much each wavelength travelled, but didn't have time to
    //float pixelDepth = DepthMap.Sample(PointClamp, coords);

    // Light of different wavelenghts gets absorbed at different rates depending on distance
    // Red:     15m
    // Green:   75m
    // Blue:    300m
    //float3 colourExtintion = float3(15, 75, 300);

    //float3 distance = saturate(pixelDepth / colourExtintion);
    //ppColour = lerp(ppColour, normalize(colourExtintion), distance);

    return float4(ppColour, 1.0f);
}

// Post-processing shader that applies a negative on top of the image
float4 PPNegativeShader(PS_POSTPROCESS_INPUT ppIn) : SV_Target
{
    float3 ppColour = PostProcessMap.Sample(PointClamp, ppIn.UVScene).rgb;
    ppColour.rgb = 1.0f - ppColour.rgb;
    return float4(ppColour, 1.0f);
}

// Post-processing shader that makes each pixel inside a "big pixel" take the same colour
float4 PPRetroShader(PS_POSTPROCESS_INPUT ppIn) : SV_Target
{
    // All of the pixels inside a "big pixel" will take the colour of the first pixel
    // of that "big pixel"
    float2 pixelatedUV = ppIn.UVScene.xy / PixelSize;
    pixelatedUV = round(pixelatedUV);
    pixelatedUV *= PixelSize;

    float3 ppColour = PostProcessMap.Sample(PointClamp, pixelatedUV).rgb;

    // Force the color into one of the available ones
    ppColour = floor(ppColour * NumberOfColours) / NumberOfColours;

    return float4(ppColour, 1.0f);
}

// Post-processing shader that blooms the image
float4 PPBloomShader(PS_POSTPROCESS_INPUT ppIn) : SV_Target
{
    // Get the unblurred pixel colour
    float3 ppColour = PostProcessMap.Sample(PointClamp, ppIn.UVScene).rgb;

    // Get the blurred pixel colour
    // Multiply it so that it is obvious
    float3 blurColour = BlurredMap.Sample(PointClamp, ppIn.UVScene).rgb * 5;

    // DEBUG
    //ppColour = float3(0, 0, 0);

    return float4(ppColour + blurColour, 1.0f);
}

// Intermediate shader to brighten an image
float4 PPBrightFilterShader(PS_POSTPROCESS_INPUT ppIn) : SV_Target
{
    static const float luminance = 0.08f;
    static const float middleGrey = 0.18f;
    static const float whiteCutoff = 0.8f;

    float3 ppColour = PostProcessMap.Sample(PointClamp, ppIn.UVScene).rgb;

    ppColour *= middleGrey / (luminance + 0.001);
    ppColour *= (1.0f + (ppColour / (whiteCutoff * whiteCutoff)));
    ppColour -= 5.0f;

    ppColour = max(ppColour, 0.0f);
    ppColour /= (10.0f + ppColour);
    return float4(ppColour, 1.0f);
}

// Post-processing shader that applies a depth of field
float4 PPDOFShader(PS_POSTPROCESS_INPUT ppIn) : SV_Target
{
    // Get pixel depth
    float pixelDepth = DepthMap.Sample(PointClamp, ppIn.UVScene).r;
    float focalDistance = FocalDistance;
    if (MousePos.z >= 0.5)
    {
        focalDistance = DepthMap.Sample(PointClamp, MousePos.xy).r;
    }

    float distanceFromFocus = saturate(abs(pixelDepth - focalDistance) / FocalRange);

    // Get the unblurred pixel colour
    float3 ppColour = PostProcessMap.Sample(PointClamp, ppIn.UVScene).rgb;

    // Get the blurred pixel colour
    float3 blurColour = BlurredMap.Sample(PointClamp, ppIn.UVScene).rgb;

    // Probably should give a few "grace pixels" around objects closer to the camera

    // Return a lerp of the colour depending on distance to the focus
    return float4(lerp(ppColour, blurColour, distanceFromFocus), 1.0f);
}

float4 PPMotionBlurShader(PS_POSTPROCESS_INPUT ppIn) : SV_Target
{
    float3 ppColour = LastFrameMap.Sample(PointClamp, ppIn.UVScene);

    return float4(ppColour, 0.8f);
}

float4 PPVignetteShader(PS_POSTPROCESS_INPUT ppIn) : SV_Target
{
    float3 ppColour = PostProcessMap.Sample(PointClamp, ppIn.UVScene).rgb;
    float distanceToLeftRight = min(ppIn.UVScene.x, abs(ppIn.UVScene.x - 1));
    float distanceToTopBottom = min(ppIn.UVScene.y, abs(ppIn.UVScene.y - 1));
    float distanceToEdge = min(distanceToLeftRight, distanceToTopBottom);

    return float4(lerp(float3(0, 0, 0), ppColour, distanceToEdge), 0.5);
}


//--------------------------------------------------------------------------------------
// States
//--------------------------------------------------------------------------------------

RasterizerState CullBack  // Cull back facing polygons - post-processing quads should be oriented facing the camera
{
	CullMode = None;
};
RasterizerState CullNone  // Cull none of the polygons, i.e. show both sides
{
	CullMode = None;
};

DepthStencilState DepthWritesOn  // Write to the depth buffer - normal behaviour 
{
	DepthWriteMask = ALL;
};
DepthStencilState DepthWritesOff // Don't write to the depth buffer, but do read from it - useful for area based post-processing. Full screen post-process is given 0 depth, area post-processes
{                                // given a valid depth in the scene. Post-processes will not obsucre each other (in particular full-screen will not obscure area), but sorting issues may remain
	DepthWriteMask = ZERO;
};
DepthStencilState DisableDepth   // Disable depth buffer entirely
{
	DepthFunc      = ALWAYS;
	DepthWriteMask = ZERO;
};

BlendState NoBlending // Switch off blending - pixels will be opaque
{
    BlendEnable[0] = FALSE;
};
BlendState AlphaBlending
{
    BlendEnable[0] = TRUE;
    SrcBlend = SRC_ALPHA;
    DestBlend = INV_SRC_ALPHA;
    BlendOp = ADD;
};


//--------------------------------------------------------------------------------------
// Post Processing Techniques
//--------------------------------------------------------------------------------------

// Simple copy technique - no post-processing (pointless but illustrative)
technique10 PPCopy
{
    pass P0
    {
        SetVertexShader( CompileShader( vs_4_0, PPQuad() ) );
        SetGeometryShader( NULL );                                   
        SetPixelShader( CompileShader( ps_4_0, PPCopyShader() ) );

        SetBlendState(AlphaBlending, float4(0.0f, 0.0f, 0.0f, 0.0f), 0xFFFFFFFF);
		SetRasterizerState( CullBack ); 
		SetDepthStencilState( DepthWritesOff, 0 );
     }
}


// Tint the scene to a colour
technique10 PPTint
{
    pass P0
    {
        SetVertexShader( CompileShader( vs_4_0, PPQuad() ) );
        SetGeometryShader( NULL );                                   
        SetPixelShader( CompileShader( ps_4_0, PPTintShader() ) );

        SetBlendState(AlphaBlending, float4(0.0f, 0.0f, 0.0f, 0.0f), 0xFFFFFFFF);
		SetRasterizerState( CullBack ); 
		SetDepthStencilState( DepthWritesOff, 0 );
    }
}

// Tint the scene to a gradient between two colours
technique10 PPGradient
{
	pass P0
	{
		SetVertexShader( CompileShader( vs_4_0, PPQuad() ) );
		SetGeometryShader(NULL);
		SetPixelShader(CompileShader(ps_4_0, PPGradientShader()));

        SetBlendState(AlphaBlending, float4(0.0f, 0.0f, 0.0f, 0.0f), 0xFFFFFFFF);
		SetRasterizerState( CullBack );
		SetDepthStencilState( DepthWritesOff, 0 );
	}
};

// Turn the scene greyscale and add some animated noise
technique10 PPGreyNoise
{
    pass P0
    {
        SetVertexShader( CompileShader( vs_4_0, PPQuad() ) );
        SetGeometryShader( NULL );                                   
        SetPixelShader( CompileShader( ps_4_0, PPGreyNoiseShader() ) );

		SetBlendState( AlphaBlending, float4( 0.0f, 0.0f, 0.0f, 0.0f ), 0xFFFFFFFF );
		SetRasterizerState( CullBack ); 
		SetDepthStencilState( DepthWritesOff, 0 );
     }
}

// Burn the scene away
technique10 PPBurn
{
    pass P0
    {
        SetVertexShader( CompileShader( vs_4_0, PPQuad() ) );
        SetGeometryShader( NULL );                                   
        SetPixelShader( CompileShader( ps_4_0, PPBurnShader() ) );

		SetBlendState( NoBlending, float4( 0.0f, 0.0f, 0.0f, 0.0f ), 0xFFFFFFFF );
		SetRasterizerState( CullBack ); 
		SetDepthStencilState( DepthWritesOff, 0 );
     }
}

// Distort the scene as though viewed through cut glass
technique10 PPDistort
{
    pass P0
    {
        SetVertexShader( CompileShader( vs_4_0, PPQuad() ) );
        SetGeometryShader( NULL );                                   
        SetPixelShader( CompileShader( ps_4_0, PPDistortShader() ) );

		SetBlendState( NoBlending, float4( 0.0f, 0.0f, 0.0f, 0.0f ), 0xFFFFFFFF );
		SetRasterizerState( CullBack ); 
		SetDepthStencilState( DepthWritesOff, 0 );
     }
}

// Spin the image in a vortex
technique10 PPSpiral
{
    pass P0
    {
        SetVertexShader( CompileShader( vs_4_0, PPQuad() ) );
        SetGeometryShader( NULL );                                   
        SetPixelShader( CompileShader( ps_4_0, PPSpiralShader() ) );

		SetBlendState( AlphaBlending, float4( 0.0f, 0.0f, 0.0f, 0.0f ), 0xFFFFFFFF );
		SetRasterizerState( CullBack ); 
		SetDepthStencilState( DepthWritesOff, 0 );
     }
}

// Wiggling alpha blending to create a heat haze effect
technique10 PPHeatHaze
{
    pass P0
    {
        SetVertexShader( CompileShader( vs_4_0, PPQuad() ) );
        SetGeometryShader( NULL );                                   
        SetPixelShader( CompileShader( ps_4_0, PPHeatHazeShader() ) );

		SetBlendState( AlphaBlending, float4( 0.0f, 0.0f, 0.0f, 0.0f ), 0xFFFFFFFF );
		SetRasterizerState( CullBack ); 
		SetDepthStencilState( DepthWritesOff, 0 );
     }
}

// Simple Box Blur
technique10 PPBoxBlur
{
    pass P0
    {
        SetVertexShader(CompileShader(vs_4_0, PPQuad()));
        SetGeometryShader(NULL);
        SetPixelShader(CompileShader(ps_4_0, PPBoxBlurShader()));

        SetBlendState(AlphaBlending, float4(0.0f, 0.0f, 0.0f, 0.0f), 0xFFFFFFFF);
        SetRasterizerState(CullBack);
        SetDepthStencilState(DepthWritesOff, 0);
    }
}

// Two-pass Gaussian blur
technique10 PPGaussianBlur
{
	pass Horizontal
	{
		SetVertexShader(CompileShader(vs_4_0, PPQuad()));
		SetGeometryShader(NULL);
		SetPixelShader(CompileShader(ps_4_0, PPGaussianBlurHorizontalShader()));

        SetBlendState(AlphaBlending, float4(0.0f, 0.0f, 0.0f, 0.0f), 0xFFFFFFFF);
		SetRasterizerState(CullBack);
		SetDepthStencilState(DepthWritesOff, 0);
	}
    pass Vertical
	{
		SetVertexShader(CompileShader(vs_4_0, PPQuad()));
		SetGeometryShader(NULL);
		SetPixelShader(CompileShader(ps_4_0, PPGaussianBlurVerticalShader()));

        SetBlendState(AlphaBlending, float4(0.0f, 0.0f, 0.0f, 0.0f), 0xFFFFFFFF);
		SetRasterizerState(CullBack);
		SetDepthStencilState(DepthWritesOff, 0);
	}
}

// Underwater effect
technique10 PPUnderWater
{
    pass P0
    {
        SetVertexShader(CompileShader(vs_4_0, PPQuad()));
        SetGeometryShader(NULL);
        SetPixelShader(CompileShader(ps_4_0, PPUnderWaterShader()));

        SetBlendState(AlphaBlending, float4(0.0f, 0.0f, 0.0f, 0.0f), 0xFFFFFFFF);
        SetRasterizerState(CullBack);
        SetDepthStencilState(DepthWritesOff, 0);
    }
}

// Turn the image into its negative
technique10 PPNegative
{
    pass P0
    {
        SetVertexShader(CompileShader(vs_4_0, PPQuad()));
        SetGeometryShader(NULL);
        SetPixelShader(CompileShader(ps_4_0, PPNegativeShader()));

        SetBlendState(AlphaBlending, float4(0.0f, 0.0f, 0.0f, 0.0f), 0xFFFFFFFF);
        SetRasterizerState(CullBack);
        SetDepthStencilState(DepthWritesOff, 0);
    }
}

technique10 PPRetro
{
    pass P0
    {
        SetVertexShader(CompileShader(vs_4_0, PPQuad()));
        SetGeometryShader(NULL);
        SetPixelShader(CompileShader(ps_4_0, PPRetroShader()));

        SetBlendState(AlphaBlending, float4(0.0f, 0.0f, 0.0f, 0.0f), 0xFFFFFFFF);
        SetRasterizerState(CullBack);
        SetDepthStencilState(DepthWritesOff, 0);
    }
}

technique10 PPBloom
{
    pass P0
    {
        SetVertexShader(CompileShader(vs_4_0, PPQuad()));
        SetGeometryShader(NULL);
        SetPixelShader(CompileShader(ps_4_0, PPBloomShader()));

        SetBlendState(AlphaBlending, float4(0.0f, 0.0f, 0.0f, 0.0f), 0xFFFFFFFF);
        SetRasterizerState(CullBack);
        SetDepthStencilState(DepthWritesOff, 0);
    }
    pass BrightFilter
    {
        SetVertexShader(CompileShader(vs_4_0, PPQuad()));
        SetGeometryShader(NULL);
        SetPixelShader(CompileShader(ps_4_0, PPBrightFilterShader()));

        SetBlendState(AlphaBlending, float4(0.0f, 0.0f, 0.0f, 0.0f), 0xFFFFFFFF);
        SetRasterizerState(CullBack);
        SetDepthStencilState(DepthWritesOff, 0);
    }
}

technique10 PPDepthOfField
{
    pass P0
    {
        SetVertexShader(CompileShader(vs_4_0, PPQuad()));
        SetGeometryShader(NULL);
        SetPixelShader(CompileShader(ps_4_0, PPDOFShader()));

        SetBlendState(AlphaBlending, float4(0.0f, 0.0f, 0.0f, 0.0f), 0xFFFFFFFF);
        SetRasterizerState(CullBack);
        SetDepthStencilState(DepthWritesOff, 0);
    }
}

technique10 PPMotionBlur
{
    pass P0
    {
        SetVertexShader(CompileShader(vs_4_0, PPQuad()));
        SetGeometryShader(NULL);
        SetPixelShader(CompileShader(ps_4_0, PPMotionBlurShader()));

        SetBlendState(AlphaBlending, float4(0.0f, 0.0f, 0.0f, 0.0f), 0xFFFFFFFF);
        SetRasterizerState(CullBack);
        SetDepthStencilState(DepthWritesOff, 0);
    }
}

technique10 PPVignette
{
    pass P0
    {
        SetVertexShader(CompileShader(vs_4_0, PPQuad()));
        SetGeometryShader(NULL);
        SetPixelShader(CompileShader(ps_4_0, PPVignetteShader()));

        SetBlendState(NoBlending, float4(0.0f, 0.0f, 0.0f, 0.0f), 0xFFFFFFFF);
        SetRasterizerState(CullBack);
        SetDepthStencilState(DepthWritesOff, 0);
    }
}