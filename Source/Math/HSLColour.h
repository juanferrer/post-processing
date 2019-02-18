#ifndef GEN_HSL_COLOUR
#define GEN_HSL_COLOUR

#include <d3dx10.h>

namespace gen
{
	// HSL colour space
	class HSLColour
	{
		// 0 - 360
		float h;
		// 0.0 - 1.0
		float s;
		// 0.0 - 1.0
		float l;
	public:

		HSLColour(float newH, float newS, float newL);
		HSLColour(D3DXCOLOR rgb);

		D3DXCOLOR GetD3DXCOLOR();
		void ModifyHue(float changeVal);
		void ModifySaturation(float changeVal);
		void ModifyLuminance(float changeVal);
	};
}

#endif // GEN_HSL_COLOUR