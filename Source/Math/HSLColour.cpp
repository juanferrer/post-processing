#include "HSLColour.h"
#include "ColourMath.h"

gen::HSLColour::HSLColour(float newH = 0, float newS = 0, float newL = 0)
{
	h = newH;
	s = newS;
	l = newL;
}

gen::HSLColour::HSLColour(D3DXCOLOR rgb)
{
	RGBToHSL(rgb.r, rgb.g, rgb.b, h, s, l);
}

D3DXCOLOR gen::HSLColour::GetD3DXCOLOR()
{
	float r, g, b;
	HSLToRGB(h, s, l, r, g, b);
	return D3DXCOLOR(r, g, b, 1.0f);
}

void gen::HSLColour::ModifyHue(float changeVal)
{
	h += changeVal;
	if (h < 0) h += 360;
	else if (h > 360) h -= 360;
}

void gen::HSLColour::ModifySaturation(float changeVal)
{
	if (0.0f < s && s < 1.0f) s += changeVal;
}

void gen::HSLColour::ModifyLuminance(float changeVal)
{
	if (0.0f < l && l < 1.0f) l += changeVal;
}
