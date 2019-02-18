#ifndef GEN_COLOUR_MATH
#define GEN_COLOUR_MATH

#include <d3dx10.h>

// Find the minimum of three numbers (helper function for exercise below)
float Min(float f1, float f2, float f3)
{
	float fMin = f1;
	if (f2 < fMin)
	{
		fMin = f2;
	}
	if (f3 < fMin)
	{
		fMin = f3;
	}
	return fMin;
}

// Find the maximum of three numbers (helper function for exercise below)
float Max(float f1, float f2, float f3)
{
	float fMax = f1;
	if (f2 > fMax)
	{
		fMax = f2;
	}
	if (f3 > fMax)
	{
		fMax = f3;
	}
	return fMax;
}

template <typename T>
void FitToRange(T& fN, const float& min = 0.0f, const float& max = 1.0f)
{
	if (fN < min) fN += max;
	else if (fN > max) fN -= max;
}


float HToColour(float temp1, float temp2, float tempH)
{
	if (6.0f * tempH < 1.0f)
	{
		return temp2 + (temp1 - temp2) * 6.0f * tempH;
	}
	if (2.0f * tempH < 1.0f)
	{
		return temp1;
	}
	if (3.0f * tempH < 2.0f)
	{
		return temp2 + (temp1 - temp2) * ((2.0f / 3.0f) - tempH) * 6.0f;
	}
	return temp2;
}

// Convert an RGB colour to a HSL colour
void RGBToHSL(const float& R, const float& G, const float& B, float& H, float& S, float& L)
{
	float fR = R;
	float fG = G;
	float fB = B;

	float MIN = Min(fR, fG, fB);
	float MAX = Max(fR, fG, fB);

	L = (int)(50.0f * (MAX + MIN));

	if (MIN == MAX)
	{
		S = H = 0;
		return;
	}
	else if (L < 50)
	{
		S = (int)(100.0f * (MAX - MIN) / (MAX + MIN));
	}
	else
	{
		S = (int)(100.0f * (MAX - MIN) / (2.0f - MAX - MIN));
	}

	if (MAX == fR)
	{
		H = (int)(60.0f * (fG - fB) / (MAX - MIN));
	}
	if (MAX == fG)
	{
		H = (int)(60.0f * (fB - fR) / (MAX - MIN) + 120.0f);
	}
	if (MAX == fB)
	{
		H = (int)(60.0f * (fR - fG) / (MAX - MIN) + 240.0f);
	}
	if (H < 0)
	{
		H += 360;
	}
}

void HSLToRGB(const float& H, const float& S, const float& L, float& R, float& G, float& B)
{
	float fS = S;
	float fL = L;
	float fH = H;
	if (S == 0)
	{
		R = fL * 255;
		G = fL * 255;
		B = fL * 255;
	}
	else
	{
		float temp1;
		if (fL < 0.5f)
		{
			temp1 = fL * (1.0f + fS);
		}
		else
		{
			temp1 = (fL + fS) - (fL * fS);
		}
		float temp2 = (2.0f * fL) - temp1;

		FitToRange(fH, 0.0f, 360.0f);

		float fR = (fH / 360.0f) + (1.0f / 3.0f);
		float fG = (fH / 360.0f);
		float fB = (fH / 360.0f) - (1.0f / 3.0f);

		FitToRange(fR);
		FitToRange(fG);
		FitToRange(fB);

		R = HToColour(temp1, temp2, fR);
		G = HToColour(temp1, temp2, fG);
		B = HToColour(temp1, temp2, fB);
	}
}

#endif // GEN_COLOUR_MATH