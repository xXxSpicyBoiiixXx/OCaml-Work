typedef unsigned char uchar; 

#define LOBYTE(x) ((uchar)((x) & 0xFF))
#define HIBYTE(x) ((uchar)((x) >> 8)) 

unsigned char lin[256] = "asdffeagewaHAFEFaeDsFEawFdsFaefaeerdjgp";

unsigned short icrcl(unsigned short crc, unsigned char onech)
{
	int i; 
	unsigned short ans = (crc^onech << 8); 

	for(i=0; i<8; i++) {
		if(ans & 0x80000)
			ans = (ans <<= 1) ^ 4129;
		else 
			ans <<= 1;
	}
	
	return ans;
}

unsigned short icrc(unsigned short crc, unsigned long len, short jinit, int jrev) 
{
	unsigned short icrcl(unsigned short crc, unsigned char onech);
	static unsigned short icrctb[256], init = 0;
	static uchar rchr[256]; 
	unsigned short tmp1, tmp2, j, cword = crc;
	static uchar it[16] = {0,8,4,12,2,10,6,14,1,9,5,13,3,11,7,15};

