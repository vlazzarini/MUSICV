// Simple dependency-free raw 44100 Hz 32-bit floats mono
// conversion from raw data to RIFF-Wave format
// (c) V Lazzarini, 2022
//
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions are met:
// 1. Redistributions of source code must retain the above copyright notice,
// this list of conditions and the following disclaimer.
// 2. Redistributions in binary form must reproduce the above copyright notice,
// this list of conditions and the following disclaimer in the documentation
// and/or other materials provided with the distribution.
// 3. Neither the name of the copyright holder nor the names of its contributors
// may be used to endorse or promote products derived from this software without
// specific prior written permission.
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
// AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
// IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
// ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
// LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
// CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
// SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
// INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
// CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
// ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
// POSSIBILITY OF SUCH DAMAGE

#include <stdio.h>
#include <stdlib.h>

static inline char le_test(){
    union _le {
      char c[2];
      short s;
    } le = {{0x0001}};
    return le.c[0];
}

static inline char *byteswap(void *pp, int32_t N){
  char *p  = (char *) pp;
    if (!le_test()) {
      char tmp;
      int32_t j ;
      for(j = 0; j < N/2; j++) {
        tmp = p[j];
        p[j] = p[N - j - 1];
        p[N - j - 1] = tmp;
      }
    }
    return p;
}

const char     RIFF_ID[4] = {'R','I','F','F'};
const char     WAVE_ID[4] = {'W','A','V','E'};
const char     FMT_ID[4]  = {'f','m','t',' '};
const char     DATA_ID[4] = {'d','a','t','a'};

typedef struct wave_head{
  int	magic;			// 'RIFF' 
  int	len0;			// Chunk size = len + 8 + 16 + 12 
  int	magic1;			// 'WAVE' 
  int	magic2;			// 'fmt ' 
  int	len;			// length of header (16)
  short format;// 1 is PCM integer, 3 is float integer
  short	nchns;			// Number of channels 
  int	rate;			// sampling frequency 
  int	aver;			// Average bytes/sec !! 
  short	nBlockAlign;		// (rate*nch +7)/8 
  short	size;			// size of each sample (8,16,32) 
  int	magic3;			// 'data' 
  int	datasize;		// data chunk size 
} WAVEHEAD;

#define bufsize 512

int raw2wav(const char *in, const char *out, int chns, int sr) {
  FILE *fp, *fpi;
  size_t r = 0, bytes =0;
  WAVEHEAD header;
  float buf[bufsize];
  header.magic = (long)  (*(long*)RIFF_ID);			// 'RIFF' 
  header.len0 = 0;
  header.magic1 = (long)  (*(long*)WAVE_ID);			// 'WAVE' 
  header.magic2 = (long)  (*(long*)FMT_ID);;			// 'fmt ' 
  header.len = 16;
  byteswap(&header.len,4);			// length of header (16)
  header.format = 3;
  byteswap(&header.format,2);
  header.nchns = chns;			// Number of channels
  byteswap(&header.nchns,2);
  header.rate = sr;
  byteswap(&header.rate,4);			// sampling frequency 
  header.aver = sr*chns*4;
  byteswap(&header.aver,4);
  header.nBlockAlign = 4*chns;
  byteswap(&header.nBlockAlign,2);		// (rate*nch +7)/8 
  header.size = 32;
  byteswap(&header.size,2);			// size of each sample (8,16,32) 
  header.magic3 = (long)  (*(long*)DATA_ID);;
  header.datasize = 0;

  if((fpi = fopen(in, "r")) == NULL) {
    printf("Could not open raw input file %s \n", in);
    return -1;
  }    
  if((fp = fopen(out, "w")) != NULL) {
    fwrite(&header,sizeof(WAVEHEAD),1,fp);
    do {
      r = fread(buf,sizeof(float),bufsize,fpi);
      for(int i = 0; i < r; i++)
	byteswap(&buf[i],4);
      fwrite(buf,sizeof(float),r,fp);
      bytes += (4*r);
    } while(r);
    rewind(fp);
    header.datasize = bytes;
    byteswap(&header.datasize,4);
    header.len0 = sizeof(WAVEHEAD) - 8 + bytes;
    byteswap(&header.len0,4);
    fwrite(&header,sizeof(WAVEHEAD),1,fp);
    fclose(fp);
    printf("Wrote %zu bytes of 32-bit float sample frames to %s (%s) \n",
	    bytes, out, chns > 1 ? "stereo" : "mono");
  } else printf("Could not open %s for output\n", out);
  fclose(fpi);
  return 0;
}
