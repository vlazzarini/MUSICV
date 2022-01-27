// MUSIC V driver program
// calls pass1, pass2, pass3 and raw2wav to produce an output
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

#include <stdlib.h>
#include <stdio.h>
#define BUFSIZE 1024

int main(int argc, char *argv[]) {
  if (argc > 2) {
    FILE *fin,*fout;
    int ret;
    char buffer[BUFSIZE];
    fin = fopen(argv[1],"r");
    if(fin) {
      fout = fopen("score","w");
      if(fout) do {
          ret = fread(buffer,1,BUFSIZE,fin);
          fwrite(buffer,1,ret,fout);
	} while(ret);
      else {
	fclose(fin);
	printf("could not open score file for writing\n");
        return -1;
      }
      fclose(fin);
      fclose(fout);
    } else {
      printf("could not open %s for reading\n", argv[1]);
        return -1;
    } 
    ret = system("pass1");
    if(ret == 0) {
      ret = system("pass2");
      if(ret == 0) {
	ret = system("pass3");
	if(ret == 0) {
	  int chn = 1;
	  fin = fopen("chn","r");
	  fscanf(fin, "%d", &chn);
	  fclose(fin);
	  snprintf(buffer,BUFSIZE,"raw2wav %s %d", argv[2], chn);
	  printf(" PASS III completed successfully\n"
		 "Created snd.raw (32-bit floas, sr = 44100 KHz, %s)\n",
		  chn > 1 ? "stereo" : "mono");
	  ret = system(buffer);
	  if(ret) {
	    printf("Failed to convert snd.raw to %s\n", argv[2]);
	    return -1;
	  }
	  return 0;
	}
	else printf("pass 3 failed\n");
      } else printf("pass 2 failed\n");
    } else printf("pass 1 failed\n");
    return 1;
  } else printf("usage: %s score output.wav \n", argv[0]);
  return -1;
}


				    
