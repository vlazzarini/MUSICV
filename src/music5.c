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
#include <string.h>
#include <unistd.h>

#define BUFSIZE 1024


int main(int argc, char *argv[])
{

  if (argc < 3) {
    fprintf(stderr, "Usage: %s scorefile outputfile\n", argv[0]);
    return EXIT_FAILURE;
  }

  char buf[BUFSIZE];
  char cwd[BUFSIZE];
  snprintf(buf, BUFSIZE, "%s/..", argv[0]);
  if(!realpath(buf, cwd))
    strcpy(cwd, ".");

  /* copy score to working directory */
  snprintf(buf, BUFSIZE, "cp %s %s/score", argv[1], cwd);
  printf("%s\n", buf);
  if(system(buf))
    return EXIT_FAILURE;

  chdir(cwd);
  if(system("./pass1") || system("./pass2") || system("./pass3"))
    return EXIT_FAILURE;

  int chn = 1;
  int sr = 44100;
  FILE* fin = fopen("./snd_params.txt", "r");
  if(!fin){
    fprintf(stderr, "could not open snd_params.txt for reading\n");
    return EXIT_FAILURE;
  }
  fscanf(fin, "%d", &chn);
  fscanf(fin, "%d", &sr);
  fclose(fin);

  printf(" PASS III completed successfully\n"
    "Created snd.raw (32-bit float, sr = %d KHz, %s)\n", sr,
    (chn > 1) ? "stereo" : "mono");

  snprintf(buf,BUFSIZE, "./raw2wav %s %d %d", argv[2], chn, sr);

  if(system(buf))
    return EXIT_FAILURE;

  return EXIT_SUCCESS;
}
