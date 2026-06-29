#include <stdio.h>
#include "hopfield_step.h"
int main(void){
  double in[13]={0,0,0,0,0,0, /*x*/  20,5, -10,-3, -8,-2, /*theta*/  0.5/*alpha*/};
  double out[7]={0};
  double z0[0];
  hopfield_step(0,0,0,0,0,0,0,0,0,0,in, 0,0,0,0,0,0,0,0,0,0,out);
  printf("settled flows: e0=(%.4f,%.4f) e1=(%.4f,%.4f) e2=(%.4f,%.4f)\n",
         out[0],out[1],out[2],out[3],out[4],out[5]);
  printf("gridImport (slack edge P) = %.4f\n", out[6]);
  return 0;
}
