#include <stdio.h>
#include "hopfield_step.h"
// 4-node radial feeder, 3 edges: e0:0->1, e1:1->2, e2:1->3
// directed incidence B[node][edge]
int main(void){
  double in[30];
  // B (node-major): n0:[+1,0,0] n1:[-1,+1,+1] n2:[0,-1,0] n3:[0,0,-1]
  double B[4][3]={{+1,0,0},{-1,+1,+1},{0,-1,0},{0,0,-1}};
  int k=0;
  for(int n=0;n<4;n++)for(int e=0;e<3;e++)in[k++]=B[n][e];
  // injections P: node0 slack 0, node1 PV +30, node2 load -10, node3 load -8
  in[k++]=0; in[k++]=30; in[k++]=-10; in[k++]=-8;
  // injections Q
  in[k++]=0; in[k++]=10; in[k++]=-3; in[k++]=-2;
  // conductances g (1/R): e0,e1,e2
  in[k++]=5.0; in[k++]=3.0; in[k++]=3.0;
  // initial flows x (zero)
  for(int i=0;i<6;i++) in[k++]=0.0;
  // alpha
  in[k++]=0.5;
  double out[7]={0};
  hopfield_step(0,0,0,0,0,0,0,0,0,0,in, 0,0,0,0,0,0,0,0,0,0,out);
  printf("settled: e0=(%.3f,%.3f) e1=(%.3f,%.3f) e2=(%.3f,%.3f)  gridImport=%.3f\n",
         out[0],out[1],out[2],out[3],out[4],out[5],out[6]);
  return 0;
}
