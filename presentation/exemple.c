#include <stdio.h>

int main(){
	float x = 0.0;
	float y = 0.1;
	float res = 0.1*1e5;
	/*double x = 0.0;
	double y = 0.1;
	double res = 0.1*1e8;*/
	int i =0;
	for(i=0; i< 1e5; i++)
	 x+=y;
	
	printf("x = %f\n",x);
	printf("error = %f\n", res-x);
	return 0;
}
