/*
	An interpretation of the ADM2 - automated detection algorithm.	
	Author:	Tomas William Fitzgerald
	Date:	13/03/2011	
	
*/

#include <R.h>
#include <Rmath.h>


void _P(double *dat, double *prob, double *threshold, int *size);
void _F(double *d, double *st, double *so, double *threshold, int *size);
double *_FR(double *d, double *dat, double *prob, double T, int N);

double sum(double *array, int start, int stop) { double sum=0; for(int i=start;i<=stop;i++) sum+=array[i]; return sum; }
double msum(double *array1, double *array2, int start, int stop) { double sum=0; for(int i=start;i<=stop;i++) { double p = 1/(pow(array1[i],2)); double a=p*array2[i]; sum+=a; } return sum; }
//double msum(double *array1, double *array2, int start, int stop) { double sum=0; for(int i=start;i<=stop;i++) { double a=array1[i]*array2[i]; sum+=a; } return sum; }
double mean(double *array, int N) { if(N==1) { return array[0]; } else { double sum=0; for (int i=0; i<N; i++){ sum=sum+array[i]; } return sum/N; } }
double var(double *array, int N) { int D = N-1; double xbar[N]; double m = mean(array, N); double var = 0; for (int i=0;i<N;i++) { double dev = array[i] - m; var += pow(dev,2); } return(var/D); } 

void _P(double *dat, double *prob, double *threshold, int *size) {
	int N=(int)*size; double T=(double)*threshold; double d[N];
	double v=var(dat, N); double m= mean(dat, N);
    for(int i=0;i<N;i++) dat[i] =(dat[i]-m)/v;		
	for(int i=0;i<N;i++) prob[i] = 1/(pow(prob[i],2));
	double *dd = _FR(d, dat, prob, T, N); for(int i=0;i<N;i++) dat[i] = dd[i];
}

double *_FR(double *d, double *dat, double *prob, double T, int N) {	
	double t=T; int i=0; int j=1;
	do {
		if(j>=N | i >=N) { break; }
		double q = sum(prob,i, j); double s = msum(prob, dat, i, j);
		double sc = abs(abs(s)/sqrt(abs(q))); d[j]=sc; 
		if(sc>t) { 
		double scc = sc;
			while(sc>t & j<N & scc >= sc) { 
				scc=sc; j++;
				if(j==N | i ==N) { break; }
				q = sum(prob,i, j); s = msum(prob, dat, i, j); sc = abs(abs(s)/sqrt(abs(q)));  	
			 }		
			 for(int k=i;k<j;k++) { d[k]=sc; } i=j;
		}
		else { i++; j++; }
		
	} while(j<N & i <N);
	return(d);
}

void _F(double *d, double *st, double *so, double *threshold, int *size) {	
	int N=(int)*size; double T=(double)*threshold; int count=0; int x=0; int tol=3;
	while(x<N) {	
		if(abs(d[x])>T) {
			int con=0; int pin=x+1;
			while(abs(d[pin])>T & pin<N | tol>con) {  pin++; if(abs(d[pin])<T) { con++; } }
			while(abs(d[pin])>T) { pin++; } while(abs(d[pin])<T) { pin--; }
			while(abs(d[x])>T) { x--; } while(abs(d[x])<T) { x++; }
			int s1=x+1; int s2=pin+1;
			if(s1<s2) { st[count]=s1; so[count]=s2; count++; }
			else if (s2>s1) { st[count]=s2; so[count]=s1; count++; }	
			x=pin+1;
		} else x++;
	}
}
