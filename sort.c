#include<stdio.h>
#include<stdlib.h>
#include<math.h>
#include<sys/time.h>
#include<omp.h>
#include <string.h>
//#include <stdafx.h>
#include <windows.h>
int getIndex(int a[],int low ,int high)
{
	int base=a[low];
	int i=low,j=high,temp;
	while(i<j)
	{
		while(a[j]>base&&i<j)
			j--;
		a[i]=a[j];
		while(i<j&&a[++i]<base);
		a[j]=a[i];
	}
	a[i]=base;
	return i;
}
void QuickSort(int a[],int low,int high)
{
	if(low<high)
	{
		int index=getIndex(a,low,high);
		QuickSort(a,low,index-1);
		QuickSort(a,index+1,high);
	}
}
void quicksort_omp(int *array,int start,int end)
{
	if(start>=end)
		return;
	int index=getIndex(array,start,end);
	if(end-start<1500)
	{
		quicksort_omp(array,start,index-1);
		quicksort_omp(array,index+1,end);
	}
	else
	{
		#pragma omp task
		quicksort_omp(array,start,index-1);
		#pragma omp task
		quicksort_omp(array,index+1,end);
	}
}
void qsort_omp(int *array,int start,int end)
{
	#pragma omp parallel
	#pragma omp single
	quicksort_omp(array,start,end);
}

void mergesort_omp(int *array,int *array_temp,int start,int end)
{
	if(start>=end)
		return;
	int mid=(end-start)/2+start;
	int start1=start;
	int end1=mid;
	int start2=mid+1;
	int end2=end;
	if(end-start<1500)
	{
		mergesort_omp(array,array_temp,start1,end1);
		mergesort_omp(array,array_temp,start2,end2);
	}
	else
	{
		#pragma omp task
		mergesort_omp(array,array_temp,start1,end1);
		#pragma omp task
		mergesort_omp(array,array_temp,start2,end2);
		#pragma omp taskwait
	}
	int pointer=start;
	while(start1<=end1&&start2<=end2)
	{
		if(array[start1]<array[start2])
		{
			array_temp[pointer]=array[start1];
			start1++;
		}
		else
		{
			array_temp[pointer]=array[start2];
			start2++;
		}
		pointer++;
	}
	for(int i=start1;i<=end1;i++)
	{
		array_temp[pointer]=array[i];
		pointer++;
	}
	for(int i=start2;i<=end2;i++)
	{
		array_temp[pointer]=array[i];
		pointer++;
	}
	for(int i=start;i<=end;i++)
		array[i]=array_temp[i];
}
void msort_omp(int *array,int *array_temp,int start,int end)
{
	#pragma omp parallel
	#pragma omp single
	mergesort_omp(array,array_temp,start,end);
}
int findmaxbit(int *array,int len)
{
	int maxitem=array[0];
	for(int i=1;i<len;i++)
		if(array[i]>maxitem)
			maxitem=array[i];
	int maxbit;
	for(int i=1;i<=32;i++)
	{
		maxitem=maxitem>>1;
		if((maxitem&1)>0)
			maxbit=i+1;
	}
	return maxbit;
}
void radixsort_serial(int *array,int *bucket0,int *bucket1,int len)
{
	int maxbit=findmaxbit(array,len);
	for(int bit=0;bit<maxbit;bit++)
	{
		int start0=0;
		int start1=0;
		for(int i=0;i<len;i++)
		{
			int item=array[i];
			int mask=1<<bit;
			if((item&mask)==0)
			{
				bucket0[start0]=item;
				start0++;
			}
			else
			{
				bucket1[start1]=item;
				start1++;
			}
		}
		memcpy(array,bucket0,sizeof(int)*start0);
		memcpy(&array[start0],bucket1,sizeof(int)*start1);
	}
}
void scan(int *array,int len)
{
	int olditem,newitem;
	olditem=array[0];
	array[0]=0;
	for(int i=1;i<len;i++)
	{
		newitem=array[i];
		array[i]=olditem+array[i-1];
		olditem=newitem;
	}
}
void getMemoryUse(int len)
{
	printf("%f\n",len*sizeof(int)/1024.0/1024.);
}
void radixsort_omp(int *array,int *bucket0,int *bucket1,int len,int nthreads)
{
	int *bucketscan0=(int*)malloc(sizeof(int)*(nthreads+1));
	int *bucketscan1=(int*)malloc(sizeof(int)*(nthreads+1));
	int maxbit=findmaxbit(array,len);
	int locallen=ceil((double)len/(double)nthreads);
	for(int bit=0;bit<maxbit;bit++)
	{
		#pragma omp parallel for
		for(int tid=0;tid<nthreads;tid++)
		{
			int start0=0;
			int start1=0;
			int localstart=tid*locallen;
			int localend=((tid+1)*locallen)>len?len:((tid+1)*locallen);
			for(int i=localstart;i<localend;i++)
			{
				int item=array[i];
				int mask=1<<bit;
				if((item&mask)==0)
				{
					bucket0[tid*locallen+start0]=item;
					start0++;
				}
				else
				{
					bucket1[tid*locallen+start1]=item;
					start1++;
				}
			}
			bucketscan0[tid]=start0;
			bucketscan1[tid]=start1;
		}
		scan(bucketscan0,nthreads+1);
		scan(bucketscan1,nthreads+1);
		#pragma omp parallel for
		for(int tid=0;tid<nthreads;tid++)
		{
			memcpy(&array[bucketscan0[tid]],&bucket0[tid*locallen],sizeof(int)*(bucketscan0[tid+1]-bucketscan0[tid]));
			memcpy(&array[bucketscan0[nthreads]+bucketscan1[tid]],&bucket1[tid*locallen],sizeof(int)*(bucketscan1[tid+1]-bucketscan1[tid]));
		}
	}
	free(bucketscan0);
	free(bucketscan1);
}
int findmaxbit10(int *array,int n)
{
	int maxitem=array[0];
	for(int i=1;i<n;i++)
		if(array[i]>maxitem)
			maxitem=array[i];
	int maxbit10=0;
	while(maxitem)
	{
		maxitem/=10;
		maxbit10++;
	}
	return maxbit10;
}
void radixsortbase10_serial(int *array,int **base10,int len)
{
	int maxbit=findmaxbit10(array,len);
	int base=1;
	int start[10];
	for(int bit=0;bit<maxbit;bit++,base*=10)
	{
		for(int i=0;i<10;i++)
			start[i]=0;
		int item,index;
		for(int i=0;i<len;i++)
		{
			item=array[i];
			index=item/base%10;
			base10[index][start[index]]=item;
			start[index]++;
		}
		int sum=0;
		for(int i=0;i<10;i++)
		{
			memcpy(&array[sum],base10[i],sizeof(int)*start[i]);
			sum+=start[i];
		}
	}
}
void radixsortbase10_omp(int *array,int **base10,int len,int nthreads)
{
	int maxbit=findmaxbit10(array,len);
	int base=1;
	int start[nthreads][10];
	int bucketscan[10][nthreads+1];
	for(int i=0;i<10;i++)
	{
		for(int j=0;j<len;j++)
		base10[i][j]=0;
	}
	int locallen=ceil((double)len/(double)nthreads);
	for(int bit=0;bit<maxbit;bit++,base*=10)
	{
		#pragma omp parallel for
		for(int tid=0;tid<nthreads;tid++)
		{
			for(int i=0;i<10;i++)
				start[tid][i]=0;
			int localstart=tid*locallen;
			int localend=((tid+1)*locallen)>len?len:((tid+1)*locallen);
			for(int i=localstart;i<localend;i++)
			{
				int item=array[i];
				int index=item/base%10;
				base10[index][localstart+start[tid][index]]=item;
				start[tid][index]++;
			}
			for(int i=0;i<10;i++)
			{
				bucketscan[i][tid]=start[tid][i];
			 } 
		}
		for(int i=0;i<10;i++)
				scan(bucketscan[i],nthreads+1);
		int sum[10];
		sum[0]=0;
		for(int i=1;i<10;i++)
			sum[i]=sum[i-1]+bucketscan[i-1][nthreads];
		#pragma omp parallel for
		for(int tid=0;tid<nthreads;tid++)
			for(int i=0;i<10;i++)
				memcpy(&array[sum[i]+bucketscan[i][tid]],&base10[i][tid*locallen],sizeof(int)*(bucketscan[i][tid+1]-bucketscan[i][tid]));
	}
}
void checksort(int *array,int len)
{
	int sorted=1;
	for(int i=1;i<len;i++)
		if(array[i]<array[i-1])
		{
			sorted=0;
			break;
		}
	if(sorted)
		printf("check PASSED!\n");
	else
		printf("check NOT PASSED\n");
}
void getPhysMemory()
{
	MEMORYSTATUSEX statex;
	statex.dwLength=sizeof(statex);
	GlobalMemoryStatusEx(&statex);//获取当前内存状态
	printf("%uM\n",statex.ullAvailPhys/1024/1024); 
}
int main(int argc,char**argv)
{
	
	struct timeval t1,t2;
	int len=atoi(argv[1]);
	printf("%d-------------------------\n",len);
	int *array=(int *)malloc(sizeof(int)*len);
	int *array1=(int *)malloc(sizeof(int)*len);
	int *array2=(int *)malloc(sizeof(int)*len);
	int *array3=(int *)malloc(sizeof(int)*len);
	int *array4=(int *)malloc(sizeof(int)*len);	
	int *array5= (int *)malloc(sizeof(int)*len);	
	int *array6=(int *)malloc(sizeof(int)*len);	
	srand(time(NULL));
	int tt=len/10000;
	for(int i=0;i<len;i++)
		array[i]=rand()%len;
		//array[i]=i/1000;
		//array[i]=tt-i/10000;
	double time_use;
	
	
	//quicksort
	for(int nthreads=1;nthreads<=16;nthreads*=2)
	{
		 
		omp_set_num_threads(nthreads);
		memcpy(array1,array,sizeof(int)*len);
		gettimeofday(&t1,NULL);
		qsort_omp(array1,0,len-1);
		gettimeofday(&t2,NULL);
		double time_use=(t2.tv_sec-t1.tv_sec)*1000+(t2.tv_usec-t1.tv_usec)/1000;
		printf("[%2i-t] Quicksort used %4.2f ms\n",nthreads,time_use);
		 
	}
	checksort(array1,len);
	free(array1);
	
	
	
	//mergesort
	int *array_temp=(int *)malloc(sizeof(int)*len);
	for(int nthreads=1;nthreads<=16;nthreads*=2)
	{
		omp_set_num_threads(nthreads);
		memcpy(array2,array,sizeof(int)*len);
		gettimeofday(&t1,NULL);
		msort_omp(array2,array_temp,0,len-1);
		gettimeofday(&t2,NULL);
		double time_use=(t2.tv_sec-t1.tv_sec)*1000+(t2.tv_usec-t1.tv_usec)/1000;
		printf("[%2i-t] Mergesort used %4.2f ms\n",nthreads,time_use);
		
	}
	free(array_temp);
	checksort(array2,len);
	free(array2);
	
	
	
	//radix sort serial
	memcpy(array3,array,sizeof(int)*len);
	int *bucket0_serial=(int*)malloc(sizeof(int)*len);
	int *bucket1_serial=(int*)malloc(sizeof(int)*len);
	gettimeofday(&t1,NULL);
	radixsort_serial(array3,bucket0_serial,bucket1_serial,len);
	gettimeofday(&t2,NULL);
	free(bucket0_serial);
	free(bucket1_serial);
	time_use=(t2.tv_sec-t1.tv_sec)*1000+(t2.tv_usec-t1.tv_usec)/1000;
	printf("serial radixsort used %4.2f ms\n",time_use);
	checksort(array3,len);
	free(array3);
	
	
	
	//radix sort openmp
	int *bucket0_omp=(int*)malloc(sizeof(int)*len);
	int *bucket1_omp=(int*)malloc(sizeof(int)*len);
	for(int nthreads=1;nthreads<=16;nthreads*=2)
	{
		omp_set_num_threads(nthreads);
		memcpy(array4,array,sizeof(int)*len);
		gettimeofday(&t1,NULL);
		radixsort_omp(array4,bucket0_omp,bucket1_omp,len,nthreads);
		gettimeofday(&t2,NULL);
		double time_use=(t2.tv_sec-t1.tv_sec)*1000+(t2.tv_usec-t1.tv_usec)/1000;
		printf("[%2i-t] radixsort used %4.2f ms\n",nthreads,time_use);
	}
	checksort(array4,len);
	
	
	int *b=(int*)malloc(sizeof(int)*10*len);
	int* base10[10];
	for(int i=0;i<10;i++)
	base10[i]=b+i*len;
	memcpy(array5,array,sizeof(int)*len);
	gettimeofday(&t1,NULL);
	radixsortbase10_serial(array5,base10,len);
	gettimeofday(&t2,NULL);
	time_use=(t2.tv_sec-t1.tv_sec)*1000+(t2.tv_usec-t1.tv_usec)/1000;
	printf("radixsortbase-10 serial used %4.2f ms\n",time_use);
	checksort(array5,len);

	
	
	for(int nthreads=1;nthreads<=16;nthreads*=2)
	{
		omp_set_num_threads(nthreads);
		memcpy(array6,array,sizeof(int)*len);
		gettimeofday(&t1,NULL);
		radixsortbase10_omp(array6,base10,len,nthreads);
		gettimeofday(&t2,NULL);
		double time_use=(t2.tv_sec-t1.tv_sec)*1000+(t2.tv_usec-t1.tv_usec)/1000;
		printf("[%2i-t] RasixBase10-OMP used %4.2f ms\n",nthreads,time_use);
	}
	checksort(array6,len);
	free(array6);
	free(array5);
	
	
	free(bucket0_omp);
	free(bucket1_omp);
	free(array4);
	free(array);
	free(b);
}


