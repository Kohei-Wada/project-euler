#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <math.h>

#define ARRAYSIZE 2000000

typedef struct list {
	int a[ARRAYSIZE], start, end; 
} List; 

inline int listLength(List *l) { return l->end - l->start;}

inline List *listInit(List *l) { return memset(l, 0, sizeof(List)); }

inline int listGet(List *l, int index) { return l->a[l->start + index]; }

List* listAdd(List *l, int i) 
{ 
	if (l->end < ARRAYSIZE) l->a[l->end] = i, l->end++;

	return l;
}


int listPop(List *l) 
{ 
	int value = 0; 
	if (l->start != l->end) value = l->a[l->start], l->start++; 

	return value;
}

List* listFilter(List *l, int a) 
{
	List new = {0}; 

	for (int i = 0; i < l->end; ++i) 
		if (l->a[i] % a != 0) 
			listAdd(&new, l->a[i]); 

	l->start = new.start; l->end   = new.end; 
	memcpy(l->a, &new.a, sizeof(int) * listLength(&new)); 
	
	return l; 
}

int listSum(List *l)
{
	int sum = 0, value; 
	for (int i = 0; (value = listGet(l, i)) != 0; i++) 
		sum += value; 

	return sum; 
}

List *makeNumberList(List *l, int start, int end) 
{
	for (int i = start; (i < end) && (i < ARRAYSIZE); i++) listAdd(l, i); 
	return l;
}

void printlist(List *l){ for(int i = l->start; i < l->end; ++i)  printf("%d, ", l->a[i]); }


List *erast(List *l, List *res) 
{
	int newPrime; 
	while ((newPrime = listPop(l)) < sqrt(ARRAYSIZE)) 
		listAdd(res, newPrime), listFilter(l, newPrime);

	while ((newPrime = listPop(l)) != 0) 
		listAdd(res, newPrime);

	return res; 
}


int main(void)
{
	List *res  = malloc(sizeof(List)); 
	List *nums = malloc(sizeof(List)); 
	listInit(res); 
	listInit(nums); 

	makeNumberList(nums, 2, ARRAYSIZE); 
	erast(nums, res); 
	printlist(res); 

	printf("sum of primes ( < %d) :  %d\n", ARRAYSIZE, listSum(res)); 

	return 0; 
}

