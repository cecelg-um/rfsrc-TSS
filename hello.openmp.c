#include <omp.h>
#include <stdio.h>
#include <stdlib.h>

int main (int argc, char *argv[]) {
int nthreads, threadID;

// Fork a team of OpenMP threads giving them their own copies of variables.
#pragma omp parallel private(nthreads, threadID)
{
// Get the thread number.
threadID = omp_get_thread_num();
nthreads = omp_get_num_threads();
printf("OpenMP thread = %d of %d reporting for duty.\n",
threadID,
nthreads);

}

printf("\n");
return 0;
}