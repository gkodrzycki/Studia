#include <stdio.h>
#include <stdlib.h>

int main(int argc, char *argv[]) {

    long long memory_size = atoll(argv[1]);

    char *memory_block = malloc(memory_size);
	
    if (memory_block == NULL) {
        fprintf(stderr, "Killed");
        return 1;
    } else {
	    for (long long i = 0; i < memory_size; ++i) {
	        memory_block[i] = i;
	    }
	}

    printf("Successfully allocated %lld bytes of memory\n", memory_size);
	sleep(100);	
    return 0;
}
