#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/wait.h>

int main() {
    pid_t child_pid;

    child_pid = fork();

    if (child_pid == -1) {
        perror("Failed fork");
        exit(EXIT_FAILURE);
    }

    if (child_pid == 0) {
        printf("Child: PID = %d\n", getpid());
        
        sleep(5);
        
        exit(EXIT_SUCCESS);
    } else {
        printf("Parent: PID = %d\n", getpid());
        sleep(100);
    }

    return 0;
}
