#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <getopt.h>
#include <ctype.h>

#define DEFAULT_GREETING "Hello,"

int can_color() {
	return isatty(1) ? 1 : 0;
}

int main(int argc, char *argv[]) {
    int opt;
    int capitalize = 0;
    char *color = "auto";
    char *greeting = DEFAULT_GREETING;
    int print_extra_world = 0;
    int id = 0;

    static struct option long_options[] = {
		{ "capitalize", no_argument, 		NULL, 'c' },
		{ "greeting", 	required_argument,	NULL, 'g' },
		{ "color", 		required_argument, 	NULL, 'x' },
		{ "world", 		no_argument, 		NULL, 'w' },
		{ "version", 	no_argument, 		NULL, 'v' },
		{ "help", 		no_argument, 		NULL, 'h' },
	};

    while ((opt = getopt_long(argc, argv, "cg:hvw", long_options, &id)) != -1) {
        switch (opt) {
            case 'c':
                capitalize = 1;
                break;
            case 'g':
                greeting = optarg;
                break;
            case 'h':
                printf("Usage: %s [options] [arguments]\n", argv[0]);
                printf("Options:\n");
                printf("-c, --capitalize            = capitalize first letter of string\n");
                printf("-g, --greeting <text>       = substitute 'Hello' for given text\n");
                printf("--color=<auto|always|never> = colorize the output\n");
                printf("-w, --world                 = print 'Hello, world!' in next line\n");
                printf("-v, --version               = print version\n");
                printf("-h, --help                  = print help\n");
                exit(EXIT_SUCCESS);
            case 'v':
                printf("hwb version 0.1 created by Grzegorz Kodrzycki\n");
                exit(EXIT_SUCCESS);
            case 'w':
                print_extra_world = 1;
                break;
            case 'x': 
                color = optarg; 
                break;
            default:
                printf("Try '%s --help' for more information.\n", argv[0]);
                exit(EXIT_FAILURE);
        }
    }

    for (int i = optind; i < argc; i++) {
        char *name = argv[i];

        if (capitalize) {
            name[0] = toupper(name[0]);
        }

        if ((!strcmp(color, "auto") || !strcmp(color, "always")) && can_color()) {
            printf("\033[0;34m");
        }

        printf("%s %s!\033[0m\n", greeting, name);
    }

    if (print_extra_world) {
        if ((strcmp(color, "auto") || !strcmp(color, "always")) && can_color()) {
            printf("\033[0;34m");
        }

        printf("%s world!\033[0m\n", DEFAULT_GREETING);
    }

    return 0;
}
