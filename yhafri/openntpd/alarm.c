/* 
   run a command with a limited timeout
   tridge@samba.org, January 2002
   Updated: Younes Hafri - yhafri@club-internet.fr - november 16, 2006
*/

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>
#include <errno.h>

static void usage(const char * progname)
{
	fprintf(stderr, "\nRun a command with a limited timeout\n");
	fprintf(stderr, "Usage  :\n");
	fprintf(stderr, "\t%s <time_in_sec> <command>\n", progname);
	fprintf(stderr, "Example:\n");
	fprintf(stderr, "\t%s 5 find / -xdev\n", progname);
}

int main(int argc, char *argv[])
{
	long int maxtime;

	if (3 > argc) {
		usage( argv[0] );
		exit(1);
	}
	
	/* read the timeout */
	maxtime = strtol(argv[1], NULL, 0);
	if (ERANGE == errno)
	{
		fprintf(stderr, "time conversion over/underflow\n");
		exit(2);
	}

	/* set up the alarm*/
	alarm(maxtime);

	/* run the command within the timeout */
	return execvp(argv[2], argv + 2);
}
