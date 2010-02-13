/*
 * This little program makes openoffice use gtk as the toolkit.
 * http://forums.gentoo.org/viewtopic.php?t=26114#463181
 *
 * gcc -Wall -L/usr/X11R6/lib -lX11 oo_gtk_theme.c -o oo_gtk_theme
 * strip oo_gtk_theme
 * sudo install oo_gtk_theme /usr/local/bin
 * oo_gtk_theme
 * soffice
 */

#include <stdio.h>
#include <X11/Xlib.h>
#include <X11/Xatom.h>

int
main(void)
{
	Display *d;
	Atom gnome;

	d = XOpenDisplay(NULL);
	if (!d) {
		printf("X needs to be running\n");
		return 1;
	}

	/* OpenOffice.org checks for a "GNOME_SM_PROXY" atom to determine
	 * whether to use the gtk theme colors, so we just pretend that we
	 * support that atom. This may break other apps. */
	gnome = XInternAtom(d, "GNOME_SM_PROXY", False);

	XChangeProperty(d, DefaultRootWindow(d), gnome, XA_STRING, 8,
	    PropModeReplace, "GNOME_SM_PROXY", 14);

	XCloseDisplay(d);

	return 0;
}
