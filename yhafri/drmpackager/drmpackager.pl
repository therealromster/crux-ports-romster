#!/usr/bin/perl

# file: drmpackager.pl
# version: 1.0 - 2005-10-16
# author: Cédric Luthi

use File::Basename;

use constant MP3_SUFFIX => '.mp3';
use constant DM_SUFFIX  => '.dm';

# process every argument passed
foreach $argnum (0 .. $#ARGV) {

	# get base, dirname and suffix for every file passed in argument
	($base, $dirname, $suffix) = fileparse($ARGV[$argnum], MP3_SUFFIX);

	# very cheap test to process only .mp3 files
	if($suffix eq MP3_SUFFIX) {
		# open the .mp3 file to read and the the .dm file to write
		open(INF, $ARGV[$argnum]) or die "can't read '$ARGV[$argnum]' : $!";
		$outname = "$dirname$base".DM_SUFFIX;
		open(OUTF, ">$outname") or die "can't write to '$outname' : $!";
		
		# write drm header
    	print OUTF "--mime_content_boundary\r\nContent-Type: audio/mpeg\r\nContent-Transfer-Encoding: binary\r\n\r\n";
    	
    	# switch to binary mode to copy the input file to the output file
		binmode INF;
		binmode OUTF;
		while (
			read (INF, $buffer, 65536)	# read in (up to) 64k chunks, write
			and print OUTF $buffer	# exit if read or write fails
		) {};
		die "problem copying : $!\n" if $!;
		
    	close INF;
		
		# switch back to text in order to write the last part of the drm
		binmode OUTF;
		print OUTF "\r\n--mime_content_boundary--\r\n";

		close OUTF;
		
		print chr(27).'[01;32m'.$outname.chr(27)."[00m converted\n";
	}
}
