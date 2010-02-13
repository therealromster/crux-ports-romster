#!/usr/bin/perl

# called by:
# msslogread.pl <file to process> > msssummary.log

use strict;

my %allocated;
my %deallocated;
my %warnings;

while (<>)
{
   chomp;
   my $line = $_;

   if ($line =~ /^LOG\:/)
   {
      $line =~ /^LOG\: \w+ \(line \d+ of .*?\) (\w+)/;

      if ($1 eq "allocated")
      {
         if ($line =~ /^LOG\: (\w+) \(line (\d+) of (.*?)\) (\w+) (\d+) .*?\`(\w+)\'/)
         {
            my $func = $1;
            my $line = $2;
            my $file = $3;
            my $alloc = $4;
            my $allocbytes = $5;
            my $allocMethod = $6;

            my $key = "$file:$line:$allocMethod";

            if (exists($allocated{$key}))
            {
               $allocated{$key}{'count'}++;
               $allocated{$key}{'bytes'} += $allocbytes;
            }
            else
            {
               $allocated{$key}{'count'} = 1;
               $allocated{$key}{'bytes'} = $allocbytes;
            }
         }
      }
      elsif ($1 eq "deallocated")
      {
         $line =~ /^LOG\: (\w+) \(line (\d+) of (.*?)\) (\w+).*?(\d+) bytes.*?\`(\w+)\'.*?\`(\w+)\' \w+ (\w+) \(line (\d+) of (.*?)\)/;

         my $func = $1;
         my $line = $2;
         my $file = $3;
         my $dealloc = $4;
         my $deallocbytes = $5;
         my $deallocMethod = $6;
         my $allocMethod = $7;
         my $allocFunc = $8;
         my $allocLine = $9;
         my $allocFile = $10;

         my $key = "$allocFile:$allocLine:$allocMethod";

         if (exists($deallocated{$key}))
         {
            $deallocated{$key}{'count'}++;
            $deallocated{$key}{'bytes'} += $deallocbytes;
         }
         else
         {
            $deallocated{$key}{'count'} = 1;
            $deallocated{$key}{'bytes'} = $deallocbytes;
         }
      }

   }
   elsif ($line =~ /^WARNING\:/)
   {
      print "$line\n";
   }
}

foreach my $allocInfo (sort {$a cmp $b} (keys %allocated))
{
   if (exists($deallocated{$allocInfo}))
   {
      if ($allocated{$allocInfo}{'count'} == $deallocated{$allocInfo}{'count'})
      {
         print "$allocInfo allocated ($allocated{$allocInfo}{'count'}) $allocated{$allocInfo}{'bytes'} bytes deallocated ($deallocated{$allocInfo}{'count'}) $deallocated{$allocInfo}{'bytes'} bytes\n";
      }
      else
      {
         print "$allocInfo allocated ($allocated{$allocInfo}{'count'}) $allocated{$allocInfo}{'bytes'} bytes deallocated ($deallocated{$allocInfo}{'count'}) $deallocated{$allocInfo}{'bytes'} bytes NOT MATCHING ".($allocated{$allocInfo}{'bytes'} - $deallocated{$allocInfo}{'bytes'})." bytes remaining\n";
      }
      delete $deallocated{$allocInfo};
   }
   else
   {
      print "$allocInfo allocated ($allocated{$allocInfo}{'count'}) $allocated{$allocInfo}{'bytes'} bytes NOT DEALLOCATED\n";
   }
}

foreach my $deallocInfo (sort {$a cmp $b} (keys %deallocated))
{
   print "$deallocInfo deallocated ($deallocated{$deallocInfo}{'count'}) $deallocated{$deallocInfo}{'bytes'} bytes NOT ALLOCATED IN THE FIRST PLACE\n";
}

