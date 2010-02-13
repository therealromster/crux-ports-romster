#!/usr/bin/perl -w


sub url_encode 
{
    my ($encode) = @_;
    return () unless defined $encode;
    $encode =~ s/([^A-Za-z0-9\-_.!~*\'() ])/ uc sprintf "%%%02x",ord $1 /eg;
    $encode =~ tr/ /+/;
    return $encode;
}


print "$ARGV[0]\n";
print url_encode ($ARGV[0]) . "\n";

