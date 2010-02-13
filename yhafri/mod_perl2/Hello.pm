package Hello;
use strict;
use warnings;

#use lib qw();

use Apache2::Const -compile => qw(OK);

sub handler {
    my $r   = shift;

    $r->content_type('text/html');
    $r->print(<<EOT);
<?xml version="1.0" encoding="UTF-8"?>
<html>
<body bgcolor=000000>
<font color=FFCC00>mod_perl 2.0</font>
<font color=FFFFFF>under </font>
<font color=FFCC00>Apache2 </font>
<font color=FFFFFF>is successfully loaded.</font><br>
<font color=FFFFFF>Boths says:</font><br>
<center><h2><font color=FFFF66>Hello Perl Guru !</font></h2></center>
</body>
</html>
EOT

    return Apache2::Const::OK;
}
1;
