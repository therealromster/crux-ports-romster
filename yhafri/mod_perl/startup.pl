#use Apache2 ( );

# Modify the path below (/var/www/perl)
# to somehing else if you want 
use lib qw(/var/www/perl);

# enable if the mod_perl 1.0 compatibility is needed
# use Apache2::compat ( );

# preload all mp2 modules
# use ModPerl::MethodLookup;
# ModPerl::MethodLookup::preload_all_modules( );

use ModPerl::Util ( ); #for CORE::GLOBAL::exit

use Apache2::RequestRec ( );
use Apache2::RequestIO ( );
use Apache2::RequestUtil ( );

use Apache2::ServerRec ( );
use Apache2::ServerUtil ( );
use Apache2::Connection ( );
use Apache2::Log ( );
use Apache::Session ();

use CGI ();
use CGI::Cookie ();

use APR::Table ( );
use APR::Pool ();

#use LWP;
use LWP::UserAgent ();

use ModPerl::Registry ( );

use Apache2::Const -compile => ':common';
use APR::Const -compile => ':common';

1;
