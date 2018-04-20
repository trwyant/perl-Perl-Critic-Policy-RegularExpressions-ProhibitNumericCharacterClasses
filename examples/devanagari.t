package main;

use 5.006001;

use strict;
use warnings;

use English qw{ -no_match_vars };
use Readonly;

BEGIN {
    local $EVAL_ERROR = undef;
            # If we're running on a Perl too old to have the
            # 'experimental' pragma, there is really nothing we can do
            # about it. So maugre perlcritic we do nothing if the eval
            # fails.
    eval {  ## no critic (RequireCheckingReturnValueOfEval)
        require experimental;
        experimental->import( 'regex_sets' );
    }
}

use charnames qw{ :full };

use Test::More 0.88;        # Because of done_testing();

our $VERSION = '0.000_002';

Readonly::Scalar my $CODE_REF   => ref sub {};
Readonly::Scalar my $SCALAR_REF => ref \0;

note <<'EOD';
Demonstrate the capability of various character classes to match
"\N{DEVANAGARI DIGIT ONE}"
EOD

my $text = "\N{DEVANAGARI DIGIT ONE}";
my $want = 1;

# Note that we eval to get a compiled regular expression because not all
# of these compile under all supported versions of Perl.
foreach my $class (
    \'The following should match',
    q<\d>,
    q<[[:digit:]]>,
    q<\p{Digit}>,
    q<\p{Is_Digit}>,
    sub { $want = 0 },
    \'The following should not match',
    q<[0-9]>,
    q<[[:xdigit:]]>,
    q<\p{AsciiHexDigit}>,
    q<\p{AHex}>,
    q<\p{PosixDigit}>,
    q<\p{IsPosixDigit}>,
    q<\p{Is_PosixDigit}>,
    q<\p{Is_Posix_Digit}>,
    q<[[:ascii:]]>,
    q<\p{Ascii}>,
    q<\p{Basic_Latin}>,        # Discouraged
    q<\p{Block=Basic_Latin}>,
    q<\p{Block: ASCII}>,
    q<(?[ [[:ascii:]] & \d ])>,
) {
    if ( $CODE_REF eq ref $class ) {
        $class->();
    } elsif ( $SCALAR_REF eq ref $class ) {
        note ${ $class };
    } elsif (
        # We're getting the Regexp object by stringy eval because expect
        # to run under versions of Perl too old to compile them.
        my $re = eval "qr{$class}"  ## no critic (ProhibitStringyEval)
    ) {
        my $got = ( $text =~ $re );
        if ( $want ) {
            ok $got, qq<"\\N{DEVANAGARI DIGIT ONE} matches $class>;
        } else {
            ok !$got, qq<"\\N{DEVANAGARI DIGIT ONE} does not match $class>;
        }
    } else {
        SKIP: {
            skip "Could not compile $class", 1;
        }
    }
}

done_testing;

1;

# Local Variables:
#   mode: cperl
#   cperl-indent-level: 4
#   fill-column: 72
#   indent-tabs-mode: nil
#   c-indentation-style: bsd
# End:
# ex: set ts=8 sts=4 sw=4 tw=72 ft=perl expandtab shiftround :
