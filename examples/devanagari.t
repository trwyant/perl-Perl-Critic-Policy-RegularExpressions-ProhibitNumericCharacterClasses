package main;

use 5.006001;

use strict;
use warnings;

use English qw{ -no_match_vars };
use Readonly;
use Test::More 0.88;        # Because of done_testing();

BEGIN {
    local $EVAL_ERROR = undef;
            # If we're running on a Perl too old to have the
            # 'experimental' pragma, there is really nothing we can do
            # about it. So maugre perlcritic we do nothing if the eval
            # fails.
    eval {  ## no critic (RequireCheckingReturnValueOfEval)
        require experimental;
        experimental->import( 'regex_sets' );
    };

    # We can bring \p{IsPosixDigit} back to 5.8 by adding it as a custom
    # property

    eval { '1' =~ m/\p{IsPosixDigit}/smx }
        or $] lt '5.008'    # The following requires at least 5.8
        or eval 'sub IsPosixDigit { "30 39\n" }'        ## no critic (ProhibitStringyEval,RequireInterpolationOfMetachars)
        or plan skip_all => 'Unable to create sub IsPosixDigit()';
}

use charnames qw{ :full };

our $VERSION = '0.000_002';

Readonly::Scalar my $CODE_REF   => ref sub {};
Readonly::Scalar my $SCALAR_REF => ref \0;

note <<'EOD';

Demonstrate the ability of various character classes to match
"\N{DEVANAGARI DIGIT ONE}"

EOD

__PACKAGE__->can( 'IsPosixDigit' )
    and note <<"EOD";
We defined our own IsPosixDigit(), since Perl $] does not have it.

EOD

my $text = "\N{DEVANAGARI DIGIT ONE}";
my $want = 1;

# Note that we eval to get a compiled regular expression because not all
# of these compile under all supported versions of Perl.
foreach my $class (
    \'The following should match',
    q<\\d>,
    q<[[:digit:]]>,
    q<\\p{Digit}>,
    q<\\p{Is_Digit}>,
    sub { $want = 0 },
    \'The following should not match',
    q<[0-9]>,
    q<[[:xdigit:]]>,
    q<\\p{AsciiHexDigit}>,
    q<\\p{AHex}>,
    q<\\p{Posix_Digit}>,
    q<\\p{IsPosixDigit}>,
    q<\\p{Is_PosixDigit}>,
    q<\\p{Is_Posix_Digit}>,
    q<\\p{Posix_Alnum}>,
    q<\\p{Posix_Print}>,
    q<\\p{Posix_Word}>,
    q<\\p{Posix_XDigit}>,
    q<[[:ascii:]]>,
    q<\\p{Ascii}>,
    q<\\p{Basic_Latin}>,        # Discouraged
    q<\\p{Latin}>,
    q<\\p{Latin_1}>,
    q<\\p{Latin_1_Sup}>,
    q<\\p{Latin_1_Supplement}>,
    q<\\p{Latin_Ext_A}>,
    q<\\p{Latin_Ext_Additional}>,
    q<\\p{Latin_Ext_B}>,
    q<\\p{Latin_Ext_C}>,
    q<\\p{Latin_Ext_D}>,
    q<\\p{Latin_Ext_E}>,
    q<\\p{Latin_Extended_A}>,
    q<\\p{Latin_Extended_Additional}>,
    q<\\p{Latin_Extended_B}>,
    q<\\p{Latin_Extended_C}>,
    q<\\p{Latin_Extended_D}>,
    q<\\p{Latin_Extended_E}>,
    q<\\p{Latn}>,
    q<\\p{Block=Basic_Latin}>,
    q<\\p{Block: ASCII}>,
    q<(?[ [[:ascii:]] & \\d ])>,
) {
    if ( $CODE_REF eq ref $class ) {
        $class->();
    } elsif ( $SCALAR_REF eq ref $class ) {
        note ${ $class };
    } elsif (
        defined( my $got = eval { $text =~ m/$class/smx } )
    ) {
        if ( $want ) {
            ok $got, qq<"\\N{DEVANAGARI DIGIT ONE} matches $class>;
        } else {
            ok !$got, qq<"\\N{DEVANAGARI DIGIT ONE} does not match $class>;
        }
    } else {
        SKIP: {
            skip "Could not find $class", 1;
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
