package Perl::Critic::Policy::RegularExpressions::ProhibitNumericCharacterClasses;

use 5.006001;
use strict;
use warnings;

use PPIx::Regexp 0.057; # To force version.
use Readonly;

use Perl::Critic::Utils qw< :booleans :characters hashify :severities >;

use base 'Perl::Critic::Policy';

our $VERSION = '0.000_003';
# The problem we are solving with the following is that older Perls do
# not like the underscore in a development version number. I do not
# believe this violates the spirit of the disabled policy.
$VERSION =~ s/ _ //smxg;    ## no critic (RequireConstantVersion)

#-----------------------------------------------------------------------------

Readonly::Scalar my $EXPL =>
    q<Some numeric character classes include non-ASCII characters>;

# The following character classes _may_ represent violations. The key is
# the content of the character class object, and the value is the
# parameter that allows the object to be accepted.
Readonly::Hash my %NUMERIC_CHARACTER_CLASS => (
    q<\\d>          => {
        parameter   => '_allow_back_slash_dee',
        replacement => '[0-9] or \\p{PosixDigit}',
    },
    q<\\D>          => {
        parameter   => '_allow_back_slash_dee',
        replacement => '[^0-9] or \\P{PosixDigit}',
    },
    q<[:digit:]>    => {
        parameter   => '_allow_posix_digit',
        replacement => '[0-9] or \\p{PosixDigit}',
    },
    q<[:^digit:]>    => {
        parameter   => '_allow_posix_digit',
        replacement => '[^0-9] or \\P{PosixDigit}',
    },
);

# Analyze a potentially-offending character class in an extended
# bracketed character class. We need a hash entry for each permitted
# value of the {allow_in_extended_character_class} policy parameter. The
# code receives the element under analysis as its only argument. It
# returns a true value to accept the element, and a false value to
# reject it (i.e. make it a violation).
Readonly::Hash my %OK_IN_EXTENDED_CHARACTER_CLASS => (
    always  => sub { return $TRUE },
    safe    => \&_is_intersected_with_ascii,
    never   => sub { return $FALSE },
);

#-----------------------------------------------------------------------------

sub supported_parameters { return (
        {
            name        => 'allow_back_slash_dee',
            description => 'Allow the \\d class',
            behavior    => 'boolean',
            default_string  => '0',
        },
        {
            name        => 'allow_posix_digit',
            description => 'Allow the [:digit:] class',
            behavior    => 'boolean',
            default_string  => '0',
        },
        {
            name        => 'allow_in_extended_character_class',
            description => 'Allow \\d and [:digit:] in extended character classes',
            behavior    => 'enumeration',
            enumeration_values  => [ qw{ always safe never } ],
            default_string  => 'always',
        },
        {
            name        => 'allow_if_single_script',
            description => 'Allow \\d and [:digit:] if restricted to a single script',
            behavior    => 'boolean',
            default_string  => '0',
        },
        {
            name        => 'allow_if_singleton',
            description => 'Allow \\d and [:digit:] if unquantified or quantified to at most one',
            behavior    => 'boolean',
            default_string  => '0',
        },
    ) }

sub default_severity     { return $SEVERITY_MEDIUM       }
sub default_themes       { return qw< trw maintenance >  }
sub applies_to           { return qw<
                                PPI::Token::Regexp::Match
                                PPI::Token::Regexp::Substitute
                                PPI::Token::QuoteLike::Regexp
                                >  }

#-----------------------------------------------------------------------------

sub violates {
    my ( $self, $elem, $document ) = @_;

    # Make a PPIx::Regexp from the PPI element for further analysis.
    my $ppix = $document->ppix_regexp_from_element( $elem )
        or return;

    # If /a (or /aa) is present, character classes are restricted to the
    # ASCII range, so the use of them is not a problem.
    $ppix->modifier_asserted( 'a*' )
        and return;

    my @violations;
    foreach my $char_class (
        @{ $ppix->find( 'PPIx::Regexp::Token::CharClass' ) || [] } ) {

        my $content = $char_class->content();

        # Only interested in character classes that match digits.
        my $ctrl = $NUMERIC_CHARACTER_CLASS{$content}
            or next;

        # If the user wants to allow this class, we give him or her the
        # rope, and even tie the noose on the end.
        $self->{$ctrl->{parameter}}
            and next;

        # If we're part of a bracketed character class that contains
        # non-numerics, we are OK.
        if ( my $parent = $char_class->parent() ) {
            $parent->isa( 'PPIx::Regexp::Structure::CharClass' )
                and not _is_char_class_numeric( $parent )
                and next;
        }

        # The /a or /aa modifiers can be asserted in the scope of this
        # element even if they are not asserted globally.
        $char_class->modifier_asserted( 'a*' )
            and next;

        # Extended bracketed character classes need some more analysis
        _is_in_extended_character_class( $char_class )
            and $OK_IN_EXTENDED_CHARACTER_CLASS{
                $self->{_allow_in_extended_character_class}}->(
                    $char_class )
            and next;

        # Allow inside (*script_run:...) if the user wants that
        $self->{_allow_if_single_script}
            and _is_in_script_run( $char_class )
            and next;

        # Allow singletons if the user wants that
        $self->{_allow_if_singleton}
            and _is_singleton( $char_class )
            and next;

        # We have exhausted all appeals. Guilty as charged.
        push @violations, $self->violation(
            sprintf(
                '%s can match outside ASCII range; use %s',
                $content,
                $ctrl->{replacement},
            ),
            $EXPL,
            $elem,
        );


    }

    return @violations;
}

#-----------------------------------------------------------------------------

# Return true if a bracketed character class represents only a number of
# some sort, and is therefore something we might want to forbid.
sub _is_char_class_numeric {
    my ( $elem ) = @_;
    foreach my $kid ( $elem->schildren() ) {
        $kid->isa( 'PPIx::Regexp::Token::CharClass' )
            and $NUMERIC_CHARACTER_CLASS{$kid->content()}
            and next;
        my $content = $kid->content();
        $kid->isa( 'PPIx::Regexp::Token::Literal' )
            and $content =~ m/ \d /smx  # SIC
            and next;
        $kid->isa( 'PPIx::Regexp::Node::Range' )
            and $content =~ m/ \A \d - \d \z /smx   # SIC
            and next;

        # At this point, the current child, and therefore the entire
        # bracketed character class, MUST represent something other than
        # just a number.
        return $FALSE;
    }
    return $TRUE;
}

#-----------------------------------------------------------------------------

# Return true if the given element is in an extended character class
sub _is_in_extended_character_class {
    my ( $elem ) = @_;
    while ( 1 ) {
        $elem->isa( 'PPIx::Regexp::Structure::RegexSet' )
            and return $TRUE;
        $elem = $elem->parent()
            or return $FALSE;
    }
    return $FALSE;  # Can't get here, but perlcritic does not know that.
}

#-----------------------------------------------------------------------------

# Return true if the given element is contained in a script run

sub _is_in_script_run {
    my ( $elem ) = @_;
    while ( $elem = $elem->parent() ) {
        $elem->isa( 'PPIx::Regexp::Structure::Script_Run' )
            and return $TRUE;
    }
    return $FALSE;
}

#-----------------------------------------------------------------------------

# Return true if the given element matches at most one digit

Readonly::Hash my %AT_MOST_ONE => hashify( q<?>, q<{0,1}>, q<{1}> );

sub _is_singleton {
    my ( $elem ) = @_;

    # If our element is part of a character class we analyze that.
    # CAVEAT: if the class has not already been checked, we need to do
    # that here, essentially re-enabling the commented-out code a couple
    # lines below.
    if ( my $parent = $elem->parent() ) {
        $parent->isa( 'PPIx::Regexp::Structure::CharClass' )
##          and not _is_char_class_numeric( $parent )
##          and return $TRUE;
            and $elem = $parent;
    }

    # Check the next significant sibling if there is one.
    if ( my $next = $elem->snext_sibling() ) {
        my $content = $next->content();

        # If it is a quantifier, we fail the test unless it specifies at
        # most one match.
        if ( $next->is_quantifier() ) {
            $AT_MOST_ONE{$content}
                or return $FALSE;

            # If there is nothing after it, we're good.
            $next = $next->snext_sibling()
                or return $TRUE;
            $content = $next->comtemt();
        }

        # If it's another numeric character class, we have at least two
        # in a row. So we flunk.
        $next->isa( 'PPIx::Regexp::Token::CharClass' )
            and $NUMERIC_CHARACTER_CLASS{$content}
            and return $FALSE;

        # If it's a bracketed character class we need to dig into it a
        # bit. If it contains any of the offending character classes, we
        # flunk.
        $next->isa( 'PPIx::Regexp::Structure::CharClass' )
            and _is_char_class_numeric( $next )
            and return $FALSE;

    }

    # If we get here, our argument is a numeric class not followed by
    # another and either not quantified or quantified to at most one. So
    # we accept it.
    return $TRUE;
}

#-----------------------------------------------------------------------------

Readonly::Hash my %ASCII_CLASS => hashify( qw<
    ahex
    ascii
    asciihexdigit
    basiclatin
    latin
    latin1
    latin1sup
    latin1supplement
    latinexta
    latinextadditional
    latinextb
    latinextc
    latinextd
    latinexte
    latinextendeda
    latinextendedadditional
    latinextendedb
    latinextendedc
    latinextendedd
    latinextendede
    latn
    posixalnum
    posixprint
    posixword
    posixxdigit
    xdigit
    > );
Readonly::Hash my %INTERSECTION_OPERATOR => hashify( qw< & > );

# Return a true value if the argument is intersected with a character
# class that is known to restrict it to ASCII. The operations can occur
# in either order.
sub _is_intersected_with_ascii {
    my ( $elem ) = @_;
    foreach my $nav ( qw{ sprevious_sibling snext_sibling } ) {
        my $sib = $elem->$nav()
            or next;
        $sib->isa( 'PPIx::Regexp::Token::Operator' )
            or next;
        $INTERSECTION_OPERATOR{ $sib->content() }
            or next;
        $sib = $sib->$nav()
            or next;
        $sib->isa( 'PPIx::Regexp::Token::CharClass' )
            or next;
        local $_ = $sib->content();
        if ( $sib->isa( 'PPIx::Regexp::Token::CharClass::POSIX' ) ) {
            s/ \A \[ : //smx; s/ : \] \z //smx;
        } elsif ( $sib->isa( 'PPIx::Regexp::Token::CharClass::Simple' ) ) {
            s/ \A \\p [{] \s* //smxi; s/ \s* [}] \z //smx;
            s{ \A (?: is_ |
            (?: block | blk | script | script_extensions )
            \s* [:=] \s*) }{}smx;
            s/ [-_\s]+ //smxg;
            $_ = lc;
        }
        return $ASCII_CLASS{$_};
    }
    return $FALSE;
}

#-----------------------------------------------------------------------------

1;

__END__

#-----------------------------------------------------------------------------

=pod

=head1 NAME

Perl::Critic::Policy::RegularExpressions::ProhibitNumericCharacterClasses - Don't use \d and [:digit:], since they may match outside the ASCII range.


=head1 AFFILIATION

This Policy is stand-alone, and is not part of the core
L<Perl::Critic|Perl::Critic>.


=head1 DESCRIPTION

This L<Perl::Critic|Perl::Critic> policy is intended for those who are
concerned with the use of regular expressions to sanitize input before
conversion to internal form. It addresses the potential problem that
C<\d> and C<[[:digit:]]> may match more than the usual ASCII digit
characters, which are what the usual numeric conversion expects, and
recommends C<[0-9]> or C<\p{PosixDigit}> instead.

In the default configuration, the C<\d> and C<[[:digit:]]> classes are
accepted under the following conditions:

=over

=item The C</a> or C</aa> modifier is in effect, because in that case
they are restricted to match only ASCII digits;

=item In bracketed character classes that also specify non-digits;

=item In extended bracketed character classes, because there they can be
intersected with things like C<[:ascii:]> to exclude non-ASCII digits.

=back

Because its recommendations run more or less counter to those of core
policy
L<RegularExpressions::ProhibitEnumeratedClasses|Perl::Critic::Policy::RegularExpressions::ProhibitEnumeratedClasses>,
the user should consider whether this policy meets the specific needs of
the code base.

If you really have to deal with input conversion of non-ASCII digits,
see the L<Unicode::UCD|Unicode::UCD> L<num()|Unicode::UCD/num>
subroutine. You should be aware that even this will not convert
everything matched by C</\d+/>; it requires all the digits to be in the
same script, and has other restrictions as well.

Oddly enough, the C<[:xdigit:]> character class appears not to have this
problem.

=head1 CONFIGURATION

This policy supports the following configuration items. The author
strongly advises against turning these on unless you know what you are
doing. Note that turning on both C<allow_back_slash_dee> and
C<allow_posix_digit> effectively disables the policy.


=head2 allow_back_slash_dee

By default, this policy prohibits C<\d> unless within the scope of the
C</a> or C</aa> modifiers.

If you wish to allow C<\d>, you can add a block like this to your
F<.perlcriticrc> file:

    [RegularExpressions::ProhibitNumericCharacterClasses]
    allow_back_slash_dee = 1


=head2 allow_posix_digit

By default, this policy prohibits C<[:digit:]> unless within the scope
of the C</a> or C</aa> modifiers.

If you wish to allow C<[:digit:]>, you can add a block like this to your
F<.perlcriticrc> file:

    [RegularExpressions::ProhibitNumericCharacterClasses]
    allow_posix_digit = 1

=head2 allow_in_extended_character_class

By default, this policy allows C<\d> and C<[:digit:]> within an extended
bracketed character class, because these allow things like

    (?[ [:ascii:] & \d ])

If you wish to tighten things up here, you can add a block like this to
your F<.perlcriticrc> file:

    [RegularExpressions::ProhibitNumericCharacterClasses]
    allow_in_extended_character_class = 'safe'

The permitted values are:

=over

=item always

The default. C<\d> and C<[:digit:]> are always allowed in extended
bracketed character classes.

=item never

C<\d> and C<[:digit:]> are never allowed in extended bracketed character
classes.

=item safe

The policy tries to determine whether the use of C<\d> or C<[:digit:]>
is intersected with another class that restricts it to ASCII. The policy
makes this determination by seeing if the class is intersected on the
right or the left with one of the following Unicode character
classes or their trivial variants, or the corresponding POSIX class if
any:

    p{AHex}
    p{ASCII}
    p{ASCII_Hex_Digit}
    p{Basic_Latin}
    p{Latin}
    p{Latin_1}
    p{Latin_1_Sup}
    p{Latin_1_Supplement}
    p{Latin_Ext_A}
    p{Latin_Ext_Additional}
    p{Latin_Ext_B}
    p{Latin_Ext_C}
    p{Latin_Ext_D}
    p{Latin_Ext_E}
    p{Latin_Extended_A}
    p{Latin_Extended_Additional}
    p{Latin_Extended_B}
    p{Latin_Extended_C}
    p{Latin_Extended_D}
    p{Latin_Extended_E}
    p{Latn}
    p{Posix_Alnum}
    p{Posix_Print}
    p{Posix_Word}
    p{Posix_X_Digit}
    p{X_Digit}

Contemplation of the above list (which is surely incomplete) and the
meaning of the phrase 'trivial variants', informed by a perusal of
F<perluniprops>, should give the reader an inkling of why the author
recommends caution with this configuration item.

=back

=head2 allow_if_single_script

Perl 5.27.9 introduced the C<(*script_run:...)> construction, which
requires all characters inside it to come from the same script. If you
wish to allow C<\d> and C<[:digit:]> within the scope of this
construction, you can add a block like this to your F<.perlcriticrc>
file:

    [RegularExpressions::ProhibitNumericCharacterClasses]
    allow_if_single_script = 1

Note that the script_run construction was actually introduced in 5.27.8,
but in that release it was spelled C<(+script_run:...)>.

=head2 allow_if_singleton

In the spirit of C<allow_if_single_script>, you can configure this
policy to accept C<\d> and C<[:digit:]> if they are unquantified, or
quantified to allow at most one, by adding a block like this to your
F<.perlcriticrc> file:

    [RegularExpressions::ProhibitNumericCharacterClasses]
    allow_if_singleton = 1

In the case of something like C<\d\d>, the first C<\d> will be declared
in violation of this policy even if this configuration item has been set
true, because the whole is equivalent to C<\d{2}>.

The rationale for this is that a single digit can not represent more
than one script, and is therefore not subject to the kind of problem
this policy is intended to detect.

This logic is controlled by a separate configuration item because the
analysis involved in the implementation is a bit involved, and I am
unsure I have covered all the corner cases.

=head1 AUTHOR

Thomas R. Wyant, III F<wyant at cpan dot org>

=head1 COPYRIGHT

Copyright (C) 2018 Thomas R. Wyant, III

=head1 LICENSE

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl 5.10.0. For more details, see the full text
of the licenses in the directory LICENSES.

This program is distributed in the hope that it will be useful, but
without any warranty; without even the implied warranty of
merchantability or fitness for a particular purpose.

=cut

# Local Variables:
#   mode: cperl
#   cperl-indent-level: 4
#   fill-column: 72
#   indent-tabs-mode: nil
#   c-indentation-style: bsd
# End:
# ex: set ts=8 sts=4 sw=4 tw=72 ft=perl expandtab shiftround :
