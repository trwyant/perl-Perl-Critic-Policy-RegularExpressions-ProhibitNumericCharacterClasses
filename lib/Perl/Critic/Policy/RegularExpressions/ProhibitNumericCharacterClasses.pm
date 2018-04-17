package Perl::Critic::Policy::RegularExpressions::ProhibitNumericCharacterClasses;

use 5.006001;
use strict;
use warnings;

use PPIx::Regexp 0.057; # To force version.
use Readonly;

use Perl::Critic::Utils qw< :booleans :characters hashify :severities >;

use base 'Perl::Critic::Policy';

our $VERSION = '0.000_001';

#-----------------------------------------------------------------------------

Readonly::Scalar my $EXPL =>
    q<Some numeric character classes include non-ASCII characters>;

# The following character classes _may_ represent violations. The key is
# the content of the character class object, and the value is the
# parameter that allows the object to be accepted.
Readonly::Hash my %NUMERIC_CHARACTER_CLASS => (
    q<\\d>          => {
        parameter   => '_allow_back_slash_dee',
        replacement => '[0-9] or \p{PosixDigit}',
    },
    q<\\D>          => {
        parameter   => '_allow_back_slash_dee',
        replacement => '[^0-9] or \P{PosixDigit}',
    },
    q<[:digit:]>    => {
        parameter   => '_allow_posix_digit',
        replacement => '[0-9] or \p{PosixDigit}',
    },
    q<[:^digit:]>    => {
        parameter   => '_allow_posix_digit',
        replacement => '[^0-9] or \P{PosixDigit}',
    },
);

#-----------------------------------------------------------------------------

sub supported_parameters { return (
        {
            name        => 'allow_back_slash_dee',
            description => 'Allow the \d class',
            behavior    => 'boolean',
            default_string  => '0',
        },
        {
            name        => 'allow_posix_digit',
            description => 'Allow the [:digit:] class',
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

        # The /a or /aa modifiers can be asserted in the scope of this
        # element even if they are not asserted globally.
        $char_class->modifier_asserted( 'a*' )
            and next;

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

The C<\d> and C<[[:digit:]]> classes are accepted if the C</a> or C</aa>
modifier is in effect, because in that case they are restricted to match
only ASCII digits.

Because its recommendations run more or less counter to those of core
policy
L<RegularExpressions::ProhibitEnumeratedClasses|Perl::Critic::Policy::RegularExpressions::ProhibitEnumeratedClasses>,
the user should consider whether this policy meets the specific needs of
the code base.

Oddly enough, the C<[:xdigit:]> character class appears not to have this
problem.

Telling L<Perl::Critic|Perl::Critic> to allow a specific violation will
be problematic in the case of multi-line regular expressions, because 
L<Perl::Critic|Perl::Critic> can not see comments inside a regular
expression. Unfortunately the only workable options known to this author
are:

=over

=item Re-code using C<\p{...}>, or

=item Annotate the block of code in which the regular expression
appears.

=back

Neither of these is particularly desirable, and it may simply be that
this policy spends most of its time disabled.

=head1 CONFIGURATION

This policy supports the following configuration items. The author
strongly advises against turning these on unless you know what you are
doing. Note that turning them both on effectively disables the policy.


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
