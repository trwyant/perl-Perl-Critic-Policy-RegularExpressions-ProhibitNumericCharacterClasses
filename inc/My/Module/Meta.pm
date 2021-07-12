package My::Module::Meta;

use 5.006001;

use strict;
use warnings;

our $VERSION = '0.000_016';

sub new {
    my ( $class ) = @_;
    return bless {}, ref $class || $class;
}

sub abstract {
    return 'Critique numeric character classes in Perl source';
}

sub add_to_cleanup {
    return [ qw{ cover_db xt/author/optionals } ];
}

sub author {
    return 'Thomas R. Wyant, III (wyant at cpan dot org)';
}

sub build_required_module_versions {
    return +{
        'lib'       => 0,
        'charnames' => 0,
        'open'      => 0,
        'English'   => 0,
        'PPI::Document'             => 0,
        'Perl::Critic::TestUtils'   => 0,
        'Test::More'                => 0,
        'Test::Perl::Critic::Policy'    => 0,
    };
}

sub configure_requires {
    return +{
	'lib'	=> 0,
	'strict'	=> 0,
	'warnings'	=> 0,
    };
}

sub dist_name {
    return 'Perl-Critic-Policy-RegularExpressions-ProhibitNumericCharacterClasses';
}

sub license {
    return 'perl';
}

sub meta_merge {
    return {
	'meta-spec'	=> {
	    version	=> 2,
	},
#       homepage    => 'http://perlcritic.com',
	resources	=> {
	    bugtracker	=> {
		web	=> 'https://rt.cpan.org/Public/Dist/Display.html?Name=Perl-Critic-Policy-RegularExpressions-ProhibitNumericCharacterClasses',
		# web	=> 'https://github.com/trwyant/perl-Perl-Critic-Policy-RegularExpressions-ProhibitNumericCharacterClasses/issues',
                mailto  => 'wyant@cpan.org',
            },
	    license	=> 'http://dev.perl.org/licenses/',
	    repository	=> {
		type	=> 'git',
		url	=> 'git://github.com/trwyant/perl-Perl-Critic-Policy-RegularExpressions-ProhibitNumericCharacterClasses.git',
		web	=> 'https://github.com/trwyant/perl-Perl-Critic-Policy-RegularExpressions-ProhibitNumericCharacterClasses',
	    },
#            MailingList => 'http://perlcritic.tigris.org/servlets/SummarizeList?listName=users',
	}
    };
}

sub module_name {
    return 'Perl::Critic::Policy::RegularExpressions::ProhibitNumericCharacterClasses';
}

sub no_index {
    return +{
        directory => [ qw{ doc examples inc tools xt } ],
        file => [ qw{ TODO.pod } ],
    };
}

sub provides {
    my $provides;
    local $@ = undef;

    eval {
	require CPAN::Meta;
	require ExtUtils::Manifest;
	require Module::Metadata;

	my $manifest;
	{
	    local $SIG{__WARN__} = sub {};
	    $manifest = ExtUtils::Manifest::maniread();
	}
	keys %{ $manifest || {} }
	    or return;

	# Skeleton so we can use should_index_file() and
	# should_index_package().
	my $meta = CPAN::Meta->new( {
		name	=> 'Euler',
		version	=> 2.71828,
		no_index	=> no_index(),
	    },
	);

	# The Module::Metadata docs say not to use
	# package_versions_from_directory() directly, but the 'files =>'
	# version of provides() is broken, and has been known to be so
	# since 2014, so it's not getting fixed any time soon. So:

	foreach my $fn ( sort keys %{ $manifest } ) {
	    $fn =~ m/ [.] pm \z /smx
		or next;
	    my $pvd = Module::Metadata->package_versions_from_directory(
		undef, [ $fn ] );
	    foreach my $pkg ( keys %{ $pvd } ) {
		$meta->should_index_package( $pkg )
		    and $meta->should_index_file( $pvd->{$pkg}{file} )
		    and $provides->{$pkg} = $pvd->{$pkg};
	    }
	}

	1;
    } or return;

    return ( provides => $provides );
}

sub recommended_module_versions {
    return +{
##      'File::Which'   => 0,
    };
}

sub required_module_versions {
    my ( undef, @args ) = @_;
    return +{
        'base'                      => 0,
        'strict'                    => 0,
        'warnings'                  => 0,
        # 'Perl::Critic::Document'    => 1.119,   # need 1.119 here
        'Perl::Critic::Policy'      => 1.119,
        'Perl::Critic::Utils'       => 1.119,
        'PPIx::Regexp'              => 0.057,
        'Readonly'                  => 0,
        @args,
    };
}

sub requires_perl {
    return '5.006001';
}

sub script_files {
    return [
    ];
}

sub version_from {
    return 'lib/Perl/Critic/Policy/RegularExpressions/ProhibitNumericCharacterClasses.pm';
}

1;

__END__

=head1 NAME

My::Module::Meta - Metadata for the current module

=head1 SYNOPSIS

 use lib qw{ inc };
 use My::Module::Meta qw{ recommended_module_versions };

=head1 DESCRIPTION

This Perl module holds metadata for the current module. It is private to
the current module.

=head1 METHODS

Ths following public methods are provided:

=head2 new

This static method instantiates the object.

=head2 abstract

This method returns the distribution's abstract.

=head2 add_to_cleanup

This method returns a reference to an array of files to be added to the
cleanup.

=head2 author

This method returns the name of the distribution author

=head2 build_required_module_versions

This method returns an array of the names and versions of modules
required for the build.

=head2 configure_requires

 use YAML;
 print Dump( $meta->configure_requires() );

This method returns a reference to a hash describing the modules
required to configure the package, suitable for use in a F<Build.PL>
C<configure_requires> key, or a F<Makefile.PL>
C<< {META_MERGE}->{configure_requires} >> or C<CONFIGURE_REQUIRES> key.

=head2 dist_name

This method returns the distribution name.

=head2 license

This method returns the distribution's license.

=head2 meta_merge

 use YAML;
 print Dump( $meta->meta_merge() );

This method returns a reference to a hash describing the meta-data which
has to be provided by making use of the builder's C<meta_merge>
functionality. This includes the C<dynamic_config>, C<no_index> and
C<resources> data.

=head2 module_name

This method returns the name of the module the distribution is based
on.

=head2 no_index

This method returns the names of things which are not to be indexed
by CPAN.

=head2 provides

 use YAML;
 print Dump( [ $meta->provides() ] );

This method attempts to load L<Module::Metadata|Module::Metadata>. If
this succeeds, it returns a C<provides> entry suitable for inclusion in
L<meta_merge()|/meta_merge> data (i.e. C<'provides'> followed by a hash
reference). If it can not load the required module, it returns nothing.

=head2 recommended_module_versions

This method returns an array of the names and versions of
recommended modules.

=head2 required_module_versions

This method returns an array of the names and versions of required
modules. Any arguments will be appended to the returned list.

=head2 requires_perl

This method returns the version of Perl required by the module.

=head2 script_files

This method returns a reference to an array containing the names of
script files provided by this distribution. This array may be empty.

=head2 version_from

This method returns the name of the distribution file from which the
distribution's version is to be derived.

=head1 SUPPORT

Support is by the author. Please file bug reports at
L<https://rt.cpan.org/Public/Dist/Display.html?Name=Perl-Critic-Policy-RegularExpressions-ProhibitNumericCharacterClasses>,
L<https://github.com/trwyant/perl-Perl-Critic-Policy-RegularExpressions-ProhibitNumericCharacterClasses/issues>, or in
electronic mail to the author.

=head1 AUTHOR

Thomas R. Wyant, III F<wyant at cpan dot org>

=head1 COPYRIGHT AND LICENSE

Copyright (C) 2013-2021 by Thomas R. Wyant, III

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl 5.10.0. For more details, see the full text
of the licenses in the directory LICENSES.

This program is distributed in the hope that it will be useful, but
without any warranty; without even the implied warranty of
merchantability or fitness for a particular purpose.

=cut

##############################################################################
# Local Variables:
#   mode: cperl
#   cperl-indent-level: 4
#   fill-column: 72
#   indent-tabs-mode: nil
#   c-indentation-style: bsd
# End:
# ex: set ts=8 sts=4 sw=4 tw=72 ft=perl expandtab shiftround :

