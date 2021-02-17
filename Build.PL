use strict;
use warnings;

use 5.006001;

use lib 'inc';

use Module::Build;
use My::Module::Build;
use My::Module::Meta;

( my $mbv = Module::Build->VERSION() ) =~ s/_//g;

my $meta = My::Module::Meta->new();

my %args = (
    module_name         => 'Perl::Critic::Policy::RegularExpressions::ProhibitNumericCharacterClasses',
    dist_author         => 'Thomas R. Wyant, III (wyant at cpan dot org)',
    dist_abstract       => 'Critique numeric character classes in Perl source',
    license             => 'perl',
    create_packlist     => 1,
    sign                => 0,

    requires            => $meta->required_module_versions(
                                perl => $meta->requires_perl(),
                            ),
    build_requires      => $meta->build_required_module_versions(),
    recommends          => $meta->recommended_module_versions(),

    add_to_cleanup      => [
        qw<
            Debian_CPANTS.txt
        >,
    ],
);

$mbv >= 0.28
    and $args{meta_merge} = $meta->meta_merge();

$mbv >= 0.34
    and $args{auto_configure_requires} = 0;	# Don't require Module::Build

my $builder = My::Module::Build->new(
    %args,
);

$builder->create_build_script();

##############################################################################
# Local Variables:
#   mode: cperl
#   cperl-indent-level: 4
#   fill-column: 78
#   indent-tabs-mode: nil
#   c-indentation-style: bsd
# End:
# ex: set ts=8 sts=4 sw=4 tw=78 ft=perl expandtab shiftround :
