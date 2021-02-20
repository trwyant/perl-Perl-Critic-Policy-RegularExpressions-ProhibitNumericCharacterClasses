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
    add_to_cleanup	=> $meta->add_to_cleanup(),
    build_requires	=> $meta->build_required_module_versions(),
    configure_requires	=> $meta->configure_requires(),
    dist_abstract	=> $meta->abstract(),
    dist_author	=> $meta->author(),
    dist_name	=> $meta->dist_name(),
    license	=> $meta->license(),
    module_name	=> $meta->module_name(),
    recommends  => $meta->recommended_module_versions(),
    requires	=> $meta->required_module_versions(
	perl	=> $meta->requires_perl(),
    ),
    script_files	=> $meta->script_files(),
);

if ( $mbv >= 0.28 ) {
    $args{meta_merge} = $meta->meta_merge();
    $args{meta_add} = {
	no_index	=> $meta->no_index(),
    };
}

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
