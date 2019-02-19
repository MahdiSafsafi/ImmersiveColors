#!/usr/bin/perl

#=================================================
# https://github.com/MahdiSafsafi/ImmersiveColors
#=================================================

use strict;
use warnings;
use List::Util qw/min max/;

my $file               = "ImmersiveColors.txt";
my $immersiveTypeFile  = '..\Source\ImmersiveType.inc';
my $immersiveTableFile = '..\Source\ImmersiveTable.inc';

open my $fh, '<', $file or die "unable to open file '$file'";
my $line  = 0;
my @items = ();
my ( $min, $max ) = ( 0, 0 );
while (<$fh>) {
	chomp;
	$line++;
	s/#.+//;
	if (/^\s*(\w+)\s*:\s*(.+)\s*$/) {
		my ( $name, $others ) = ( $1, $2 );
		my %item   = ( name => $name, value => undef, rival => $name, kind => '' );
		my @fields = $others =~ /(\w+\s*=\s*\w+)/g;
		foreach (@fields) {
			/(\w+)\s*=\s*(\w+)/;
			$item{$1} = $2;
		}
		defined $item{value} or die "invalid line $line";
		my $value = $item{value};
		$value = $value =~ /^0x/ ? hex($value) : $value;
		$item{value} = $value;
		$min = min( $min, $value );
		$max = max( $max, $value );
		defined $items[$value] and die "duplicated value at line $line";
		$items[$value] = \%item;
	}
}
close $fh;
defined $items[$_] or die "there is no entry for value $_" for ( $min .. $max );

open $fh, '>', $immersiveTypeFile or die "unable to create file '$immersiveTypeFile'";
open my $th, '>', $immersiveTableFile or close $fh and die "unable to create file '$immersiveTableFile'";

foreach ( $fh, $th ) {
	printf $_ <<'__@__';
//=================================================
// https://github.com/MahdiSafsafi/ImmersiveColors
//=================================================

//=================================================
//  automatically-generated file. do not edit !!!
// ================================================
__@__
}

printf $fh "\ntype\n TImmersiveColorType = (\n";
printf $th "\nconst\n ImmersiveTable : array [TImmersiveColorType] of TImmersiveEntry = (\n";

for my $i ( 0 .. $#items ) {
	my $item  = $items[$i];
	my $comma = $i == $#items ? '' : ',';
	my $flags = { '' => 0, dark => 1, light => 2 }->{ $item->{kind} };
	printf $fh "  %s%s\n", $item->{name}, $comma;
	printf $th "  (Name: '%s'; Flags: \$%04X; Rival: SmallInt(%s))%s\n", $item->{name}, $flags, $item->{rival}, $comma;
}
printf $fh ");\n\n";
printf $th ");\n\n";

close $fh;
close $th;
