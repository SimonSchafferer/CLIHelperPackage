#! /usr/bin/perl -w
use strict;
use warnings;

use List::MoreUtils qw/ uniq /;
use Getopt::Long;

my $out_fn;
my @groups;
my @inputBED;

GetOptions( "outputFile|o=s" => \$out_fn,
	    "groups|g=i{,}"    => \@groups,
	    "inputFiles|i=s{1,}" => \@inputBED)
or die("Error in command line arguments\n");

unless ($out_fn  && @inputBED ) {
        print "\nUsage: multiIntersectClust.pl -o output_file -g groups -i input_bed_files\n";
        print "\nAll options:\n";
        print " -o|--outputFile [filename]     output filename (mandatory)\n";
	print " -g|--groups [integer]       grouping vector separated by space (optional)\n";
	print " -i|--inputFiles [filenames]   filenames separated by space (mandatory)\n";
        die;
}

if(scalar @inputBED < 2){ die ("Please Specify at least two input files!\n") }
if(scalar @groups != 0 && scalar @groups != scalar @inputBED){ 
  die ("The group vector must have the same length as the input files and must be separated by space!\n") 
}

my $min_coverage = scalar @inputBED;

my @inputBEDplus;
my @inputBEDminus;

foreach my $bed (@inputBED) {
	push (@inputBEDplus, "$bed.plus");
	push (@inputBEDminus, "$bed.minus");
	open (PLUS, ">$bed.plus");
	open (MINUS, ">$bed.minus");
	open (BED, "sortBed -i $bed |");
	while (<BED>) {
		print PLUS if /\t\+/;
		print MINUS if /\t-/;
	}
	close BED;
	close MINUS;
	close PLUS;
}

my $counter = 1;
my @segments = intersect ("+", @inputBEDplus);
push (@segments, (intersect ("-", @inputBEDminus)));

open (TMP, ">$out_fn");
foreach (@segments) {
	print TMP;
}
close TMP;

#my $sortedFN = "${out_fn}.sorted"; would be just another way of writing...
my $sortedFN = $out_fn."sorted";

unless(open SORTED,'>', $sortedFN) {
       die "nUnable to open '$sortedFN'\n";
}

open (BED, "sortBed -i $out_fn |");
while (<BED>) {
	print SORTED;
}
close BED;
close SORTED;

unlink $out_fn;
rename $sortedFN, $out_fn;


if( scalar @groups != 0 ){
	my $mergedFN = $out_fn."merged";

	unless(open MERGED,'>', $mergedFN) {
	       die "nUnable to open '$mergedFN'\n";
	}
	open (BED, "mergeBed -s -c 6 -o distinct -i $out_fn |");
	while (<BED>) {
		print MERGED;
	}
	close BED;
	close MERGED;
	unlink $out_fn;
	rename $mergedFN, $out_fn;
}


#removing the files generated temporarily
unlink @inputBEDplus;
unlink @inputBEDminus;

sub intersect {
	my ($strand, @bed) = @_;
	my @segments = ();
	my $seg_start = 0;

	if( scalar @groups != 0){
	open (INTERSECT, "multiIntersectBed -i @bed |");
		while (<INTERSECT>) {
		#	print;
			chomp;
			my @line = split (/\t/, $_);
			my @un_group = uniq(@groups);
			my $keep = 0;#FALSE
			my $idxline = 5;
			foreach my $i ( @un_group ){
				my @groupsVal = ();
				foreach my $j (@groups){
					if( $i == $j ){
						push( @groupsVal, $line[$idxline] );
						$idxline ++;
					}
				}
				my $sum = 0;
				for ( @groupsVal ) {
				    $sum = $sum + $_;
				}
				if( $sum == scalar @groupsVal ){ $keep = 1; }#TRUE
			}
			if($keep){
				my $seg_start = $line[1];
				my $seg_end = $line[2];
				#$max_coverage = $line[3];
				my $chr = $line[0];
				push (@segments, "$chr\t$seg_start\t$seg_end\t$counter\t0\t$strand\n");
				$counter++;
			}
		}
		close INTERSECT;
	} else{
	open (INTERSECT, "multiIntersectBed -cluster -i @bed |");
		while (<INTERSECT>) {
		#	print;
			chomp;
			my @line = split (/\t/, $_);
				if($line[3] == $min_coverage){
					my $seg_start = $line[1];
					my $seg_end = $line[2];
					#$max_coverage = $line[3];
					my $chr = $line[0];
					push (@segments, "$chr\t$seg_start\t$seg_end\t$counter\t0\t$strand\n");
					$counter++;
				}

		}
		close INTERSECT;

	}
	return @segments;
}











