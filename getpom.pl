#!/usr/bin/perl

# use module
use XML::Simple;
use Cwd;
sub findfile {
  if (-e "pom.xml"){
    $xml = new XML::Simple;
    $data = $xml->XMLin("pom.xml");
    print "$data->{name}\@$data->{version}";
  }else{
    if( getcwd() ne "/"){
      chdir "..";
      findfile();
    }else{
      print ""
    }
  }
}

findfile