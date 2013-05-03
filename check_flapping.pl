#!/usr/bin/perl
#
# check_flapping.pl
#
# Checks a IDO Database if a certain service is flapping
# and returns CRITICAL if it is.
#
# (c) 2013 NETWAYS GmbH, Markus Frosch <markus.frosch@netways.de>
#
# This program is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License
# as published by the Free Software Foundation; either version 2
# of the License, or (at your option) any later version.
# 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.

use strict;
use DBI;

my $dbdsn = "DBI:mysql:database=icinga;host=localhost";
my $dbuser = "icinga";
my $dbpass = "XXXX";

my $query = "SELECT is_flapping
    FROM icinga_servicestatus ss
    INNER JOIN icinga_services s ON s.service_object_id = ss.service_object_id
    INNER JOIN icinga_objects o ON o.object_id = s.service_object_id
    WHERE o.name1 LIKE ? and o.name2 LIKE ?;
";

my $host;
my $service;

#
# SUBS
#
sub usage {
    print STDERR "Usage check_flapping.pl <HOSTNAME> <SERVICE>\n";
    print STDERR "\n";
    exit(3);
}

sub error {
    my $msg = shift;
    print STDERR "$msg\n";
    print STDERR "\n";
    usage;
}

#
# PARAMS
#
if(!defined $ARGV[0]) {
    error("Please specify HOSTNAME!");
}
$host = $ARGV[0];

if(!defined $ARGV[1]) {
    error("Please specify SERVICE!");
}
$service = $ARGV[1];

#
# CONNECT DB
#

my $dbh = DBI->connect($dbdsn, $dbuser, $dbpass, {RaiseError => 1});

my $stm = $dbh->prepare($query);

if($stm->execute($host, $service)) {
    my @arr = $stm->fetchrow_array();
    if(defined $arr[0]) {
        if($arr[0] eq "0") {
            print "OK - $host / $service is not flapping\n";
            exit(0);
        }
        else {
            print "CRITICAL - $host / $service IS flapping\n";
            exit(2);
        }
    }
    else {
        print "UNKNOWN - $host / $service not found in database!\n";
        exit(3);
    }
}
else {
    error("SELECT failed!");
}

# vi: ts=4 sw=4 expandtab :

