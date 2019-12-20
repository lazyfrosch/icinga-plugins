#!/usr/bin/perl
########################################################################
# check_linux_stats
# written by  : Damien SIAUD <noemailfound>
# modified by : Markus Frosch <markus@lazyfrosch.de>
#
# version     : 1.3~dev
# last change : 2012-08-16
#
# INFO #################################################################
#
# You will find this plugin on:
#  https://github.com/lazyfrosch/icinga-plugins
#
# This plugins allow the checking and measuring of
# various Linux statistic values.
#
# This script requires Sys::Statistics::Linux from CPAN
# see http://search.cpan.org/dist/Sys-Statistics-Linux/
#
# LICENSE ##############################################################
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#
# CHANGELOG ############################################################
#
# 1.3~dev
#  * code cleanup
#  * added swapping and removed swap perfdata from paging
#  * netstats rewritten
#    now with pkg counters, better perfdata, statfile
#    for better deltas, units
#
# 1.2
#  initially imported into Markus Frosch's repo
#  source: http://exchange.nagios.org/directory/Plugins/Operating-Systems/Linux/check_linux_stats/details
#
########################################################################

use lib "/usr/lib/nagios/plugins";
use utils qw($TIMEOUT %ERRORS &print_revision &support);
use Getopt::Long;
use Sys::Statistics::Linux::CpuStats;
use Sys::Statistics::Linux::DiskStats;
use Sys::Statistics::Linux::DiskUsage;
use Sys::Statistics::Linux::FileStats;
use Sys::Statistics::Linux::LoadAVG;
use Sys::Statistics::Linux::MemStats;
use Sys::Statistics::Linux::NetStats;
use Sys::Statistics::Linux::PgSwStats;
use Sys::Statistics::Linux::ProcStats;
use Sys::Statistics::Linux::Processes;
use Sys::Statistics::Linux::SockStats;
use Data::Dumper;

use vars qw($script_name $script_version $o_sleep $o_pattern $o_cpu
  $o_procs $o_process $o_mem $o_net $o_disk $o_io $o_load
  $o_file $o_socket $o_paging $o_help $o_version $o_warning
  $o_critical $o_unit $o_swapping $o_statfile);
use strict;

# --------------------------- globals -------------------------- #

$script_name    = "check_linux_stats";
$script_version = "1.3~dev";
$o_help         = undef;
$o_pattern      = undef;
$o_version      = undef;
$o_warning      = 70;
$o_critical     = 90;
$o_sleep        = 1;
$o_unit         = "MB";
my $status = 'UNKNOWN';

# ---------------------------- main ----------------------------- #
check_options();

$o_warning  = convert_units($o_warning);
$o_critical = convert_units($o_critical);

if ($o_cpu) {
    check_cpu();
}
elsif ($o_mem) {
    check_mem();
}
elsif ($o_disk) {
    check_disk();
}
elsif ($o_io) {
    check_io();
}
elsif ($o_net) {
    check_net();
}
elsif ($o_load) {
    check_load();
}
elsif ($o_file) {
    check_file();
}
elsif ($o_procs) {
    check_procs();
}
elsif ($o_socket) {
    check_socket();
}
elsif ($o_process) {
    check_process();
}
elsif ($o_paging) {
    check_paging();
}
elsif ($o_swapping) {
    check_swapping();
}
else {
    print "no check selected!\n\n";
    usage();
}

print "\n";

exit $ERRORS{$status};

sub convert_units {
    my $conv_str = shift @_;
    unless ( $conv_str =~ /\d+((K|M|G|T)(i)?)?/ ) {
        die "You can use only K, Ki, M, Mi, G, Gi, T, Ti convertion units!"
    }
    my @result_array;
    my @tmp = split(/,/, $conv_str);
    foreach my $item (@tmp) {
        if ($item =~ /^(\d+)K$/) {
            push @result_array, int($1) * 1000;
        } elsif ($item =~ /^(\d+)Ki$/) {
            push @result_array, int($1) * 1024;
        } elsif ($item =~ /^(\d+)M$/) {
            push @result_array, $1 * 1000 * 1000;
        } elsif ($item =~ /^(\d+)Mi$/) {
            push @result_array, int($1) * 1024 * 1024;
        } elsif ($item =~ /^(\d+)G$/) {
            push @result_array, int($1) * 1000 * 1000 * 1000;
        } elsif ($item =~ /^(\d+)Gi$/) {
            push @result_array, int($1) * 1024 * 1024 * 1024;
        } elsif ($item =~ /^(\d+)T$/) {
            push @result_array, int($1) * 1000 * 1000 * 1000 * 1000;
        } elsif ($item =~ /^(\d+)Ti$/) {
            push @result_array, int($1) * 1024 * 1024 * 1024 * 1024;
        } else { # case without units
            push @result_array, int($item);
        }
    }
    return join(',', @result_array);
}

sub check_cpu {
    my $stat;
    my $lxs;
    if(defined($o_statfile)) {
        $lxs = Sys::Statistics::Linux::CpuStats->new(initfile => $o_statfile);
        $lxs->init;
        $stat = $lxs->get;
    }
    else {
        $lxs = Sys::Statistics::Linux::CpuStats->new;
        $lxs->init;
        sleep $o_sleep;
        $stat = $lxs->get;
    }

    if ( defined( $stat ) ) {
        $status = "OK";
        my $cpu = $stat->{cpu};

        if ( $cpu->{total} >= $o_critical ) {
            $status = "CRITICAL";
        }
        elsif ( $cpu->{total} >= $o_warning ) {
            $status = "WARNING";
        }
        print "$status - CPU Usage: $cpu->{total}% | ".
              "cpu=$cpu->{total}%;$o_warning;$o_critical;0;100 ".
              "system=$cpu->{system}% ".
              "user=$cpu->{user}% ".
              "iowait=$cpu->{iowait}% ".
              "nice=$cpu->{nice}% ".
              "irq=$cpu->{irq}% ".
              "softirq=$cpu->{softirq}%";
        print " steal:$cpu->{steal}%" if ( defined ( $cpu->{steal} ) );
    }
    else {
        print "No data";
    }
}

sub check_procs {
    my $procs;
    my $lxs;
    if(defined($o_statfile)) {
        $lxs = Sys::Statistics::Linux::ProcStats->new(initfile => $o_statfile);
        $lxs->init;
        $procs = $lxs->get;
    }
    else {
        $lxs = Sys::Statistics::Linux::ProcStats->new;
        $lxs->init;
        sleep $o_sleep;
        $procs = $lxs->get;
    }

    if ( defined( $procs ) ) {
        $status = "OK";

        if ( $procs->{count} >= $o_critical ) {
            $status = "CRITICAL";
        }
        elsif ( $procs->{count} >= $o_warning ) {
            $status = "WARNING";
        }
        print "PROCS $status : count $procs->{count} | ".
              "count=$procs->{count};$o_warning;$o_critical ".
              "runqueue=$procs->{runqueue} blocked=$procs->{blocked} ".
              "running=$procs->{running} new=$procs->{new}";

    }
}

sub check_process {
    my $return_str = "";
    my $perfdata   = "";

    # pidfiles
    my @pids = ();
    for my $file ( split( /,/, $o_pattern ) ) {
        open FILE, $file or die "Could not read from $file, program halting.";

        # read the record, and chomp off the newline
        chomp( my $pid = <FILE> );
        close FILE;
        if ( $pid =~ /^\d+$/ ) {
            push @pids, $pid;
        }
    }

    if ( $#pids > -1 ) {
        my $lxs = Sys::Statistics::Linux::Processes->new( pids => \@pids );
        $lxs->init;
        sleep $o_sleep;
        my $processes = $lxs->get;
        my @pname     = ();

        if ( defined($processes) ) {
            $status = "OK";

            my $crit = 0;    #critical counter
            my $warn = 0;    #warning counter
            foreach my $process ( keys(%$processes) ) {
                my $vsize  = $processes->{$process}->{vsize};
                my $nswap  = $processes->{$process}->{nswap};
                my $cnswap = $processes->{$process}->{cnswap};
                my $cpu    = $processes->{$process}->{cpu};
                my $cmd    = $processes->{$process}->{cmd};
                $cmd =~ s/\W+//g;

                if    ( $vsize >= $o_critical ) { $crit++; push @pname, $cmd; }
                elsif ( $vsize >= $o_warning )  { $warn++; push @pname, $cmd; }

                $perfdata .= " "
                  . $cmd
                  . "_vsize=$vsize;$o_warning;$o_critical "
                  . $cmd
                  . "_nswap=$nswap "
                  . $cmd
                  . "_cnswap=$cnswap "
                  . $cmd
                  . "_cpu=$cpu";
            }

            if    ( $crit > 0 ) { $status = "CRITICAL"; }
            elsif ( $warn > 0 ) { $status = "WARNING"; }

        }
        print "PROCESSES $status : " . join( ',', @pname ) . " |$perfdata";
    }
}

sub check_socket {
    my $lxs  = Sys::Statistics::Linux::SockStats->new;
    my $socks = $lxs->get;

    if ( defined( $socks ) ) {
        $status = "OK";

        if ( $socks->{used} >= $o_critical ) {
            $status = "CRITICAL";
        }
        elsif ( $socks->{used} >= $o_warning ) {
            $status = "WARNING";
        }
        print "SOCKET USAGE $status : used $socks->{used} | ".
              "used=$socks->{used};$o_warning;$o_critical ".
              "tcp=$socks->{tcp} udp=$socks->{udp} raw=$socks->{raw}";
    }
    else {
        print "No data";
    }
}

sub check_file {
    my $lxs = Sys::Statistics::Linux::FileStats->new;
    my $file = $lxs->get;

    if ( defined( $file ) ) {
        $status = "OK";

        my ( $fh_crit, $inode_crit ) = split( /,/, $o_critical );
        my ( $fh_warn, $inode_warn ) = split( /,/, $o_warning );

        if (   ( $file->{fhalloc} >= $fh_crit )
            || ( $file->{inalloc} >= $inode_crit ) )
        {
            $status = "CRITICAL";
        }
        elsif (( $file->{fhalloc} >= $fh_warn )
            || ( $file->{inalloc} >= $inode_warn ) )
        {
            $status = "WARNING";
        }
        print "OPEN FILES $status allocated: $file->{fhalloc} (inodes: $file->{inalloc})".
              " | fhalloc=$file->{fhalloc};$fh_warn;$fh_crit;$file->{fhmax} ".
              "inalloc=$file->{inalloc};$inode_warn;$inode_crit;$file->{inmax} ".
              "dentries=$file->{dentries}";
    }
    else {
        print "No data";
    }
}

sub check_mem {
    my $lxs = Sys::Statistics::Linux::MemStats->new;
    my $mem = $lxs->get;

    if ( defined( $mem ) ) {
        $status = "OK";

        my ( $mem_crit_pct, $swap_crit_pct ) = split( /,/, $o_critical );
        my ( $mem_warn_pct, $swap_warn_pct ) = split( /,/, $o_warning );

        my $mem_warn_abs  = sprintf("%.2f", $mem_warn_pct  / 100 * $mem->{memtotal});
        my $mem_crit_abs  = sprintf("%.2f", $mem_crit_pct  / 100 * $mem->{memtotal});
        my $swap_warn_abs = sprintf("%.2f", $swap_warn_pct / 100 * $mem->{swaptotal});
        my $swap_crit_abs = sprintf("%.2f", $swap_crit_pct / 100 * $mem->{swaptotal});
        my $real_memused  = $mem->{memused} - $mem->{cached} - $mem->{buffers};

        my $memtotal_mb   = sprintf("%.2f", $mem->{memtotal} / 1024.0);
        my $memused_mb    = sprintf("%.2f", $real_memused / 1024.0);
        my $swaptotal_mb  = sprintf("%.2f", $mem->{swaptotal} / 1024.0);
        my $swapused_mb   = sprintf("%.2f", $mem->{swapused} / 1024.0);

        if ( ( $real_memused >= $mem_crit_abs ) || ( $mem->{swapused} >= $swap_crit_abs ) ) {
            $status = "CRITICAL";
        }
        elsif ( ( $real_memused >= $mem_warn_abs ) || ( $mem->{swapused} >= $swap_warn_abs ) ) {
            $status = "WARNING";
        }

        print "$status - Memory used: ${memused_mb}MB / ${memtotal_mb}MB, ".
              "Swap used: ${swapused_mb}MB / ${swaptotal_mb}MB | ".
              "MemUsed=${real_memused}KB;${mem_warn_abs};${mem_crit_abs};0;$mem->{memtotal} ".
              "MemCached=$mem->{cached}KB ".
              "SwapUsed=$mem->{swapused}KB;${swap_warn_abs};${swap_crit_abs};0;$mem->{swaptotal} ".
              "SwapCached=$mem->{swapcached}KB ".
              "Active=$mem->{active}KB ".
              "Committed=$mem->{committed_as}KB";
    }
    else {
        print "No data";
    }
}

sub check_disk {
    my $lxs  = Sys::Statistics::Linux::DiskUsage->new;
    my $disk = $lxs->get;
    my $return_str = "";
    my $perfdata   = "";

    if ( defined( $disk ) ) {
        $status = "OK";

        if ( !defined($o_pattern) ) { $o_pattern = 'all'; }

        my $checkthis;
        map { $checkthis->{$_}++ } split( /,/, $o_pattern );

        my $crit = 0;    #critical counter
        my $warn = 0;    #warning counter
        foreach my $device ( keys(%$disk) ) {
            my $usage       = $disk->{$device}->{usage};                    # KB
            my $free        = $disk->{$device}->{free};                     # KB
            my $total       = $disk->{$device}->{total};                    # KB
            my $mountpoint  = $disk->{$device}->{mountpoint};
            my $percentused = sprintf( "%.2f", ( $usage / $total ) * 100 );
            my $percentfree = sprintf( "%.2f", ( $free / $total ) * 100 );
            my $MBused      = sprintf( "%.2f", ( $usage / 1024 ) );
            my $MBfree      = sprintf( "%.2f", ( $free / 1024 ) );
            my $MBtotal     = sprintf( "%.2f", ( $total / 1024 ) );
            my $GBused      = sprintf( "%.2f", ( $MBused / 1024 ) );
            my $GBfree      = sprintf( "%.2f", ( $MBfree / 1024 ) );
            my $GBtotal     = sprintf( "%.2f", ( $MBtotal / 1024 ) );

            if (   defined( $checkthis->{$mountpoint} )
                || defined( $checkthis->{all} ) )
            {
                if ( $o_unit =~ /\%/ ) {
                    if    ( $percentfree <= $o_critical ) { $crit++; }
                    elsif ( $percentfree <= $o_warning )  { $warn++; }
                    $return_str .= " "
                      . $mountpoint . " "
                      . $usage
                      . "KB on "
                      . $total
                      . "KB ($percentfree% free)";
                    $perfdata .= " " . $mountpoint . "=" . $usage . "KB";
                }
                elsif ( $o_unit =~ /KB/i ) {
                    if    ( $free <= $o_warning )  { $warn++; }
                    elsif ( $free <= $o_critical ) { $crit++; }
                    $return_str .= " "
                      . $mountpoint . " "
                      . $usage
                      . "KB on "
                      . $total
                      . "KB ($percentfree% free)";
                    $perfdata .= " " . $mountpoint . "=" . $usage . "KB";
                }
                elsif ( $o_unit =~ /MB/i ) {
                    if    ( $MBfree <= $o_warning )  { $warn++; }
                    elsif ( $MBfree <= $o_critical ) { $crit++; }
                    $return_str .= " "
                      . $mountpoint . " "
                      . $MBused
                      . "MB on "
                      . $MBtotal
                      . "MB ($percentfree% free)";
                    $perfdata .= " " . $mountpoint . "=" . $MBused . "MB";
                }
                elsif ( $o_unit =~ /GB/i ) {
                    if    ( $GBfree <= $o_warning )  { $warn++; }
                    elsif ( $GBfree <= $o_critical ) { $crit++; }
                    $return_str .= " "
                      . $mountpoint . " "
                      . $GBused
                      . "GB on "
                      . $GBtotal
                      . "GB ($percentfree% free)";
                    $perfdata .= " " . $mountpoint . "=" . $GBused . "GB";
                }
            }
        }

        if    ( $crit > 0 ) { $status = "CRITICAL"; }
        elsif ( $warn > 0 ) { $status = "WARNING"; }
    }
    print "DISK $status used : $return_str|$perfdata";
}

sub check_io {
    my $disk;
    my $lxs;
    if(defined($o_statfile)) {
        $lxs = Sys::Statistics::Linux::DiskStats->new(initfile => $o_statfile);
        $lxs->init;
        $disk = $lxs->get;
    }
    else {
        $lxs = Sys::Statistics::Linux::DiskStats->new;
        $lxs->init;
        sleep $o_sleep;
        $disk = $lxs->get;
    }

    my $return_str = "";
    my $perfdata   = "";

    if ( defined( $disk ) ) {
        $status = "OK";

        if ( !defined($o_pattern) ) { $o_pattern = 'all'; }

        my $checkthis;
        map { $checkthis->{$_}++ } split( /,/, $o_pattern );

        my ( $read_crit, $write_crit ) = split( /,/, $o_critical );
        my ( $read_warn, $write_warn ) = split( /,/, $o_warning );

        my $crit = 0;    #critical counter
        my $warn = 0;    #warning counter
        foreach my $device ( keys(%$disk) ) {
            my $rdreq  = $disk->{$device}->{rdreq};
            my $wrtreq = $disk->{$device}->{wrtreq};
            my $rdbyt  = $disk->{$device}->{rdbyt};
            my $wrtbyt = $disk->{$device}->{wrtbyt};

            if (   defined( $checkthis->{$device} )
                || defined( $checkthis->{all} ) )
            {
                if ( $o_unit =~ /BYTES/i ) {
                        if (   ( $rdbyt >= $read_crit )
                            || ( $wrtbyt >= $write_crit ) )
                        {
                            $crit++;
                        }
                        elsif (( $rdbyt >= $read_warn )
                            || ( $wrtbyt >= $write_warn ) )
                        {
                            $warn++;
                        }
                        $perfdata .= " "
                            . $device
                            . "_read=${rdbyt}B;$read_warn;$read_crit "
                            . $device
                            . "_write=${wrtbyt}B;$write_warn;$write_crit "
                            . $device
                            . "_read_req=$rdreq "
                            . $device
                            . "_write_req=$wrtreq";
                } else {
                        if (   ( $rdreq >= $read_crit )
                            || ( $wrtreq >= $write_crit ) )
                        {
                            $crit++;
                        }
                        elsif (( $rdreq >= $read_warn )
                            || ( $wrtreq >= $write_warn ) )
                        {
                            $warn++;
                        }
                        $perfdata .= " "
                            . $device
                            . "_read_req=$rdreq;$read_warn;$read_crit "
                            . $device
                            . "_write_req=$wrtreq;$write_warn;$write_crit "
                            . $device
                            . "_read=${rdbyt}B "
                            . $device
                            . "_write=${wrtbyt}B";
                }
                my $rdbyt_kb  = sprintf("%.2f", $rdbyt / 1024);
                my $wrtbyt_kb = sprintf("%.2f", $wrtbyt / 1024);
                $return_str .= " "
                    . "$device: "
                    . "${rdbyt_kb}kB/sec read, "
                    . "${wrtbyt_kb}kB/sec write, "
                    . "${rdreq}req/sec read, "
                    . "${wrtreq}req/sec write;";
            }


        }
        if    ( $crit > 0 ) { $status = "CRITICAL"; }
        elsif ( $warn > 0 ) { $status = "WARNING"; }

        if ( length($return_str) == 0 ) {
            $status = "CRITICAL";
            print "$status - Block device not found: $o_pattern";
        } else {
            $return_str = substr($return_str, 0, -1);
            print "$status -$return_str |$perfdata";
        }
    }
}

sub check_net {
    my $net;
    my $lxs;
    if(defined($o_statfile)) {
        $lxs = Sys::Statistics::Linux::NetStats->new(initfile => $o_statfile);
        $lxs->init;
        $net = $lxs->get;
    }
    else {
        $lxs = Sys::Statistics::Linux::NetStats->new;
        $lxs->init;
        sleep $o_sleep;
        $net = $lxs->get;
    }

    my $return_str = "";
    my $perfdata   = "";
    if ( defined( $net ) ) {
        $status = "UNKOWN";
        if ( !defined($o_pattern) ) { $o_pattern = 'all'; }

        my $checkthis;
        map { $checkthis->{$_}++ } split( /,/, $o_pattern );

        my ($traf_crit, $pkg_crit, $err_crit) = split( /,/, $o_critical);
        my ($traf_warn, $pkg_warn, $err_warn) = split( /,/, $o_warning);

        # unit handling
        my $unit_calc;
        if($o_unit eq "b")    { $unit_calc = 8;          }
        elsif($o_unit eq "B") { $unit_calc = 1;          }
        elsif($o_unit eq "Kb"){ $unit_calc = 0.008;      }# byte -> Kbit  | x * 8 / 1000
        elsif($o_unit eq "KB"){ $unit_calc = 0.001;      }# byte -> Kbyte | x / 1000
        elsif($o_unit eq "Mb"){ $unit_calc = 0.000008;   }# byte -> Mbit  | x * 8 / 1000000
        elsif($o_unit eq "MB"){ $unit_calc = 0.000001;   }# byte -> Mbyte | x / 10000000
        elsif($o_unit eq "Gb"){ $unit_calc = 0.000000008;}# byte -> Mbyte | x * 8 / 10000000000
        elsif($o_unit eq "GB"){ $unit_calc = 0.000000001;}# byte -> Mbyte | x / 10000000000
        else {
            $o_unit = "Kb";
            $unit_calc = 0.008;
        }
        
        my $crit = 0;    #critical counter
        my $warn = 0;    #warning counter
        foreach my $device ( keys(%$net) ) {
            # the returns are bytes/sec or packages/sec
            my $txbyt   = $net->{$device}->{txbyt} * $unit_calc;
            my $rxerrs  = $net->{$device}->{rxerrs};
            my $txerrs  = $net->{$device}->{txerrs};
            my $txdrop  = $net->{$device}->{txdrop};
            my $txcolls = $net->{$device}->{txcolls};
            my $rxbyt   = $net->{$device}->{rxbyt} * $unit_calc;
            my $rxdrop  = $net->{$device}->{rxdrop};
            my $rxpcks  = $net->{$device}->{rxpcks};
            my $txpcks  = $net->{$device}->{txpcks};

            if (   defined( $checkthis->{$device} )
                || defined( $checkthis->{all} ) )
            {
                $status = "OK";
                my $tmp_str;

                if(defined($traf_crit) && ($txbyt >= $traf_crit || $rxbyt >= $traf_crit)) {
                    $tmp_str .= "traffic critical (in:${rxbyt}$o_unit/s out:${txbyt}$o_unit/s out:) ";
                    $crit++;
                }
                elsif(defined($traf_warn) && ($txbyt >= $traf_warn || $rxbyt >= $traf_warn)) {
                    $tmp_str .= "traffic warning (in:${rxbyt}$o_unit/s out:${txbyt}$o_unit/s out:) ";
                    $warn++;
                }

                if(defined($pkg_crit) && ($txpcks >= $pkg_crit || $rxpcks >= $pkg_crit)) {
                    $tmp_str .= "packages critical (in:$rxpcks/s out:$txpcks/s) ";
                    $crit++;
                }
                elsif(defined($pkg_warn) && ($txpcks >= $pkg_warn || $rxpcks >= $pkg_warn)) {
                    $tmp_str .= "packages warning (in:$rxpcks/s out:$txpcks/s) ";
                    $warn++;
                }

                if(defined($err_crit) && ($rxerrs >= $err_crit || $txerrs >= $err_crit)) {
                    $tmp_str .= "error counters critical (in:$rxerrs/s out:$txerrs/s) ";
                    $crit++;
                }
                elsif(defined($err_warn) && ($rxerrs >= $err_warn || $txerrs >= $err_warn)) {
                    $tmp_str .= "error counters warning (in:$rxerrs/s out:$txerrs/s) ";
                    $warn++;
                }

                $return_str .= "$device: ";
                if($tmp_str) { $return_str .= $tmp_str." "; }
                else         { $return_str .= "OK "; }

                $perfdata   .= " "
                  ."${device}_rxbyt=$rxbyt$o_unit;$traf_warn;$traf_crit;0 "
                  ."${device}_txbyt=$txbyt$o_unit;$traf_warn;$traf_crit;0 "
                  ."${device}_rxpcks=$rxpcks;$pkg_warn;$pkg_crit;0 "
                  ."${device}_txpcks=$txpcks;$pkg_warn;$pkg_crit;0 "
                  ."${device}_rxerrs=$rxerrs;$err_warn;$err_crit;0 "
                  ."${device}_txerrs=$txerrs;$err_warn;$err_crit;0 "
                ;
            }
        }

        if    ( $crit > 0 ) { $status = "CRITICAL"; }
        elsif ( $warn > 0 ) { $status = "WARNING"; }
        print "NET USAGE $status $return_str|$perfdata";
    }
}

sub check_load {
    my $lxs = Sys::Statistics::Linux::LoadAVG->new;
    my $load = $lxs->get;

    if ( defined( $load) ) {
        $status = "OK";
        my ( $warn_1, $warn_5, $warn_15 ) = split( /,/, $o_warning );
        my ( $crit_1, $crit_5, $crit_15 ) = split( /,/, $o_critical );

        if (   ( $load->{avg_1} >= $crit_1 )
            || ( $load->{avg_5} >= $crit_5 )
            || ( $load->{avg_15} >= $crit_15 ) )
        {
            $status = "CRITICAL";
        }
        elsif (( $load->{avg_1} >= $warn_1 )
            || ( $load->{avg_5} >= $warn_5 )
            || ( $load->{avg_15} >= $warn_15 ) )
        {
            $status = "WARNING";
        }
        print "LOAD AVERAGE $status : $load->{avg_1},$load->{avg_5},$load->{avg_15} | ".
              "load1=$load->{avg_1};$warn_1;$crit_1;0 ".
              "load5=$load->{avg_5};$warn_5;$crit_5;0 ".
              "load15=$load->{avg_15};$warn_15;$crit_15;0";

    }
    else {
        print "No data";
    }
}

sub check_paging {
    my $page;
    my $lxs;
    if(defined($o_statfile)) {
        $lxs = Sys::Statistics::Linux::PgSwStats->new(initfile => $o_statfile);
        $lxs->init;
        $page = $lxs->get;
    }
    else {
        $lxs = Sys::Statistics::Linux::PgSwStats->new;
        $lxs->init;
        sleep $o_sleep;
        $page = $lxs->get;
    }

    if ( defined( $page ) ) {
        $status = "OK";
        my ( $warn_in, $warn_out ) = split( /,/, $o_warning );
        my ( $crit_in, $crit_out ) = split( /,/, $o_critical );
        if (   ( $page->{pgpgin} >= $crit_in )
            || ( $page->{pgpgout} >= $crit_out ) )
        {
            $status = "CRITICAL";
        }
        elsif (( $page->{pgpgin} >= $warn_in )
            || ( $page->{pgpgout} >= $warn_out ) )
        {
            $status = "WARNING";
        }
        print "Paging $status : In:$page->{pgpgin}, Out:$page->{pgpgout} |".
              "pgpgin=$page->{pgpgin};$warn_in;$crit_in;0 ".
              "pgpgout=$page->{pgpgout};$warn_out;$crit_out;0 ";
    }
    else {
        print "No data";
    }
}

sub check_swapping {
    my $page;
    my $lxs;
    if(defined($o_statfile)) {
        $lxs = Sys::Statistics::Linux::PgSwStats->new(initfile => $o_statfile);
        $lxs->init;
        $page = $lxs->get;
    }
    else {
        $lxs = Sys::Statistics::Linux::PgSwStats->new;
        $lxs->init;
        sleep $o_sleep;
        $page = $lxs->get;
    }

    if ( defined( $page ) ) {
        $status = "OK";
        my ( $warn_in, $warn_out ) = split( /,/, $o_warning );
        my ( $crit_in, $crit_out ) = split( /,/, $o_critical );
        if (   ( $page->{pswpin} >= $crit_in )
            || ( $page->{pswpout} >= $crit_out ) )
        {
            $status = "CRITICAL";
        }
        elsif (( $page->{pswpin} >= $warn_in )
            || ( $page->{pswpout} >= $warn_out ) )
        {
            $status = "WARNING";
        }
        print "Swapping $status : In:$page->{pswpin}, Out:$page->{pswpout} |".
              "pgpgin=$page->{pswpin};$warn_in;$crit_in;0 ".
              "pgpgout=$page->{pswpout};$warn_out;$crit_out;0 ";
    }
    else {
        print "No data";
    }
}

sub usage {
    print "Usage: $0 -C|-P|-M|-N|-D|-I|-L|-F|-S|-O|-w -p <pattern> ".
          "-w <warning> -c <critical> [-s <sleep>] [-u <unit>] [-f <filename>] [-V] [-h]\n";
}

sub version {
    print "$script_name v$script_version\n";
}

sub help {
    version();
    usage();

    print <<HELP;
    -h, --help
          print this help message
    -C, --cpu=CPU USAGE
    -P, --procs
    -M, --memory=MEMORY USAGE
    -N, --network=NETWORK USAGE
    -D, --disk=DISK USAGE
    -I, --io=DISK IO USAGE
    -L, --load=LOAD AVERAGE
    -F, --file=FILE STATS
    -S, --socket=SOCKET STATS
    -W, --paging=PAGING STATS (disk/mem io)
    -O, --swapping=SWAPPING STATS (swap/mem io)
    -p, --pattern
          eth0,eth1...sda1,sda2.../usr,/tmp
    -w, --warning
    -c, --critical
          for paging/swapping: in,out
          for netstat: bytes,packages,errors (in the unit specified)
    -s, --sleep
          seconds to sleep for measuring
    -u, --unit
          %, KB, MB or GB left on disk usage, default : MB
          REQS OR BYTES on disk io statistics, default : REQS
          b B Kb KB Mb MB Gb GB for net stats, default : MB
    -f, --statfile
          used for delta generation of net stats, sleep is
          not used when specified (delta is calcutated with
          the stat data of the last run)
    -V, --version
          print version number

    some examples:
    Memory usage                    : perl check_linux_stats.pl -M -w 90 -c 95
    Cpu usage                       : perl check_linux_stats.pl -C -w 90 -c 95 -s 5
    Disk usage                      : perl check_linux_stats.pl -D -w 95 -c 100 -u % -p /tmp,/usr,/var
    Load average                    : perl check_linux_stats.pl -L -w 10,8,5 -c 20,18,15
    Paging statistics               : perl check_linux_stats.pl -W -w 10,1000 -c 20,2000 -s 3
    Swapping statistics             : perl check_linux_stats.pl -O -w 10,50 -c 20,100 -s 3
    Process statistics              : perl check_linux_stats.pl -P -w 100 -c 200
    I/O statistics on disk device   : perl check_linux_stats.pl -I -w 95 -c 100 -p sda1,sda4,sda5,sda6
    Network usage    with statfile  : perl check_linux_stats.pl -N -w 1000,500,1 -c 20000,1000,10 -p eth0 -u Kb -f /tmp/linuxstat_net
                     with sleeptime : perl check_linux_stats.pl -N -w 1000,500,1 -c 20000,1000,10 -p eth0 -u Kb -s 10
    Processes virtual memory        : perl check_linux_stats.pl -T -w 9551820 -c 9551890 -p /var/run/sendmail.pid
HELP
}

sub check_options {
    Getopt::Long::Configure("bundling");
    GetOptions(
        'h'          => \$o_help,
        'help'       => \$o_help,
        's:i'        => \$o_sleep,
        'sleep:i'    => \$o_sleep,
        'C'          => \$o_cpu,
        'cpu'        => \$o_cpu,
        'P'          => \$o_procs,
        'procs'      => \$o_procs,
        'T'          => \$o_process,
        'top'        => \$o_process,
        'M'          => \$o_mem,
        'memory'     => \$o_mem,
        'N'          => \$o_net,
        'network'    => \$o_net,
        'D'          => \$o_disk,
        'disk'       => \$o_disk,
        'I'          => \$o_io,
        'io'         => \$o_io,
        'L'          => \$o_load,
        'load'       => \$o_load,
        'F'          => \$o_file,
        'file'       => \$o_file,
        'f:s'        => \$o_statfile,
        'statfile:s' => \$o_statfile,
        'S'          => \$o_socket,
        'socket'     => \$o_socket,
        'W'          => \$o_paging,
        'paging'     => \$o_paging,
        'O'          => \$o_swapping,
        'swapping'   => \$o_swapping,
        'V'          => \$o_version,
        'version'    => \$o_version,
        'p:s'        => \$o_pattern,
        'pattern:s'  => \$o_pattern,
        'w:s'        => \$o_warning,
        'warning:s'  => \$o_warning,
        'c:s'        => \$o_critical,
        'critical:s' => \$o_critical,
        'u:s'        => \$o_unit,
        'unit:s'     => \$o_unit
    );

    if ( defined($o_help) ) {
        help();
        exit $ERRORS{'UNKNOWN'};
    }

    if ( defined($o_version) ) {
        version();
        exit $ERRORS{'UNKNOWN'};
    }
}

# vim: set ts=4 sw=4 expandtab :

