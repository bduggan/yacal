#!/usr/bin/perl -wT

=head1 NAME

    Yacal -- yet another calendar program

=head1 DESCRIPTION

    A simple web calendar.  This program is available for download from <http://yacal.sf.net>.

=head1 AUTHOR

    Brian Duggan <bduggan at matatu.org>

=head1 COPYING

    Copyright (C) 2005 Brian Duggan

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA

=head1 TODO

    Recurring events
    Exporting as ical, csv, pdf 
    Importing from various formats.
    Serializing with JSON::XS

=cut

use strict;
use CGI;
$CGI::POST_MAX = 1024x2;
use CGI::Carp qw(fatalsToBrowser);
use Time::Piece;
use Time::Piece::Month;

use constant DATAFILE => '/set/me/to/a/directory/writeable/only/by/web/cal_data.dmp';
use constant TITLE => "<your name here>'s calendar";
$Yacal::Title = TITLE;
$Yacal::VERSION = '0.05';

#
# Time::Piece -- some extra handy functions
#
package Time::Piece;
sub to_cgi {    
    my $d = shift;
    return join '&','month='.$d->mon,"year=".$d->year,"day=".$d->mday;
}

sub from_cgi {
    my ($self,$q) = @_; 
    return $self->strptime(sprintf('%04d-%02d-%02d',map $q->param($_), qw(year month day)),'%Y-%m-%d');
}

#
# Yacal::Event -- represents an event on the calendar
#
package Yacal::Event;
use XML::RSS;

# Constructor, using cgi paramaters.
sub from_cgi {
    my ($class, $q) = @_;
    # minutes left blank by accident?
    $q->param('start_minute', '00') if $q->param('start_hour') && $q->param('start_minute') eq '1';
    $q->param('end_minute',   '00') if $q->param('end_hour')   && $q->param('end_minute')   eq '1';
    my $start = sprintf('%04d-%02d-%02d %02d:%02d',
                        map $q->param($_), qw(year month day start_hour start_minute));
    my $end = sprintf('%04d-%02d-%02d %02d:%02d', map $q->param($_),
              qw(year month day end_hour end_minute));
    bless {
            subject   => $q->param('subject'),
            fullstart => Time::Piece->strptime($start, '%Y-%m-%d %H:%M'),
            fullend   => Time::Piece->strptime($end, '%Y-%m-%d %H:%M'),
    }, $class;
}
# Set the CGI paramaters based on this event. (midnight is blank)
sub to_cgi {    # doesn't set year/month/day since these'll already be set.
    my ($self, $q) = @_;
    $q->param('subject',      $self->subject);
    $q->param('start_hour',   '');
    $q->param('start_minute', '');
    if (my ($h, $m) = $self->start_time) {
        $q->param('start_hour',   $h);
        $q->param('start_minute', $m);
    }
    if (my ($h, $m) = $self->end_time) {
        $q->param('end_hour',   $h);
        $q->param('end_minute', $m);
    }
}

# Some accessors
sub subject  { $_[0]->{subject} }
sub fullstart {  # will return 12:01 am when there's no start time
    my $self = shift;
    return $self->{fullstart} if $self->{fullstart};
    $self->{fullstart} =
      Time::Piece->strptime($self->{date} . ' ' . $self->{start_time}, '%Y-%m-%d %H:%M');
    return $self->{fullstart};
}
sub fullend {  # will return 12:01 am when there's no end time
    my $self = shift;
    return $self->{fullend} if $self->{fullend};
    $self->{fullend} =
      Time::Piece->strptime($self->{date} . ' ' . $self->{end_time}, '%Y-%m-%d %H:%M');
    return $self->{fullend};
}
sub start_time {
    my $h_m = $_[0]->fullstart->strftime("%H:%M");
    return ($h_m eq '00:01' ? '' : $h_m) unless wantarray;
    return ($h_m eq '00:01' ? () : (split /:/, $h_m));
}
sub end_time {
    my $h_m = $_[0]->fullend->strftime("%H:%M");
    return ($h_m eq '00:01' ? '' : "-".$h_m) unless wantarray;
    return ($h_m eq '00:01' ? () : (split /:/, $h_m));
}
sub date     { Time::Piece->strptime($_[0]->fullstart->ymd,'%Y-%m-%d') }

# How Data::Dumper should store events :
sub freezer {
    my $self = shift;
    return bless {
                   subject    => $self->{subject},
                   date       => $self->fullstart->ymd,
                   start_time => $self->fullstart->strftime('%H:%M'),
                   end_time   => $self->fullend->strftime('%H:%M')
      },
      (ref $self || $self);
}

#
# Yacal -- represents the calendar as a whole
#
package Yacal;
use Data::Dumper;
use Time::Seconds;
$Data::Dumper::Indent = 1;
$Data::Dumper::Freezer = 'freezer';
$Data::Dumper::Sortkeys = 1;

# Constructor : reads in the file if it exists.
sub new {
    my ($class, $filename) = @_;
    bless {
            filename => $filename,
            data     => (-e $filename ? _read($filename) : {})
      },
      $_[0];
}

# Read the data from the file.
sub _read {
    my $filename = shift;
    my $data     = eval "do '$filename'";
    die "error reading $filename : $@" if $@;
    die "corrupt file $filename" unless (ref($data) && ref($data) eq 'HASH');
    return $data;
}

# Write out the data atomically (using a write and rename).
sub write {
    my $self = shift;
    my $tmpfile = $self->{filename}."-$$-".time.(int rand 1000);
    open FP, ">$tmpfile" or die "error writing to $tmpfile : $!";
    print FP Dumper($self->{data});
    close FP or die "couldn't close FP : $!";
    rename $tmpfile,$self->{filename} or 
        die "error renaming $tmpfile to $self->{filename} : $!";
}

# Return a list of the events on a day
sub events { 
    my ($self,$day) = @_;
    return $self->{data}{$day->ymd};
}

# Return an event with a given index in the range (0..# of events - 1).
sub event { 
    my ($self,$day,$event_index) = @_;
    return $self->{data}{$day->ymd}[$event_index];
}

# Add an event to the calendar.  Returns the day, and index.
sub add_event {
    my ($self, $event) = @_;
    # Insert into the proper position ordered by start date, subject
    my @events = @{ $self->events( $event->date ) || [] };
    my @before;
    push @before, shift @events while @events
      && (   $events[0]->fullstart->epoch <=> $event->fullstart->epoch
          || $events[0]->subject cmp $event->subject) < 0;
    $self->{data}{ $event->date->ymd } = [@before, $event, @events];
    return ($event->fullstart, scalar(@before));
}

# Remove the selected event from the array of events for that day.
sub delete_event {
    my ($self, $selected_day, $selected_event) = @_;
    my $events = $self->events($selected_day);
    @$events = @$events[0..$selected_event-1,$selected_event+1..$#$events];
    delete $self->{data}{ $selected_day->ymd } if @$events==0;
}

# An rss feed
sub display_rss {
    my $self = shift;
    my $rss = XML::RSS->new(version => '0.91');
    my $base = join '','http://',$ENV{SERVER_NAME},
                ($ENV{SERVER_PORT}==80 ? () : ( ':' , $ENV{SERVER_PORT})), 
                $ENV{SCRIPT_NAME};

    $rss->channel(
                title => $Yacal::Title,
                link => $base,
                language    => 'en',
                description => $Yacal::Title." events",
                );
    my $now = Time::Piece->new();
    my $today = Time::Piece->strptime($now->ymd,'%Y-%m-%d');
    for (sort { $a cmp $b } keys %{ $self->{data} }) {
        my $events = $self->{data}{$_};
        my $tp = Time::Piece->strptime($_, '%Y-%m-%d');
        next unless $tp >= $today;
        my $title = $tp == $today ? "Today,"
                  : $tp == ($today + ONE_DAY) ? "Tomorrow,"
                  : $tp->strftime("%A,");
        $title .= $tp->strftime(" %d %B %Y");
        $rss->add_item(
            title => $title,
            link => $base.'?'.$now->to_cgi,
            description => (join "\n",
                map $_->start_time . ' ' . $_->subject,
                sort {$a->start_time cmp $b->start_time}
                @$events));
    }
    print "Content-type: text/xml\n\n".$rss->as_string;
}

# A text file, compatible with install-datebook(1) format (as of pilot-link v 0.12.0)
sub display_txt { # (sadly untested since install-datebook seg faults on me)
    my $self = shift;
    my $q = shift;
    my @records;
    for my $event (map {@{$self->{data}{$_}}} sort { $a cmp $b } keys %{ $self->{data} }) {
        my $start = ($event->start_time ? $event->fullstart : $event->date);
        my $end =
           $event->end_time ? $event->fullend
          : $event->start_time ? $event->fullstart
          : $event->date;
        my $description = $event->subject;
        $description =~ tr/\t\n/  /;
        push @records,  
            join "\t",  # "August 11, 1997 0800 GMT+300"
                $start->strftime('%B %d, %Y %H%M GMT%z'),
                $end->strftime('%B %d, %Y %H%M GMT%z'),
                ' ', # no alarm, thanks
                $description;
    }
    print $q->header(-content_type => 'text/plain');
    print join "\n", @records;
}

# main program 
package main;

our @months = Time::Piece::mon_list;

&main;

sub print_cell {
    my ($date,$e,$q,$selected_day,$selected_event) = @_;
    my $highlight = ($date->ymd eq $selected_day->ymd ? $selected_event : -1);
    print '<td class="'.($_->mdy eq localtime->mdy ? 'today">' : 'day">');
    unless ($e) {
        print '<p>&nbsp;<br>&nbsp;<br>&nbsp;<br></p></td>';
        return;
    }
    my $i = 0;
    for (@$e) {
        print $q->li( $i==$highlight ? {-class=>'selected_event'} : (),
              $q->a({-href=>$ENV{SCRIPT_NAME}."?selected_event=".($i++).'&'.$date->to_cgi},
                    $_->start_time.$_->end_time.'&nbsp;'.$q->escapeHTML($_->subject)));
    }
    print "</td>";
}

sub print_cal {
    my ($q,$yc,$selected_day,$selected_event) = @_;
    my $this_month = Time::Piece::Month->new($selected_day->ymd);
    my @days = $this_month->wraparound_dates;
    print $q->div({-align=>'center'},
        $q->a({-href=>$ENV{SCRIPT_NAME}."?".$this_month->prev_month->start->to_cgi}," <-- ").
        $q->b($this_month->start->fullmonth.' '.$this_month->start->year).
        $q->a({-href=>$ENV{SCRIPT_NAME}."?".$this_month->next_month->start->to_cgi}," --> "));
    print $q->div({-class=>'topleft'},
            map 
            { $q->a({-href=>$ENV{SCRIPT_NAME}."?action=$_"},$_) } qw(rss txt)
            );
    print
        $q->div({-class=>'topright'},
            $q->a({-href=>$ENV{SCRIPT_NAME}."?".localtime->to_cgi},localtime->strftime('%A, %B %d'))
        );
     
    print qq|<table border="0" cellpadding="1" cellspacing="1" width="100%"><tbody><tr>|.
        (join " ", map "<th>$_</th> ",qw(sun mon tue wed thu fri sat)).
        qq|</tr><tr>|;
    for (@days) {
        if ($_->day eq 'Sun') { # Print a row of date labels
            print "</tr><tr>";
            my $d = $_;
            do {
                print $q->td({class=>'date',($selected_day->ymd eq $d->ymd ? (-class=>'selected') : ()),-width=>'14%'},
                            $q->a({-href=>$ENV{SCRIPT_NAME}."?".$d->to_cgi}),
                                $d->mday);
                $d += 60*60*24;
            } until $d->day eq 'Sun';
            print "</tr><tr>";
        }
        my $e = $yc->events($_);
        print_cell($_,$e,$q,$selected_day,$selected_event);
        print "</tr><tr>" if $_->day eq 'Sat';
    }
    print "</tr></tbody></table>";
}

sub print_form {
    my ($q,$yc,$selected_day,$selected_event) = @_;
    $yc->event($selected_day,$selected_event)->to_cgi($q) if $selected_event!=-1;
    print $q->start_form(-method=>'POST',-action=>$ENV{SCRIPT_NAME}),
        $q->table({-align=>'center'},$q->Tr({-valign=>'center'},$q->td([
        $q->popup_menu(-name=>'month',-values=>[1..12], -labels => { map { ($_ => $months[$_-1]) } (1..12) }, -default=>localtime->mon).
        $q->popup_menu(-name=>'day',-values=>[1..31],-default=>localtime->mday).
        $q->popup_menu(-name=>'year',-values=>[localtime->year..localtime->year+10]).
        $q->br."Start :".
        $q->popup_menu(-name=>'start_hour',-values=>['',"00".."23"],-default=>'').
        $q->popup_menu(-name=>'start_minute',-values=>['1',grep $_%5==0,'00'..'59'],-default=>'1',-labels=>{1=>''}).
        $q->br."End : ".
        $q->popup_menu(-name=>'end_hour',-values=>['',"00".."23"],-default=>'').
        $q->popup_menu(-name=>'end_minute',-values=>['1',grep $_%5==0,'00'..'59'],-default=>'1',-labels=>{1=>''}),
        $q->textarea(-rows => 4, -columns => 60, -name=>'subject',-value=>''),
        $q->submit(-name=>'action',-value=>'add').
        ($selected_event == -1 ? '' :
            $q->br.$q->submit(-name=>'action',-value=>'edit').
            $q->br.$q->submit(-name=>'action',-value=>'delete'))
        ]))),
        $q->hidden(-name=>'selected_ymd', -value=>$selected_day->ymd,-override=>1),
        $q->hidden(-name=>'selected_event',-value=>$selected_event,-override=>1);
        $q->end_form;
}

sub main {
    my $q = new CGI;
    my $selected_event = -1;
    my $selected_day;
    $selected_event = $q->param('selected_event') if defined($q->param('selected_event'));
    my $action = $q->param('action') || 'view';
    $selected_day =
        $action =~ /edit|delete/ ? Time::Piece->strptime($q->param('selected_ymd'), '%Y-%m-%d')
      : $q->param('day')         ? Time::Piece->from_cgi($q)
      : localtime;

    my $yc = Yacal->new(DATAFILE);
    for ($action) {
        /add/         and ($selected_day, $selected_event) = $yc->add_event(Yacal::Event->from_cgi($q)); 
        /edit|delete/ and $yc->delete_event($selected_day,$selected_event);
        /edit/        and ($selected_day, $selected_event) = $yc->add_event(Yacal::Event->from_cgi($q));
        /delete/      and $selected_event = -1;;
        /add|edit|delete/ and $yc->write;
        /rss/         and do { $yc->display_rss($q); exit; };
        /txt/         and do { $yc->display_txt($q); exit; };
    }

    my $style = join '', <DATA>;
    print $q->header,
          qq|<html><head><title>$Yacal::Title</title></head><body><style>$style</style>|;
    print_cal($q,$yc,$selected_day,$selected_event);
    print $q->hr;
    print_form($q,$yc,$selected_day,$selected_event);
    print $q->p({-align=>'right'},'<a href="http://yacal.sf.net">Yacal</a>');
    print $q->end_html;
}

__DATA__
body  {
	background-color : #ffffff;
	font-family :  Helvetica;
	font-size : 10pt;
	color : #000000;
}

a {
	text-decoration: none;
    color: #000000;
}

th {
    font-size : 10pt;
	font-weight : bold;
	background-color : #FF9900;
}

td {
   font-size : 10pt;
   vertical-align : top; 
   text-align : left; 
}

.day { background : #EEEEEE; }
.today { background : #FFCC66; }
.date { background : #999999; 
        text-align : center;
    }
.selected {
     background : lightgreen; 
     text-align : center;
    }
.selected_event { background : lightgreen; }

.topright {
  position: absolute;
  top: 8px;
  right: 10px;
}

.topleft {
  position: absolute;
  top: 8px;
  left: 10px;
}



