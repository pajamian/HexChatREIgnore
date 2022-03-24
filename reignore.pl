#
# hexchat plugin which allows regex /ignore patterns.
#
# Copyright (C) 2022  Peter Ajamian
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
# Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301,
# USA.
#
use strict;
use warnings;
use HexChat qw(:constants :hooks register get_info strip_code);
use JSON qw(to_json from_json);
use Text::Glob qw(match_glob);
$Text::Glob::strict_wildcard_slash = 0;

my $scriptname = 'REIgnore';
my $version = '0.2';
my $description = 'A HexChat plugin which allows regex /ignore patterns';

my $help_text = <<EOH;
$scriptname version $version

$description

Synopsis: /REIGNORE [ADD|REMOVE|LIST|CLEAR] <mask> <types> <options> <pattern>

ADD - Add a new /reignore entry. This is the default action.

REMOVE - REMOVE a matching /reignore entry.

LIST - List all matching /reignore entries.

CLEAR - Clear all /reignore entries, functionally equivalent to
	`/reignore REMOVE *!*@*`

mask: hostmask to match for the ignore.

types: One of PRIV, CHAN, NOTI, CTCP, DCC, INVI, ALL

options: NOSAVE, QUIET.

pattern: A regular expression pattern that must be matched for a message to be
	ignored.

This plugin requires: perl, JSON, Text::Glob
EOH

# Memory storage of the ignore list
my $ignore_path = get_info('configdir') . '/reignore.json';
my @ignore_list;

# Load ignore list.
sub load_ignore_list {
    my $list = [];
    if (-r $ignore_path) {
	open(my $fh, '<', $ignore_path) or
	    die "Cannot open $ignore_path for reading: $!";
	local $/;
	$list = from_json(<$fh>);
	close $fh;
    }

    # Remove any items from ignore_list that are not marked as NOSAVE before
    # merging it with the saved list.
    @ignore_list = grep { $_->{NOSAVE} } @ignore_list if @ignore_list;
    push @ignore_list, @$list;
}

# Save the ignore list to disk.
sub save_ignore_list {
    # Remove entries from the list that are tagged as NOSAVE.
    my @tosave = grep { !$_->{NOSAVE} } @ignore_list;
    open(my $fh, '>', $ignore_path) or
	die "Can't open $ignore_path for writing: $!";
    print $fh to_json(\@tosave), "\n";
    close $fh;
}

hook_command('REIGNORE', sub {
    my ($args, $remain) = @_;
    # We already know the command
    shift @$args;
    shift @$remain;

    # Find the sub-command.
    my %commands = (
	ADD => sub {
	    my @default = qw(PRIV CHAN NOTI);
	    my ($mask, $opts, $pattern) = @_;

	    # Make sure the pattern compiles as a regex.
	    if ($pattern && !eval{qr/$pattern/}) {
		HexChat::printf('Not a valid regex: %s %s', $pattern, $@);
		return;
	    }

	    $mask ||= '*!*@*';
	    $mask .= '!*@*' unless $mask =~ /!/;

	    # Check to see if we have any types other than NOSAVE or QUIET.
	    # If so we don't use the default types.
	  OPTCHECK: for (@$opts) {
	      next if $_ eq 'NOSAVE';
	      next if $_ eq 'QUIET';
	      @default = ();
	      last;
	  }
	    push @$opts, @default;

	    my $entry = {
		mask => $mask,
		pattern => $pattern,
	    };
	    @{$entry}{@$opts} = (\1) x scalar(@$opts);
	    push @ignore_list, $entry;
	    if (!$entry->{NOSAVE}) {
		save_ignore_list();
	    }
	    HexChat::print("$mask added to pattern ignore list.")
		unless $entry->{QUIET};
	},
	REMOVE => sub {
	    my ($mask, $opts, $pattern) = @_;
	    $mask .= '!*@*' unless $mask =~ /!/;

	    # Loop through and find matching entries
	    my @optlist = qw(PRIV CHAN NOTI CTCP DCC INVI);

	    my $save;
	    my $matches = 0;
	    my $partial = 0;
	    my $quiet;
	    my @new_list = grep {
		my $eldel;
		my %optskept;
		@optskept{@optlist}=@{$_}{@optlist};
	      LIST_LOOP: {
		  next LIST_LOOP if $mask && $_->{mask} ne $mask;
		  next LIST_LOOP if $pattern && $_->{pattern} ne $pattern;
		  my $found;
		  for my $o (@$opts) {
		      if ($o eq 'QUIET') {
			  $quiet = 1;
			  next;
		      }
		      next if $o eq 'NOSAVE';
		      next LIST_LOOP unless $_->{$o};
		      $found = 1;
		  }
		  delete @optskept{@$opts};
		  if ($found) {
		      for my $o (values %optskept) {
			  if ($o) {
			      # Partial options delete, so we need to set the
			      # record to remove only those options that match
			      # but leave the record in place.
			      delete @{$_}{@optlist};
			      @{$_}{keys %optskept} = values %optskept;
			      $save = 1 unless $_->{NOSAVE};
			      $partial++;
			      next LIST_LOOP;
			  }
		      }
		  }

		  # Everything relevant matches, schedule a deletion.
		  $eldel = 1;
		  $save = 1 unless $_->{NOSAVE};
		  $matches++;
		} # LIST_LOOP
		!$eldel
	    } @ignore_list;
	    @ignore_list = @new_list if $matches;
	    save_ignore_list() if $save;

	    if (!$quiet) {
		if ($matches && $partial) {
		    HexChat::printf(
			'%d ignore entries updated and %d entries removed.',
			$partial, $matches);
		}
		elsif ($matches) {
		    HexChat::printf('%d ignore entries removed.', $matches);
		}
		elsif ($partial) {
		    HexChat::printf('%d ignore entries updated.', $partial);
		}
		else {
		    HexChat::print('No matching ignore entries found.');
		}
	    }
	},
	LIST => sub {
	    my ($mask, $opts, $pattern) = @_;
	    # Loop through and find matching entries
	    my $matches = 0;
	    my @types = qw(PRIV CHAN NOTI CTCP DCC INVI NOSAVE);
	  LIST_LOOP: for my $entry (@ignore_list) {
	      next LIST_LOOP if $mask && $entry->{mask} ne $mask;
	      next LIST_LOOP if $pattern && $entry->{pattern} ne $pattern;
	      for my $o (@$opts) {
		  next if $o eq 'QUIET';
		  next LIST_LOOP unless $entry->{$o};
	      }

	      if (!$matches) {
		  # header
		  HexChat::printf(' %-40s  %-35s  %-50s',
				  'HOSTMASK', 'TYPES', 'PATTERN');
		  HexChat::print('==========================================' .
		      '=====================================' .
		      '===================================================');
	      }

	      my %th;
	      @th{@types} = @{$entry}{@types};
	      delete @th{grep {!$th{$_}} keys %th};
	      HexChat::printf(' %-40s  %-35s  %-50s',
			      $entry->{mask}, join(' ', sort keys %th),
			      $entry->{pattern}||'');

	      $matches++;
	  } # LIST_LOOP

	    if ($matches) {
		HexChat::print('==========================================' .
		    '=====================================' .
		    '===================================================');
		HexChat::printf('Number of matches: %d', $matches);
	    }
	    else {
		HexChat::print('No matching entries found.');
	    }
	},
	CLEAR => sub {
	    my (undef, $opts) = @_;
	    my $quiet;
	    for (@$opts) {
		if ($_ eq 'QUIET') {
		    $quiet = 1;
		    last;
		}
	    }
	    # Just donk the list and the unlink the file.
	    @ignore_list = ();
	    unlink $ignore_path;

	    HexChat::print('Ignore list cleared.') unless $quiet;
	},
	);
    my $cmd = uc $args->[0];
    if ($commands{$cmd}) {
	shift @$args;
	shift @$remain;
    }
    else {
	$cmd = 'ADD';
    }

    # Then any number of matching options.
    my %options;
    @options{qw(PRIV CHAN NOTI CTCP DCC INVI NOSAVE QUIET)}=();
    $options{ALL} = [qw(PRIV CHAN NOTI)];

    # The next arg is the hostmask.
    my $mask;
    unless (! @$args || exists $options{uc $args->[0]}) {
	$mask = shift @$args;
	shift @$remain;
    }

    my @opts;
    while (@$args && exists $options{uc $args->[0]}) {
	my $o = uc shift @$args;
	shift @$remain;
	if ($options{$o}) {
	    push @opts, @{$options{$o}};
	}
	else {
	    push @opts, $o;
	}
    }

    # Call the relevant sub-command
    $commands{$cmd}->($mask, \@opts, $remain->[0]);
    return EAT_ALL;
}, {help_text => $help_text});

hook_server('RAW LINE', sub {
    my ($args, $remain) = @_;
    my $nick = get_info('nick');
    my ($type,$msg);
    my $host = $args->[0];
    $host =~ s/^://;

    # Determine the type of message
    if ($args->[1] eq 'PRIVMSG') {
	$type = $args->[2] eq $nick ? 'PRIV' : 'CHAN';
	if ($args->[3] =~ /^:\001/ && $remain->[4] =~ /\001$/) {
	    if ($args->[3] eq ":\001ACTION") {
		$msg = $remain->[4];
		$msg =~ s/\001$//;
	    }
	    else {
		$type = 'CTCP';
		$msg = $remain->[3];
		$msg =~ s/^\001//;
		$msg =~ s/\001$//;
	    }
	}
	else {
	    $msg = $remain->[3];
	    $msg =~ s/^://;
	}
    }
    elsif ($args->[1] eq 'NOTICE') {
	$type = $args->[2] eq $nick ? 'NOTI' : 'CHAN';
	$msg = $remain->[3];
	$msg =~ s/^://;
    }
    # TODO: Add support for DCC and INVI.
    else {
	# This is not something we filter.
	return EAT_NONE;
    }

    # Strip color codes, etc.
    $msg = strip_code($msg);

    # See if this matches any filters.
    for my $entry (@ignore_list) {
	next unless $entry->{$type};
	next unless match_glob($entry->{mask}, $host);
	next if $entry->{pattern} && !eval { $msg =~ /$entry->{pattern}/ };
	# We have a match, ignore this line.
	return EAT_ALL;
    }

    # Nothing matched, so allow this line to go through.
    return EAT_NONE;
});

register($scriptname, $version, $description, sub {
    HexChat::print("$scriptname unloaded.");
});
load_ignore_list();

HexChat::print("$scriptname version $version loaded.");
