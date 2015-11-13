#!/usr/bin/perl -w
# This file was install in /usr/local/share/perl5/V6evalRemote.pm

########################################################################
=pod

=head1 NAME

V6evalRemote - Perl interface to control NUT via ssh

=head1 SYNOPSIS

=head1 DESCRIPTION

=head1 INTERFACE OF REMOTE CONTROL SCRIPT

=cut


package V6evalRemote;
use Getopt::Std;
use Exporter;
@ISA = qw(Exporter);

use Expect;
$Expect::Log_Stdout = 1;
#$Expect::Debug = 1;

# Some functions we don't use now
# rDevice : Set/get serial device name
# rUser : Set/get user name
# rUserPrompt : Set/get user prompt
# rPasswordPrompt : Set/get password prompt
# rCommandPrompt : Set/get command prompt
# rDebug : Set/get Expect.pm's debug()
# rExpInternal : Set/get Expect.pm's exp_internal()
# rLogStdout : Set/get Expect.pm's log_stdout()
# rSendSpeed :

@EXPORT = qw(
    rOpen
    rLogin
    rCommand
    rCommandAsync
    rReboot
    rRebootAsync
    rPutfile
    rGetfile
    rLogout
    rClose
    rType
    rCmdOutput
    rEnableLogout
    );

########################################################################

# The following status values must be same as V6evalTool.pm's ones.

$exitPass=0;            # PASS
$exitNS=2;              # Not yet supported
$exitFail=32;           # FAIL
$exitFatal=64;          # FATAL

########################################################################
BEGIN
{
    # I don't known what $Type used for
    $Type = "unitedlinux";
    $User = "root";
    $Password = "redhat";
    $SendSpeed = 0;
    undef $Remote;

    $CmdOutput = "";
    $EnableLogout = 0;
}

########################################################################
END
{
    rClose();
}

###########################code start here##############################

########################################################################
=item rOpen()

Usage

    $obj->rOpen($var_name)
    $obj->rOpen(var_name=>$var_name)
    $obj->rOpen(var_name=>$var_name,opt_var=>$opt_var)
Purpose
    Open the remote control program
Parameter
    $var_name       # like "/dev/sda"
    $opt_var   # like "-s 512"
Returns
    undef: error
    Otherwise : Expect handle
Exceptions
    When will we die()
See Also
    fooc()

=cut
sub rOpen {
    return undef if (defined $Remote);

    # it seems perl will auto enable autoflush after 5.6
    #aotoflush STDOUT 1;

    getopts('t:d:l:u:p:v:i:o:Vs:h');
    if($opt_h){
        usage();
        exit 0;
    }
    $Type = $opt_t if defined($opt_t);  # target type
    # Because all the cases use serial device to cotrol the remote before,
    # we use $Device to get the retmoe host name.
    $Host = $opt_d if defined($opt_d);    # Remote host name
    $User = $opt_u if defined($opt_u);  # user name
    $Password = $opt_p if defined($opt_p);  # password

    $debug = 1 if defined($opt_V);  # debug flag
    $SendSpeed=$opt_s if defined($opt_s); #send speed
    $EnableLogout = $opt_l if defined($opt_l);  #enable logout

    my $TermCmd = "ssh -l root $Host";
    $Remote = new Expect;
    $Remote = Expect->spawn("$TermCmd");
    if (!defined $Remote) {
        print STDERR "$TermCmd exec failed\n";
        return undef;
    }

    $Remote->debug($opt_v) if defined($opt_v);  # debug level
    $Remote->exp_internal($opt_i) if defined($opt_i);  # internal debug level
    $Remote->log_stdout($opt_o) if defined($opt_o);  # stdout

    # add other parameters like timeout, cmd, gateway, if...
    my($v, $lval, $rval);
    while($v = shift(@ARGV)) {
        ($lval, $rval) = split(/=/, $v, 2);
        $rval=1 if $rval =~ /^\s*$/;
        $v='$main::rOpt_'."$lval".'=\''."$rval".'\'';
        eval($v);       # eval ``$main::rOpt<LVAL>=<RVAL>''
        print STDERR "eval $v\n" if $debug;
    }

    $Remote;
}   # End of rOpen()

sub usage()
{
    print "V6evalRemote.pm system options:\n".
	  "  -h<help>\n".
	  "  -t<target_type>\n".
	# "  -T<cu command path>\n".
	  "  -d<remote_host_name>\n".
	  "  -u<user_name>\n".
	  "  -p<password>\n".
	  "  -v<debug level>\n".
	  "  -i<internal debug level>\n".
	  "  -o<stdout>\n".
	  "  -l<enable_logout>\n".
	  "  -V<debug>\n";
}

########################################################################
=item rLogin()

Usage

    $obj->rLogin($timeout)
Purpose
    Log into the target machine
Parameter
    $timeout       # like "300"
Returns
    ==0: error
    !=0: success
Exceptions
    When will we die()
See Also
    fooc()

=cut
sub rLogin($)
{

    my ($timeout) = @_;

    if (!defined $Remote) {
        print STDERR "rOpen() should be called first.\n";
        return 0;
    }

    # $Remote->expect( $timeout, @match_patterns );
    # OK, we extend the timeout time to 10s incased of slow response
    $Remote->expect(300, [
            'connecting (yes/no)?',
            sub {
                my $self = shift;
                $self->send("yes\n");
                exp_continue;
            }
        ],
        [
            qr/password:/i,
            sub {
                my $self = shift;
                $self->send("$Password\n");
                exp_continue;
            }
        ],
        [
            '# ',
            sub {
                my $self = shift;
                $self->send("uname -r\n");
            }
        ]
    # here we use exit but not return incases of error like
    # Can't return outside a subroutine at ...
    ) or print "Match fail... Check password ??\n" and exit 0;

    return 1;
}   # End of rLogin()


=item rCommand()

Usage
    sub rCommand($$) { my (
        $command    # command
        $timeout    # timeout sed
    ) = @_;
Purpose
    Execute a command.
Parameter
    $command    # command
    $timeout    # timeout sed
Returns
    ==0: error
    ==1: success, command exit status is 0
    ==2: warning, command exit status isn't 0
    ==3: warning, command exit status unknown
Exceptions
    When will we exit 0
See Also
    fooc()

=cut
sub rCommand($$)
{
    my($cmd, $timeout) = @_;

    return 0 if (!defined $Remote);

    $Remote->expect($timeout, [
            '# ',
            sub {
                my $self = shift;
                $self->send("$cmd\n");
            }
        ]) or print "rCommand failed...\n" and exit 0;

    $CmdOutput = $Remote->exp_before();
    print STDERR "rCommand: CmdOutput=''$CmdOutput''\n" if $debug;

    # get exit status
    $Remote->send("echo \$?\n") if ($Remote->expect($timeout, '# '));
    if(defined $Remote->expect($timeout, '-re', '[0-9]+')) {
        $s = $Remote->exp_match();
        print STDERR "rCommand : exit status $s\n" if $debug;
        return (($s == 0) ? 1 : 2);
    }

    print STDERR "rCommand: nerver got command exit status\n" if $debug;
    return 3;
}   # End of rCommand()

sub rCommandAsync($$)
{
    my($cmd, $timeout) = @_;
    return rCommand($cmd, $timeout);
}

=item rReboot()

Usage
    sub rReboot($) { my (
        $timeout    # timeout sec
    ) = @_;
Purpose
    Reboot the target machine 
Parameter
    $timeout    # timeout sec
Returns
    ==0: error
    !=0: success
Exceptions
    When will we exit 0
See Also
    fooc()

=cut
sub rReboot($)
{
    my($timeout) = @_;
    my($i, $retry, $t) = (0, 10, 20);

    $Remote->expect(5, [
            '# ',
            sub {
                my $self = shift;
                $self->send("reboot\n");
            }
        ]) or print "rReboot: reboot failed...\n" and exit 0;

    print "\n";

    # sleep to wait for target boot up
    sleep $timeout;

    # Try $i time, if still fail, then die
    for ($i = 0; $i < $retry; $i++) {
        print "rReboot: start $i times ping retry\n" if (defined $debug);
        # we use ping the judge if the machine already boot up
        if ( system("ping $Host -c 4 > /dev/null") == 0 ) {
            print "rReboot: ping pass, start ssh\n" if (defined $debug);
            # sleep $t sec to wait remote start sshd server.
            sleep $t;
            $Remote = Expect->spawn("ssh -l root $Host") or print "rReboot: ".
            "ssh fail\n" and exit 0;
            last;
        }
        sleep $t;
    }
    # So , we totally slept $timeout + ($t + 4) * $i + $t sec.

    if ( $i >= $retry ) { 
        print "rReboot : Fail, target didn't reboot up in 500s\n";
        return 0;
    }

    # exp->expect( $timeout, @match_patterns );
    # OK, we extend the timeout time to 10s incased of slow response
    $Remote->expect(300, [
            'connecting (yes/no)?',
            sub {
                my $self = shift;
                $self->send("yes\n");
                exp_continue;
            }
        ],
        [
            qr/password:/i,
            sub {
                my $self = shift;
                $self->send("$Password\n");
                exp_continue;
            }
        ],
        [
            '# ',
            sub {
                my $self = shift;
                $self->send("date\n");
            }
        ]
    ) or print "rReboot : Didn't expect anything...\n" and exit 0;

    return 1;
}   # End of rReboot()

# We use network restart instead of reboot here to save time
sub rRebootAsync($)
{
    my($timeout) = @_;
    $Remote->expect(5, [
            '# ',
            sub {
                my $self = shift;
				#$self->send("sleep 20 && service network restart &\n");
				# As NUT always don't run network restart after sleep,
				# We use at instead. Will find why sleep not work later.
                $self->send("at now + 2 minutes -f restart_network.sh\n");
            }
        ]) or print "rReboot: reboot failed...\n" and exit 0;

    print "\n";
    return 1;
}   # End of rRebootAsync()

#--------------------------------------------------------------------------#
# sendMessages ()
# Usage
#   sendMessages($var_name)
# Purpose
#   Query out something
# Parameter
#   $var_name       # like "/dev/sda"
#   $optional_var   # like "-s 512"
# Returns
#   ref_2_array     # $somethings_ref
# Exceptions
#   We die when we are old enough.
# See Also
#   fooc()
#--------------------------------------------------------------------------#
sub sendMessages(@) {
    my(@strings) = @_;

	foreach(@strings) {
		if($SendSpeed == 0) {
			print $Remote $_; # the same as $Remote->send()
		} else {
			$Remote->send_slow($SendSpeed, $_);
		}
	}
}
####################### End of functoin sendMessages()

#--------------------------------------------------------------------------#
# get_prompt ()
# Usage
#   get_prompt($var_name)
# Purpose
#   Query out something
# Parameter
#   $var_name       # like "/dev/sda"
#   $optional_var   # like "-s 512"
# Returns
#   ref_2_array     # $somethings_ref
# Exceptions
#   We die when we are old enough.
# See Also
#   fooc()
#--------------------------------------------------------------------------#
sub get_prompt($) {
	my ($timeout) = @_;
	my $count = 0;

	for(my $d = 0; $d < $timeout; $d ++) {
		if(defined($Remote->expect(1, '-re', "# "))) {
			$count ++;
			last;
		}
		sendMessages("\n");
	}
	return($count);
}
####################### End of functoin get_prompt()

#--------------------------------------------------------------------------#
# rPutfile ()
# Usage
#   rPutfile($from, $to, $timeout)
# Purpose
#   Copy file $from remote $to local
# Parameter
#   $from       # like "/root/test"
#   $to   		# like "/tmp/test"
# Returns
#       ==0: error
#   	!=0: success
# Exceptions
#   We die when we are old enough.
# See Also
#   fooc()
# Note
#   This function is only used for ssh special mode. we should add the
#   following config to /etc/ssh/ssh_config or .ssh/config on TN
#
#   EscapeChar ~
#   PermitLocalCommand yes
#   ControlMaster yes
#
#   If you ssh from A to C like A -> B -> C, and exec local cmd on C, it will
#   actually exec cmd on A, not B. So if you want run test on remote manually,
#   you have to use console to login B
#--------------------------------------------------------------------------#
sub rPutfile($$$) {
	my($from, $to, $timeout) = @_;

    if (!defined $Remote) {
        print "rOpen() should be called first.\n";
        return 0;
    }

	unless(get_prompt($timeout)) {
		goto error;
	}

	$Remote->send("~C");
	$Remote->expect($timeout, [
			'> ',
			sub {
				my $self = shift;
				$self->send("!scp $from root\@$Host:$to\n");
				$self->send("\n");
				exp_continue;
			}
		],
		[
			'# ',
			sub {
				my $self = shift;
				$self->send("date\n");
			}
		]
	) or print STDERR "can't scp file from ssh\n" and return(0);

	unless(get_prompt($timeout)) {
		goto error;
	}

	if(!defined $Remote->expect($timeout, '-re', "# ")) {
		print STDERR "Never sync with rPutfile\n";
		goto error;
	}

	print STDERR "rPutfile: Copying completed\n" if $debug;
	return(1);

error:
	return(0);
}
####################### End of functoin rPutfile()

#--------------------------------------------------------------------------#
# rGetfile ()
# Usage
#   rGetfile($from, $to, $timeout)
# Purpose
#   Copy file $from local $to remote
# Parameter
#   $from       # like "/root/test"
#   $to   		# like "/tmp/test"
# Returns
#       ==0: error
#   	!=0: success
# Exceptions
#   We die when we are old enough.
# See Also
#   fooc()
# Note
#   This function is only used for ssh special mode. we should add the
#   following config to /etc/ssh/ssh_config or .ssh/config on TN
#
#   EscapeChar ~
#   PermitLocalCommand yes
#   ControlMaster yes
#--------------------------------------------------------------------------#
sub rGetfile($$$) {
	my($from, $to, $timeout) = @_;

    if (!defined $Remote) {
        print "rOpen() should be called first.\n";
        return 0;
    }

	unless(get_prompt($timeout)) {
		goto error;
	}

	$Remote->send("~C");
	$Remote->expect($timeout, [
			'> ',
			sub {
				my $self = shift;
				$self->send("!scp root\@$Host:$from $to\n");
				$self->send("\n");
				exp_continue;
			}
		],
		[
			'# ',
			sub {
				my $self = shift;
				$self->send("date\n");
				$self->send("\n");
			}
		]
	) or print STDERR "can't scp file from ssh\n" and return(0);

	unless(get_prompt($timeout)) {
		goto error;
	}

	if(!defined $Remote->expect($timeout, '-re', "# ")) {
		print STDERR "Never sync with rGetfile\n";
		goto error;
	}

	print STDERR "rGetfile: Copying completed\n" if $debug;
	return(1);

error:
	return(0);
}
####################### End of functoin rGetfile()

=item rLogout()

Usage
    $obj->rLogout($timeout)
Purpose
    Log out of the target machine.
Parameter
    $timeout       # timeout sec, like "5"
Returns
    ==0: error
    !=0: success
Exceptions
    When will we die()
See Also
    fooc()

=cut
sub rLogout($)
{
    my($timeout) = @_;

    if (!defined $Remote) {
        print "rOpen() should be called first.\n";
        return 0;
    }

    $Remote->expect($timeout, [
        '# ',
        sub {
            my $self = shift;
            $self->send("exit\n");
        }
        ]) or print "rLogout failed...\n" and exit 0;

    return 1;
}   # End of rLogout()


=item rClose()

Usage

    $obj->rClose($timeout)
Purpose
    Terminate the remote control program
Parameter
    $timeout       # timeout sec, like "5"
Returns
    ==0: error
    !=0: success
Exceptions
    When will we exit 0
See Also
    fooc()

=cut
sub rClose {
    return 0 if (!defined $Remote);

    $Remote->soft_close();
    undef $Remote;

    print "\n";
    return 1;
}   # End of rClose()


########################################################################
=item rType()

Usage

    rType($var_name)
    rType(var_name=>$var_name)
    rType(var_name=>$var_name,opt_var=>$opt_var)
Purpose
    Set/get a target machine type
Parameter
    $var_name       # like "/dev/sda"
    $opt_var   # like "-s 512"
Returns
    Returns a target machine type
Exceptions
    When will we die()
See Also
    fooc()

=cut

sub rType(;$) {
    my($x) = $Type;
    $Type = shift(@_) if @_ != ();
    $x;
}    # End of rType()

########################################################################
=item rCmdOutput()

Usage

    $obj->rCmdOutput($var_name)
    $obj->rCmdOutput(var_name=>$var_name)
    $obj->rCmdOutput(var_name=>$var_name,opt_var=>$opt_var)
Purpose
    Query out something
Parameter
    $var_name       # like "/dev/sda"
    $opt_var   # like "-s 512"
Returns
    ref_2_array     # $somethings_ref
Exceptions
    When will we die()
See Also
    fooc()

=cut
sub rCmdOutput {
    my(@x);
    @x = split(/\n/, $CmdOutput);
}   # End of rCmdOutput()

########################################################################
=item rEnableLogout()

Usage

    $obj->rEnableLogout($var_name)
    $obj->rEnableLogout(var_name=>$var_name)
    $obj->rEnableLogout(var_name=>$var_name,opt_var=>$opt_var)
Purpose
    Enalbe( !=0 )/Disable( ==0, default) rLogout()
Parameter
    $var_name       # like "/dev/sda"
    $opt_var   # like "-s 512"
Returns
    Returns previous value
Exceptions
    When will we die()
See Also
    fooc()

=cut
sub rEnableLogout {
    my($x) = $EnableLogout;
    $EnableLogout = shift(@_) if (@_ > 0);
    $x;
}   # End of rEnableLogout()

1;

__END__

=head1 AUTHOR

Hangbin Liu <haliu@redhat.com>

=head1 COPYRIGHT AND LICENSE

Copyright (c) 2006 Red Hat, Inc. All rights reserved.

This copyrighted material is made available to anyone wishing to use, modify,
copy, or redistribute it subject to the terms and conditions of the GNU
General Public License v.2.

This program is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301,
USA.

=head1 SEE ALSO

ST::Cmd
