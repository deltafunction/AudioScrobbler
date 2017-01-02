package Slim::Plugin::AudioScrobbler::Plugin;

# $Id$

# This plugin handles submission of tracks to Last.fm's
# Audioscrobbler service.

# Logitech Media Server Copyright 2001-2011 Logitech.
# This program is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License,
# version 2.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# The basic algorithm used by this plugin is very simple:
# On newsong, figure out how long from now until half of song or 240 secs
# Set a timer for that time, the earliest possible time they could meet the criteria
# When timer fires, make sure same track is playing
# If not yet at the time (maybe they paused), recalc and set the timer again
# If time has passed, great, submit it

# Thanks to the SlimScrobbler plugin for inspiration and feature ideas.
# http://slimscrobbler.sourceforge.net/

use strict;
use base qw(Slim::Plugin::Base);

use Slim::Plugin::AudioScrobbler::LastFMAsyncHTTP;

use Slim::Player::ProtocolHandlers;
use Slim::Player::Source;

if ( main::WEBUI ) {
	require Slim::Plugin::AudioScrobbler::Settings;
	require Slim::Plugin::AudioScrobbler::PlayerSettings;
}

use Slim::Networking::SimpleAsyncHTTP;
use Slim::Utils::Log;
use Slim::Utils::Prefs;
use Slim::Utils::Strings qw(string);
use Slim::Utils::Timers;

use Class::Struct;
use Digest::MD5 qw(md5_hex);
use JSON::XS::VersionOneAndTwo;
use JSON qw(decode_json);
use URI::Escape qw(uri_escape_utf8 uri_unescape);
use Data::Dumper;

my $prefs = preferences('plugin.audioscrobbler');
my $lastfm_user ;

my $log = Slim::Utils::Log->addLogCategory( {
	category     => 'plugin.audioscrobbler',
	defaultLevel => 'ERROR',
	description  => 'PLUGIN_AUDIOSCROBBLER_MODULE_NAME',
} );

use constant HANDSHAKE_URL => 'http://post.audioscrobbler.com/';
use constant CLIENT_ID     => main::SLIM_SERVICE ? 'snw' : 'ss7';
use constant CLIENT_VER    => 'sc' . $::VERSION;

sub getDisplayName {
	return 'PLUGIN_AUDIOSCROBBLER_MODULE_NAME';
}

my %players;
my $api_key = "dd63f28d741d0a9fa98377c3b678054b";
my $api_secret = "fa78775417f96f5467d12a60510e8bc7";
my $lastfm_root = 'https://ws.audioscrobbler.com/2.0/?format=json';
my $now_playing_url = $lastfm_root;
my $submit_url = $lastfm_root;

sub initPlugin {
	my $class = shift;

	$class->SUPER::initPlugin();

	if ( main::WEBUI ) {
		Slim::Plugin::AudioScrobbler::Settings->new;
		Slim::Plugin::AudioScrobbler::PlayerSettings->new;
	}
	
	# init scrobbling prefs
	$prefs->init({
		enable_scrobbling => 1,
		include_radio     => 0,
		account           => 0,
	});
	
	# Subscribe to new song events
	Slim::Control::Request::subscribe(
		\&newsongCallback, 
		[['playlist'], ['newsong']],
	);

	Slim::Control::Request::subscribe(
		\&playCallback, 
		[['playlist'], ['play']],
	);
	
	# Track Info item for loving tracks
	Slim::Menu::TrackInfo->registerInfoProvider( lfm_love => (
		before => 'top',
		func   => \&infoLoveTrack,
	) );
	# Track Info item for banning tracks
	Slim::Menu::TrackInfo->registerInfoProvider( lfm_ban => (
		before => 'bottom',
		func   => \&infoBanTrack,
	) );
	# Track Info item for removing tracks
	Slim::Menu::TrackInfo->registerInfoProvider( lfm_remove => (
		before => 'bottom',
		func   => \&infoRemoveTrack,
	) );
	
	# A way for other things to notify us the user loves a track
	Slim::Control::Request::addDispatch(['audioscrobbler', 'loveTrack', '_url'],
		[0, 1, 1, \&loveTrack]);
	
	Slim::Control::Request::addDispatch(['audioscrobbler', 'banTrack', '_url', '_skip'],
		[0, 1, 1, \&banTrack]);

	Slim::Control::Request::addDispatch(['audioscrobbler', 'removeTrack', '_artist', '_title'],
		[0, 1, 1, \&removeTrack]);
	
	Slim::Control::Request::addDispatch([ 'audioscrobbler', 'settings' ],
		[1, 1, 0, \&jiveSettingsMenu]);

	Slim::Control::Request::addDispatch([ 'audioscrobbler', 'account' ],
		[1, 0, 1, \&jiveSettingsCommand]);

	# Pref change hooks
	$prefs->setChange( sub {
		my $value  = $_[1];
		my $client = $_[2] || return;
		changeAccount( $client, $value );
	}, 'account' );
}

sub shutdownPlugin {
	Slim::Control::Request::unsubscribe( \&newsongCallback );
}

# Only show player UI settings menu if account is available
sub condition {
	my ( $class, $client ) = @_;
	
	my $accounts = getAccounts($client);
	
	if ( ref $accounts eq 'ARRAY' && scalar @{$accounts} ) {
		return 1;
	}
	
	return;
}

# Button interface to change account or toggle scrobbling
sub setMode {
	my $class  = shift;
	my $client = shift;
	my $method = shift;

	if ( $method eq 'pop' ) {
		Slim::Buttons::Common::popMode($client);
		return;
	}
	
	my $listRef = [ 0 ];
	
	my $accounts = getAccounts($client);
	
	for my $account ( @{$accounts} ) {
		push @{$listRef}, $account->{username};
	}

	Slim::Buttons::Common::pushModeLeft( $client, 'INPUT.List', {

		header         => $client->string('PLUGIN_AUDIOSCROBBLER_MODULE_NAME'),
		headerAddCount => 1,
		listRef        => $listRef,
		externRef      => sub {
			my ( $client, $account ) = @_;
			
			if ( !$account ) {
				return $client->string('PLUGIN_AUDIOSCROBBLER_SCROBBLING_DISABLED');
			}
			
			return $client->string( 'PLUGIN_AUDIOSCROBBLER_USE_ACCOUNT', $account );
		},
		initialValue   => sub { $prefs->client(shift)->get('account'); },
		overlayRef     => sub {
			my ( $client, $account ) = @_;
			my $overlay;
			
			my $curAccount = $prefs->client($client)->get('account') || 0;

			if ( $account eq $curAccount ) {
				$overlay = Slim::Buttons::Common::radioButtonOverlay( $client, 1 );
			} else {
				$overlay = Slim::Buttons::Common::radioButtonOverlay( $client, 0 );
			}
			
			return ( undef, $overlay );
		},
		callback      => sub { 
			my ( $client, $exittype ) = @_;

			$exittype = uc $exittype;

			if ( $exittype eq 'LEFT' ) {

				Slim::Buttons::Common::popModeRight($client);
			}
			elsif ( $exittype eq 'RIGHT' ) {
				my $value = $client->modeParam('valueRef');
				
				my $curAccount = $prefs->client($client)->get('account') || 0;
				
				if ( $curAccount ne $$value ) {
					changeAccount( $client, $$value );

					$client->update();
				}
			}
			else {
				$client->bumpRight;
			}
		},
	} );
}

sub changeAccount {
	my ( $client, $account ) = @_;
	
	$prefs->client($client)->set( account => $account );

	if ( $account eq '0' ) {
		# Kill any timers so the current track is not scrobbled
		Slim::Utils::Timers::killTimers( $client, \&checkScrobble );
		Slim::Utils::Timers::killTimers( $client, \&submitScrobble );
		
		# Dump queue
		setQueue( $client, [] );
	}
	else {	
		# If you change accounts and have more than 1 track queued for scrobbling,
		# dump all but the most recent queued track
		my $queue = getQueue($client);
	
		my $count = scalar @{$queue};
		if ( $count > 1 ) {
			$log->warn( "Changed scrobble accounts with $count queued items, removing items:" );
		
			my $newQueue = [ pop @{$queue} ];
		
			for my $item ( @{$queue} ) {
				$log->warn( '  ' . uri_unescape( $item->{t} ) );
			}
		
			setQueue( $client, $newQueue );
		}
	}

	# Clear session
	clearSession($client);

	if ( main::DEBUGLOG && $log->is_debug ) {
		$log->debug( "Changing account for player " . $client->id . " to $account" );
	}
}

sub clearSession {
	my $client = shift;
	
	# Reset our state
	$client->master->pluginData( session_id      => 0 );
	$client->master->pluginData( now_playing_url => 0 );
	$client->master->pluginData( submit_url      => 0 );
}

sub handshake {
	my $params = shift || {};
	if ( my $client = $params->{client} ) {
		clearSession( $client );
		
		# Get client's account information
		if ( !$params->{username} ) {
			$params->{username} = $prefs->client($client)->get('account');
			
			my $accounts = getAccounts($client);
			
			for my $account ( @{$accounts} ) {
				if ( $account->{username} eq $params->{username} ) {
					$params->{password} = $account->{password};
					last;
				}
			}
		}
		getLastFMSession($client, 0, $params);
	}
}

sub playCallback {
	my $request = shift;
	my $client  = $request->client() || return;
	my $url   = Slim::Player::Playlist::url($client);
	my $uri    = $request->getParam('uri');
	$log->error( "URL: ".$url);
	$log->error( "URI: ".$uri);
	if($url =~ /^spotify:lfmuser:(.*):library:(.*):(.*)/){
		getTracks($request, $1, $2, $3);
	}
	elsif($url =~ /^spotify:lfmuser:(.*):library:(.*)/){
		getTracks($request, $1, $2);
	}
}

sub newsongCallback {
	my $request = shift;
	my $client  = $request->client() || return;

	# Check if this player has an account selected
	if ( ! (my $account = $prefs->client($client)->get('account')) ) {
		# set a zero value so we don't need to query the DB any more in the future
		$prefs->client($client)->set('account', 0) if main::SLIM_SERVICE && !defined $account;
		return ;
	}
	
	# If synced, only listen to the master
	if ( $client->isSynced() ) {
		return unless Slim::Player::Sync::isMaster($client);
	}
	
	my $accounts = getAccounts($client);
	
	my $enable_scrobbling;
	
	if ( main::SLIM_SERVICE ) {
		# Get enable_scrobbling from the user_prefs table
		$enable_scrobbling = $prefs->client($client)->get( 'enable_scrobbling', undef, 'UserPref' );
	}
	else {
		$enable_scrobbling = $prefs->get('enable_scrobbling');
	}
	
	return unless $enable_scrobbling && scalar @{$accounts};

	my $url   = Slim::Player::Playlist::url($client);
	my $track = Slim::Schema->objectForUrl( { url => $url } );

	# Disable scrobbling of Spotify and other on-demand services
	#if ( $track->remote ) {
	#	return;
	#}
	
	my $duration = $track->secs;
	
	if ( $track->remote ) {
		my $handler = Slim::Player::ProtocolHandlers->handlerForURL($url);
		if ( $handler && $handler->can('getMetadataFor') ) {
			# this plugin provides track metadata, i.e. Pandora, Rhapsody
			my $meta = $handler->getMetadataFor( $client, $url, 'forceCurrent' );
			if ( $meta && $meta->{duration} ) {
				$duration = $meta->{duration};
			}
		}
	}
	
	# If this is a radio track (no track length) and contains a playlist index value
	# it is the newsong notification from the station title, which we want to ignore
	if ( !$duration && defined $request->getParam('_p3') ) {
		main::DEBUGLOG && $log->debug( 'Ignoring radio station newsong notification' );
		return;
	}
	
	# report all new songs as now playing
	my $queue = getQueue($client);
	
	if ( scalar @{$queue} && scalar @{$queue} <= 50 ) {
		# before we submit now playing, submit all queued tracks, so that
		# a scrobbled track doesn't clobber the now playing data
		main::DEBUGLOG && $log->debug( 'Submitting scrobble queue before now playing track' );
		
		submitScrobble( $client, {
			cb => sub {
				# delay by 1 second so we don't hit the server too fast after
				# the submit call
				Slim::Utils::Timers::killTimers( $client, \&submitNowPlaying );
				Slim::Utils::Timers::setTimer(
					$client,
					time() + 1,
					\&submitNowPlaying,
					$track,
				);
			},
		} );
	}
	else {
		submitNowPlaying( $client, $track );
	}

	# Determine when we need to check again
	
	# Track must be > 30 seconds
	if ( $duration && $duration < 30 ) {
		if ( main::DEBUGLOG && $log->is_debug ) {
			$log->debug( 'Ignoring track ' . $track->title . ', shorter than 30 seconds' );
		}
		
		return;
	}
	
	my $title = $track->title;
	
	if ( $track->remote ) {
		my $handler = Slim::Player::ProtocolHandlers->handlerForURL($url);
		if ( $handler && $handler->can('getMetadataFor') ) {
			# this plugin provides track metadata, i.e. Pandora, Rhapsody
			my $meta = $handler->getMetadataFor( $client, $url, 'forceCurrent' );
			$title   = $meta->{title};
			
			# Handler must return at least artist and title
			unless ( $meta->{artist} && $meta->{title} ) {
				main::DEBUGLOG && $log->debug( "Protocol Handler didn't return an artist and title for " . $track->url . ", ignoring" );
				return;
			}

			# Save the title in the track object so we can compare it in checkScrobble
			$track->stash->{_plugin_title} = $title;
		}
		else {
			main::DEBUGLOG && $log->debug("Ignoring remote URL $url");
			return;
		}
	}
	
	# We check again at half the song's length or 240 seconds, whichever comes first
	my $checktime;
	if ( $duration ) {
		$checktime = $duration > 480 ? 240 : ( int( $duration / 2 ) );
	}
	else {
		# For internet radio, check again in 30 seconds
		$checktime = 30;
	}
	
	if ( main::DEBUGLOG && $log->is_debug ) {
		$log->debug( "New track to scrobble: $title, will check in $checktime seconds" );
	}
	
	Slim::Utils::Timers::killTimers( $client, \&checkScrobble );
	Slim::Utils::Timers::setTimer(
		$client,
		Time::HiRes::time() + $checktime + 5,	# 5 seconds added to allow for startup and avoid unnecessary callback 
		\&checkScrobble,
		$track,
		$checktime,
	);
}

sub submitNowPlaying {
	my ( $client, $track, $retry ) = @_;
	
	# Abort if the user disabled scrobbling for this player
	return if !$prefs->client($client)->get('account');
	
	if ( !$client->master->pluginData('now_playing_url') ) {
		# Get a new session
		handshake( {
			client => $client,
			cb     => sub {
				submitNowPlaying( $client, $track, $retry );
			},
		} );
		return;
	}
	
	my $artist   = $track->artistName || '';
	my $album    = $track->album  ? $track->album->name  : '';
	my $title    = $track->title;
	my $tracknum = $track->tracknum || '';
	my $duration = $track->secs;
	
	if ( $track->remote ) {
		my $handler = Slim::Player::ProtocolHandlers->handlerForURL( $track->url );
		if ( $handler && $handler->can('getMetadataFor') ) {
			# this plugin provides track metadata, i.e. Pandora, Rhapsody
			my $meta  = $handler->getMetadataFor( $client, $track->url, 'forceCurrent' );
			$artist   = $meta->{artist};
			$album    = $meta->{album} || '';
			$title    = $meta->{title};
			$tracknum = $meta->{tracknum} || '';
			$duration = $meta->{duration} || $track->secs;
			
			# Handler must return at least artist and title
			unless ( $meta->{artist} && $meta->{title} ) {
				main::DEBUGLOG && $log->debug( "Protocol Handler didn't return an artist and title for " . $track->url . ", ignoring" );
				return;
			}
		}
		else {
			main::DEBUGLOG && $log->debug( 'Ignoring remote URL ' . $track->url );
			return;
		}
	}

	my %args = (
	    'method'        => 'track.updateNowPlaying',
	    'artist'            => uri_escape_utf8( $artist ),
	    'track'            => uri_escape_utf8( $title ),
	    'album'           => uri_escape_utf8( $album ),
	    'trackNumber' => $tracknum,
	    'mbid'             => $track->musicbrainz_id || '',
	    'duration'        => $duration ? int( $duration ) : ''
	);

	commandLastFM($client, \%args);
}

sub _submitNowPlayingOK {
	my $http    = shift;
	my $content = $http->content;
	my $client  = $http->params('client');
	my $track   = $http->params('track');
	my $retry   = $http->params('retry');
	
	if ( $content =~ /^OK/ ) {
		main::DEBUGLOG && $log->debug('Now Playing track submitted successfully');
	}
	elsif ( $content =~ /^BADSESSION/ ) {
		main::DEBUGLOG && $log->debug('Now Playing failed to submit: bad session');
		
		# Re-handshake and retry once
		handshake( {
			client => $client,
			cb     => sub {
				if ( !$retry ) {
					main::DEBUGLOG && $log->debug('Retrying failed Now Playing submission');				
					submitNowPlaying( $client, $track, 'retry' );
				}
			},
		} );
	}
	else {
		# Treat it as an error
		chomp $content;
		if ( !$content ) {
			$content = 'Unknown error';
		}
		$http->error( $content );
		_submitNowPlayingError( $http );
	}
}

sub _submitNowPlayingError {
	my $http   = shift;
	my $error  = $http->error;
	my $client = $http->params('client');
	my $track  = $http->params('track');
	my $retry  = $http->params('retry');
	
	if ( $retry ) {
		main::DEBUGLOG && $log->debug("Now Playing track failed to submit after retry: $error, giving up");
		return;
	}
	
	main::DEBUGLOG && $log->debug("Now Playing track failed to submit: $error, retrying in 5 seconds");
	
	# Retry once after 5 seconds
	Slim::Utils::Timers::killTimers( $client, \&submitNowPlaying );
	Slim::Utils::Timers::setTimer(
		$client,
		Time::HiRes::time() + 5,
		\&submitNowPlaying,
		$track,
		'retry',
	);
}

sub checkScrobble {
	my ( $client, $track, $checktime, $rating ) = @_;
	
	return unless $client && $track;
	
	# Make sure player is either playing or paused
	if ( $client->isStopped() ) {
		main::DEBUGLOG && $log->debug( $client->id . ' no longer playing or paused, not scrobbling' );
		return;
	}
	
	# Make sure the track is still the currently playing track
	my $cururl = Slim::Player::Playlist::url($client);
	
	my $artist   = $track->artistName || '';
	my $album    = $track->album  ? $track->album->name  : '';
	my $title    = $track->title;
	my $tracknum = $track->tracknum || '';
	my $duration = $track->secs;
	my $source   = 'P';
	
	if ( $track->remote ) {
		my $handler = Slim::Player::ProtocolHandlers->handlerForURL( $cururl );
		if ( $handler && $handler->can('getMetadataFor') ) {
			# this plugin provides track metadata, i.e. Pandora, Rhapsody
			my $meta  = $handler->getMetadataFor( $client, $cururl, 'forceCurrent' );			
			$artist   = $meta->{artist};
			$album    = $meta->{album} || '';
			$title    = $meta->{title};
			$tracknum = $meta->{tracknum} || '';
			$duration = $meta->{duration} || $track->secs;
			
			# Handler must return at least artist and title
			unless ( $meta->{artist} && $meta->{title} ) {
				main::DEBUGLOG && $log->error( "Protocol Handler didn't return an artist and title for $cururl, ignoring" );
				return;
			}
			
			# Make sure user is still listening to the same track
			if ( $track->stash->{_plugin_title} && $title ne $track->stash->{_plugin_title} ) {
				main::DEBUGLOG && $log->error( $track->stash->{_plugin_title} . ' - Currently playing track has changed, not scrobbling' );
				return;
			}
			
			# Get the source type from the plugin
			if ( $handler->can('audioScrobblerSource') ) {
				$source = $handler->audioScrobblerSource( $client, $cururl );
				
				# Ignore radio tracks if requested, unless rating = L
				if ( !defined $rating || $rating ne 'L' ) {
					my $include_radio;
					if ( main::SLIM_SERVICE ) {
						$include_radio = $prefs->client($client)->get( 'include_radio', undef, 'UserPref' );
					}
					else {
						$include_radio = $prefs->get('include_radio');
					}
					
					if ( defined $include_radio && !$include_radio && $source =~ /^[RE]$/ ) {
						main::DEBUGLOG && $log->error("Ignoring radio URL $cururl, scrobbling of radio is disabled");
						return;
					}
				}
			}
		}
		else {
			main::DEBUGLOG && $log->error( 'Ignoring remote URL ' . $cururl );
			return;
		}
	}
	elsif ( $cururl ne $track->url ) {
		if ( main::DEBUGLOG && $log->is_debug ) {
			$log->error( $track->title . ' - Currently playing track has changed, not scrobbling' );
		}
		
		return;
	}
	
	# Check songtime for the song to see if they paused the track
	my $songtime = Slim::Player::Source::songTime($client);
	if ( $songtime < $checktime ) {
		my $diff = $checktime - $songtime;
		
		main::DEBUGLOG && $log->error( "$title - Not yet reached $checktime playback seconds, waiting $diff more seconds" );
		
		Slim::Utils::Timers::killTimers( $client, \&checkScrobble );
		Slim::Utils::Timers::setTimer(
			$client,
			Time::HiRes::time() + $diff,
			\&checkScrobble,
			$track,
			$checktime,
			$rating,
		);
		
		return;
	}
	
	main::DEBUGLOG && $log->warn( "$title - Queueing track for scrobbling in $checktime seconds" );
	
	my $queue = getQueue($client);
	
	push @{$queue}, {
		_url => $cururl,
		a    => uri_escape_utf8( $artist ),
		t    => uri_escape_utf8( $title ),
		i    => int( $client->currentPlaylistChangeTime() ),
		o    => $source,
		r    => $rating || '', # L for thumbs-up for Pandora/Lastfm, B for Lastfm ban, S for Lastfm skip
		l    => ( $duration ? int( $duration ) : '' ),
		b    => uri_escape_utf8( $album ),
		n    => $tracknum,
		m    => ( $track->musicbrainz_id || '' ),
	};
	
	setQueue( $client, $queue );
	
	# Report the Love
	if ( $rating && $rating eq 'L') {
		submitLoveTrack( $client, $queue->[-1] );
	}
	if ( $rating && $rating eq 'B' ) {
		main::DEBUGLOG && $log->error( "$title - banning" );
		submitBanTrack( $client, $queue->[-1] );
	}	
	#warn "Queue is now: " . Data::Dump::dump($queue) . "\n";
	
	# Scrobble in $checktime seconds, the reason for this delay is so we can report
	# thumbs up/down status.  The reason for the extra 10 seconds is so if there is a
	# Now Playing request from the next track, it will do the submit instead.
	Slim::Utils::Timers::killTimers( $client, \&submitScrobble );
	Slim::Utils::Timers::setTimer(
		$client,
		Time::HiRes::time() + $checktime + 10,
		\&submitScrobble,
	);
}

sub submitScrobble {
	my ( $client, $params ) = @_;
	
	$params ||= {};
	$params->{retry} ||= 0;
	
	my $cb = $params->{cb} || sub {};
	
	# Remove any other pending submit timers
	Slim::Utils::Timers::killTimers( $client, \&submitScrobble );

	my $queue = getQueue($client);
	
	if ( !scalar @{$queue} ) {
		# Queue was already submitted, probably by the Now Playing request
		return $cb->();
	}
	
	# Abort if the user disabled scrobbling for this player
	my $account = $prefs->client($client)->get('account');
	if ( !$account ) {
		main::DEBUGLOG && $log->debug( 'User disabled scrobbling for this player, wiping queue and not submitting' );
		setQueue( $client, [] );
		return $cb->();
	}

	if ( !$client->master->pluginData('submit_url') ) {
		# Get a new session
		handshake( {
			client => $client,
			cb     => sub {
				submitScrobble( $client, $params );
			},
		} );
		return;
	}

	if ( main::DEBUGLOG && $log->is_debug ) {
		$log->warn( 'Scrobbling ' . scalar( @{$queue} ) . ' queued item(s)' );
		#$log->debug( Data::Dump::dump($queue) );
	}
	
	# Get the currently playing track
	my $current_track;
	if ( my $url = Slim::Player::Playlist::url($client) ) {
		$current_track = Slim::Schema->objectForUrl( { url => $url } );
	}
	
	my $current_item;
	my @tmpQueue;

	my $playername = getPlayerKey($client);
	my $player = getPlayer($playername);
	my %args = ('method' => 'track.scrobble', 'sk' => $player->session);

	my $key;
	my $value;
	my $index = 0;
	my $url;
	while ( my $item = shift @{$queue} ) {
		# Don't submit tracks that are still playing, to allow user
		# to rate the track
		if ( $current_track && stillPlaying( $client, $current_track, $item ) ) {
			main::DEBUGLOG && $log->warn( "Track " . $item->{t} . " is still playing, not submitting" );
			$current_item = $item;
			next;
		}
		# Ignore tracks from Spotify and other on-demand services from queue
		if ($item->{_url} =~ /^spotify/) {
			next;
		}
		push @tmpQueue, $item;
		for my $p ( keys %{$item} ) {
			$key = '';
			$value = '';
			# each value is already uri-escaped
			if($p eq 'l'){
				$key = 'duration[' . $index . ']';
				$value = $item->{$p};
			}
			elsif($p eq 'a'){
				$key = 'artist[' . $index . ']';
				$value = uri_unescape($item->{$p});
			}
			elsif($p eq 'b'){
				$key = 'album[' . $index . ']';
				$value = uri_unescape($item->{$p});
			}
			elsif($p eq 'i'){
				$key = 'timestamp[' . $index . ']';
				$value = $item->{$p};
			}
			elsif($p eq 't'){
				$key = 'track[' . $index . ']';
				$value = uri_unescape($item->{$p});
			}
			else{
				next;
			}
			# Skip internal items i.e. _url
			next if $p =~ /^_/ || $item->{$p} eq '' || $key eq '';
			$args{$key} = $value;
		}
		
		$index++;
		
		# Max size of each scrobble request is 50 items
		last if $index == 50;
	}

	# Add the currently playing track back to the queue
	if ( $current_item ) {
		unshift @{$queue}, $current_item;
	}
	
	if ( @tmpQueue ) {
		# Only setQueue if tmpQueue is nonempty
		# otherwise it means we didn't shift anything out of queue into tmpQueue
		# and $queue is therefore unchanged. prevents disk writes enabling some disks to spindown
		setQueue( $client, $queue );

		main::DEBUGLOG && $log->warn( "Scrobbling track " . Dumper(%args) );
		my $http = Slim::Plugin::AudioScrobbler::LastFMAsyncHTTP->new(
			\&_submitScrobbleOK,
			\&_submitScrobbleError,
			{
				tmpQueue => \@tmpQueue,
				params   => $params,
				client   => $client,
				'api_key'          => $api_key,
				'api_secret'      => $api_secret,
				timeout  => 30,
			},
		);
	
		$http->post_lastfm_signed($lastfm_root, %args);

	}
	
	# If there are still items left in the queue, scrobble again in a minute
	if ( scalar @{$queue} ) {
		Slim::Utils::Timers::killTimers( $client, \&submitScrobble );
		Slim::Utils::Timers::setTimer(
			$client,
			time() + 60,
			\&submitScrobble,
			$params,
		);
	}
}

# Check if a track is still playing
sub stillPlaying {
	my ( $client, $track, $item ) = @_;
	
	my $artist   = $track->artistName || '';
	my $album    = $track->album  ? $track->album->name  : '';
	my $title    = $track->title;
	
	# Bug 12240: if we have stopped (probably at the end of the playlist) then we are not still playing
	if ($client->isStopped()) {
		return 0;
	}
	
	if ( $track->remote ) {
		my $handler = Slim::Player::ProtocolHandlers->handlerForURL( $track->url );
		if ( $handler && $handler->can('getMetadataFor') ) {
			# this plugin provides track metadata, i.e. Pandora, Rhapsody
			my $meta  = $handler->getMetadataFor( $client, $track->url, 'forceCurrent' );
			$artist   = $meta->{artist};
			$album    = $meta->{album};
			$title    = $meta->{title};
		}
	}
	
	if ( $title ne uri_unescape( $item->{t} ) ) {
		return 0;
	}
	elsif ( $album ne uri_unescape( $item->{b} ) ) {
		return 0;
	}
	elsif ( $artist ne uri_unescape( $item->{a} ) ) {
		return 0;
	}
	
	return 1;
}

sub _submitScrobbleOK {
	my $http     = shift;
	my $params   = $http->params('params');
	main::DEBUGLOG && $log->info( 'Scrobble submit successful' );
 	# If we had a callback on success, call it now
 	if ( $params->{cb} ) {
 		$params->{cb}->();
 	}
}

sub _submitScrobbleError {
	my $http     = shift;
	my $error    = $http->error;
	my $tmpQueue = $http->params('tmpQueue') || [];
	my $params   = $http->params('params');
	my $client   = $http->params('client');
	
	# put the tmpQueue items back into the main queue
	my $queue = getQueue($client);
	push @{$queue}, @{$tmpQueue};
	setQueue( $client, $queue );
	
	if ( $params->{retry} == 3 ) {
		# after 3 failures, give up and handshake
		main::DEBUGLOG && $log->error( "Scrobble submit failed after 3 tries, re-handshaking" );
		handshake( { client => $client } );
		return;
	}
	
	my $tries = 3 - $params->{retry};
	main::DEBUGLOG && $log->error( "Scrobble submit failed: $error, will retry in 5 seconds ($tries tries left)" );
	
	# Retry after a short delay
	$params->{retry}++;
	Slim::Utils::Timers::killTimers( $client, \&submitScrobble );
	Slim::Utils::Timers::setTimer(
		$client,
		Time::HiRes::time() + 5,
		\&submitScrobble,
		$params,
	);
}

sub loveTrack {
	my $request = shift;
	my $client  = $request->client || return;
	my $url     = $request->getParam('_url');
	
	# Ignore if not Scrobbling
	return if !$prefs->client($client)->get('account');
	my $enable_scrobbling;
	if ( main::SLIM_SERVICE ) {
		$enable_scrobbling  = $prefs->client($client)->get('enable_scrobbling');
	}
	else {
		$enable_scrobbling  = $prefs->get('enable_scrobbling');
	}
	return unless $enable_scrobbling;
	
	main::DEBUGLOG && $log->debug( "Love: $url" );
	# Look through the queue and update the item we want to love
	my $queue = getQueue($client);
	for my $item ( @{$queue} ) {
		if ( $item->{_url} eq $url ) {
			$item->{r} = 'L';
			setQueue( $client, $queue );
			submitLoveTrack( $client, $item );
			return 1;
		}
	}
	
	# The track wasn't already in the queue, they probably rated the track
	# before getting halfway through.  Call checkScrobble with a checktime
	# of 0 to force it to be added to the queue with the rating of L
	my $track = Slim::Schema->objectForUrl( { url => $url } );
	Slim::Utils::Timers::killTimers( $client, \&checkScrobble );
	checkScrobble( $client, $track, 0, 'L' );
	return 1;
}

sub submitLoveTrack {
	my ( $client, $item ) = @_;
	my %args = (
	    'method'      => 'track.love',
	    'artist'          =>uri_unescape($item->{a}),
	    'track'          => uri_unescape($item->{t})
	);
	commandLastFM($client, \%args);
}

sub banTrack {
	my $request = shift;
	my $client  = $request->client || return;
	my $url     = $request->getParam('_url');
	my $skip    = $request->getParam('_skip') || 0;

	# Ban is only supported for Spotify URLs
	return unless $url =~ /^spotify/;
	
	# Skip to the next track
	if ( $skip ) {
		$client->execute([ 'playlist', 'jump', '+1' ]);
	}
	
	# Ignore if not Scrobbling
	return if !$prefs->client($client)->get('account');
	my $enable_scrobbling;
	if ( main::SLIM_SERVICE ) {
		$enable_scrobbling  = $prefs->client($client)->get('enable_scrobbling');
	}
	else {
		$enable_scrobbling  = $prefs->get('enable_scrobbling');
	}
	return unless $enable_scrobbling;

	main::DEBUGLOG && $log->error( "Banning: $url" );
	# Look through the queue and update the item we want to ban
	my $queue = getQueue($client);
 	for my $item ( @{$queue} ) {
 		if ( $item->{_url} eq $url ) {
 			$item->{r} = 'B';
 			main::DEBUGLOG && $log->error( "Queueing for ban: $url" );
 			setQueue( $client, $queue );
 			submitBanTrack( $client, $item );
 			return 1;
 		}
 	}

	# The track wasn't already in the queue, they probably rated the track
	# before getting halfway through.  Call checkScrobble with a checktime
	# of 0 to force it to be added to the queue with the rating of B
	my $track = Slim::Schema->objectForUrl( { url => $url } );
	Slim::Utils::Timers::killTimers( $client, \&checkScrobble );
	checkScrobble( $client, $track, 0, 'B' );
	return 1;
}

sub submitBanTrack {
	my ( $client, $item ) = @_;
	my %args = (
	    'method'      => 'track.ban',
	    'artist'          => uri_unescape($item->{a}),
	    'track'          => uri_unescape($item->{t})
	);
	commandLastFM($client, \%args);
}

sub removeTrack {
	my $request = shift;
	my $client  = $request->client || return;

	if ( !$client->master->pluginData('now_playing_url') ) {
		# Get a new session
		handshake( {
			client => $client,
			cb     => sub {
				removeTrack( $request );
			},
		} );
		return;
	}

	my $artist     = $request->getParam('_artist');
	my $title    = $request->getParam('_title') || 0;
	my %args = (
	    'method'      => 'library.removeTrack',
	    'artist'          => $artist,
	    'track'          => $title
	);
	commandLastFM($client, \%args);
}

sub getTracks {
	my ($request, $user, $list, $page) = @_;
	my $request = shift;
	my $client  = $request->client() || return;
	my $method;
	if($list eq 'loved'){
		$method = 'user.getLovedTracks';
	}
	elsif($list eq 'music'){
		$method = 'user.getTopTracks';
	}
	else{
		$log->error( "Unknown LastFM list: $list" );
		return;
	}

	my %args = (
	    'method'      => $method,
	    'user'      => $user,
	    'page'      => $page,
	);

	commandLastFM($client, \%args,
		sub{
			my $http = shift;
			playLastFMTracks($client, $http, $page);
		}
	);
}

sub playLastFMTracks {
	my $json = eval { from_json($_[1]->content) };
	if ($@) {
		$log->warn("bad json: $@");
		return;
	}
	$log->debug("json: ".Dumper($json->{'toptracks'}));
	my $client = $_[0];
	my $name;
	my $duration;
	my $mbid;
	my $artist;
	my $album;
	my $search;
	my @tracks = ();
	for my $track (@{ $json->{'toptracks'}->{'track'} }) {
		$name = uri_escape_utf8($track->{'name'});
		$name =~ s/\%20/+/g;
		$duration = $track->{'duration'};
		$mbid = $track->{'mbid'};
		$artist = $track->{'artist'}?uri_escape_utf8($track->{'artist'}->{'name'}):'';
		$artist =~ s/\%20/+/g;
		$album = $track->{'album'}?uri_escape_utf8($track->{'album'}->{'name'}):'';
		$album =~ s/\%20/+/g;
		$search = "artist:".$artist."+album:".$album."+track:".$name;
		#$log->warn("search: ".$search);
		my $tmpuri =  `/home/fjob/bin/mycurl "https://api.spotify.com/v1/search?type=track&q=".$search | grep '"uri" : "spotify:track:' | head -1 | awk -F' : ' '{print \$2}' | sed 's|"||g'`;
		if($tmpuri eq ""){
			$search = "artist:".$artist."+track:".$name;
			$tmpuri =  `/home/fjob/bin/mycurl "https://api.spotify.com/v1/search?type=track&q=".$search | grep '"uri" : "spotify:track:' | head -1 | awk -F' : ' '{print \$2}' | sed 's|"||g'`;
		}
		$tmpuri =~ s/^\s+|\s+$//g;
		$log->warn("uri: ".$tmpuri);
		if($tmpuri ne ""){
			push @tracks, $tmpuri;
		}
	}
	my $uri = shift(@tracks);
	Slim::Utils::Timers::setTimer(undef, Time::HiRes::time(), sub {
		$client->master->execute([ 'spotifyplcmd', 'cmd:load', "uri:$uri" ]);
	});
	foreach $uri (@tracks){
		sleep 0.3;
		Slim::Utils::Timers::setTimer(undef, Time::HiRes::time()+10, sub {
			$client->master->execute([ 'spotifyplcmd', 'cmd:add', "uri:$uri" ]);
		});
	}
	# Not working
	#Slim::Utils::Timers::setTimer(undef, Time::HiRes::time()+20, sub {
	#	$client->master->execute([ 'spotifyplcmd', 'cmd:add', "uri:".$client->track->url ]);
	#});
}

#/home/fjob/bin/mycurl https://api.spotify.com/v1/search?type=track&q=artist:Kyo+track:Je+te+vends+mon+%C3%A2me

#######################

sub commandLastFM {
	my $client = shift;
	my %args = %{$_[0]};
	shift;
	my $cb = shift;
	my $playername = getPlayerKey($client);
	my $player = getPlayer($playername);

	my $params = {
	    'client'              => $client,
	    'api_key'           => $api_key,
	    'api_secret'       => $api_secret
	};

	$args{'sk'} = $player->session;

	my $http = Slim::Plugin::AudioScrobbler::LastFMAsyncHTTP->new(
		 sub {
			my $http = shift;
			main::DEBUGLOG && $log->debug( ' track response: ' . $http->content );
			if($cb){
				$cb->($http);
			};
		},
		sub {
			my $http = shift;
			main::DEBUGLOG && $log->warn(' track error: ' . $http->error );
		},
		$params
	);
	#$log->warn("Connecting with: ".Dumper($http)."\n");
	#$log->warn("Connecting to: ".Dumper($lastfm_root)."\n");
	#$log->warn("Sending: ".Dumper(%args)."\n");
	$http->post_lastfm_signed($lastfm_root, %args);

	if ($args{'method'} eq 'track.ban') {
		#skip current track
		$client->execute(['playlist','jump', "+1"]);
	}
}

sub showMessage {
	my ($client, $line1, $line2) = @_;
	$client->showBriefly(
		{line=>[$line1, $line2],
		jive => { 
			'type'	  => 'song', #or popupplay?
			'icon'	  => Slim::Plugin::AudioScrobbler::Plugin->_pluginDataFor('icon'),
			'text'    => [ $line1, $line2 ],
		   	},
		},
		{
			duration => 2,
			block => 1
		}
	)
}

sub getLastFMSession {     
	my $client = shift;
	my $force = shift;
	my $params = shift;
	$lastfm_user = $params->{username};
	my $lastfm_password = $params->{password};
	$log->warn("Credentials: $lastfm_user, $lastfm_password\n");
	my $playername = getPlayerKey($client);
	my $player = getPlayer($playername);
	
	return 0 unless (defined $lastfm_user and defined $lastfm_password);
	return 1 if ($player->session ne 'UNSET' and !$force);

	$player->status("STARTING");
	$params->{client} = $client;
	$params->{api_key} = $api_key;
	$params->{api_secret} = $api_secret;
	my $http = Slim::Plugin::AudioScrobbler::LastFMAsyncHTTP->new(
		\&gotSession,
		\&failedSession,
		$params,
	);
	my %args;
	%args = (
	    'method'              => "auth.getMobileSession",
	    'username'           => $lastfm_user,
	    'password'           => $lastfm_password
	);
	$log->warn("Connecting to: $http\n");
	$http->post_lastfm_signed($lastfm_root, %args);
	return 1;
}

sub gotSession{
	my $http = shift;
    	if (defined $http) {
		my $lastFMData = $http->content();
		my $client = $http->params('client');
		$http->close();
		$log->warn("lastFMData:". $lastFMData);
		my $playername = getPlayerKey($client);
		my $player = getPlayer($playername);
		my $data = parseLastFMData($lastFMData);
		if ($lastFMData =~ /FAILED/) {
			$log->warn('Failed login!');
			$player->session('UNSET');
			$player->status('FAILEDLOGIN');
			return;
		}
		$player->session($data->{session}->{key});
		$player->status(undef);
		$client->master->pluginData( handshake_delay => 0 );
		$client->master->pluginData( now_playing_url => $now_playing_url );
		$client->master->pluginData( submit_url      => $submit_url );
		if ( $http->params->{'cb'} ) {
			$http->params->{'cb'}->();
		}
	}
}

sub failedSession{
	my $http = shift;
    	$log->warn("Handshake connection failed! ");
	if (my $client = $http->params('client')) {
		my $playername = getPlayerKey($client);
		my $player = getPlayer($playername);
    		$player->status('FAILEDCONNECT');
		$player->session('UNSET');
	}
}

sub parseLastFMData {
	my $lastFMData = Slim::Utils::Misc::unescape(shift);
	#my $xml = new XML::Simple;
	#my $data = $xml->XMLin("$lastFMData");
	my $data = decode_json($lastFMData);
	#my $data = JSON::XS::decode_json($lastFMData);
	#$log->warn(Dumper($data));
	return $data;
}

sub getPlayerKey {
	my $client = shift;
	return unless $client;
	return $client->macaddress || $client->ip;
}

sub getPlayer {
	my $name = shift;
	if (!$players{$name}) {
		$players{$name} = LastFMStatus->new();
		$players{$name}->listitem(0);
		$players{$name}->subscriber(1);
		$players{$name}->session("UNSET");
		$players{$name}->tracks("UNSET");
		$players{$name}->disco(0);
		$players{$name}->rtp(1);
	}
	return $players{$name};
}

sub getTrackData{
	my $client = shift;
	return unless $client;
	my $song = $client->playingSong(); #or streamingSong() ?
	return unless $song;
	my $dat = $song->pluginData();
	return $dat;
}

struct LastFMStatus => {
	session => '$',			#lastfm session id
	stream_url => '$',		#stream url
	rtp => '$',			#1/0 - record to profile status
	disco => '$',			#1/0 - discovery status
	subscriber => '$',		#1/0 - true/false
	station => '$',			#current station name
	station_url => '$',		#current station url
	listitem => '$',			#position in top-level menu
	base_url => '$',		#lastfm streaming server address
	status => '$',			#our status
	datahash => '$',		#hashref - everything we get from LastFM in an update goes here
	commands => '$',		#arrayref of commands
	tracks => '$',			#ref of array of upcoming tracks
	skips => '$',			#skips left
};

###############

# Return whether or not the given track will be scrobbled
sub canScrobble {
	my ( $class, $client, $track ) = @_;
	
	# Ignore if not Scrobbling
	return if !$prefs->client($client)->get('account');

	my $enable_scrobbling;
	if ( main::SLIM_SERVICE ) {
		$enable_scrobbling  = $prefs->client($client)->get('enable_scrobbling');
	}
	else {
		$enable_scrobbling  = $prefs->get('enable_scrobbling');
	}
	
	return unless $enable_scrobbling;
	
	if ( $track->remote ) {
		my $handler = Slim::Player::ProtocolHandlers->handlerForURL( $track->url );
		if ( $handler ) {
			
			# Must be over 30 seconds
			if ( $handler->can('getMetadataFor') ) {
				my $meta = $handler->getMetadataFor( $client, $track->url, 'forceCurrent' );
				my $duration = $meta->{duration} || $track->secs;
				if ( $duration && $duration < 30 ) {
					return;
				}
			}
			
			# Must provide a source
			if ( $handler->can('audioScrobblerSource') ) {
				if ( my $source = $handler->audioScrobblerSource( $client, $track->url ) ) {
					return 1;
				}
			}
		}
	}
	else {
		# Must be over 30 seconds
		return if $track->secs && $track->secs < 30;
		
		return 1;
	}

	return;
}

sub getAccounts {
	my $client = shift;
	
	my $accounts;
	
	if ( main::SLIM_SERVICE ) {
		$accounts = $prefs->client($client)->get( 'accounts', undef, 'UserPref' ) || [];
	
		if ( !ref $accounts ) {
			$accounts = from_json( $accounts );
		}
	}
	else {	
		$accounts = $prefs->get('accounts') || [];
	}
	
	return $accounts;
}

sub getQueue {
	my $client = shift;	
	my $queue;
	if ( main::SLIM_SERVICE ) {
		$queue = SDI::Service::Model::ScrobbleQueue->get( $client->playerData->id );
	}
	else {
		$queue = $prefs->client($client)->get('queue') || [];
	}

	my @cleanQueue = ();
	my $item;
	foreach (@{$queue} ) {
		 $item = $_;
		# Ignore tracks from Spotify and other on-demand services from queue
		if ($item->{_url} =~ /^spotify/) {
			push @cleanQueue, $item;
			next;
		}
	}
	# Clean out non-scrobblable tracks from queue
	if ( @cleanQueue ) {
		$log->warn( "Cleaning tracks " . scalar @cleanQueue);
		my @cleanIndices = ();
		my @delIndices;
		while ( my $item = shift @cleanQueue ) {
			my @delIndices = grep { ${$queue}[$_] eq $item} 0..$#{$queue};
		}
		push @delIndices, pop @delIndices;
		while ( my $index = shift @cleanIndices ) {
			splice(@{$queue}, $index, 1);
		}
		setQueue( $client, $queue );
	}
	$log->warn("Got queue: ".@{$queue});
	return $queue;
}

sub setQueue {
	my ( $client, $queue ) = @_;
	
	if ( main::SLIM_SERVICE ) {
		SDI::Service::Model::ScrobbleQueue->set( $client->playerData->id, $queue );
	}
	else {
		$prefs->client($client)->set( queue => $queue );
	}
}

sub infoLoveTrack {
	my ( $client, $url, $track, $remoteMeta ) = @_;

	return unless $client;
	
	# Ignore if the current track can't be scrobbled
	if ( !__PACKAGE__->canScrobble( $client, $track ) ) {
		return;
	}
	
	# Ignore if this track isn't currently playing, you can only love
	# something that is playing and being scrobbled
	if ( $track->url ne Slim::Player::Playlist::url($client) ) {
		return;
	}
	
	return {
		type        => 'link',
		name        => $client->string('PLUGIN_AUDIOSCROBBLER_LOVE_TRACK'),
		url         => \&infoLoveTrackSubmit,
		passthrough => [ $url ],
		favorites   => 0,
	};
}

sub infoBanTrack {
	my ( $client, $url, $track, $remoteMeta ) = @_;

	return unless $client;
	
	# Ignore if the current track can't be scrobbled
	if ( !__PACKAGE__->canScrobble( $client, $track ) ) {
		return;
	}
	
	# Ignore if this track isn't currently playing, you can only love
	# something that is playing and being scrobbled
	if ( $track->url ne Slim::Player::Playlist::url($client) ) {
		return;
	}
	
	return {
		type        => 'link',
		name        => 'Last.fm: ban this track',
		url         => \&infoBanTrackSubmit,
		passthrough => [ $url ],
		favorites   => 0,
	};
}

sub infoRemoveTrack {
	my ( $client, $url, $track, $remoteMeta ) = @_;

	return unless $client;
	
	return {
		type        => 'link',
		name        => 'Last.fm: remove this track',
		url         => \&infoRemoveTrackSubmit,
		passthrough => [ $track->artistName, $track->title  ],
		favorites   => 0,
	};
}

sub infoLoveTrackSubmit {
	my ( $client, $callback, undef, $url ) = @_;

	return unless $client;
	
	$client->execute( [ 'audioscrobbler', 'loveTrack', $url ] );
	
	$callback->( {
		items => [{
			type => 'text',
			name        => $client->string('PLUGIN_AUDIOSCROBBLER_TRACK_LOVED'),
			wrap => 0,
			showBriefly => 10,
			favorites   => 10,
		}]
	}, @_ );
}

sub infoBanTrackSubmit {
	my ( $client, $callback, undef, $url ) = @_;

	return unless $client;
	
	$client->execute( [ 'audioscrobbler', 'banTrack', $url , '_skip'] );
	
	$callback->( {
		items => [{
			type => 'text',
			name =>'This track has been banned',
			wrap => 0,
			showBriefly => 10,
			favorites   => 10,
		}]
	}, @_ );
}

sub infoRemoveTrackSubmit {
	my ( $client, $callback, undef, $artist, $title ) = @_;

	return unless $client;
	
	$client->execute( [ 'audioscrobbler', 'removeTrack', $artist, $title ] );
	
	$log->error( "callback: "+Dumper($callback));

	$callback->( {
		items => [{
			type => 'text',
			name =>'This track has been removed from your library',
			wrap => 0,
			showBriefly => 10,
			favorites   => 10,
		}]
	}, @_ );

}

sub jiveSettings {

	my $client = shift;

	return [ {
		stringToken    => getDisplayName(),
		id             => 'audioscrobbler',
		node           => 'advancedSettings',
		weight         => 100,
		actions => {
			go => {
				player => 0,
				cmd    => [ 'audioscrobbler', 'settings' ],
			},
		},
	} ];
}

sub jiveSettingsCommand {
	my $request = shift;
	my $client  = $request->client();
	my $account = $request->getParam('user');

	main::DEBUGLOG && $log->debug('Setting account to: ' . $account);
	changeAccount( $client, $account );

	$request->setStatusDone();

}
	
sub jiveSettingsMenu {

	my $request  = shift;
	my $client   = $request->client();
	my $accounts = getAccounts($client);
	my $enabled  = $prefs->get('enable_scrobbling');
	my $selected = $prefs->client($client)->get('account');

	my @menu     = ();

	for my $account (@$accounts) {
		my $item = {
			text    => $account->{username},
			radio   => ($selected eq $account->{username} && $enabled) + 0,
			actions => {
				do => {
					player => 0,
					cmd    => [ 'audioscrobbler' , 'account' ],
					params => {
						user => $account->{username},
					},
				},
			},
		};
		push @menu, $item;
	}

	# disable for this player
	my $disableItem = {
		text    => $client->string('PLUGIN_AUDIOSCROBBLER_SCROBBLING_DISABLED'),
		radio   => ($selected eq '0') + 0,
		actions => {
			do => {
				player => 0,
				cmd    => [ 'audioscrobbler' , 'account' ],
				params => {
					user => '0',
				},
			},
		},
	};
	push @menu, $disableItem;

	my $numitems = scalar(@menu);
	$request->addResult("count", $numitems);
	$request->addResult("offset", 0);
	my $cnt = 0;
	for my $eachItem (@menu[0..$#menu]) {
		$request->setResultLoopHash('item_loop', $cnt, $eachItem);
		$cnt++;
	}
	$request->setStatusDone();
}

1;
