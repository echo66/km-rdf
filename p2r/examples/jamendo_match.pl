:- module(jamendo_match,[]).

:- consult(library('semweb/rdf_db')).

:- use_module(match).

:- consult(jamendo_ns).


/**
 * Class mappings
 */
match:
	(jamendo:artist(Id))
		eq
	[
		rdf(pattern(['http://dbtune.org/jamendo/resource/artist/',Id]),rdf:type,mo:'MusicArtist')
	].

match:
	(jamendo:album(Id))
		eq
	[
		rdf(pattern(['http://dbtune.org/jamendo/resource/record/',Id]),rdf:type,mo:'Record')
	].


match:
	(jamendo:track(Id))
		eq
	[
		rdf(pattern(['http://dbtune.org/jamendo/resource/track/',Id]),rdf:type,mo:'Track')
	,	rdf(pattern(['http://dbtune.org/jamendo/resource/signal/',Id]),mo:publishedAs,pattern(['http://dbtune.org/jamendo/resource/track/',Id]))
	,	rdf(pattern(['http://dbtune.org/jamendo/resource/signal/',Id]),rdf:type,mo:'Signal')
	,	rdf(pattern(['http://dbtune.org/jamendo/resource/signal/',Id]),mo:signalTime,pattern(['http://dbtune.org/jamendo/resource/interval/',Id]))
	,	rdf(pattern(['http://dbtune.org/jamendo/resource/interval/',Id]),rdf:type,time:'Interval')
	,	rdf(pattern(['http://dbtune.org/jamendo/resource/interval/',Id]),tl:onTimeLine,pattern(['http://dbtune.org/jamendo/resource/timeline/',Id]))
	,	rdf(pattern(['http://dbtune.org/jamendo/resource/performance/',Id]),mo:recordedAs,pattern(['http://dbtune.org/jamendo/resource/signal/',Id]))
	].

/**
 * Property mappings
 */

/**
 * Artists
 */
match:
	(jamendo:artist_dispname(Id,Name))
		eq
	[
		rdf(pattern(['http://dbtune.org/jamendo/resource/artist/',Id]),foaf:name,literal(type('http://www.w3.org/2001/XMLSchema#string',Name)))
	].

match:
	(jamendo:artist_description(Id,Desc))
		eq
	[
		rdf(pattern(['http://dbtune.org/jamendo/resource/artist/',Id]),mo:biography,literal(type('http://www.w3.org/2001/XMLSchema#string',Desc)))
	].

%make use of tabled equivalence here
match:
	(jamendo:artist_geo(Id,Geo)) 
		eq
	[
		rdf(pattern(['http://dbtune.org/jamendo/resource/artist/',Id]),foaf:based_near,literal(Geo))
	].

match:
	(jamendo:artist_homepage(Id,HomePage)) 
		eq
	[
		rdf(pattern(['http://dbtune.org/jamendo/resource/artist/',Id]),foaf:homepage,HomePage)
	].

match:
	(jamendo:artist_image(Id,Image))
		eq
	[
		rdf(pattern(['http://dbtune.org/jamendo/resource/artist/',Id]),foaf:img,Image)
	].

/**
 * Albums
 */
match:
	(jamendo:album_name(Id,Name))
		eq
	[
		rdf(pattern(['http://dbtune.org/jamendo/resource/record/',Id]),dc:title,literal(type('http://www.w3.org/2001/XMLSchema#string',Name)))
	].

match:
	(jamendo:album_description(Id,Desc))
		eq
	[
		rdf(pattern(['http://dbtune.org/jamendo/resource/record/',Id]),dc:description,literal(type('http://www.w3.org/2001/XMLSchema#string',Desc)))
	].

match:
	(jamendo:album_artist(Id,ArtistId))
		eq
	[
		rdf(pattern(['http://dbtune.org/jamendo/resource/record/',Id]),dc:creator,pattern(['http://dbtune.org/jamendo/resource/artist/',ArtistId]))
	].

match:
	(jamendo:album_releasedate(Id,Date))
		eq
	[
		rdf(pattern(['http://dbtune.org/jamendo/resource/record/',Id]),dc:date,literal(Date))
	].

match:
	(jamendo:album_cover(Id,_,_,Cover))
		eq
	[
		rdf(pattern(['http://dbtune.org/jamendo/resource/record/',Id]),mo:image,Cover)
	].
match:
	(jamendo:album_p2p(Id,ogg3,ed2k,Link))
		eq
	[
		rdf(pattern(['http://dbtune.org/jamendo/resource/record/',Id]),mo:availableAs,pattern(['http://dbtune.org/jamendo/resource/item/',Id]))
	,	rdf(pattern(['http://dbtune.org/jamendo/resource/item/ed2k/ogg3/',Id]),rdf:type,mo:'Ed2kItem')
	,	rdf(pattern(['http://dbtune.org/jamendo/resource/item/ed2k/ogg3/',Id]),dc:format,literal(ogg3))
	,	rdf(pattern(['http://dbtune.org/jamendo/resource/item/ed2k/ogg3/',Id]),mo:link,Link)
	].
match:
        (jamendo:album_p2p(Id,mp32,ed2k,Link))
                eq
        [
                rdf(pattern(['http://dbtune.org/jamendo/resource/record/',Id]),mo:availableAs,pattern(['http://dbtune.org/jamendo/resource/item/ed2k/mp32/',Id]))
        ,       rdf(pattern(['http://dbtune.org/jamendo/resource/item/ed2k/mp32/',Id]),rdf:type,mo:'Ed2kItem')
        ,       rdf(pattern(['http://dbtune.org/jamendo/resource/item/ed2k/mp32/',Id]),dc:format,literal(mp32))
        ,       rdf(pattern(['http://dbtune.org/jamendo/resource/item/ed2k/mp32/',Id]),mo:link,Link)
        ].
match:
        (jamendo:album_p2p(Id,ogg3,bittorrent,Link))
                eq
        [
                rdf(pattern(['http://dbtune.org/jamendo/resource/record/',Id]),mo:availableAs,pattern(['http://dbtune.org/jamendo/resource/item/bittorrent/ogg3/',Id]))
        ,       rdf(pattern(['http://dbtune.org/jamendo/resource/item/bittorrent/ogg3/',Id]),rdf:type,mo:'BittorrentItem')
        ,       rdf(pattern(['http://dbtune.org/jamendo/resource/item/bittorrent/ogg3/',Id]),dc:format,literal(ogg3))
        ,       rdf(pattern(['http://dbtune.org/jamendo/resource/item/bittorrent/ogg3/',Id]),mo:link,Link)
        ].
match:
        (jamendo:album_p2p(Id,mp32,bittorrent,Link))
                eq
        [
                rdf(pattern(['http://dbtune.org/jamendo/resource/record/',Id]),mo:availableAs,pattern(['http://dbtune.org/jamendo/resource/item/bittorrent/mp32/',Id]))
        ,       rdf(pattern(['http://dbtune.org/jamendo/resource/item/bittorrent/mp32/',Id]),rdf:type,mo:'BittorrentItem')
        ,       rdf(pattern(['http://dbtune.org/jamendo/resource/item/bittorrent/mp32/',Id]),dc:format,literal(mp32))
        ,       rdf(pattern(['http://dbtune.org/jamendo/resource/item/bittorrent/mp32/',Id]),mo:link,Link)
        ].

match:
	(jamendo:album_tag(_,Tag)) 
		eq
	[
		rdf(pattern(['http://dbtune.org/jamendo/resource/tag/',Tag]),tags:tagName,literal(type('http://www.w3.org/2001/XMLSchema#string',Tag)))
	,	rdf(pattern(['http://dbtune.org/jamendo/resource/tag/',Tag]),rdf:type,tags:'Tag')
	].
match:
	(jamendo:album_tag(Id,Tag))
		eq
	[
		rdf(pattern(['http://dbtune.org/jamendo/resource/record/',Id]),tags:taggedWithTag,pattern(['http://dbtune.org/jamendo/resource/tag/',Tag]))
	].

/**
 * Tracks
 */
match:
	(jamendo:track_album(Id,AlbumId))
		eq
	[
		rdf(pattern(['http://dbtune.org/jamendo/resource/record/',AlbumId]),mo:hasTrack,pattern(['http://dbtune.org/jamendo/resource/track/',Id]))
	].

match:
	(jamendo:track_no(Id,No))
		eq
	[
		rdf(pattern(['http://dbtune.org/jamendo/resource/track/',Id]),mo:trackNum,literal(type('http://www.w3.org/2001/XMLSchema#int',No)))
	].

match:
	(jamendo:track_name(Id,Name))
		eq
	[
		rdf(pattern(['http://dbtune.org/jamendo/resource/track/',Id]),dc:title,literal(type('http://www.w3.org/2001/XMLSchema#string',Name)))
	].

match:
	(jamendo:track_licenseurl(Id,License))
		eq
	[
		rdf(pattern(['http://dbtune.org/jamendo/resource/track/',Id]),mo:license,License)
	,	rdf(License,rdf:type,foaf:'Document')
	].
match:
	(jamendo:track_lyrics(Id,Lyrics))
		eq
	[
		rdf(pattern(['http://dbtune.org/jamendo/resource/performance/',Id]),event:hasFactor,pattern(['http://dbtune.org/jamendo/resource/lyrics/',Id]))
	,	rdf(pattern(['http://dbtune.org/jamendo/resource/lyrics/',Id]),rdf:type,mo:'Lyrics')
	,	rdf(pattern(['http://dbtune.org/jamendo/resource/lyrics/',Id]),mo:lyricsText,literal(type('http://www.w3.org/2001/XMLSchema#string',Lyrics)))
	].


