#!/bin/sh
cabal-dev build && {
	a=/tmp/npttest
	9umount test
	killall 9ptorrent
	rm $a
	export NPTORRENT_ADDRESS="unix!$a"
	dist/build/9ptorrent/9ptorrent &
	sleep 1
	9mount $NPTORRENT_ADDRESS test
	#printf Bruce\ Hood\ -\ The\ Self\ Illusion:\ How\ the\ Social\ Brain\ Creates\ Identity.mobi.8d076d21e33a449f.torrent >> test/add_torrent
	printf Michael\ Pollan\ -\ Food\ Rules:\ An\ Eater\'s\ Manual.epub.torrent >> test/add_torrent
}
