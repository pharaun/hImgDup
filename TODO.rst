Feature list

1. Scan directories for duplicate files
    a. winnow it down by size
        I. scan and group files by size that way we only need to hash/file compare files in the same size group
        II. Remove all grouping of 1 files so its only 2 or more
    b. Either perform a hash of the entire file
        I. Easy, just hash each file and compare, and can use for future comparsion
        II. Possible waste of disk space
        III. Partial hashing of the file. Then compare the hit/miss rate
    c. Or compare it 1:1 byte wise
        I. Can lead to rapid elimation since difference probably will appear early on
        II. Can lead to complicated multi-way compare, which can be bad file IO
        III. Can be complicated for comparing, IE 4 files and all 4 are same in pairs, ie A<->B, C<->D.
        IV. Can probably do a hybrid algo that checks a few bytes in the file before checksumming to identify if its worth checksumming or not.
        V. Scan it in certain way to.. guess the chance of a duplicate
            > It starts by grouping files by size.
            >
            > Then it starts reading small chunks of the files of the same size and
            > comparing them. It creates smaller groups depending on these comparisons.
            >
            > It goes on with bigger and bigger chunks (of size up to a hard-coded limit).
            >
            > It stops reading from files as soon as they form a single-element group or
            > they are read completely (which only happens when they have a very high
            > probability of having duplicates).
            >
            > log2

    d. Finally compare the files that hashes same with 1:1 byte compare
        I. If use weak but fast hashing its probably wise
        II. Can also just not do this and *bet* on the fact that the odds of two different file hasing the same is pretty remote for a good hash.

2. Priority of source for files to come from.
    a. For supporting "canonical source" of data and to remove the dupes from other stuff
    b. IE: a dupes1 dupes2 - dupes1 will have priority over dupes2
    c. Probably can just take a list of directory and assign them a descending priority order and priorize the ones higher over the ones not.
        >  If A was found while scanning an input argument earlier than than B, A is higher ranked.
        > If A was found at a depth lower than B, A is higher ranked (A closer to the root)
        > If A was found earlier than B, A is higher ranked.
    d. should have option to disable/enable/tweak this somehow

3. Speeding up read/filerange
    a. http://dkrotx-prg.blogspot.com/2012/08/speedup-file-reading-on-linux.html
    b. http://lwn.net/Articles/297696/


Possible issues to keep in mind:

1. symlink and hardlinks, multiple mounts, etc
    a. Avoid comparing two files that are hardlinked to eachothers
    b. Decide if following symlinks is desirable or not
    c. Also decide if we want to treat/cleanup the symlink
    d. http://dupedit.com/ - False Copies
        > symlinks
        > hardlinks
        > multiple simultaneous mounts of the same filesystem
        > mount --bind
        > specifying the same path twice
        > absolute vs relative paths
        > redundant '.' and '..' components in path (such as './dir1/../dir2' vs 'dir2')
        > glibc thinks empty path components are ok (that is, 'dir1//dir2' is equivalent to 'dir1/dir2').
        > reasons I might not have imagined
        >
        > Comparing a file against itself is a serious hazard which can only result in
        > falsely accusing it to be a copy of itself, which is easy enough for users to
        > believe when presenting the file with its multiple filenames (not to mention
        > auto-deduplication purposes). A file on a UNIX filesystem has only one inode
        > number, and a filesystem has only one device number (even in multiple
        > simultaneous mounts of the filesystem*). Files that share inode- and device
        > numbers are considered by dupedit as «false copies» and treated as one file.
    e. This does not work if its running over Network Filesystem.


Output format:

1. http://dupedit.com/


Additional work:

a. Context Piecewise Hashing
b. perceptual hashing
    1. SAD
    2. SSAD - x264 code



14:05:40 < CcxCZ> they have 3d stuff too, but otherwise not as big iirc
14:07:42 < CcxCZ> guessing from the help page they run danbooru's software
14:12:41 < CcxCZ> 17:52:40    ~piespy | and for iqdb use the index.xml interface, same parameters as a regular 
search but you get an xml document back
14:12:43 < CcxCZ> 17:54:02    ~piespy | POST and GET supported, and you're limited to 60 queries in any 5-minute 
period or you get blocked
Image Manager
	- Deduplication
		- SAD (works good, has problems)
		- SVD, etc....
		- Needs to deal with crop and rotated images
		- Look at PNF (image quality) for calculating which image to prefer when deduping so that the highest quality one is selected by default
	- Plays gif
	- Drag & drop moving
	- Shortcuts

Image Rating
	- Good, Meh, Bad
	- Retain hash and SAD fingerprints of the meh/bad for rapid cleaning out from bulk downloads

Image Bucketing
	- Sort into "real vs anime vs etc..."

Tag based FS
	- fuse
	- Central collection of images in hash datastore
	- You have various tags such as - artist, series, character, etc

Downloading
	- Find a way to just download the image out of say danbooru with the tags intact
	- this will pre-tag the images and pre-organize which will be helpful with upkeep
