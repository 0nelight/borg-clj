# borg-imager

Images Windows disks and volumes straight into a [BorgBackup](https://www.borgbackup.org/)
repository, reading only the clusters NTFS says are in use. The archives it
writes are ordinary borg archives: `borg list`, `borg check` and `borg extract`
work on them, from any machine that has borg.

The command is `borgi`.

```
borgi image ssh://user@host/srv/repo::$(hostname)-$(date +%F) C: --init
```

## Looking for a maintainer

This is a one-person project and it needs more hands — or a new home
altogether. If you use borg, know Clojure, Windows internals or NTFS, and this
looks like something you would want to exist, please open an issue. Taking over
parts of it, or all of it, is genuinely welcome.

See [Where help is most useful](#where-help-is-most-useful) below.

## Status

Early, but not a sketch: 100 tests with 484 assertions pass, and the archives it
produces survive a full `borg check` by stock borg 1.4.0 — repository check,
index check and archive consistency check, no problems found. There are no
tagged releases yet and the on-disk behaviour may still change.

Interoperability is the whole point, so it is worth being precise about what
that means: borg-imager writes the borg 1.x repository format (repository
`version = 1`). It is not a borg 2.x client.

## Why

Backing up a running Windows machine file-by-file misses the things that make
it bootable — the partition table, the boot sector, the reserved partitions. A
sector image captures those, but a naive one copies the empty space too.

borg-imager reads the NTFS allocation bitmap and stores only allocated
clusters, sending the rest as zeros that deduplicate away. The result is a
whole-disk image that boots when restored, at roughly the size of the data
actually on the disk, in a repository you can already manage with borg.

## What it does

    disks           list disks, volumes and VSS providers
    image           image volumes or disks into a repository
    restore-image   write an image back to a disk, a volume or a file
    plan            print key=value facts about an image, for scripts
    layout          print an sfdisk script recreating the partition table
    ls / get        look inside an image, fetch one file, without unpacking it
    snapshot        take a VSS snapshot and print its device path
    create          back up plain files to a borg serve target
    list / info     list archives, show repository or archive info
    extract         extract an archive as files
    verify          check every object: segment crc + chunk id
    chunk           print borg chunk ids of a file
    ui              serve the local web interface and open it
    platform        report host capabilities

Run `borgi` with no arguments for the full option list.

Notable details: a run can be watched from outside with `--status FILE` and
stopped cleanly with `--stop FILE`, so a detached run can be ended without
losing its VSS snapshot. `--dry-run` opens the sources and reports what it
would read without touching the repository. `get` pulls a single file out of
an image without unpacking the image first.

## Installing

Releases carry a binary for Linux and Windows, a Windows installer, and a
`SHA256SUMS` file. There are no tagged releases yet, so for now: build it.

There is no macOS build. Nothing in the code rules it out, but nobody tests it
there, and an untested binary is worse than none — imaging and restoring write
to raw disks.

    clojure -M:test                 # run the tests
    bin/build-native.sh             # GraalVM native-image -> ./borgi

Building natively needs GraalVM (set `GRAALVM_HOME`) and the Clojure CLI.
Pushing a `v*` tag runs [the release workflow](.github/workflows/native.yml),
which builds both binaries, packages the Windows one into an NSIS installer,
and publishes them.

The recovery ISO is released on its own schedule rather than with these —
it tracks Debian point releases, not this project's version. See below.

Imaging needs administrator rights on Windows: VSS and raw device access both
require them.

## Recovery system

`recovery/` builds a bootable Debian live ISO with `borgi` baked in, for
restoring a machine that no longer starts. It boots on BIOS, UEFI and Secure
Boot machines. tty1 shows the `borgi gui` window on a bare X server with
matchbox as its only window manager; tty2 keeps a whiptail menu as a fallback.

    sudo recovery/build.sh --borgi ./borgi

Building it needs Debian with `live-build`, `debootstrap` and `xorriso`, so it
cannot be built on Windows. You should not have to: a CI job watches for Debian
point releases and publishes a ready ISO whenever one lands, to be written to a
stick with Rufus, balenaEtcher or `dd`. Images are built without an SSH key, so
the recovery system has no remote access.

For booting over the network, `--netboot` also leaves `vmlinuz`, both initrds
and `filesystem.squashfs` in `recovery/out`. `recovery/test/netboot-test.sh`
boots them in QEMU the way a machine on the network does — UEFI HTTP boot,
GRUB, `fetch=http://`, no TFTP — and logs the serial console with timestamps
until the interface is up on tty1:

    sudo recovery/build.sh --borgi ./borgi --netboot
    sudo recovery/test/netboot-test.sh

## Graphical interface

`borgi gui` opens the interface as a window: Swing with the FlatLaf look, from
the same binary on Windows and in the recovery system. Swing draws every
control — title bar and frame included — itself, so it looks the same on both.

Its flow is the one of the Acronis True Image rescue media: a home page with
*Back Up* and *Recover*, each for *My Disks* or *Files & Folders*, and wizards
with the steps listed on the left. Recovering a disk goes through *Archive
selection* (archives with the same name before the date form one backup series),
*Recovery method*, *Recovery point* (a calendar of the series), *What to
recover* (the disk, its partitions and "MBR and Track 0"), *Destination* and a
*Summary* that lists exactly the verbs that will run. Whole-disk images made on
Linux (`file:/dev/sda`) show their partitions too: the partition table is read
from the image itself.

The window speaks English and German: the system locale decides, `BORGCLJ_LANG=
en|de` overrides it, and in the recovery system `borgclj.lang=de` on the kernel
command line does.

The native build puts AWT's shared libraries (`libawt*.so`, `libfontmanager.so`
and a few more; DLLs and `lib/fontconfig.bfc` on Windows) next to the binary,
and they have to travel with it.

`borgi ui` serves a small web interface on `127.0.0.1`, guarded by a one-time
token from the URL it opens, and opens a browser on it. It is the same binary
and the same verbs: the interface runs `image`, `restore-image` and `get` in a
thread of its own and reads their progress directly, so there is no second
program to install and nothing parses anybody's printed output.

The pages are plain HTML built down to what a text browser can render. Without
JavaScript the run page carries a two-second meta-refresh and the server
re-renders the same numbers; with it, `app.js` updates them in place over an
event stream. That floor is what lets the recovery system get by with `w3m`
instead of 300-2000 MB of graphical browser — `recovery/ui/` has the kiosk
script and the extra package for it, prepared but not yet wired into the ISO,
which today offers a whiptail console instead.

Imaging still needs an elevated token on Windows, and `borgi.exe` does not ask
for one: start it from an administrator prompt.

## Layout

    src/borgclj/        the client: chunker, repository, remote, NTFS, VSS
    src/borgclj/win/    Windows: VSS, MFT, volume and IOCTL access via FFM
    src/borgclj/ui/     the local web interface served by `borgi ui`
    resources/          icons, and the stylesheet and script the interface serves
    test/               100 tests
    recovery/           bootable recovery ISO (Debian live-build)
    installer/          Windows installer (NSIS)

## Where help is most useful

- **Testing against real hardware.** Restores are the part that matters and
  the part hardest to test alone. Odd partition layouts, OEM recovery
  partitions, dynamic disks, BitLocker.
- **`src/borgclj/cli.clj` is 1115 lines** and wants splitting up.
- **Wiring the interface into the recovery ISO.** `recovery/ui/` holds the
  kiosk script and the one extra package it needs; the ISO still boots into a
  whiptail console.
- **Other filesystems.** Only NTFS allocation is understood today; ext4 and
  XFS would extend the same trick to Linux disks.
- **borg 2.x.** A different repository format, currently unsupported.

## License

BSD 3-Clause — see [LICENSE](LICENSE).

Parts derive from BorgBackup, which is BSD 3-Clause as well; the chunker's
buzhash table is taken from it verbatim, because chunk boundaries have to land
where borg lands them or nothing deduplicates. See [NOTICE](NOTICE) for the
attribution.

borg-imager is not affiliated with, endorsed by, or a product of the
BorgBackup project.
