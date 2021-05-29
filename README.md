# zig-downloader

## Install

Install with `stack install`.

## Run

```bash
$ zig-downloader list
Master version: 0.8.0-dev.2684+3483931d20.8.0-dev.2684+3483931d2
Other versions:
0.7.1
0.7.0
0.6.0
0.5.0
0.4.0
0.3.0
0.2.0
0.1.1
$ zig-downloader list -q
0.8.0-dev.2684+3483931d2
0.7.1
0.7.0
0.6.0
0.5.0
0.4.0
0.3.0
0.2.0
0.1.1
$ zig-downloader show master
Master
------
Version: 0.8.0-dev.2684+3483931d2
Date: 2021-05-29
Docs: https://ziglang.org/documentation/master/
Standard library docs: https://ziglang.org/documentation/master/std/
Source: https://ziglang.org/builds/zig-0.8.0-dev.2684+3483931d2.tar.xz

Architectures
-------------

aarch64-macos: https://ziglang.org/builds/zig-macos-aarch64-0.8.0-dev.2684+3483931d2.tar.xz (35.56 MB)
x86_64-macos: https://ziglang.org/builds/zig-macos-x86_64-0.8.0-dev.2684+3483931d2.tar.xz (39.50 MB)
x86_64-windows: https://ziglang.org/builds/zig-windows-x86_64-0.8.0-dev.2684+3483931d2.zip (59.18 MB)
x86_64-freebsd: https://ziglang.org/builds/zig-freebsd-x86_64-0.8.0-dev.2684+3483931d2.tar.xz (41.29 MB)
aarch64-linux: https://ziglang.org/builds/zig-linux-aarch64-0.8.0-dev.2684+3483931d2.tar.xz (46.76 MB)
x86_64-linux: https://ziglang.org/builds/zig-linux-x86_64-0.8.0-dev.2684+3483931d2.tar.xz (41.66 MB)
$ zig-downloader show master -q
0.8.0-dev.2684+3483931d2
https://ziglang.org/builds/zig-macos-aarch64-0.8.0-dev.2684+3483931d2.tar.xz
https://ziglang.org/builds/zig-macos-x86_64-0.8.0-dev.2684+3483931d2.tar.xz
https://ziglang.org/builds/zig-windows-x86_64-0.8.0-dev.2684+3483931d2.zip
https://ziglang.org/builds/zig-freebsd-x86_64-0.8.0-dev.2684+3483931d2.tar.xz
https://ziglang.org/builds/zig-linux-aarch64-0.8.0-dev.2684+3483931d2.tar.xz
https://ziglang.org/builds/zig-linux-x86_64-0.8.0-dev.2684+3483931d2.tar.xz
$ zig-downloader show 0.7.1
0.7.1
------
Version: N/A
Date: 2020-12-13
Docs: https://ziglang.org/documentation/0.7.1/
Standard library docs: https://ziglang.org/documentation/0.7.1/std/
Source: https://ziglang.org/download/0.7.1/zig-0.7.1.tar.xz

Architectures
-------------

riscv64-linux: https://ziglang.org/download/0.7.1/zig-linux-riscv64-0.7.1.tar.xz (37.45 MB)
x86_64-macos: https://ziglang.org/download/0.7.1/zig-macos-x86_64-0.7.1.tar.xz (36.21 MB)
x86_64-windows: https://ziglang.org/download/0.7.1/zig-windows-x86_64-0.7.1.zip (54.91 MB)
i386-linux: https://ziglang.org/download/0.7.1/zig-linux-i386-0.7.1.tar.xz (39.23 MB)
x86_64-freebsd: https://ziglang.org/download/0.7.1/zig-freebsd-x86_64-0.7.1.tar.xz (39.07 MB)
bootstrap: https://ziglang.org/download/0.7.1/zig-bootstrap-0.7.1.tar.xz (40.23 MB)
armv7a-linux: https://ziglang.org/download/0.7.1/zig-linux-armv7a-0.7.1.tar.xz (35.81 MB)
aarch64-linux: https://ziglang.org/download/0.7.1/zig-linux-aarch64-0.7.1.tar.xz (33.78 MB)
x86_64-linux: https://ziglang.org/download/0.7.1/zig-linux-x86_64-0.7.1.tar.xz (37.85 MB)
i386-windows: https://ziglang.org/download/0.7.1/zig-windows-i386-0.7.1.zip (54.37 MB)
$ zig-downloader show 0.7.1 -q
https://ziglang.org/download/0.7.1/zig-linux-riscv64-0.7.1.tar.xz
https://ziglang.org/download/0.7.1/zig-macos-x86_64-0.7.1.tar.xz
https://ziglang.org/download/0.7.1/zig-windows-x86_64-0.7.1.zip
https://ziglang.org/download/0.7.1/zig-linux-i386-0.7.1.tar.xz
https://ziglang.org/download/0.7.1/zig-freebsd-x86_64-0.7.1.tar.xz
https://ziglang.org/download/0.7.1/zig-bootstrap-0.7.1.tar.xz
https://ziglang.org/download/0.7.1/zig-linux-armv7a-0.7.1.tar.xz
https://ziglang.org/download/0.7.1/zig-linux-aarch64-0.7.1.tar.xz
https://ziglang.org/download/0.7.1/zig-linux-x86_64-0.7.1.tar.xz
https://ziglang.org/download/0.7.1/zig-windows-i386-0.7.1.zip
$ zig-downloader download master
Not downloading 'https://ziglang.org/builds/zig-macos-aarch64-0.8.0-dev.2684+3483931d2.tar.xz' because it already exists in download directory.
Not downloading 'https://ziglang.org/builds/zig-macos-x86_64-0.8.0-dev.2684+3483931d2.tar.xz' because it already exists in download directory.
Not downloading 'https://ziglang.org/builds/zig-windows-x86_64-0.8.0-dev.2684+3483931d2.zip' because it already exists in download directory.
Not downloading 'https://ziglang.org/builds/zig-freebsd-x86_64-0.8.0-dev.2684+3483931d2.tar.xz' because it already exists in download directory.
Not downloading 'https://ziglang.org/builds/zig-linux-aarch64-0.8.0-dev.2684+3483931d2.tar.xz' because it already exists in download directory.
Not downloading 'https://ziglang.org/builds/zig-linux-x86_64-0.8.0-dev.2684+3483931d2.tar.xz' because it already exists in download directory.
$ zig-downloader download master -q
$ zig-downloader download 0.7.1
Not downloading 'https://ziglang.org/download/0.7.1/zig-linux-riscv64-0.7.1.tar.xz' because it already exists in download directory.
Not downloading 'https://ziglang.org/download/0.7.1/zig-macos-x86_64-0.7.1.tar.xz' because it already exists in download directory.
Not downloading 'https://ziglang.org/download/0.7.1/zig-windows-x86_64-0.7.1.zip' because it already exists in download directory.
Not downloading 'https://ziglang.org/download/0.7.1/zig-linux-i386-0.7.1.tar.xz' because it already exists in download directory.
Not downloading 'https://ziglang.org/download/0.7.1/zig-freebsd-x86_64-0.7.1.tar.xz' because it already exists in download directory.
Not downloading 'https://ziglang.org/download/0.7.1/zig-bootstrap-0.7.1.tar.xz' because it already exists in download directory.
Not downloading 'https://ziglang.org/download/0.7.1/zig-linux-armv7a-0.7.1.tar.xz' because it already exists in download directory.
Not downloading 'https://ziglang.org/download/0.7.1/zig-linux-aarch64-0.7.1.tar.xz' because it already exists in download directory.
Not downloading 'https://ziglang.org/download/0.7.1/zig-linux-x86_64-0.7.1.tar.xz' because it already exists in download directory.
Not downloading 'https://ziglang.org/download/0.7.1/zig-windows-i386-0.7.1.zip' because it already exists in download directory.
$ zig-downloader download 0.7.1 -q
$ zig-downloader download 0.7.0 -q
Downloading: https://ziglang.org/download/0.7.0/zig-linux-riscv64-0.7.0.tar.xz
Downloading: https://ziglang.org/download/0.7.0/zig-macos-aarch64-0.7.0.tar.xz
Downloading: https://ziglang.org/download/0.7.0/zig-macos-x86_64-0.7.0.tar.xz
Downloading: https://ziglang.org/download/0.7.0/zig-windows-x86_64-0.7.0.zip
Downloading: https://ziglang.org/download/0.7.0/zig-linux-i386-0.7.0.tar.xz
Downloading: https://ziglang.org/download/0.7.0/zig-freebsd-x86_64-0.7.0.tar.xz
Downloading: https://ziglang.org/download/0.7.0/zig-bootstrap-0.7.0.tar.xz
Downloading: https://ziglang.org/download/0.7.0/zig-linux-armv7a-0.7.0.tar.xz
Downloading: https://ziglang.org/download/0.7.0/zig-linux-aarch64-0.7.0.tar.xz
Downloading: https://ziglang.org/download/0.7.0/zig-linux-x86_64-0.7.0.tar.xz
Downloading: https://ziglang.org/download/0.7.0/zig-windows-i386-0.7.0.zip
$ ll ~/.zig-downloader/0.7.0/x86_64-linux
total 36M
-rw-r--r-- 1 gonz gonz 36M May 29 11:07 zig-linux-x86_64-0.7.0.tar.xz
$ cat ~/.zig-downloader/latest-master
0.8.0-dev.2684+3483931d2
```
