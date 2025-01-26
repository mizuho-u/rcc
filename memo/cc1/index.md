---
title: "Writing C Compiler"
description: "環境構築"
date: "Jan 26 2025"
---

M1 Macなのでx86向けにdevconatinerを作成
https://blog.jp.square-enix.com/iteng-blog/posts/00084-devcon-amd64/

```json:.devcontainer
	"build": {
		"dockerfile": "Dockerfile"
	}
```

```
FROM --platform=linux/amd64 mcr.microsoft.com/devcontainers/base:ubuntu
```

```
vscode ➜ /workspaces/rc (master) $ gcc --version
gcc (Debian 10.2.1-6) 10.2.1 20210110
Copyright (C) 2020 Free Software Foundation, Inc.
This is free software; see the source for copying conditions.  There is NO
warranty; not even for MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
```

gdbは入ってないので入れる
```
vscode ➜ /workspaces/rc (master) $ sudo apt-get install gdb
Reading package lists... Done
Building dependency tree... Done
Reading state information... Done
The following additional packages will be installed:
  libbabeltrace1 libboost-regex1.74.0 libc6-dbg libdebuginfod1 libdw1 libipt2 libsource-highlight-common libsource-highlight4v5
Suggested packages:
  gdb-doc gdbserver
The following NEW packages will be installed:
  gdb libbabeltrace1 libboost-regex1.74.0 libc6-dbg libdebuginfod1 libdw1 libipt2 libsource-highlight-common libsource-highlight4v5
0 upgraded, 9 newly installed, 0 to remove and 2 not upgraded.
Need to get 12.2 MB of archives.
After this operation, 27.9 MB of additional disk space will be used.
Do you want to continue? [Y/n] Y
Get:1 http://deb.debian.org/debian bullseye/main amd64 libdw1 amd64 0.183-1 [234 kB]
Get:2 http://deb.debian.org/debian bullseye/main amd64 libbabeltrace1 amd64 1.5.8-1+b3 [174 kB]
Get:3 http://deb.debian.org/debian bullseye/main amd64 libdebuginfod1 amd64 0.183-1 [27.4 kB]
Get:4 http://deb.debian.org/debian bullseye/main amd64 libipt2 amd64 2.0.3-1 [43.7 kB]
Get:5 http://deb.debian.org/debian bullseye/main amd64 libsource-highlight-common all 3.1.9-3 [79.7 kB]
Get:6 http://deb.debian.org/debian bullseye/main amd64 libboost-regex1.74.0 amd64 1.74.0-9 [516 kB]
Get:7 http://deb.debian.org/debian bullseye/main amd64 libsource-highlight4v5 amd64 3.1.9-3+b1 [259 kB]
Get:8 http://deb.debian.org/debian bullseye/main amd64 gdb amd64 10.1-1.7 [3,395 kB]
Get:9 http://deb.debian.org/debian bullseye/main amd64 libc6-dbg amd64 2.31-13+deb11u11 [7,518 kB]
Fetched 12.2 MB in 1s (18.9 MB/s)    
Selecting previously unselected package libdw1:amd64.
(Reading database ... 29691 files and directories currently installed.)
Preparing to unpack .../0-libdw1_0.183-1_amd64.deb ...
Unpacking libdw1:amd64 (0.183-1) ...
Selecting previously unselected package libbabeltrace1:amd64.
Preparing to unpack .../1-libbabeltrace1_1.5.8-1+b3_amd64.deb ...
Unpacking libbabeltrace1:amd64 (1.5.8-1+b3) ...
Selecting previously unselected package libdebuginfod1:amd64.
Preparing to unpack .../2-libdebuginfod1_0.183-1_amd64.deb ...
Unpacking libdebuginfod1:amd64 (0.183-1) ...
Selecting previously unselected package libipt2.
Preparing to unpack .../3-libipt2_2.0.3-1_amd64.deb ...
Unpacking libipt2 (2.0.3-1) ...
Selecting previously unselected package libsource-highlight-common.
Preparing to unpack .../4-libsource-highlight-common_3.1.9-3_all.deb ...
Unpacking libsource-highlight-common (3.1.9-3) ...
Selecting previously unselected package libboost-regex1.74.0:amd64.
Preparing to unpack .../5-libboost-regex1.74.0_1.74.0-9_amd64.deb ...
Unpacking libboost-regex1.74.0:amd64 (1.74.0-9) ...
Selecting previously unselected package libsource-highlight4v5.
Preparing to unpack .../6-libsource-highlight4v5_3.1.9-3+b1_amd64.deb ...
Unpacking libsource-highlight4v5 (3.1.9-3+b1) ...
Selecting previously unselected package gdb.
Preparing to unpack .../7-gdb_10.1-1.7_amd64.deb ...
Unpacking gdb (10.1-1.7) ...
Selecting previously unselected package libc6-dbg:amd64.
Preparing to unpack .../8-libc6-dbg_2.31-13+deb11u11_amd64.deb ...
Unpacking libc6-dbg:amd64 (2.31-13+deb11u11) ...
Setting up libdw1:amd64 (0.183-1) ...
Setting up libdebuginfod1:amd64 (0.183-1) ...
Setting up libsource-highlight-common (3.1.9-3) ...
Setting up libc6-dbg:amd64 (2.31-13+deb11u11) ...
Setting up libboost-regex1.74.0:amd64 (1.74.0-9) ...
Setting up libipt2 (2.0.3-1) ...
Setting up libbabeltrace1:amd64 (1.5.8-1+b3) ...
Setting up libsource-highlight4v5 (3.1.9-3+b1) ...
Setting up gdb (10.1-1.7) ...
Processing triggers for man-db (2.9.4-2) ...
Processing triggers for libc-bin (2.31-13+deb11u11) ...
```

```pythonは入ってる
vscode ➜ /workspaces/rc (master) $ python3 --version
Python 3.9.2
```

テストスクリプトをサブモジュールで追加
```
vscode ➜ /workspaces/rc (master) $ git submodule add https://github.com/nlsandler/writing-a-c-compiler-tests.git
```

確認
```
vscode ➜ /workspaces/rc/writing-a-c-compiler-tests (main) $ ./test_compiler --check-setup
All system requirements met!
```

OKOK