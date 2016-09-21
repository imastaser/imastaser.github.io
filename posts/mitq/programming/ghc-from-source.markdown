---
title: Install Haskell on Ubuntu
tags: OS, ubuntu, ghc
date: 2014-02-07
description: DESCRIPTION
comments: true
toc: false
---


### Ubuntu

#### 1. Soft Approach (from [learnhaskell](https://github.com/bitemyapp/learnhaskell))

This PPA is excellent and is what I use on all my Linux dev and build machines: http://launchpad.net/~hvr/+archive/ghc

Specifically:

``` bash
sudo apt-get update`
#**12.04 and below** -> 
sudo apt-get install python-software-properties
#**12.04 and above** -> 
sudo apt-get install software-properties-common
sudo add-apt-repository -y ppa:hvr/ghc`
sudo apt-get update`
sudo apt-get install cabal-install-1.20 ghc-7.8.3 happy-1.19.4 alex-3.1.3`
```
Then add `~/.cabal/bin:/opt/cabal/1.20/bin:/opt/ghc/7.8.3/bin:/opt/happy/1.19.4/bin:/opt/alex/3.1.3/bin` to your `PATH` (bash_profile, zshrc, bashrc, etc)

*Optional* You could also add `.cabal-sandbox/bin` to your path. Code that you are actively developing will be available to you from the command line.
This only works when your current working directory is a cabal sandbox.


#### 2. ubuntu prerequisites

``` bash
sudo apt-get install libgmp-dev -y  # Multiprecision arithmetic library developers tools
sudo -K  # ??

Get ghc from http://www.haskell.org/ghc/ and install
wget http://www.haskell.org/ghc/dist/7.8.3/ghc-7.8.3-i386-unknown-linux-deb7.tar.xz
tar -xvf ghc-7.8.3-i386-unknown-linux-deb7.tar.xz
cd ghc-7.8.3
./configure --prefix=/opt/ghc-7.8.3
make -j 2 install
Open ~/.bashrc file and add:
PATH=$PATH:/opt/ghc-7.8.3/bin
source ~/.bashrc # reload bash
```


#### 3. if you have only GHC (but not cabal) installed:

``` bash
wget http://www.haskell.org/cabal/release/cabal-install-1.20.0.3/cabal-install-1.20.0.3.tar.gz
tar xvf cabal-install-1.20.0.3.tar.gz
cd cabal-1.20.0.3
./bootstrap.sh --user
runhaskell Setup.hs configure --user
runhaskell Setup.hs build
runhaskell Setup.hs install
cd ../cabal-install
sh bootstrap.sh
```

* if error
<command line>: can't load .so/.DLL for: libgmp.so (libgmp.so: cannot open shared object file: No such file or directory)
You either add /usr/local/lib and/or /usr/local/lib64 to $LD_LIBRARY_PATH, or add them to /etc/ld.so.conf, or (since you already have /usr/lib64/libgmp.so.3) add a missing symbolic link:

``` bash
cd /usr/lib64
sudo ln -s libgmp.so.3 libgmp.so
```
(and perhaps the same for /usr/lib).

Note that ```/usr/lib64/libgmp.so.3``` might be a different version from ```/usr/local/lib64/libgmp.so```, make sure ```ghc``` can actually be used with the former.

* if error regarding Locals see

[Configure Locales in Ubuntu](http://www.thomas-krenn.com/en/wiki/Configure_Locales_in_Ubuntu)

[ERROR: 'invalid byte sequence'](http://ppenzin.github.io/haskell/quickcheck/freebsd/2014/06/25/tf-random-invalid-byte-sequence/)


see also: [Safer Haskell Install](http://yannesposito.com/Scratch/en/blog/Safer-Haskell-Install/index.html)
### Links regarding cabal and ghc

[http://hyperpolyglot.org/](http://hyperpolyglot.org/)


[Install Haskell on Ubuntu from source](https://gist.github.com/Dexyne/5791465)


[https://gist.github.com/wting/8498731](https://gist.github.com/wting/8498731)


[http://coldwa.st/e/blog/2013-08-20-Cabal-sandbox.html](http://coldwa.st/e/blog/2013-08-20-Cabal-sandbox.html)


[https://gist.github.com/yantonov/10083524](https://gist.github.com/yantonov/10083524)
