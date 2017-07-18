## Synopsis

This is a demonstration Yesod-based server that provides
web access to delimited continuations. The server runs a
(interpreted, mostly pure) Prolog code with browser
interaction on a continuation monad while storing its
(tree of) intermediate states in its internal database.

## How to experiment

    # sudo apt-get install stack (if you are on Ubuntu)
    # git clone --recursive http://github.com/nishiuramakoto/logiku
	# cd logiku
	# stack setup
	# stack build
	# stack exec logiku
	# firefox http://localhost:3000

## Motivation

This is part of my effort which aims for implementing a
language-agnostic "continuation server".  Imagine some
clever AI algorithm with its "useful" continuations already
cached in our imaginary server. Then we would be able to
play back/forth its computation states even on a device with
modest computational power.  This could allow, for example,
those important computation states to be easily inspected
post-factum by human experts for scientific, technical, or
legal reasons in possibly the most general way, because a
continuation is not just a vendor-specific tag that can only
be recognized by a particular program running on a
particular OS, but an independent working computation state
which could be rerun for the variety of purposes.


Another motivation is to achieve complete seperation of IO
and pure mathematical code in any language and on any kind
of IT devices, not just in Haskell on a PC. In Android, for
example, you would be requested to give certain sets of
permissions to an app you would like to install once and for
all. With just the permissions for networking and access to
your photos, there seems to be already no easy way to tell
whether the app is doing the right thing withing its ethical
limits, not just its technical limits. By introducing
language-agnostic continuations the situation could change as
follows:

    Pure Cont Server               IO device

      app-k0	   		             Supervisor
        / \         read(file)       Log read
 	   k1  k2       ------->
	  /\    /\      return(file)     Log return value
	 k3 k4 k5 k6    <--------
           /        upload(data)     Log networking
          k7        -------->
		  ...

Each time an app running on the Cont server requests an IO action on a
device, its continuation is cached and then diverges according to its
return value. The actual IO is performed on the device possibly under
a security-aware supervisor program. (A continuation may call another
continuation which can lead to compact yet efficient algorithms, while
still remaining pure.) A continuation here serves as evidence of a
possible wrong-doing as well as a computation cache which may be
reused for multiple users or devices. Note thant a
continuation may be restarted just by passing it a data --
that is, actually neither the continuation store nor the IO
device is strictly forced to act as a server. If we give an
URL to each continuation, this scheme may be thought of as
dynamically and automatically giving a REST-ful interface to
the important states of an arbitrary Turing machine.

Although the project is currently hindered by a performance
limitation of fork system call as implemented in recent
versions of Linux kernels circa. 2.6.32 or so, which
unfortunately leads to a quadratic behavior for a fork-based
continuation systems, I am still optimistic that this is
still a doable project withing a reasonable amount of time.

## License

GPL3
