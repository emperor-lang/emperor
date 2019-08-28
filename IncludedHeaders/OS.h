#ifdef _WIN64
#	error "64-bit Windows is not supported"
#elif _WIN32
#	error "32-bit Windows is not supported"
#elif __APPLE__
#	error "Apple products are not currently (!) supported"
#elif __ANDROID__
#	error "Android is not supported"
#elif __linux
// This is fine.
#elif __unix
#	error Your version of *nix is not specifically supported.
#elif __posix
#	error Posix is not specifically supported.
#else
#	error "Could not detect operating system, what are you running? If you are running something other people use, \
please file this as a bug report at <https://github.com/emperor-lang/emperor/issues>"
#endif
