# Spotify Web API bindings for Haskell

Designed to be very high-level (eg. handling token refresh etc. automatically). Though it should be possible to expose more of the lower level functionality if anyone requests it. At the very least, the raw Servant encoding of the API is available in `spotify-servant`, on which the main library depends.

Designed for use with lenses and/or modern record extensions (`OverloadedRecordDot` etc.), e.g. to work around duplicated identifiers. This allows us to mirror the Spotify API in a methodical way without being too verbose.

All requests take place inside a monad implementing `MonadSpotify`. In particular, this abstracts away authentication, meaning that we don't need to explicitly pass tokens around. A concrete monad `Spotify` is provided, though you'll often want to roll your own. There is also an instance `MonadSpotify IO`, which caches authentication data to disk, so one should be careful about security. It also creates a new connection `Manager` for every request, which can be expensive. Thus a `State`-like monad is preferred for serious code. Nonetheless, being able to run the functions directly in `IO` can be very convenient, especially from `GHCI`.
