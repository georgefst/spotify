# Spotify Web API bindings for Haskell

Built around Servant + Aeson.

Work in progress. Will be put on Hackage once remotely stable.

Designed to be very high-level (eg. handling token refresh etc. automatically). I can think about exposing the lower level functionality if anyone requests it.

Designed for use with lenses and/or modern record extensions, eg. to work around duplicated identifiers. This allows us to mirror the Spotify API in a methodical way without being too verbose. `RecordDotSyntax` will suit us nicely whenever it lands.

All request take place inside a monad implementing `MonadSpotify`. In particular, this abstracts away authentication - we don't need to explicitly pass tokens around. A concrete monad `Spotify` is provided, though you'll often want to roll your own. There is also an instance `MonadSpotify IO` - this caches authentication data to disk, so one should be careful about security. It also creates a new connection `Manager` for every request, which can be expensive. Thus a `State`-like monad is preferred for serious code. Nonetheless, being able to run the functions directly in `IO` can be very convenient, especially from `GHCI`.
