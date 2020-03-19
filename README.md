# Spotify Web API bindings for Haskell

Built around Servant + Aeson.

Work in progress. Will be put on Hackage once remotely stable.

Designed to be very high-level (eg. handling token refresh etc. automatically). I can think about exposing the lower level functionality if anyone requests it.

Designed for use with lenses and/or modern record extensions, eg. to work around duplicated identifiers. This allows us to mirror the Spotify API in a methodical way without being too verbose.
