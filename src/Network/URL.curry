------------------------------------------------------------------------------
--- Base library for dealing with URLs (Uniform Resource Locators).
--- 
--- Currently, it contains operations for encoding and decoding strings
--- to and from URL encoded format which are useful to construct URLs
--- with parameter strings.
---
--- @author Michael Hanus
--- @version April 2025
------------------------------------------------------------------------------

module Network.URL
  ( string2urlencoded, urlencoded2string )
 where

import Data.Char ( isAlphaNum )
import Numeric   ( readHex )

--- Translates arbitrary strings into equivalent URL encoded strings.
string2urlencoded :: String -> String
string2urlencoded [] = []
string2urlencoded (c:cs)
  | isAlphaNum c = c : string2urlencoded cs
  | c == ' '     = '+' : string2urlencoded cs
  | otherwise
  = let oc = ord c
    in '%' : int2hex(oc `div` 16) : int2hex(oc `mod` 16) : string2urlencoded cs
 where
  int2hex i = if i<10 then chr (ord '0' + i)
                      else chr (ord 'A' + i - 10)

--- Translates an URL encoded string into an equivalent ASCII string.
urlencoded2string :: String -> String
urlencoded2string []     = []
urlencoded2string (c:cs)
  | c == '+'  = ' ' : urlencoded2string cs
  | c == '%'  = chr (case readHex (take 2 cs) of [(n,"")] -> n
                                                 _        -> 0)
                 : urlencoded2string (drop 2 cs)
  | otherwise = c : urlencoded2string cs

------------------------------------------------------------------------------
