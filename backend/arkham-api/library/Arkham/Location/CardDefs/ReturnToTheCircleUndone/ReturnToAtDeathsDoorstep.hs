module Arkham.Location.CardDefs.ReturnToTheCircleUndone.ReturnToAtDeathsDoorstep where

import Arkham.Location.CardDefs.Import

wineCellar :: CardDef
wineCellar =
  location
    "54027"
    "Wine Cellar"
    []
    Hourglass
    [T]
    ReturnToAtDeathsDoorstep

wineCellarSpectral :: CardDef
wineCellarSpectral =
  location
    "54028"
    "Wine Cellar"
    [Spectral]
    Hourglass
    [T]
    ReturnToAtDeathsDoorstep
