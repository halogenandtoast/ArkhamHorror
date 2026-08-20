module Arkham.Location.CardDefs.ReturnToTheForgottenAge.ReturnToTheBoundaryBeyond where

import Arkham.Location.CardDefs.Import

returnToChapultepecPark :: CardDef
returnToChapultepecPark =
  location
    "53041"
    "Chapultepec Park"
    [MexicoCity, PresentDay]
    Triangle
    [Star]
    ReturnToTheBoundaryBeyond

returnToCoyoacan :: CardDef
returnToCoyoacan =
  location
    "53044"
    "Coyoacán"
    [MexicoCity, PresentDay]
    Star
    [Diamond, Triangle, Circle, Heart]
    ReturnToTheBoundaryBeyond

returnToMetropolitanCathedral :: CardDef
returnToMetropolitanCathedral =
  location
    "53040"
    "Metropolitan Cathedral"
    [MexicoCity, PresentDay]
    Square
    [Diamond]
    ReturnToTheBoundaryBeyond

returnToTempleRuins :: CardDef
returnToTempleRuins =
  location
    "53039"
    "Temple Ruins"
    [MexicoCity, PresentDay]
    Circle
    [Diamond, Star]
    ReturnToTheBoundaryBeyond

returnToXochimilco :: CardDef
returnToXochimilco =
  location
    "53043"
    "Xochimilco"
    [MexicoCity, PresentDay]
    Heart
    [Diamond, Star]
    ReturnToTheBoundaryBeyond

returnToZocalo :: CardDef
returnToZocalo =
  location
    "53042"
    "Zócalo"
    [MexicoCity, PresentDay]
    Diamond
    [Heart, Square, Star, Circle]
    ReturnToTheBoundaryBeyond
