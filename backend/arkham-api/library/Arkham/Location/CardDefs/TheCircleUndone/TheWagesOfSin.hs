{- HLINT ignore "Use camelCase" -}
module Arkham.Location.CardDefs.TheCircleUndone.TheWagesOfSin where

import Arkham.Location.CardDefs.Import

abandonedChapel :: CardDef
abandonedChapel =
  victory 1
    $ otherSideIs "05168b"
    $ location "05168" "Abandoned Chapel" mempty Plus [Squiggle, Circle, Diamond, Moon] TheWagesOfSin

abandonedChapelSpectral :: CardDef
abandonedChapelSpectral =
  victory 1
    $ otherSideIs "05168"
    $ location
      "05168b"
      "Abandoned Chapel"
      [Spectral]
      Plus
      [Squiggle, Circle, Diamond, Moon]
      TheWagesOfSin

chapelAtticSpectral_175 :: CardDef
chapelAtticSpectral_175 =
  location "05175b" "Chapel Attic" [Spectral] Moon [Plus, Diamond] TheWagesOfSin
    & otherSideIs "05175"

chapelAtticSpectral_176 :: CardDef
chapelAtticSpectral_176 =
  location "05176b" "Chapel Attic" [Spectral] Moon [Plus, Diamond] TheWagesOfSin
    & otherSideIs "05176"

chapelAttic_175 :: CardDef
chapelAttic_175 =
  location "05175" "Chapel Attic" mempty Moon [Plus, Diamond] TheWagesOfSin
    & otherSideIs "05175b"

chapelAttic_176 :: CardDef
chapelAttic_176 =
  location "05176" "Chapel Attic" mempty Moon [Plus, Diamond] TheWagesOfSin
    & otherSideIs "05176b"

chapelCryptSpectral_173 :: CardDef
chapelCryptSpectral_173 =
  location "05173b" "Chapel Crypt" [Spectral] Diamond [Plus, Moon] TheWagesOfSin
    & otherSideIs "05173"

chapelCryptSpectral_174 :: CardDef
chapelCryptSpectral_174 =
  location "05174b" "Chapel Crypt" [Spectral] Diamond [Plus, Moon] TheWagesOfSin
    & otherSideIs "05174"

chapelCrypt_173 :: CardDef
chapelCrypt_173 =
  location "05173" "Chapel Crypt" mempty Diamond [Plus, Moon] TheWagesOfSin
    & otherSideIs "05173b"

chapelCrypt_174 :: CardDef
chapelCrypt_174 =
  location "05174" "Chapel Crypt" mempty Diamond [Plus, Moon] TheWagesOfSin
    & otherSideIs "05174b"

hangmansBrook :: CardDef
hangmansBrook =
  otherSideIs "05166b"
    $ location "05166" "Hangman's Brook" mempty Squiggle [Circle, Plus] TheWagesOfSin

hangmansBrookSpectral :: CardDef
hangmansBrookSpectral =
  location "05166b" "Hangman's Brook" [Spectral] Squiggle [Circle, Plus] TheWagesOfSin
    & otherSideIs "05166"

hauntedFields :: CardDef
hauntedFields =
  victory 1
    $ otherSideIs "05167b"
    $ location "05167" "Haunted Fields" mempty Circle [Squiggle, Plus, Triangle, Square] TheWagesOfSin

hauntedFieldsSpectral :: CardDef
hauntedFieldsSpectral =
  victory 1
    $ otherSideIs "05167"
    $ location
      "05167b"
      "Haunted Fields"
      [Spectral]
      Circle
      [Squiggle, Plus, Triangle, Square]
      TheWagesOfSin

hereticsGravesSpectral_171 :: CardDef
hereticsGravesSpectral_171 =
  otherSideIs "05171"
    $ location
      "05171b"
      "Heretics' Graves"
      [Spectral]
      Square
      [Triangle, Circle]
      TheWagesOfSin

hereticsGravesSpectral_172 :: CardDef
hereticsGravesSpectral_172 =
  location "05172b" "Heretics' Graves" [Spectral] Square [Triangle, Circle] TheWagesOfSin
    & otherSideIs "05172"

hereticsGraves_171 :: CardDef
hereticsGraves_171 =
  otherSideIs "05171b"
    $ location "05171" "Heretics' Graves" mempty Square [Triangle, Circle] TheWagesOfSin

hereticsGraves_172 :: CardDef
hereticsGraves_172 =
  location "05172" "Heretics' Graves" mempty Square [Triangle, Circle] TheWagesOfSin
    & otherSideIs "05172b"

theGallowsSpectral_169 :: CardDef
theGallowsSpectral_169 =
  otherSideIs "05169"
    $ location "05169b" "The Gallows" [Spectral] Triangle [Circle, Square] TheWagesOfSin

theGallowsSpectral_170 :: CardDef
theGallowsSpectral_170 =
  otherSideIs "05170"
    $ location
      "05170b"
      "The Gallows"
      [Spectral]
      Triangle
      [Circle, Square]
      TheWagesOfSin

theGallows_169 :: CardDef
theGallows_169 =
  otherSideIs "05169b"
    $ location "05169" "The Gallows" mempty Triangle [Circle, Square] TheWagesOfSin

theGallows_170 :: CardDef
theGallows_170 =
  otherSideIs "05170b" $ location "05170" "The Gallows" mempty Triangle [Circle, Square] TheWagesOfSin
