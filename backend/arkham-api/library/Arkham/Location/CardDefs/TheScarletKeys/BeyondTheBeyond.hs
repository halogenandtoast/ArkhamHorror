module Arkham.Location.CardDefs.TheScarletKeys.BeyondTheBeyond where

import Arkham.Keyword qualified as Keyword
import Arkham.Location.CardDefs.Import

alienFrontierA :: CardDef
alienFrontierA =
  ( locationWithUnrevealed_
      "09748a"
      "City of Remnants"
      [Otherworld]
      "Alien Frontier"
      [Otherworld]
      BeyondTheBeyond
  )
    { cdKeywords = singleton (Keyword.Concealed CityOfRemnants (Static 1))
    }

alienFrontierB :: CardDef
alienFrontierB =
  ( locationWithUnrevealed_
      "09748b"
      "City of Remnants"
      [Otherworld]
      "Alien Frontier"
      [Otherworld]
      BeyondTheBeyond
  )
    { cdKeywords = singleton (Keyword.Concealed CityOfRemnants (Static 1))
    }

cliffsOfInsanity :: CardDef
cliffsOfInsanity =
  victory 1
    $ ( locationWithUnrevealed_
          "09750"
          "City of Remnants"
          [Otherworld]
          "Cliffs of Insanity"
          [Otherworld]
          BeyondTheBeyond
      )
      { cdKeywords = singleton (Keyword.Concealed CityOfRemnants (Static 1))
      , cdEncounterSetQuantity = Just 2
      }

ramblingRouteA :: CardDef
ramblingRouteA =
  ( locationWithUnrevealed_
      "09747a"
      "City of Remnants"
      [Otherworld]
      "Rambling Route"
      [Otherworld]
      BeyondTheBeyond
  )
    { cdKeywords = singleton (Keyword.Concealed CityOfRemnants (Static 1))
    }

ramblingRouteB :: CardDef
ramblingRouteB =
  ( locationWithUnrevealed_
      "09747b"
      "City of Remnants"
      [Otherworld]
      "Rambling Route"
      [Otherworld]
      BeyondTheBeyond
  )
    { cdKeywords = singleton (Keyword.Concealed CityOfRemnants (Static 1))
    }

ramblingRouteC :: CardDef
ramblingRouteC =
  ( locationWithUnrevealed_
      "09747c"
      "City of Remnants"
      [Otherworld]
      "Rambling Route"
      [Otherworld]
      BeyondTheBeyond
  )
    { cdKeywords = singleton (Keyword.Concealed CityOfRemnants (Static 1))
    }

wealdOfEffigiesA :: CardDef
wealdOfEffigiesA =
  victory 1
    $ ( locationWithUnrevealed_
          "09749a"
          "City of Remnants"
          [Otherworld]
          "Weald of Effigies"
          [Otherworld]
          BeyondTheBeyond
      )
      { cdKeywords = singleton (Keyword.Concealed CityOfRemnants (Static 1))
      }

wealdOfEffigiesB :: CardDef
wealdOfEffigiesB =
  victory 1
    $ ( locationWithUnrevealed_
          "09749b"
          "City of Remnants"
          [Otherworld]
          "Weald of Effigies"
          [Otherworld]
          BeyondTheBeyond
      )
      { cdKeywords = singleton (Keyword.Concealed CityOfRemnants (Static 1))
      }
