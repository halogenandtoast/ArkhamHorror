module Arkham.Location.CardDefs.ReturnToThePathToCarcosa.ReturnToEchoesOfThePast where

import Arkham.Location.CardDefs.Import

historicalSocietyBoilerRoom :: CardDef
historicalSocietyBoilerRoom =
  locationWithUnrevealed
    "52032"
    "Historical Society"
    [Basement]
    NoSymbol
    [Droplet]
    ("Historical Society" <:> "Boiler Room")
    [Basement]
    Trefoil
    [Droplet]
    ReturnToEchoesOfThePast

historicalSocietyDustyArchives :: CardDef
historicalSocietyDustyArchives =
  locationWithUnrevealed
    "52030"
    "Historical Society"
    [Basement]
    NoSymbol
    [Droplet]
    ("Historical Society" <:> "Dusty Archives")
    [Basement, Passageway]
    Trefoil
    [Droplet]
    ReturnToEchoesOfThePast

historicalSocietyMuseumStorage :: CardDef
historicalSocietyMuseumStorage =
  locationWithUnrevealed
    "52031"
    "Historical Society"
    [Basement]
    NoSymbol
    [Droplet]
    ("Historical Society" <:> "Museum Storage")
    [Basement, Passageway]
    Trefoil
    [Droplet]
    ReturnToEchoesOfThePast

returnToQuietHalls :: CardDef
returnToQuietHalls = location "52029" "Quiet Halls" [Basement] Droplet [Square] ReturnToEchoesOfThePast
