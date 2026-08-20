module Arkham.Location.CardDefs.TheFeastOfHemlockVale.TheLongestNight where

import Arkham.Location.CardDefs.Import

barn :: CardDef
barn = locationWithUnrevealed_ "10634" "Atwood Farm" [Farm] "Barn" [Farm] TheLongestNight

coop :: CardDef
coop = locationWithUnrevealed_ "10633" "Atwood Farm" [Farm] "Coop" [Farm] TheLongestNight

milkhouse :: CardDef
milkhouse = locationWithUnrevealed_ "10631" "Atwood Farm" [Farm] "Milkhouse" [Farm] TheLongestNight

outerFieldsBlightedCornfields :: CardDef
outerFieldsBlightedCornfields =
  locationWithUnrevealed_
    "10638"
    "Outer Fields"
    [Field, Blight]
    ("Outer Fields" <:> "Blighted Cornfields")
    [Field, Blight]
    TheLongestNight

outerFieldsBloodiedPaths :: CardDef
outerFieldsBloodiedPaths =
  locationWithUnrevealed_
    "10636"
    "Outer Fields"
    [Field, Blight]
    ("Outer Fields" <:> "Bloodied Paths")
    [Field, Blight]
    TheLongestNight

outerFieldsDesolateHills :: CardDef
outerFieldsDesolateHills =
  locationWithUnrevealed_
    "10637"
    "Outer Fields"
    [Field, Blight]
    ("Outer Fields" <:> "Desolate Hills")
    [Field, Blight]
    TheLongestNight

outerFieldsRancidCrops :: CardDef
outerFieldsRancidCrops =
  locationWithUnrevealed_
    "10640"
    "Outer Fields"
    [Field, Blight]
    ("Outer Fields" <:> "Rancid Crops")
    [Field, Blight]
    TheLongestNight

outerFieldsScorchedKnoll :: CardDef
outerFieldsScorchedKnoll =
  locationWithUnrevealed_
    "10639"
    "Outer Fields"
    [Field, Blight]
    ("Outer Fields" <:> "Scorched Knoll")
    [Field, Blight]
    TheLongestNight

pasture :: CardDef
pasture = locationWithUnrevealed_ "10635" "Atwood Farm" [Farm] "Pasture" [Farm] TheLongestNight

theFarmhouse :: CardDef
theFarmhouse = location_ "10630" "The Farmhouse" [Sanctum] TheLongestNight

vineyard :: CardDef
vineyard = locationWithUnrevealed_ "10632" "Atwood Farm" [Farm] "Vineyard" [Farm] TheLongestNight
