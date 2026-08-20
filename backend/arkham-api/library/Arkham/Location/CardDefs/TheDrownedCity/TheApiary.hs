module Arkham.Location.CardDefs.TheDrownedCity.TheApiary where

import Arkham.Location.CardDefs.Import

acidicCoelom :: CardDef
acidicCoelom =
  singleSided
    $ victory 1
    $ location "11571" "Acidic Coelom" [Apiary, Nest, Sanctum] Square [Heart, Diamond] TheApiary

apiaryEntranceBeckoningLight :: CardDef
apiaryEntranceBeckoningLight =
  otherSideIs "11559b"
    $ location
      "11559"
      ("Apiary Entrance" <:> "Beckoning Light")
      [Apiary, Central]
      Diamond
      [Moon, Equals, Spade, Circle, Square]
      TheApiary

apiaryEntranceDangerousExit :: CardDef
apiaryEntranceDangerousExit =
  otherSideIs "11559"
    $ location
      "11559b"
      ("Apiary Entrance" <:> "Dangerous Exit")
      [Apiary, Central]
      Diamond
      [Moon, Equals, Spade, Circle, Square]
      TheApiary

centralChamber :: CardDef
centralChamber =
  location_ "11572" "Central Chamber" [Apiary, Nest, Central] TheApiary

churningChasm :: CardDef
churningChasm =
  singleSided
    $ location "11563" "Churning Chasm" [Apiary] Droplet [Heart] TheApiary

corruptedVault :: CardDef
corruptedVault =
  singleSided
    $ victory 1
    $ location "11564" "Corrupted Vault" [Apiary, Glyph] Triangle [Circle] TheApiary

fleshyPathsEasternBurrows :: CardDef
fleshyPathsEasternBurrows =
  singleSided
    $ location "11560" ("Fleshy Paths" <:> "Eastern Burrows") [Apiary] Moon [Diamond] TheApiary

fleshyPathsWesternBurrows :: CardDef
fleshyPathsWesternBurrows =
  singleSided
    $ victory 1
    $ location "11561" ("Fleshy Paths" <:> "Western Burrows") [Apiary, Glyph] Equals [Diamond] TheApiary

graspingCorridor :: CardDef
graspingCorridor =
  singleSided
    $ location "11569" "Grasping Corridor" [Apiary, Nest] Circle [Diamond, Heart, Triangle] TheApiary

growingFields :: CardDef
growingFields =
  singleSided
    $ location "11562" "Growing Fields" [Apiary] Spade [Diamond, Star] TheApiary

hiddenVault :: CardDef
hiddenVault =
  storyOnBack' "11579b"
    $ victory 1
    $ location "11579" "Hidden Vault" [Apiary, Glyph] Star [Spade] TheApiary

lostCampsite :: CardDef
lostCampsite =
  singleSided
    $ victory 1
    $ location "11567" "Lost Campsite" [Apiary, Enclave, Sanctum] Square [Heart] TheApiary

luminousTunnels :: CardDef
luminousTunnels =
  singleSided
    $ location "11565" "Luminous Tunnels" [Apiary, Enclave] Circle [Diamond, Heart, Triangle] TheApiary

spawningGrounds :: CardDef
spawningGrounds =
  singleSided
    $ location "11566" "Spawning Grounds" [Apiary, Enclave] Heart [Circle, Square, Droplet] TheApiary

starvingCorridor :: CardDef
starvingCorridor =
  singleSided
    $ location "11570" "Starving Corridor" [Apiary, Nest] Heart [Circle, Square, Droplet] TheApiary
