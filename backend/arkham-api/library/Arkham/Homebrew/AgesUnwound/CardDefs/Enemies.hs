module Arkham.Homebrew.AgesUnwound.CardDefs.Enemies where

import Arkham.Enemy.CardDefs.Import
import Arkham.Homebrew.AgesUnwound.Sets qualified as Set
import Arkham.Homebrew.AgesUnwound.Traits
import Arkham.Keyword qualified as Keyword

eternitysSentinel_016 :: CardDef
eternitysSentinel_016 =
  (enemy ":ages-unwound:016" ("Eternity's Sentinel" <:> "Scourge in the Shadows") Set.NightOfFire 1)
    { cdFight = fight 5
    , cdHealth = healthStar
    , cdEvade = evade 3
    , cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdCardTraits = setFromList [Humanoid, Monster, Temporal, Elite]
    , cdKeywords = singleton Keyword.Hunter
    }

eternitysSentinel_017 :: CardDef
eternitysSentinel_017 =
  (enemy ":ages-unwound:017" ("Eternity's Sentinel" <:> "Watcher of the Ages") Set.NightOfFire 1)
    { cdFight = fight 3
    , cdHealth = healthPerInvestigator 5
    , cdEvade = evade 5
    , cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdCardTraits = setFromList [Humanoid, Monster, Temporal, Elite]
    , cdKeywords = setFromList [Keyword.Hunter, Keyword.Aloof]
    , cdVictoryPoints = Just 2
    }

timeSpirit :: CardDef
timeSpirit =
  (enemy ":ages-unwound:018" "Time Spirit" Set.NightOfFire 4)
    { cdFight = fight 3
    , cdHealth = health 4
    , cdEvade = evade 2
    , cdSanityDamage = sanityDamage 1
    , cdCardTraits = setFromList [Monster, Temporal]
    }

arsonists :: CardDef
arsonists =
  (enemy ":ages-unwound:019" "Arsonists" Set.NightOfFire 2)
    { cdFight = fight 2
    , cdHealth = health 2
    , cdEvade = evade 3
    , cdHealthDamage = healthDamage 1
    , cdCardTraits = setFromList [Humanoid, Criminal]
    , cdKeywords = setFromList [Keyword.Hunter, Keyword.Aloof]
    }

theMyriadGentleman_042 :: CardDef
theMyriadGentleman_042 =
  (enemy ":ages-unwound:042" ("The Myriad Gentleman" <:> "Thousandfold Man") Set.TheMyriadGentleman 1)
    { cdFight = fight 3
    , cdHealth = health 1
    , cdEvade = evade 2
    , cdHealthDamage = healthDamage 1
    , cdCardTraits = setFromList [Humanoid, Cultist, Myriad]
    }

theMyriadGentleman_043 :: CardDef
theMyriadGentleman_043 =
  ( enemy
      ":ages-unwound:043"
      ("The Myriad Gentleman" <:> "Master of the House")
      Set.TheMyriadGentleman
      1
  )
    { cdFight = fight 4
    , cdHealth = healthPerInvestigator 3
    , cdEvade = evade 3
    , cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdCardTraits = setFromList [Humanoid, Cultist, Myriad, Elite]
    , cdVictoryPoints = Just 1
    }

determinedGeneral :: CardDef
determinedGeneral =
  (enemy ":ages-unwound:088" ("Determined General" <:> "For the Glory of Rome!") Set.Unstuck 1)
    { cdFight = fight 4
    , cdHealth = health 4
    , cdEvade = evade 4
    , cdHealthDamage = healthDamage 1
    , cdCardTraits = setFromList [Humanoid, Elite]
    , cdKeywords = setFromList [Keyword.Retaliate, Keyword.Alert]
    , cdVictoryPoints = Just 1
    }

panzerIV :: CardDef
panzerIV =
  (enemy ":ages-unwound:089" ("Panzer IV" <:> "German War Machine") Set.Unstuck 1)
    { cdFight = fight 3
    , cdHealth = health 6
    , cdEvade = evade 4
    , cdHealthDamage = healthDamage 2
    , cdCardTraits = setFromList [Vehicle, Elite]
    , cdKeywords = singleton Keyword.Massive
    , cdVictoryPoints = Just 1
    }

shamblerFromTheStars :: CardDef
shamblerFromTheStars =
  ( enemy
      ":ages-unwound:090"
      ("Shambler from the Stars" <:> "Terror, Teeth and Tentacles")
      Set.Unstuck
      1
  )
    { cdFight = fight 3
    , cdHealth = health 4
    , cdEvade = evade 3
    , cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdCardTraits = setFromList [Monster, Elite]
    , cdKeywords = setFromList [Keyword.Hunter, Keyword.Aloof]
    , cdVictoryPoints = Just 1
    }

tyrannosaurusRex :: CardDef
tyrannosaurusRex =
  (enemy ":ages-unwound:091" ("Tyrannosaurus Rex" <:> "Huge. Vicious. Hungry.") Set.Unstuck 1)
    { cdFight = fight 4
    , cdHealth = health 5
    , cdEvade = evade 2
    , cdHealthDamage = healthDamage 2
    , cdSanityDamage = sanityDamage 2
    , cdCardTraits = setFromList [Creature, Dinosaur, Elite]
    , cdKeywords = singleton Keyword.Massive
    , cdVictoryPoints = Just 1
    }

eagerSphinx :: CardDef
eagerSphinx =
  (enemy ":ages-unwound:092" "Eager Sphinx" Set.Unstuck 2)
    { cdFight = fight 5
    , cdHealth = health 2
    , cdEvade = evade 4
    , cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdCardTraits = setFromList [Monster, Sphinx]
    , cdKeywords = setFromList [Keyword.Hunter, Keyword.Alert]
    }

persecutedWitch :: CardDef
persecutedWitch =
  (enemy ":ages-unwound:093" "Persecuted Witch" Set.Unstuck 2)
    { cdRevelation = IsRevelation
    , cdFight = fight 2
    , cdHealth = health 2
    , cdEvade = evade 2
    , cdSanityDamage = sanityDamage 1
    , cdCardTraits = setFromList [Humanoid, Witch]
    }

velociraptor :: CardDef
velociraptor =
  (enemy ":ages-unwound:094" "Velociraptor" Set.Unstuck 3)
    { cdFight = fight 3
    , cdHealth = health 3
    , cdEvade = evade 3
    , cdHealthDamage = healthDamage 1
    , cdCardTraits = setFromList [Creature, Dinosaur]
    , cdKeywords = setFromList [Keyword.Hunter, Keyword.Retaliate, Keyword.Alert]
    }

displacedLegion :: CardDef
displacedLegion =
  (enemy ":ages-unwound:124" "Displaced Legion" Set.AYearToPlan 1)
    { cdFight = fight 3
    , cdHealth = health 8
    , cdEvade = evade 3
    , cdHealthDamage = healthDamage 2
    , cdCardTraits = setFromList [Humanoid, Army]
    , cdKeywords = singleton Keyword.Massive
    , cdVictoryPoints = Just 1
    }

smilodon :: CardDef
smilodon =
  (enemy ":ages-unwound:125" "Smilodon" Set.AYearToPlan 2)
    { cdFight = fight 4
    , cdHealth = health 4
    , cdEvade = evade 5
    , cdHealthDamage = healthDamage 1
    , cdCardTraits = singleton Creature
    , cdKeywords = setFromList [Keyword.Hunter, Keyword.Retaliate]
    }

ancientSphinx :: CardDef
ancientSphinx =
  otherSideIs ":ages-unwound:145b"
    $ (enemy ":ages-unwound:145" ("Ancient Sphinx" <:> "Disturbed from Slumber") Set.Missions 1)
      { cdFight = fight 3
      , cdHealth = healthPerInvestigator 4
      , cdEvade = evade 5
      , cdCardTraits = setFromList [Monster, Sphinx, Elite]
      , cdKeywords = singleton Keyword.Alert
      , cdVictoryPoints = Just 1
      }

brainwashedExpedition :: CardDef
brainwashedExpedition =
  otherSideIs ":ages-unwound:146b"
    $ (enemy ":ages-unwound:146" ("Brainwashed Expedition" <:> "An Endless Onslaught") Set.Missions 1)
      { cdFight = fight 3
      , cdHealth = healthPerInvestigator 1
      , cdEvade = evade 4
      , cdCardTraits = setFromList [Humanoid, Elite]
      , cdKeywords = setFromList [Keyword.Massive, Keyword.Swarming (Static 2)]
      , cdVictoryPoints = Just 1
      }

savageYeti :: CardDef
savageYeti =
  otherSideIs ":ages-unwound:147b"
    $ (enemy ":ages-unwound:147" "Savage Yeti" Set.Missions 1)
      { cdFight = fight 3
      , cdHealth = health 6
      , cdEvade = evade 3
      , cdCardTraits = setFromList [Monster, Yeti, Elite]
      , cdKeywords = singleton Keyword.Retaliate
      }

estraviusMalone :: CardDef
estraviusMalone =
  ( enemy
      ":ages-unwound:174"
      ("Estravius Malone" <:> "Dread Sorcerer of Yog-Sothoth")
      Set.AWorldTornDownAgain
      1
  )
    { cdFight = fight 4
    , cdHealth = healthPerInvestigator 4
    , cdEvade = evade 3
    , cdSanityDamage = sanityDamage 1
    , cdCardTraits = setFromList [Humanoid, Cultist, Elite]
    , cdKeywords = singleton Keyword.Hunter
    , cdVictoryPoints = Just 0
    }

sheldonsFinest :: CardDef
sheldonsFinest =
  (enemy ":ages-unwound:178" "Sheldon's Finest" Set.AWorldTornDownAgain 3)
    { cdFight = fight 4
    , cdHealth = health 3
    , cdEvade = evade 2
    , cdHealthDamage = healthDamage 2
    , cdCardTraits = setFromList [Humanoid, Criminal]
    , cdKeywords = setFromList [Keyword.Hunter, Keyword.Retaliate]
    }

theMyriad :: CardDef
theMyriad =
  otherSideIs ":ages-unwound:190"
    $ (enemy ":ages-unwound:190b" ("The Myriad" <:> "Weapon Without Form") Set.TimeRunsOut 1)
      { cdFight = fight 3
      , cdHealth = health 2
      , cdEvade = evade 5
      , cdHealthDamage = healthDamage 1
      , cdCardTraits = setFromList [Monster, Temporal, Myriad, Servitor, Elite]
      , cdKeywords = setFromList [Keyword.Massive, Keyword.Swarming (PerPlayer 2)]
      , cdVictoryPoints = Just 1
      }

eternitysSentinel_193b :: CardDef
eternitysSentinel_193b =
  otherSideIs ":ages-unwound:193"
    $ ( enemy
          ":ages-unwound:193b"
          ("Eternity's Sentinel" <:> "He's Been Waiting For So Long")
          Set.TimeRunsOut
          1
      )
      { cdFight = fight 2
      , cdHealth = healthPerInvestigator 6
      , cdEvade = evade 5
      , cdHealthDamage = healthDamage 1
      , cdSanityDamage = sanityDamage 1
      , cdCardTraits = setFromList [Humanoid, Monster, Temporal, Servitor, Elite]
      , cdVictoryPoints = Just 1
      }

truth :: CardDef
truth =
  otherSideIs ":ages-unwound:194"
    $ (enemy ":ages-unwound:194b" ("Truth" <:> "Words Will Never Hurt You?") Set.TimeRunsOut 1)
      { cdFight = fight 4
      , cdHealth = healthPerInvestigator 4
      , cdEvade = evade 3
      , cdSanityDamage = sanityDamage 1
      , cdCardTraits = setFromList [Temporal, Servitor, Elite]
      , cdKeywords = setFromList [Keyword.Hunter, Keyword.Alert]
      , cdVictoryPoints = Just 1
      }

oblivion :: CardDef
oblivion =
  otherSideIs ":ages-unwound:204"
    $ (enemy ":ages-unwound:204b" ("Oblivion" <:> "Destruction Given Form") Set.TimeRunsOut 1)
      { cdFight = fight 3
      , cdHealth = healthStar
      , cdEvade = evade 3
      , cdHealthDamage = healthDamage 5
      , cdSanityDamage = sanityDamage 5
      , cdCardTraits = setFromList [Abomination, Temporal, Elite]
      , cdKeywords = setFromList [Keyword.Hunter, Keyword.Massive]
      }

yourself :: CardDef
yourself =
  (enemy ":ages-unwound:208" ("Yourself" <:> "Plucked From Another Reality") Set.TimeRunsOut 1)
    { cdFight = fightStar
    , cdHealth = health 4
    , cdEvade = evadeStar
    , cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdCardTraits = setFromList [Humanoid, Elite]
    , cdKeywords = singleton Keyword.Hunter
    }

agelessWatchers :: CardDef
agelessWatchers =
  (enemy ":ages-unwound:209" "Ageless Watchers" Set.TimeRunsOut 2)
    { cdFight = fight 3
    , cdHealth = health 1
    , cdEvade = evade 4
    , cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdCardTraits = setFromList [Humanoid, Monster]
    , cdKeywords = setFromList [Keyword.Hunter, Keyword.Swarming (Static 1)]
    }

blessedOfAforgomon :: CardDef
blessedOfAforgomon =
  (enemy ":ages-unwound:210" "Blessed of Aforgomon" Set.TimeRunsOut 1)
    { cdFight = fight 3
    , cdHealth = health 5
    , cdEvade = evade 5
    , cdSanityDamage = sanityDamage 3
    , cdCardTraits = setFromList [Monster, Temporal, Avatar]
    }

entropicShade :: CardDef
entropicShade =
  (enemy ":ages-unwound:211" "Entropic Shade" Set.TimeRunsOut 2)
    { cdFight = fight 5
    , cdHealth = health 4
    , cdEvade = evade 2
    , cdHealthDamage = healthDamage 2
    , cdSanityDamage = sanityDamage 1
    , cdCardTraits = setFromList [Monster, Temporal]
    , cdKeywords = setFromList [Keyword.Hunter, Keyword.Alert]
    }

fateweaver :: CardDef
fateweaver =
  (enemy ":ages-unwound:212" "Fateweaver" Set.TimeRunsOut 3)
    { cdFight = fight 4
    , cdHealth = health 2
    , cdEvade = evade 4
    , cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdCardTraits = setFromList [Monster, Temporal]
    , cdKeywords = singleton Keyword.Aloof
    }

chronophage :: CardDef
chronophage =
  (enemy ":ages-unwound:218" "Chronophage" Set.AgentsOfChronos 1)
    { cdFight = fight 4
    , cdHealth = health 4
    , cdEvade = evade 3
    , cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 2
    , cdCardTraits = setFromList [Monster, Temporal]
    , cdKeywords = setFromList [Keyword.Hunter, Keyword.Alert]
    , cdVictoryPoints = Just 1
    }

theMyriadGentleman_220 :: CardDef
theMyriadGentleman_220 =
  (enemy ":ages-unwound:220" ("The Myriad Gentleman" <:> "One of Many") Set.Myriad 4)
    { cdFight = fight 2
    , cdHealth = health 2
    , cdEvade = evade 2
    , cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdCardTraits = setFromList [Humanoid, Cultist, Myriad]
    , cdKeywords = setFromList [Keyword.Hunter, Keyword.Swarming (Static 1)]
    }

houndOfUnmaking :: CardDef
houndOfUnmaking =
  (enemy ":ages-unwound:232" ("Hound of Unmaking" <:> "Time Ends Here") Set.NightOfTheRitual 1)
    { cdFight = fight 4
    , cdHealth = healthPerInvestigator 6
    , cdEvade = evade 4
    , cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 2
    , cdCardTraits = setFromList [Monster, Abomination, Temporal, Elite]
    , cdKeywords = setFromList [Keyword.Retaliate, Keyword.Massive]
    , cdVictoryPoints = Just 2
    }

theMyriadGentleman_233 :: CardDef
theMyriadGentleman_233 =
  (enemy ":ages-unwound:233" ("The Myriad Gentleman" <:> "The High Priest") Set.NightOfTheRitual 1)
    { cdFight = fight 4
    , cdHealth = healthPerInvestigator 4
    , cdEvade = evade 3
    , cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdCardTraits = setFromList [Humanoid, Cultist, Elite]
    , cdKeywords = setFromList [Keyword.Retaliate, Keyword.Alert]
    , cdVictoryPoints = Just 1
    }

hiredThugs :: CardDef
hiredThugs =
  (enemy ":ages-unwound:249" "Hired Thugs" Set.Thugs 3)
    { cdFight = fight 2
    , cdHealth = health 2
    , cdEvade = evade 1
    , cdHealthDamage = healthDamage 1
    , cdCardTraits = setFromList [Humanoid, Criminal]
    , cdKeywords = setFromList [Keyword.Hunter, Keyword.Retaliate]
    }

myriadAssassin :: CardDef
myriadAssassin =
  (enemy ":ages-unwound:250" "Myriad Assassin" Set.Thugs 1)
    { cdFight = fight 3
    , cdHealth = health 4
    , cdEvade = evade 4
    , cdHealthDamage = healthDamage 2
    , cdCardTraits = setFromList [Humanoid, Criminal]
    , cdKeywords = setFromList [Keyword.Hunter, Keyword.Alert]
    , cdVictoryPoints = Just 1
    }

irregulars :: CardDef
irregulars =
  (enemy ":ages-unwound:251" "Irregulars" Set.Thugs 1)
    { cdFight = fight 3
    , cdHealth = health 3
    , cdEvade = evade 2
    , cdSanityDamage = sanityDamage 2
    , cdCardTraits = setFromList [Humanoid, Criminal, Temporal]
    , cdKeywords = singleton Keyword.Hunter
    , cdVictoryPoints = Just 1
    }

{- | /Roman Soldier/ has no printed card. Determined General and Roman Outpost
both say "put the top card of your deck into play in your threat area, as a
Roman Soldier enemy with 3 fight, 1 health, 3 evade, 1 damage and the
[[Humanoid]] trait", so the engine needs a def to mint the copy from.

Quantity 0 keeps it out of every gather, the way the core special enemies
(@xreanimated@, @xpolyp@ -- 'Arkham.Enemy.Cards.allSpecialEnemyCards') do. The
code sits above the printed range (001-257) to make clear it is not a card in
the pack. Build the copy with @lookupEncounterCard romanSoldier pc.id@ carrying
@ecOwner@, as @Scenarios/TheMyriadGentleman/Helpers@ does -- NOT by rewriting a
player card's @pcCardCode@, which resolves only against
@allPlayerCards <> allSpecialEnemyCards@ and so cannot see a homebrew def.
-}
romanSoldier :: CardDef
romanSoldier =
  (enemy ":ages-unwound:900" "Roman Soldier" Set.Unstuck 0)
    { cdFight = fight 3
    , cdHealth = health 1
    , cdEvade = evade 3
    , cdHealthDamage = healthDamage 1
    , cdCardTraits = singleton Humanoid
    }
