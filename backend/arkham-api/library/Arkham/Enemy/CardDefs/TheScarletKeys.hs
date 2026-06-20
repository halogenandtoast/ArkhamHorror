module Arkham.Enemy.CardDefs.TheScarletKeys where

import Arkham.Enemy.CardDefs.Import
import Arkham.Keyword qualified as Keyword

agentFletcher :: CardDef
agentFletcher =
  (weakness "09010" "Agent Fletcher")
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 2
    , cdEvade = evade 3
    , cdHealth = health 3
    , cdCardTraits = setFromList [Humanoid, Coterie, Detective]
    , cdKeywords = setFromList [Keyword.Alert, Keyword.Hunter]
    }

lurkerInTheDark :: CardDef
lurkerInTheDark =
  (basicWeakness "09124" "Lurker in the Dark")
    { cdHealthDamage = healthDamage 1
    , cdFight = fight 3
    , cdEvade = evade 1
    , cdHealth = health 2
    , cdCardTraits = setFromList [Monster, Shoggoth]
    , cdKeywords = setFromList [Keyword.Hunter]
    , cdDeckRestrictions = [OnlyClass Guardian]
    }

ectoplasmicHorror :: CardDef
ectoplasmicHorror =
  (basicWeakness "09127" "Ectoplasmic Horror")
    { cdSanityDamage = sanityDamage 1
    , cdFight = fight 2
    , cdEvade = evade 2
    , cdHealth = health 2
    , cdCardTraits = setFromList [Monster, Geist]
    , cdKeywords = setFromList [Keyword.Hunter]
    , cdDeckRestrictions = [OnlyClass Mystic]
    }

theRedGlovedManShroudedInMystery :: CardDef
theRedGlovedManShroudedInMystery =
  (enemy "09518" ("The Red-Gloved Man" <:> "Shrouded in Mystery") RiddlesAndRain 1)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 5
    , cdEvade = evade 5
    , cdHealth = healthPerInvestigator 2
    , cdCardTraits = setFromList [Humanoid, Coterie, Elite]
    , cdKeywords = setFromList [Keyword.Concealed TheRedGlovedMan (PerPlayer 1), Keyword.Retaliate]
    , cdVictoryPoints = Just 1
    , cdUnique = True
    }

amaranthLurkingCorruption :: CardDef
amaranthLurkingCorruption =
  doubleSided "09537b"
    $ (enemy "09537a" ("Amaranth" <:> "Lurking Corruption") DeadHeat 1)
      { cdHealthDamage = healthDamage 1
      , cdSanityDamage = sanityDamage 2
      , cdFight = fight 3
      , cdEvade = evade 4
      , cdHealth = healthPerInvestigator 3
      , cdCardTraits = setFromList [Humanoid, Coterie, Elite]
      , cdKeywords = setFromList [Keyword.Hunter]
      , cdVictoryPoints = Just 1
      , cdUnique = True
      }

amaranthCorruptionRevealed :: CardDef
amaranthCorruptionRevealed =
  doubleSided "09537a"
    $ (enemy "09537b" ("Amaranth" <:> "Corruption Revealed") DeadHeat 1)
      { cdHealthDamage = healthDamage 2
      , cdSanityDamage = sanityDamage 1
      , cdFight = fight 4
      , cdEvade = evade 4
      , cdHealth = healthPerInvestigator 6
      , cdCardTraits = setFromList [Humanoid, Coterie, Elite]
      , cdKeywords = setFromList [Keyword.Hunter, Keyword.Retaliate]
      , cdVictoryPoints = Just 1
      , cdUnique = True
      }

razinFarhiReanimatedArtificer :: CardDef
razinFarhiReanimatedArtificer =
  (enemy "09538" ("Razin Farhi" <:> "Reanimated Artificer") DeadHeat 1)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 4
    , cdEvade = evade 3
    , cdHealth = healthPerInvestigator 4
    , cdCardTraits = setFromList [Humanoid, Coterie, Risen, Elite]
    , cdKeywords = setFromList [Keyword.Alert, Keyword.Hunter, Keyword.Retaliate]
    , cdVictoryPoints = Just 0
    , cdUnique = True
    }

thrallDeadHeat :: CardDef
thrallDeadHeat =
  (enemy "09539" "Thrall" DeadHeat 4)
    { cdHealthDamage = healthDamage 1
    , cdFight = fight 2
    , cdEvade = evade 1
    , cdHealth = health 2
    , cdCardTraits = setFromList [Humanoid, Risen]
    }

ancientRaider :: CardDef
ancientRaider =
  (enemy "09540" "Ancient Raider" DeadHeat 2)
    { cdHealthDamage = healthDamage 1
    , cdFight = fight 3
    , cdEvade = evade 2
    , cdHealth = health 3
    , cdCardTraits = setFromList [Humanoid, Risen]
    , cdKeywords = setFromList [Keyword.Hunter, Keyword.Retaliate]
    }

khalidBelovedCompanion :: CardDef
khalidBelovedCompanion =
  (enemy "09541" ("Khalid" <:> "Beloved Companion") DeadHeat 1)
    { cdHealthDamage = healthDamage 2
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 4
    , cdEvade = evade 3
    , cdHealth = health 5
    , cdCardTraits = setFromList [Creature, Risen]
    , cdKeywords = setFromList [Keyword.Hunter]
    , cdVictoryPoints = Just 1
    , cdUnique = True
    }

laChicaRojaTheGirlInTheCarmineCoat :: CardDef
laChicaRojaTheGirlInTheCarmineCoat =
  doubleSided "09557b"
    $ (enemy "09557" ("La Chica Roja" <:> "The Girl in the Carmine Coat") SanguineShadows 1)
      { cdHealthDamage = healthDamage 1
      , cdSanityDamage = sanityDamage 1
      , cdFight = fight 3
      , cdEvade = evade 5
      , cdHealth = health 2
      , cdCardTraits = setFromList [Humanoid, Coterie, Elite]
      , cdKeywords = setFromList [Keyword.Concealed LaChicaRoja (Static 5)]
      , cdVictoryPoints = Just 1
      , cdUnique = True
      }

boundNightgaunt :: CardDef
boundNightgaunt =
  (enemy "09558" "Bound Nightgaunt" SanguineShadows 2)
    { cdSanityDamage = sanityDamage 2
    , cdFight = fight 1
    , cdEvade = evade 3
    , cdHealth = health 3
    , cdCardTraits = setFromList [Monster, Nightgaunt]
    , cdKeywords = setFromList [Keyword.Hunter]
    }

theSanguineWatcherWithTheRubySpectacles :: CardDef
theSanguineWatcherWithTheRubySpectacles =
  (enemy "09563" ("The Sanguine Watcher" <:> "With the Ruby Spectacles") SanguineShadows 1)
    { cdHealthDamage = healthDamage 2
    , cdSanityDamage = sanityDamage 2
    , cdFight = fight 4
    , cdEvade = evade 4
    , cdHealth = healthPerInvestigator 5
    , cdCardTraits = setFromList [Humanoid, Coterie, Elite]
    , cdKeywords = setFromList [Keyword.Retaliate]
    , cdVictoryPoints = Just 2
    , cdUnique = True
    }

apportionedKa :: CardDef
apportionedKa =
  (enemy "09564" "Apportioned Ka" SanguineShadows 1)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdCardTraits = setFromList [Ritual, Elite]
    , cdKeywords = setFromList [Keyword.Concealed ApportionedKa (Static 4)]
    }

umbralHarbinger :: CardDef
umbralHarbinger =
  (enemy "09585" "Umbral Harbinger" DealingsInTheDark 1)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 3
    , cdEvade = evade 1
    , cdHealth = health 5
    , cdCardTraits = setFromList [Monster, Yuggoth]
    , cdKeywords = setFromList [Keyword.Hunter]
    , cdVictoryPoints = Just 1
    }

sinisterAspirantA :: CardDef
sinisterAspirantA =
  (enemy "09586a" "Sinister Aspirant (A)" DealingsInTheDark 1)
    { cdSanityDamage = sanityDamage 1
    , cdFight = fight 2
    , cdEvade = evade 4
    , cdHealth = health 3
    , cdCardTraits = setFromList [Humanoid, Cultist]
    , cdKeywords = setFromList [Keyword.Alert]
    }

sinisterAspirantB :: CardDef
sinisterAspirantB =
  (enemy "09586b" "Sinister Aspirant (B)" DealingsInTheDark 1)
    { cdSanityDamage = sanityDamage 1
    , cdFight = fight 2
    , cdEvade = evade 4
    , cdHealth = health 3
    , cdCardTraits = setFromList [Humanoid, Cultist]
    , cdKeywords = setFromList [Keyword.Alert]
    }

sinisterAspirantC :: CardDef
sinisterAspirantC =
  (enemy "09586c" "Sinister Aspirant (C)" DealingsInTheDark 1)
    { cdSanityDamage = sanityDamage 1
    , cdFight = fight 2
    , cdEvade = evade 4
    , cdHealth = health 3
    , cdCardTraits = setFromList [Humanoid, Cultist]
    , cdKeywords = setFromList [Keyword.Alert]
    }

desiderioDelgadoAlvarez106 :: CardDef
desiderioDelgadoAlvarez106 =
  (enemy "09606" ("Desiderio Delgado Álvarez" <:> "The Man in the Blood-Soaked Suit") DancingMad 1)
    { cdHealthDamage = healthDamage 2
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 4
    , cdEvade = evade 3
    , cdHealth = health 2
    , cdCardTraits = setFromList [Humanoid, Coterie, Elite]
    , cdKeywords =
        setFromList
          [ Keyword.Alert
          , Keyword.Concealed DesiderioDelgadoAlvarez (PerPlayer 1)
          , Keyword.Hunter
          , Keyword.Retaliate
          ]
    , cdVictoryPoints = Just 1
    , cdUnique = True
    }

desiderioDelgadoAlvarez107 :: CardDef
desiderioDelgadoAlvarez107 =
  (enemy "09607" ("Desiderio Delgado Álvarez" <:> "The Man in the Blood-Soaked Suit") DancingMad 1)
    { cdHealthDamage = healthDamage 2
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 4
    , cdEvade = evade 3
    , cdHealth = health 2
    , cdCardTraits = setFromList [Humanoid, Coterie, Elite]
    , cdKeywords =
        setFromList
          [ Keyword.Alert
          , Keyword.Concealed DesiderioDelgadoAlvarez (PerPlayer 1)
          , Keyword.Hunter
          , Keyword.Retaliate
          ]
    , cdVictoryPoints = Just 1
    , cdUnique = True
    , cdDoubleSided = True
    , cdOtherSide = Just "09607b"
    }

thorneTheOneWithTheRedCravat :: CardDef
thorneTheOneWithTheRedCravat =
  (enemy "09625" ("Thorne" <:> "The One With the Red Cravat") OnThinIce 1)
    { cdHealthDamage = healthDamage 2
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 4
    , cdEvade = evade 4
    , cdHealth = health 6
    , cdCardTraits = setFromList [Humanoid, Coterie, Elite]
    , cdKeywords =
        setFromList
          [ Keyword.Hunter
          , Keyword.Retaliate
          ]
    , cdVictoryPoints = Just 0
    , cdUnique = True
    , cdDoubleSided = True
    , cdOtherSide = Just "09625b"
    }

voidChimeraTrueForm :: CardDef
voidChimeraTrueForm =
  (enemy "09626" ("Void Chimera" <:> "True Form") OnThinIce 1)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 4
    , cdEvade = evade 4
    , cdHealth = healthPerInvestigator 4
    , cdCardTraits = setFromList [Monster, Outsider, Elite]
    , cdKeywords = setFromList [Keyword.Concealed VoidChimeraTrueForm (Static 4), Keyword.Massive]
    , cdVictoryPoints = Just 1
    , cdUnique = True
    }

voidChimeraFellbeak :: CardDef
voidChimeraFellbeak =
  (enemy "09627" ("Void Chimera" <:> "Fellbeak") OnThinIce 1)
    { cdHealthDamage = healthDamage 3
    , cdFight = fight 4
    , cdEvade = evade 2
    , cdHealth = health 2
    , cdCardTraits = setFromList [Monster, Outsider, Elite]
    , cdKeywords =
        setFromList
          [Keyword.Patrol (LocationWithMostInvestigators $ LocationWithTrait Wilderness), Keyword.Retaliate]
    , cdVictoryPoints = Just 0
    , cdUnique = True
    }

voidChimeraEarsplitter :: CardDef
voidChimeraEarsplitter =
  (enemy "09628" ("Void Chimera" <:> "Earsplitter") OnThinIce 1)
    { cdSanityDamage = sanityDamage 3
    , cdFight = fight 3
    , cdEvade = evade 4
    , cdHealth = health 3
    , cdCardTraits = setFromList [Monster, Outsider, Elite]
    , cdKeywords = setFromList [Keyword.Alert, Keyword.Hunter]
    , cdVictoryPoints = Just 0
    , cdUnique = True
    }

voidChimeraGorefeaster :: CardDef
voidChimeraGorefeaster =
  (enemy "09629" ("Void Chimera" <:> "Gorefeaster") OnThinIce 1)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 2
    , cdFight = fight 3
    , cdEvade = evade 3
    , cdHealth = health 4
    , cdCardTraits = setFromList [Monster, Outsider, Elite]
    , cdKeywords = setFromList [Keyword.Hunter]
    , cdVictoryPoints = Just 0
    , cdUnique = True
    }

voidChimeraFellhound :: CardDef
voidChimeraFellhound =
  (enemy "09630" ("Void Chimera" <:> "Fellhound") OnThinIce 1)
    { cdHealthDamage = healthDamage 2
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 4
    , cdEvade = evade 4
    , cdHealth = health 2
    , cdCardTraits = setFromList [Monster, Outsider, Elite]
    , cdKeywords = setFromList [Keyword.Hunter]
    , cdVictoryPoints = Just 0
    , cdUnique = True
    }

ravenousGrizzly :: CardDef
ravenousGrizzly =
  (enemy "09631" "Ravenous Grizzly" OnThinIce 2)
    { cdHealthDamage = healthDamage 2
    , cdFight = fight 4
    , cdEvade = evade 2
    , cdHealth = health 3
    , cdCardTraits = singleton Creature
    , cdKeywords = setFromList [Keyword.Hunter, Keyword.Retaliate]
    }

theClaretKnightCoterieKingpin :: CardDef
theClaretKnightCoterieKingpin =
  (enemy "09654" ("The Claret Knight" <:> "Coterie Kingping") DogsOfWar 1)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 2
    , cdFight = fight 4
    , cdEvade = evade 3
    , cdHealth = healthPerInvestigator 4
    , cdCardTraits = setFromList [Humanoid, Coterie, Elite]
    , cdVictoryPoints = Just 1
    , cdUnique = True
    , cdDoubleSided = True
    , cdOtherSide = Just "09654b"
    }

theBeastInACowlOfCrimsonRavagerInRed :: CardDef
theBeastInACowlOfCrimsonRavagerInRed =
  (enemy "09655" ("The Beast in a Cowl of Crimson" <:> "Ravager in Red") DogsOfWar 1)
    { cdHealthDamage = healthDamage 2
    , cdSanityDamage = sanityDamage 2
    , cdFight = fight 3
    , cdEvade = evade 4
    , cdHealth = healthPerInvestigator 3
    , cdCardTraits = setFromList [Humanoid, Coterie, Elite]
    , cdKeywords =
        setFromList [Keyword.Patrol (LocationWithModifier (ScenarioModifier "keyLocus")), Keyword.Retaliate]
    , cdVictoryPoints = Just 1
    , cdUnique = True
    , cdDoubleSided = True
    , cdOtherSide = Just "09655b"
    }

theBeastInACowlOfCrimsonWolfInSheepsClothing :: CardDef
theBeastInACowlOfCrimsonWolfInSheepsClothing =
  (enemy "09655b" ("The Beast in a Cowl of Crimson" <:> "Wolf in Sheep's Clothing") DogsOfWar 1)
    { cdHealthDamage = healthDamage 2
    , cdSanityDamage = sanityDamage 2
    , cdFight = fight 3
    , cdEvade = evade 4
    , cdHealth = healthPerInvestigator 5
    , cdCardTraits = setFromList [Humanoid, Coterie, Elite]
    , cdVictoryPoints = Just 1
    , cdUnique = True
    , cdDoubleSided = True
    , cdOtherSide = Just "09655b"
    }

scarletBeast :: CardDef
scarletBeast =
  (enemy "09656" "Scarlet Beast" DogsOfWar 2)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 2
    , cdEvade = evade 4
    , cdHealth = health 4
    , cdCardTraits = setFromList [Monster, Coterie]
    , cdKeywords = setFromList [Keyword.Alert, Keyword.Hunter, Keyword.Retaliate]
    }

coterieProvocateur :: CardDef
coterieProvocateur =
  (enemy "09657" "Coterie Provocateur" DogsOfWar 2)
    { cdSanityDamage = sanityDamage 1
    , cdFight = fight 4
    , cdEvade = evade 3
    , cdHealth = health 2
    , cdCardTraits = setFromList [Monster, Coterie]
    , cdKeywords =
        setFromList [Keyword.Aloof, Keyword.Patrol (LocationWithModifier (ScenarioModifier "keyLocus"))]
    }

uncannyShadowPlayfulShadows :: CardDef
uncannyShadowPlayfulShadows =
  (enemy "09674a" "Uncanny Shadow" ShadesOfSuffering 1)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 3
    , cdEvade = evade 3
    , cdHealth = health 4
    , cdCardTraits = setFromList [Monster, Geist, Elite]
    , cdVictoryPoints = Just 0
    , cdDoubleSided = True
    , cdOtherSide = Just "09674b"
    }

uncannyShadowTimorousShadows :: CardDef
uncannyShadowTimorousShadows =
  (enemy "09674c" "Uncanny Shadow" ShadesOfSuffering 1)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 3
    , cdEvade = evade 3
    , cdHealth = health 4
    , cdCardTraits = setFromList [Monster, Geist, Elite]
    , cdVictoryPoints = Just 0
    , cdDoubleSided = True
    , cdOtherSide = Just "09674d"
    }

buriedMinerALostMemento :: CardDef
buriedMinerALostMemento =
  (enemy "09675a" "Buried Miner" ShadesOfSuffering 1)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 2
    , cdEvade = evade 4
    , cdHealth = health 4
    , cdCardTraits = setFromList [Monster, Geist, Elite]
    , cdVictoryPoints = Just 0
    , cdDoubleSided = True
    , cdOtherSide = Just "09675b"
    }

buriedMinerExhumeTheBones :: CardDef
buriedMinerExhumeTheBones =
  (enemy "09675c" "Buried Miner" ShadesOfSuffering 1)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 2
    , cdEvade = evade 4
    , cdHealth = health 4
    , cdCardTraits = setFromList [Monster, Geist, Elite]
    , cdVictoryPoints = Just 0
    , cdDoubleSided = True
    , cdOtherSide = Just "09675d"
    }

slainForemanSympathyPain :: CardDef
slainForemanSympathyPain =
  (enemy "09676a" "Slain Foreman" ShadesOfSuffering 1)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 4
    , cdEvade = evade 2
    , cdHealth = health 5
    , cdCardTraits = setFromList [Monster, Geist, Elite]
    , cdVictoryPoints = Just 0
    , cdDoubleSided = True
    , cdOtherSide = Just "09676b"
    }

slainForemanFamilialPain :: CardDef
slainForemanFamilialPain =
  (enemy "09676c" "Slain Foreman" ShadesOfSuffering 1)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 4
    , cdEvade = evade 2
    , cdHealth = health 5
    , cdCardTraits = setFromList [Monster, Geist, Elite]
    , cdVictoryPoints = Just 0
    , cdDoubleSided = True
    , cdOtherSide = Just "09676d"
    }

tzuSanNiangTheLadyWithTheRedParasol :: CardDef
tzuSanNiangTheLadyWithTheRedParasol =
  (enemy "09679" ("Tzu San Niang" <:> "The Lady with the Red Parasol") ShadesOfSuffering 1)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 2
    , cdEvade = evade 2
    , cdHealth = healthPerInvestigator 2
    , cdCardTraits = setFromList [Humanoid, Coterie, Elite]
    , cdKeywords = setFromList [Keyword.Concealed TzuSanNiang (Static 2)]
    , cdUnique = True
    , cdDoubleSided = True
    , cdOtherSide = Just "09679b"
    }

tzuSanNiangOutForBlood :: CardDef
tzuSanNiangOutForBlood =
  (enemy "09679b" ("Tzu San Niang" <:> "Out for Blood") ShadesOfSuffering 1)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 2
    , cdEvade = evade 2
    , cdHealth = health 4
    , cdCardTraits = setFromList [Humanoid, Coterie, Elite]
    , cdKeywords = setFromList [Keyword.Hunter, Keyword.Retaliate]
    , cdUnique = True
    , cdDoubleSided = True
    , cdOtherSide = Just "09679"
    , cdVictoryPoints = Just 1
    }

mimeticNemesisOtherworldlySubjugator :: CardDef
mimeticNemesisOtherworldlySubjugator =
  (enemy "09690" ("Mimetic Nemesis" <:> "Otherworldly Subjugator") WithoutATrace 1)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 3
    , cdEvade = evade 4
    , cdCardTraits = setFromList [Monster, Outsider, Elite]
    , cdKeywords = setFromList [Keyword.Alert, Keyword.Hunter, Keyword.Massive, Keyword.Retaliate]
    , cdUnique = True
    }

protoplasmicReassembler :: CardDef
protoplasmicReassembler =
  (enemy "09691" "Protoplastmic Reassembler" WithoutATrace 1)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 2
    , cdFight = fight 2
    , cdEvade = evade 2
    , cdHealth = health 6
    , cdCardTraits = setFromList [Monster, Outsider]
    , cdKeywords = singleton Keyword.Massive
    , cdVictoryPoints = Just 1
    }

mimeticNemesisInfiltratorOfRealities :: CardDef
mimeticNemesisInfiltratorOfRealities =
  (enemy "09715" ("Mimetic Nemesis" <:> "Infiltrator of Realities") CongressOfTheKeys 1)
    { cdHealthDamage = healthDamage 3
    , cdSanityDamage = sanityDamage 3
    , cdFight = fight 5
    , cdEvade = evade 5
    , cdHealth = health 3
    , cdCardTraits = setFromList [AncientOne, Outsider, Elite]
    , cdKeywords = singleton Keyword.Massive
    , cdUnique = True
    }

coterieAgentA :: CardDef
coterieAgentA =
  (enemy "09716a" "Coterie Agent (A)" CrimsonConspiracy 1)
    { cdHealthDamage = healthDamage 1
    , cdFight = fight 1
    , cdEvade = evade 1
    , cdHealth = health 1
    , cdCardTraits = setFromList [Humanoid, Coterie]
    , cdKeywords = singleton $ Keyword.Concealed CoterieAgentA (Static 2)
    }

coterieAgentB :: CardDef
coterieAgentB =
  (enemy "09716b" "Coterie Agent (B)" CrimsonConspiracy 1)
    { cdHealthDamage = healthDamage 1
    , cdFight = fight 1
    , cdEvade = evade 1
    , cdHealth = health 1
    , cdCardTraits = setFromList [Humanoid, Coterie]
    , cdKeywords = singleton $ Keyword.Concealed CoterieAgentB (Static 2)
    }

coterieAgentC :: CardDef
coterieAgentC =
  (enemy "09716c" "Coterie Agent (C)" CrimsonConspiracy 1)
    { cdHealthDamage = healthDamage 1
    , cdFight = fight 1
    , cdEvade = evade 1
    , cdHealth = health 1
    , cdCardTraits = setFromList [Humanoid, Coterie]
    , cdKeywords = singleton $ Keyword.Concealed CoterieAgentC (Static 2)
    }

coterieEnvoy :: CardDef
coterieEnvoy =
  (enemy "09720" "Coterie Envoy" MysteriesAbound 2)
    { cdHealthDamage = healthDamage 1
    , cdFight = fight 2
    , cdEvade = evade 2
    , cdHealth = health 3
    , cdCardTraits = setFromList [Humanoid, Coterie]
    , cdKeywords =
        setFromList
          [ Keyword.Aloof
          , Keyword.Patrol
              $ LocationWithConcealedCard
              <> not_ (LocationWithEnemy $ EnemyIs "09720" <> not_ ThatEnemy)
          ]
    }

coterieEnforcerA :: CardDef
coterieEnforcerA =
  (enemy "09726a" "Coterie Enforcer (A)" CleanupCrew 1)
    { cdSanityDamage = sanityDamage 1
    , cdFight = fight 4
    , cdEvade = evade 1
    , cdHealth = health 3
    , cdCardTraits = setFromList [Humanoid, Coterie]
    , cdKeywords = singleton $ Keyword.Concealed CoterieEnforcerA (Static 1)
    }

coterieEnforcerB :: CardDef
coterieEnforcerB =
  (enemy "09726b" "Coterie Enforcer (B)" CleanupCrew 1)
    { cdSanityDamage = sanityDamage 1
    , cdFight = fight 4
    , cdEvade = evade 1
    , cdHealth = health 3
    , cdCardTraits = setFromList [Humanoid, Coterie]
    , cdKeywords = singleton $ Keyword.Concealed CoterieEnforcerB (Static 1)
    }

coterieAssassinA :: CardDef
coterieAssassinA =
  (enemy "09727a" "Coterie Assassin (A)" CleanupCrew 1)
    { cdHealthDamage = healthDamage 1
    , cdFight = fight 2
    , cdEvade = evade 4
    , cdHealth = health 2
    , cdCardTraits = setFromList [Humanoid, Coterie]
    , cdKeywords = singleton $ Keyword.Concealed CoterieAssassinA (Static 1)
    }

coterieAssassinB :: CardDef
coterieAssassinB =
  (enemy "09727b" "Coterie Assassin (B)" CleanupCrew 1)
    { cdHealthDamage = healthDamage 1
    , cdFight = fight 2
    , cdEvade = evade 4
    , cdHealth = health 2
    , cdCardTraits = setFromList [Humanoid, Coterie]
    , cdKeywords = singleton $ Keyword.Concealed CoterieAssassinB (Static 1)
    }

paracausalEntity :: CardDef
paracausalEntity =
  (enemy "09731" "Paracausal Entity" Outsiders 3)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 3
    , cdEvade = evade 3
    , cdHealth = health 2
    , cdCardTraits = setFromList [Monster, Outsider]
    , cdKeywords = singleton Keyword.Hunter
    }

apocalypticPresage :: CardDef
apocalypticPresage =
  (enemy "09732" "Apocalyptic Presage" Outsiders 1)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 2
    , cdFight = fight 4
    , cdEvade = evade 2
    , cdHealth = health 5
    , cdCardTraits = setFromList [Monster, Outsider]
    , cdVictoryPoints = Just 1
    }

otherworldlyMimic :: CardDef
otherworldlyMimic =
  (enemy "09734" "Otherworldly Mimic" SecretWar 2)
    { cdSanityDamage = sanityDamage 2
    , cdFight = fight 3
    , cdEvade = evade 4
    , cdHealth = health 2
    , cdCardTraits = setFromList [Monster, Outsider]
    , cdKeywords = setFromList [Keyword.Aloof, Keyword.Hunter]
    }

paradigmEfficer :: CardDef
paradigmEfficer =
  (enemy "09737" "Paradigm Efficer" AgentsOfTheOutside 2)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 3
    , cdEvade = evade 3
    , cdHealth = health 3
    , cdCardTraits = setFromList [Monster, Outsider]
    , cdKeywords = setFromList [Keyword.Aloof, Keyword.Hunter]
    }

emissaryFromYuggoth :: CardDef
emissaryFromYuggoth =
  (enemy "09739" "Emissary from Yuggoth" AgentsOfYuggoth 1)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 3
    , cdEvade = evade 3
    , cdHealth = health 4
    , cdCardTraits = setFromList [Monster, Yuggoth]
    , cdKeywords =
        setFromList [Keyword.Concealed EmissaryFromYuggoth (Static 2), Keyword.Hunter, Keyword.Massive]
    , cdVictoryPoints = Just 1
    }

theRedGlovedManPurposeUnknown :: CardDef
theRedGlovedManPurposeUnknown =
  (enemy "09752" ("The Red-Gloved Man" <:> "Purpose Unknown") RedCoterie 1)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 5
    , cdEvade = evade 5
    , cdHealth = health 3
    , cdCardTraits = setFromList [Humanoid, Coterie, Elite]
    , cdKeywords = setFromList [Keyword.Retaliate]
    , cdVictoryPoints = Just 0
    , cdUnique = True
    }

laChicaRojaHotOnYourTrail :: CardDef
laChicaRojaHotOnYourTrail =
  (enemy "09753" ("La Chica Roja" <:> "Hot on Your Trail") RedCoterie 1)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 3
    , cdEvade = evade 5
    , cdHealth = health 3
    , cdCardTraits = setFromList [Humanoid, Coterie, Elite]
    , cdKeywords = setFromList [Keyword.Aloof]
    , cdVictoryPoints = Just 0
    , cdUnique = True
    }

theSanguineWatcherHeSeesWhatIsNotThere :: CardDef
theSanguineWatcherHeSeesWhatIsNotThere =
  (enemy "09754" ("The Sanguine Watcher" <:> "He Sees What Is Not There") RedCoterie 1)
    { cdHealthDamage = healthDamage 2
    , cdSanityDamage = sanityDamage 2
    , cdFight = fight 4
    , cdEvade = evade 4
    , cdHealth = health 4
    , cdCardTraits = setFromList [Humanoid, Coterie, Elite]
    , cdKeywords = setFromList [Keyword.Hunter]
    , cdVictoryPoints = Just 0
    , cdUnique = True
    }

theBeastInACowlOfCrimsonLeavingATrailOfDestruction :: CardDef
theBeastInACowlOfCrimsonLeavingATrailOfDestruction =
  (enemy "09755" ("The Beast in a Cowl of Crimson" <:> "Leaving a Trail of Destruction") RedCoterie 1)
    { cdHealthDamage = healthDamage 2
    , cdSanityDamage = sanityDamage 2
    , cdFight = fight 3
    , cdEvade = evade 4
    , cdHealth = health 5
    , cdCardTraits = setFromList [Humanoid, Coterie, Elite]
    , cdKeywords = setFromList [Keyword.Hunter]
    , cdVictoryPoints = Just 0
    , cdUnique = True
    }

theClaretKnightHoldsYouInContempt :: CardDef
theClaretKnightHoldsYouInContempt =
  (enemy "09756" ("The Claret Knight" <:> "Holds You in Contempt") RedCoterie 1)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 2
    , cdFight = fight 4
    , cdEvade = evade 3
    , cdHealth = health 4
    , cdCardTraits = setFromList [Humanoid, Coterie, Elite]
    , cdVictoryPoints = Just 0
    , cdUnique = True
    }

thorneOpenToNegotiation :: CardDef
thorneOpenToNegotiation =
  (enemy "09757" ("Thorne" <:> "Open to Negotiation") RedCoterie 1)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 4
    , cdEvade = evade 4
    , cdHealth = health 4
    , cdCardTraits = setFromList [Humanoid, Coterie, Elite]
    , cdKeywords = setFromList [Keyword.Hunter]
    , cdVictoryPoints = Just 0
    , cdUnique = True
    }

desiderioDelgadoAlvarezRedInHisLedger :: CardDef
desiderioDelgadoAlvarezRedInHisLedger =
  (enemy "09758" ("Desiderio Delgado Alvarez" <:> "Red in His Ledger") RedCoterie 1)
    { cdHealthDamage = healthDamage 2
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 4
    , cdEvade = evade 3
    , cdHealth = health 5
    , cdCardTraits = setFromList [Humanoid, Coterie, Elite]
    , cdKeywords =
        setFromList [Keyword.Patrol (LocationWithEnemy (EnemyWithTrait Coterie <> not_ (EnemyIs "09758")))]
    , cdVictoryPoints = Just 0
    , cdUnique = True
    }

amaranthScarletScorn :: CardDef
amaranthScarletScorn =
  (enemy "09759" ("Amaranth" <:> "Scarlet Scorn") RedCoterie 1)
    { cdHealthDamage = healthDamage 2
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 4
    , cdEvade = evade 4
    , cdHealth = health 3
    , cdCardTraits = setFromList [Humanoid, Coterie, Elite]
    , cdKeywords = setFromList [Keyword.Hunter]
    , cdVictoryPoints = Just 0
    , cdUnique = True
    }

tzuSanNiangAWhisperInYourEar :: CardDef
tzuSanNiangAWhisperInYourEar =
  (enemy "09760" ("Tzu San Niang" <:> "A Whisper in Your Ear") RedCoterie 1)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 2
    , cdFight = fight 4
    , cdEvade = evade 4
    , cdHealth = health 3
    , cdCardTraits = setFromList [Humanoid, Coterie, Elite]
    , cdKeywords = setFromList [Keyword.Hunter, Keyword.Retaliate]
    , cdVictoryPoints = Just 0
    , cdUnique = True
    }

alikiZoniUperetriaSpeaksInDeath :: CardDef
alikiZoniUperetriaSpeaksInDeath =
  (enemy "09761" ("Aliki Zoni Uperetria" <:> "Speaks in Death") RedCoterie 1)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 2
    , cdEvade = evade 2
    , cdHealth = health 3
    , cdCardTraits = setFromList [Humanoid, Coterie, Elite]
    , cdKeywords = setFromList [Keyword.Aloof]
    , cdVictoryPoints = Just 0
    , cdUnique = True
    }
