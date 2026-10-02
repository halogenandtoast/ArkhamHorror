module Arkham.Homebrew.CircusExMortis.CardDefs.Assets where

import Arkham.Asset.Cards.Import
import Arkham.Card.CardCode (CardCode)
import Arkham.Homebrew.CircusExMortis.Sets qualified as Set
import Arkham.Homebrew.CircusExMortis.Traits
import Arkham.SkillType (SkillIcon)

-- one_night_only
illusoryLocus :: CardDef
illusoryLocus =
  (encounterAsset_ ":circus-ex-mortis:012" "Illusory Locus" Set.OneNightOnly)
    { cdCardTraits = setFromList [Ritual]
    }

-- all_points_west
carrieDykstra :: CardDef
carrieDykstra =
  ( encounterAsset_
      ":circus-ex-mortis:096"
      ("Carrie Dykstra" <:> "Takes After Her Old Man")
      Set.AllPointsWest
  )
    { cdCardTraits = setFromList [Ally]
    }

ralphDykstra :: CardDef
ralphDykstra =
  ( encounterAsset_
      ":circus-ex-mortis:097"
      ("Ralph Dykstra" <:> "In For the Long Haul")
      Set.AllPointsWest
  )
    { cdCardTraits = setFromList [Ally]
    }

-- bacchanalia

{- | The five Bacchanalia Socialites share a printing; only Phillip Hutchins, who is
"woefully out of place", is not also a Liber Pater.
-}
socialite :: CardCode -> Name -> [Trait] -> CardDef
socialite code title extraTraits =
  (encounterAsset_ code title Set.Bacchanalia)
    { cdCardTraits = setFromList (Socialite : extraTraits)
    , cdUnique = True
    }

cecilSharpe :: CardDef
cecilSharpe =
  socialite ":circus-ex-mortis:138" ("Cecil Sharpe" <:> "Keeps His Hands Clean") [LiberPater]

estherMeredith :: CardDef
estherMeredith =
  socialite ":circus-ex-mortis:139" ("Esther Meredith" <:> "Deals in Gold, Exclusively") [LiberPater]

phillipHutchins :: CardDef
phillipHutchins =
  socialite ":circus-ex-mortis:140" ("Phillip Hutchins" <:> "Woefully Out of Place") []

richardStratton :: CardDef
richardStratton =
  socialite
    ":circus-ex-mortis:141"
    ("Richard Stratton" <:> "\"Connoisseur\" of Fine Wines")
    [LiberPater]

veraAshcroft :: CardDef
veraAshcroft =
  socialite ":circus-ex-mortis:142" ("Vera Ashcroft" <:> "Recently Widowed, Again") [LiberPater]

-- destiny_and_prophecy

{- | Amalthea Weaver's seven printings and De Cultus Bestiae's seven are each the same
card at a later point in the campaign: only the subtitle and the skill icons change.
-}
amaltheaWeaver :: CardCode -> Text -> [SkillIcon] -> CardDef
amaltheaWeaver code subtitle icons =
  (storyAsset code ("Amalthea Weaver" <:> subtitle) 2 Set.DestinyAndProphecy)
    { cdCardTraits = setFromList [Ally, Clairvoyant, Performer]
    , cdSkills = icons
    , cdSlots = [#ally]
    , cdUnique = True
    }

deCultusBestiae :: CardCode -> Text -> [SkillIcon] -> CardDef
deCultusBestiae code subtitle icons =
  (storyAsset code ("De Cultus Bestiae" <:> subtitle) 2 Set.DestinyAndProphecy)
    { cdCardTraits = setFromList [Item, Tome, Occult]
    , cdSkills = icons
    , cdSlots = [#hand]
    , cdUnique = True
    }

amaltheaWeaverCircusFortuneTeller :: CardDef
amaltheaWeaverCircusFortuneTeller =
  amaltheaWeaver ":circus-ex-mortis:228" "Circus Fortune Teller" [#wild]

amaltheaWeaverAspirantOfCourage :: CardDef
amaltheaWeaverAspirantOfCourage =
  amaltheaWeaver ":circus-ex-mortis:229" "Aspirant of Courage" [#willpower, #wild]

amaltheaWeaverAspirantOfWisdom :: CardDef
amaltheaWeaverAspirantOfWisdom =
  amaltheaWeaver ":circus-ex-mortis:230" "Aspirant of Wisdom" [#willpower, #wild]

amaltheaWeaverOracleOfPurity :: CardDef
amaltheaWeaverOracleOfPurity =
  amaltheaWeaver ":circus-ex-mortis:231" "Oracle of Purity" [#willpower, #wild, #wild]

amaltheaWeaverOracleOfResolve :: CardDef
amaltheaWeaverOracleOfResolve =
  amaltheaWeaver ":circus-ex-mortis:232" "Oracle of Resolve" [#willpower, #wild, #wild]

amaltheaWeaverOracleOfEnlightenment :: CardDef
amaltheaWeaverOracleOfEnlightenment =
  amaltheaWeaver ":circus-ex-mortis:233" "Oracle of Enlightenment" [#willpower, #wild, #wild]

amaltheaWeaverOracleOfMystery :: CardDef
amaltheaWeaverOracleOfMystery =
  amaltheaWeaver ":circus-ex-mortis:234" "Oracle of Mystery" [#willpower, #wild, #wild]

deCultusBestiaeForgottenWorkOfApuleius :: CardDef
deCultusBestiaeForgottenWorkOfApuleius =
  deCultusBestiae ":circus-ex-mortis:235" "Forgotten Work of Apuleius" [#wild]

deCultusBestiaeInterpretationOfConviction :: CardDef
deCultusBestiaeInterpretationOfConviction =
  deCultusBestiae ":circus-ex-mortis:236" "Interpretation of Conviction" [#intellect, #wild]

deCultusBestiaeInterpretationOfObsession :: CardDef
deCultusBestiaeInterpretationOfObsession =
  deCultusBestiae ":circus-ex-mortis:237" "Interpretation of Obsession" [#intellect, #wild]

deCultusBestiaeProphecyOfTheBeyond :: CardDef
deCultusBestiaeProphecyOfTheBeyond =
  deCultusBestiae ":circus-ex-mortis:238" "Prophecy of the Beyond" [#intellect, #wild, #wild]

deCultusBestiaeProphecyOfTheEternal :: CardDef
deCultusBestiaeProphecyOfTheEternal =
  deCultusBestiae ":circus-ex-mortis:239" "Prophecy of the Eternal" [#intellect, #wild, #wild]

deCultusBestiaeProphecyOfTheHorde :: CardDef
deCultusBestiaeProphecyOfTheHorde =
  deCultusBestiae ":circus-ex-mortis:240" "Prophecy of the Horde" [#intellect, #wild, #wild]

deCultusBestiaeProphecyOfTheBehemoth :: CardDef
deCultusBestiaeProphecyOfTheBehemoth =
  deCultusBestiae ":circus-ex-mortis:241" "Prophecy of the Behemoth" [#intellect, #wild, #wild]

-- panicked_masses
terrifiedCaptives :: CardDef
terrifiedCaptives =
  (encounterAsset_ ":circus-ex-mortis:256" "Terrified Captives" Set.PanickedMasses)
    { cdCardTraits = setFromList [Bystander, Task]
    , cdEncounterSetQuantity = Just 2
    , cdVictoryPoints = Just 1
    , cdRevelation = IsRevelation
    }

-- harm_s_way: the six identical Kidnapped Citizen story backs
kidnappedCitizen_059b :: CardDef
kidnappedCitizen_059b =
  (encounterAsset_ ":circus-ex-mortis:059b" "Kidnapped Citizen" Set.HarmsWay)
    { cdCardTraits = singleton Bystander
    , cdVictoryPoints = Just 1
    , cdOtherSide = Just ":circus-ex-mortis:059"
    , cdDoubleSided = True
    }

kidnappedCitizen_060b :: CardDef
kidnappedCitizen_060b =
  (encounterAsset_ ":circus-ex-mortis:060b" "Kidnapped Citizen" Set.HarmsWay)
    { cdCardTraits = singleton Bystander
    , cdVictoryPoints = Just 1
    , cdOtherSide = Just ":circus-ex-mortis:060"
    , cdDoubleSided = True
    }

kidnappedCitizen_061b :: CardDef
kidnappedCitizen_061b =
  (encounterAsset_ ":circus-ex-mortis:061b" "Kidnapped Citizen" Set.HarmsWay)
    { cdCardTraits = singleton Bystander
    , cdVictoryPoints = Just 1
    , cdOtherSide = Just ":circus-ex-mortis:061"
    , cdDoubleSided = True
    }

kidnappedCitizen_062b :: CardDef
kidnappedCitizen_062b =
  (encounterAsset_ ":circus-ex-mortis:062b" "Kidnapped Citizen" Set.HarmsWay)
    { cdCardTraits = singleton Bystander
    , cdVictoryPoints = Just 1
    , cdOtherSide = Just ":circus-ex-mortis:062"
    , cdDoubleSided = True
    }

kidnappedCitizen_063b :: CardDef
kidnappedCitizen_063b =
  (encounterAsset_ ":circus-ex-mortis:063b" "Kidnapped Citizen" Set.HarmsWay)
    { cdCardTraits = singleton Bystander
    , cdVictoryPoints = Just 1
    , cdOtherSide = Just ":circus-ex-mortis:063"
    , cdDoubleSided = True
    }

kidnappedCitizen_064b :: CardDef
kidnappedCitizen_064b =
  (encounterAsset_ ":circus-ex-mortis:064b" "Kidnapped Citizen" Set.HarmsWay)
    { cdCardTraits = singleton Bystander
    , cdVictoryPoints = Just 1
    , cdOtherSide = Just ":circus-ex-mortis:064"
    , cdDoubleSided = True
    }

{- | thousand_to_one: the two Task assets printed on the backs of the Recite the
Prayer (:207) and Bear the Burden (:208) Destiny stories. 'cdOtherSide' names the
story face rather than 'flippedCardCode' so the pair reads the same way round as the
Kidnapped Citizen backs above; neither carries Victory X, because both cards reach
the victory display by their own printed instruction rather than by being overcome.
-}
dianasBlessing :: CardDef
dianasBlessing =
  (encounterAsset_ ":circus-ex-mortis:207b" "Diana's Blessing" Set.ThousandToOne)
    { cdCardTraits = singleton Task
    , cdOtherSide = Just ":circus-ex-mortis:207"
    , cdDoubleSided = True
    }

darkOfTheMoon :: CardDef
darkOfTheMoon =
  (encounterAsset_ ":circus-ex-mortis:208b" "Dark of the Moon" Set.ThousandToOne)
    { cdCardTraits = singleton Task
    , cdOtherSide = Just ":circus-ex-mortis:208"
    , cdDoubleSided = True
    }

{- | curse_of_the_rougarou: the Circus printings of the side story's two signature
cards (guide p14). The campaign's overlay swaps them in for 81019/81029; each
adds a ☾ release reaction to the printed text. 'cdReplacementCardCode' is what
keeps @assetIs Assets.ladyEsprit@ and friends pointed at the stand-in.
-}
ladyEsprit :: CardDef
ladyEsprit =
  (storyAsset ":circus-ex-mortis:019c" ("Lady Esprit" <:> "Dangerous Bokor") 4 Set.TheBayou)
    { cdSkills = [#willpower, #intellect, #wild]
    , cdCardTraits = setFromList [Ally, Sorcerer]
    , cdUnique = True
    , cdSlots = [#ally]
    , cdReplacementCardCode = Just "81019"
    }
