module Arkham.Homebrew.CircusExMortis.CardDefs.Assets where

import Arkham.Asset.Cards.Import
import Arkham.Homebrew.CircusExMortis.Sets qualified as Set

-- one_night_only
illusoryLocus :: CardDef
illusoryLocus =
  (storyAsset_ "z-circus-ex-mortis-012" "Illusory Locus" Set.OneNightOnly) {cdCardTraits = setFromList [Ritual]}

-- all_points_west
carrieDykstra :: CardDef
carrieDykstra =
  (storyAsset_ "z-circus-ex-mortis-098" ("Carrie Dykstra" <:> "Takes After Her Old Man") Set.AllPointsWest) {cdCardTraits = setFromList [Ally]}

ralphDykstra :: CardDef
ralphDykstra =
  (storyAsset_ "z-circus-ex-mortis-099" ("Ralph Dykstra" <:> "In For the Long Haul") Set.AllPointsWest) {cdCardTraits = setFromList [Ally]}

-- bacchanalia
cecilSharpe :: CardDef
cecilSharpe =
  (storyAsset_ "z-circus-ex-mortis-140" ("Cecil Sharpe" <:> "Keeps His Hands Clean") Set.Bacchanalia)
      { cdCardTraits = setFromList [Socialite, LiberPater]
      , cdUnique = True
      }

estherMeredith :: CardDef
estherMeredith =
  (storyAsset_ "z-circus-ex-mortis-141" ("Esther Meredith" <:> "Deals in Gold, Exclusively") Set.Bacchanalia)
      { cdCardTraits = setFromList [Socialite, LiberPater]
      , cdUnique = True
      }

phillipHutchins :: CardDef
phillipHutchins =
  (storyAsset_ "z-circus-ex-mortis-142" ("Phillip Hutchins" <:> "Woefully Out of Place") Set.Bacchanalia)
      { cdCardTraits = setFromList [Socialite]
      , cdUnique = True
      }

richardStratton :: CardDef
richardStratton =
  (storyAsset_ "z-circus-ex-mortis-143" ("Richard Stratton" <:> "\"Connoisseur\" of Fine Wines") Set.Bacchanalia)
      { cdCardTraits = setFromList [Socialite, LiberPater]
      , cdUnique = True
      }

veraAshcroft :: CardDef
veraAshcroft =
  (storyAsset_ "z-circus-ex-mortis-144" ("Vera Ashcroft" <:> "Recently Widowed, Again") Set.Bacchanalia)
      { cdCardTraits = setFromList [Socialite, LiberPater]
      , cdUnique = True
      }

-- destiny_and_prophecy
amaltheaWeaverCircusFortuneTeller :: CardDef
amaltheaWeaverCircusFortuneTeller =
  (storyAsset "z-circus-ex-mortis-228" ("Amalthea Weaver" <:> "Circus Fortune Teller") 2 Set.DestinyAndProphecy)
      { cdCardTraits = setFromList [Ally, Clairvoyant, Performer]
      , cdSkills = [#wild]
      , cdSlots = [#ally]
      , cdUnique = True
      }

amaltheaWeaverAspirantOfCourage :: CardDef
amaltheaWeaverAspirantOfCourage =
  (storyAsset "z-circus-ex-mortis-229" ("Amalthea Weaver" <:> "Aspirant of Courage") 2 Set.DestinyAndProphecy)
      { cdCardTraits = setFromList [Ally, Clairvoyant, Performer]
      , cdSkills = [#willpower, #wild]
      , cdSlots = [#ally]
      , cdUnique = True
      }

amaltheaWeaverAspirantOfWisdom :: CardDef
amaltheaWeaverAspirantOfWisdom =
  (storyAsset "z-circus-ex-mortis-230" ("Amalthea Weaver" <:> "Aspirant of Wisdom") 2 Set.DestinyAndProphecy)
      { cdCardTraits = setFromList [Ally, Clairvoyant, Performer]
      , cdSkills = [#willpower, #wild]
      , cdSlots = [#ally]
      , cdUnique = True
      }

amaltheaWeaverOracleOfPurity :: CardDef
amaltheaWeaverOracleOfPurity =
  (storyAsset "z-circus-ex-mortis-231" ("Amalthea Weaver" <:> "Oracle of Purity") 2 Set.DestinyAndProphecy)
      { cdCardTraits = setFromList [Ally, Clairvoyant, Performer]
      , cdSkills = [#willpower, #wild, #wild]
      , cdSlots = [#ally]
      , cdUnique = True
      }

amaltheaWeaverOracleOfResolve :: CardDef
amaltheaWeaverOracleOfResolve =
  (storyAsset "z-circus-ex-mortis-232" ("Amalthea Weaver" <:> "Oracle of Resolve") 2 Set.DestinyAndProphecy)
      { cdCardTraits = setFromList [Ally, Clairvoyant, Performer]
      , cdSkills = [#willpower, #wild, #wild]
      , cdSlots = [#ally]
      , cdUnique = True
      }

amaltheaWeaverOracleOfEnlightenment :: CardDef
amaltheaWeaverOracleOfEnlightenment =
  (storyAsset "z-circus-ex-mortis-233" ("Amalthea Weaver" <:> "Oracle of Enlightenment") 2 Set.DestinyAndProphecy)
      { cdCardTraits = setFromList [Ally, Clairvoyant, Performer]
      , cdSkills = [#willpower, #wild, #wild]
      , cdSlots = [#ally]
      , cdUnique = True
      }

amaltheaWeaverOracleOfMystery :: CardDef
amaltheaWeaverOracleOfMystery =
  (storyAsset "z-circus-ex-mortis-234" ("Amalthea Weaver" <:> "Oracle of Mystery") 2 Set.DestinyAndProphecy)
      { cdCardTraits = setFromList [Ally, Clairvoyant, Performer]
      , cdSkills = [#willpower, #wild, #wild]
      , cdSlots = [#ally]
      , cdUnique = True
      }

deCultusBestiaeForgottenWorkOfApuleius :: CardDef
deCultusBestiaeForgottenWorkOfApuleius =
  (storyAsset "z-circus-ex-mortis-235" ("De Cultus Bestiae" <:> "Forgotten Work of Apuleius") 2 Set.DestinyAndProphecy)
      { cdCardTraits = setFromList [Item, Tome, Occult]
      , cdSkills = [#wild]
      , cdSlots = [#hand]
      , cdUnique = True
      }

deCultusBestiaeInterpretationOfConviction :: CardDef
deCultusBestiaeInterpretationOfConviction =
  (storyAsset "z-circus-ex-mortis-236" ("De Cultus Bestiae" <:> "Interpretation of Conviction") 2 Set.DestinyAndProphecy)
      { cdCardTraits = setFromList [Item, Tome, Occult]
      , cdSkills = [#intellect, #wild]
      , cdSlots = [#hand]
      , cdUnique = True
      }

deCultusBestiaeInterpretationOfObsession :: CardDef
deCultusBestiaeInterpretationOfObsession =
  (storyAsset "z-circus-ex-mortis-237" ("De Cultus Bestiae" <:> "Interpretation of Obsession") 2 Set.DestinyAndProphecy)
      { cdCardTraits = setFromList [Item, Tome, Occult]
      , cdSkills = [#intellect, #wild]
      , cdSlots = [#hand]
      , cdUnique = True
      }

deCultusBestiaeProphecyOfTheBeyond :: CardDef
deCultusBestiaeProphecyOfTheBeyond =
  (storyAsset "z-circus-ex-mortis-238" ("De Cultus Bestiae" <:> "Prophecy of the Beyond") 2 Set.DestinyAndProphecy)
      { cdCardTraits = setFromList [Item, Tome, Occult]
      , cdSkills = [#intellect, #wild, #wild]
      , cdSlots = [#hand]
      , cdUnique = True
      }

deCultusBestiaeProphecyOfTheEternal :: CardDef
deCultusBestiaeProphecyOfTheEternal =
  (storyAsset "z-circus-ex-mortis-239" ("De Cultus Bestiae" <:> "Prophecy of the Eternal") 2 Set.DestinyAndProphecy)
      { cdCardTraits = setFromList [Item, Tome, Occult]
      , cdSkills = [#intellect, #wild, #wild]
      , cdSlots = [#hand]
      , cdUnique = True
      }

deCultusBestiaeProphecyOfTheHorde :: CardDef
deCultusBestiaeProphecyOfTheHorde =
  (storyAsset "z-circus-ex-mortis-240" ("De Cultus Bestiae" <:> "Prophecy of the Horde") 2 Set.DestinyAndProphecy)
      { cdCardTraits = setFromList [Item, Tome, Occult]
      , cdSkills = [#intellect, #wild, #wild]
      , cdSlots = [#hand]
      , cdUnique = True
      }

deCultusBestiaeProphecyOfTheBehemoth :: CardDef
deCultusBestiaeProphecyOfTheBehemoth =
  (storyAsset "z-circus-ex-mortis-241" ("De Cultus Bestiae" <:> "Prophecy of the Behemoth") 2 Set.DestinyAndProphecy)
      { cdCardTraits = setFromList [Item, Tome, Occult]
      , cdSkills = [#intellect, #wild, #wild]
      , cdSlots = [#hand]
      , cdUnique = True
      }

-- panicked_masses
terrifiedCaptives :: CardDef
terrifiedCaptives =
  (storyAsset_ "z-circus-ex-mortis-256" "Terrified Captives" Set.PanickedMasses)
      { cdCardTraits = setFromList [Bystander, Task]
      , cdEncounterSetQuantity = Just 2
      }
