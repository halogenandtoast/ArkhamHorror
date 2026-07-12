module Arkham.Asset.Cards.CircusExMortis where

import Arkham.Asset.Cards.Import

-- one_night_only
illusoryLocusCircusExMortis :: CardDef
illusoryLocusCircusExMortis =
  (storyAsset_ "z-circus-ex-mortis-012" "Illusory Locus" CircusExMortisOneNightOnly) {cdCardTraits = setFromList [Ritual]}

-- all_points_west
carrieDykstraCircusExMortis :: CardDef
carrieDykstraCircusExMortis =
  (storyAsset_ "z-circus-ex-mortis-098" ("Carrie Dykstra" <:> "Takes After Her Old Man") CircusExMortisAllPointsWest) {cdCardTraits = setFromList [Ally]}

ralphDykstraCircusExMortis :: CardDef
ralphDykstraCircusExMortis =
  (storyAsset_ "z-circus-ex-mortis-099" ("Ralph Dykstra" <:> "In For the Long Haul") CircusExMortisAllPointsWest) {cdCardTraits = setFromList [Ally]}

-- bacchanalia
cecilSharpeCircusExMortis :: CardDef
cecilSharpeCircusExMortis =
  (storyAsset_ "z-circus-ex-mortis-140" ("Cecil Sharpe" <:> "Keeps His Hands Clean") CircusExMortisBacchanalia)
      { cdCardTraits = setFromList [Socialite, LiberPater]
      , cdUnique = True
      }

estherMeredithCircusExMortis :: CardDef
estherMeredithCircusExMortis =
  (storyAsset_ "z-circus-ex-mortis-141" ("Esther Meredith" <:> "Deals in Gold, Exclusively") CircusExMortisBacchanalia)
      { cdCardTraits = setFromList [Socialite, LiberPater]
      , cdUnique = True
      }

phillipHutchinsCircusExMortis :: CardDef
phillipHutchinsCircusExMortis =
  (storyAsset_ "z-circus-ex-mortis-142" ("Phillip Hutchins" <:> "Woefully Out of Place") CircusExMortisBacchanalia)
      { cdCardTraits = setFromList [Socialite]
      , cdUnique = True
      }

richardStrattonCircusExMortis :: CardDef
richardStrattonCircusExMortis =
  (storyAsset_ "z-circus-ex-mortis-143" ("Richard Stratton" <:> "\"Connoisseur\" of Fine Wines") CircusExMortisBacchanalia)
      { cdCardTraits = setFromList [Socialite, LiberPater]
      , cdUnique = True
      }

veraAshcroftCircusExMortis :: CardDef
veraAshcroftCircusExMortis =
  (storyAsset_ "z-circus-ex-mortis-144" ("Vera Ashcroft" <:> "Recently Widowed, Again") CircusExMortisBacchanalia)
      { cdCardTraits = setFromList [Socialite, LiberPater]
      , cdUnique = True
      }

-- destiny_and_prophecy
amaltheaWeaverCircusFortuneTellerCircusExMortis :: CardDef
amaltheaWeaverCircusFortuneTellerCircusExMortis =
  (storyAsset "z-circus-ex-mortis-228" ("Amalthea Weaver" <:> "Circus Fortune Teller") 2 CircusExMortisDestinyAndProphecy)
      { cdCardTraits = setFromList [Ally, Clairvoyant, Performer]
      , cdSkills = [#wild]
      , cdSlots = [#ally]
      , cdUnique = True
      }

amaltheaWeaverAspirantOfCourageCircusExMortis :: CardDef
amaltheaWeaverAspirantOfCourageCircusExMortis =
  (storyAsset "z-circus-ex-mortis-229" ("Amalthea Weaver" <:> "Aspirant of Courage") 2 CircusExMortisDestinyAndProphecy)
      { cdCardTraits = setFromList [Ally, Clairvoyant, Performer]
      , cdSkills = [#willpower, #wild]
      , cdSlots = [#ally]
      , cdUnique = True
      }

amaltheaWeaverAspirantOfWisdomCircusExMortis :: CardDef
amaltheaWeaverAspirantOfWisdomCircusExMortis =
  (storyAsset "z-circus-ex-mortis-230" ("Amalthea Weaver" <:> "Aspirant of Wisdom") 2 CircusExMortisDestinyAndProphecy)
      { cdCardTraits = setFromList [Ally, Clairvoyant, Performer]
      , cdSkills = [#willpower, #wild]
      , cdSlots = [#ally]
      , cdUnique = True
      }

amaltheaWeaverOracleOfPurityCircusExMortis :: CardDef
amaltheaWeaverOracleOfPurityCircusExMortis =
  (storyAsset "z-circus-ex-mortis-231" ("Amalthea Weaver" <:> "Oracle of Purity") 2 CircusExMortisDestinyAndProphecy)
      { cdCardTraits = setFromList [Ally, Clairvoyant, Performer]
      , cdSkills = [#willpower, #wild, #wild]
      , cdSlots = [#ally]
      , cdUnique = True
      }

amaltheaWeaverOracleOfResolveCircusExMortis :: CardDef
amaltheaWeaverOracleOfResolveCircusExMortis =
  (storyAsset "z-circus-ex-mortis-232" ("Amalthea Weaver" <:> "Oracle of Resolve") 2 CircusExMortisDestinyAndProphecy)
      { cdCardTraits = setFromList [Ally, Clairvoyant, Performer]
      , cdSkills = [#willpower, #wild, #wild]
      , cdSlots = [#ally]
      , cdUnique = True
      }

amaltheaWeaverOracleOfEnlightenmentCircusExMortis :: CardDef
amaltheaWeaverOracleOfEnlightenmentCircusExMortis =
  (storyAsset "z-circus-ex-mortis-233" ("Amalthea Weaver" <:> "Oracle of Enlightenment") 2 CircusExMortisDestinyAndProphecy)
      { cdCardTraits = setFromList [Ally, Clairvoyant, Performer]
      , cdSkills = [#willpower, #wild, #wild]
      , cdSlots = [#ally]
      , cdUnique = True
      }

amaltheaWeaverOracleOfMysteryCircusExMortis :: CardDef
amaltheaWeaverOracleOfMysteryCircusExMortis =
  (storyAsset "z-circus-ex-mortis-234" ("Amalthea Weaver" <:> "Oracle of Mystery") 2 CircusExMortisDestinyAndProphecy)
      { cdCardTraits = setFromList [Ally, Clairvoyant, Performer]
      , cdSkills = [#willpower, #wild, #wild]
      , cdSlots = [#ally]
      , cdUnique = True
      }

deCultusBestiaeForgottenWorkOfApuleiusCircusExMortis :: CardDef
deCultusBestiaeForgottenWorkOfApuleiusCircusExMortis =
  (storyAsset "z-circus-ex-mortis-235" ("De Cultus Bestiae" <:> "Forgotten Work of Apuleius") 2 CircusExMortisDestinyAndProphecy)
      { cdCardTraits = setFromList [Item, Tome, Occult]
      , cdSkills = [#wild]
      , cdSlots = [#hand]
      , cdUnique = True
      }

deCultusBestiaeInterpretationOfConvictionCircusExMortis :: CardDef
deCultusBestiaeInterpretationOfConvictionCircusExMortis =
  (storyAsset "z-circus-ex-mortis-236" ("De Cultus Bestiae" <:> "Interpretation of Conviction") 2 CircusExMortisDestinyAndProphecy)
      { cdCardTraits = setFromList [Item, Tome, Occult]
      , cdSkills = [#intellect, #wild]
      , cdSlots = [#hand]
      , cdUnique = True
      }

deCultusBestiaeInterpretationOfObsessionCircusExMortis :: CardDef
deCultusBestiaeInterpretationOfObsessionCircusExMortis =
  (storyAsset "z-circus-ex-mortis-237" ("De Cultus Bestiae" <:> "Interpretation of Obsession") 2 CircusExMortisDestinyAndProphecy)
      { cdCardTraits = setFromList [Item, Tome, Occult]
      , cdSkills = [#intellect, #wild]
      , cdSlots = [#hand]
      , cdUnique = True
      }

deCultusBestiaeProphecyOfTheBeyondCircusExMortis :: CardDef
deCultusBestiaeProphecyOfTheBeyondCircusExMortis =
  (storyAsset "z-circus-ex-mortis-238" ("De Cultus Bestiae" <:> "Prophecy of the Beyond") 2 CircusExMortisDestinyAndProphecy)
      { cdCardTraits = setFromList [Item, Tome, Occult]
      , cdSkills = [#intellect, #wild, #wild]
      , cdSlots = [#hand]
      , cdUnique = True
      }

deCultusBestiaeProphecyOfTheEternalCircusExMortis :: CardDef
deCultusBestiaeProphecyOfTheEternalCircusExMortis =
  (storyAsset "z-circus-ex-mortis-239" ("De Cultus Bestiae" <:> "Prophecy of the Eternal") 2 CircusExMortisDestinyAndProphecy)
      { cdCardTraits = setFromList [Item, Tome, Occult]
      , cdSkills = [#intellect, #wild, #wild]
      , cdSlots = [#hand]
      , cdUnique = True
      }

deCultusBestiaeProphecyOfTheHordeCircusExMortis :: CardDef
deCultusBestiaeProphecyOfTheHordeCircusExMortis =
  (storyAsset "z-circus-ex-mortis-240" ("De Cultus Bestiae" <:> "Prophecy of the Horde") 2 CircusExMortisDestinyAndProphecy)
      { cdCardTraits = setFromList [Item, Tome, Occult]
      , cdSkills = [#intellect, #wild, #wild]
      , cdSlots = [#hand]
      , cdUnique = True
      }

deCultusBestiaeProphecyOfTheBehemothCircusExMortis :: CardDef
deCultusBestiaeProphecyOfTheBehemothCircusExMortis =
  (storyAsset "z-circus-ex-mortis-241" ("De Cultus Bestiae" <:> "Prophecy of the Behemoth") 2 CircusExMortisDestinyAndProphecy)
      { cdCardTraits = setFromList [Item, Tome, Occult]
      , cdSkills = [#intellect, #wild, #wild]
      , cdSlots = [#hand]
      , cdUnique = True
      }

-- panicked_masses
terrifiedCaptivesCircusExMortis :: CardDef
terrifiedCaptivesCircusExMortis =
  (storyAsset_ "z-circus-ex-mortis-256" "Terrified Captives" CircusExMortisPanickedMasses)
      { cdCardTraits = setFromList [Bystander, Task]
      , cdEncounterSetQuantity = Just 2
      }
