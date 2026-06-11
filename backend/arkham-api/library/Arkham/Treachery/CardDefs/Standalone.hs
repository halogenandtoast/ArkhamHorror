module Arkham.Treachery.CardDefs.Standalone where

import Arkham.Treachery.CardDefs.Import
import Arkham.Keyword qualified as Keyword
import Arkham.Trait qualified as Trait

selfDestructive :: CardDef
selfDestructive =
  (basicWeakness "60104" "Self-Destructive") {cdCardTraits = singleton Flaw}

thriceDamnedCuriosity :: CardDef
thriceDamnedCuriosity =
  (weakness "60203" "Thrice-Damned Curiosity")
    { cdCardTraits = singleton Flaw
    }

lethalCuriosity :: CardDef
lethalCuriosity =
  (basicWeakness "60254" "Lethal Curiosity")
    { cdCardTraits = singleton Flaw
    }

obsessive :: CardDef
obsessive =
  (basicWeakness "60204" "Obsessive")
    { cdCardTraits = singleton Flaw
    }

darkFuture :: CardDef
darkFuture =
  (weakness "60403" "Dark Future")
    { cdCardTraits = setFromList [Omen, Endtimes]
    }

nihilism :: CardDef
nihilism =
  (basicWeakness "60404" "Nihilism")
    { cdCardTraits = singleton Madness
    }

calledByTheMists :: CardDef
calledByTheMists =
  (weakness "60503" "Called by the Mists")
    { cdCardTraits = setFromList [Curse]
    }

atychiphobia :: CardDef
atychiphobia =
  (basicWeakness "60504" "Atychiphobia")
    { cdCardTraits = setFromList [Madness]
    }

weightOfTheWorld :: CardDef
weightOfTheWorld =
  (weakness "60355" "Weight of the World")
    { cdCardTraits = singleton Terror
    }

cursedSwamp :: CardDef
cursedSwamp =
  (treachery "81024" "Cursed Swamp" TheBayou 3)
    { cdCardTraits = setFromList [Hazard]
    }

spectralMist :: CardDef
spectralMist =
  (treachery "81025" "Spectral Mist" TheBayou 3)
    { cdCardTraits = setFromList [Hazard]
    }

draggedUnder :: CardDef
draggedUnder =
  (treachery "81026" "Dragged Under" TheBayou 4)
    { cdCardTraits = setFromList [Hazard]
    }

ripplesOnTheSurface :: CardDef
ripplesOnTheSurface =
  (treachery "81027" "Ripples on the Surface" TheBayou 3)
    { cdCardTraits = setFromList [Terror]
    }

curseOfTheRougarou :: CardDef
curseOfTheRougarou =
  (weakness "81029" "Curse of the Rougarou")
    { cdCardTraits = setFromList [Curse]
    , cdEncounterSet = Just CurseOfTheRougarou
    , cdEncounterSetQuantity = Just 1
    }

onTheProwl :: CardDef
onTheProwl =
  (treachery "81034" "On the Prowl" CurseOfTheRougarou 5)
    { cdKeywords = setFromList [Keyword.Surge]
    }

beastOfTheBayou :: CardDef
beastOfTheBayou = treachery "81035" "Beast of the Bayou" CurseOfTheRougarou 2

insatiableBloodlust :: CardDef
insatiableBloodlust =
  treachery "81036" "Insatiable Bloodlust" CurseOfTheRougarou 3

massHysteria :: CardDef
massHysteria =
  (treachery "82031" "Mass Hysteria" CarnevaleOfHorrors 3)
    { cdCardTraits = singleton Hazard
    , cdKeywords = singleton Keyword.Peril
    }

lostInVenice :: CardDef
lostInVenice =
  (treachery "82032" "Lost in Venice" CarnevaleOfHorrors 3)
    { cdCardTraits = singleton Blunder
    , cdKeywords = singleton Keyword.Peril
    }

watchersGaze :: CardDef
watchersGaze =
  (treachery "82033" "Watchers' Gaze" CarnevaleOfHorrors 3)
    { cdCardTraits = singleton Terror
    }

chaosInTheWater :: CardDef
chaosInTheWater =
  (treachery "82034" "Chaos in the Water" CarnevaleOfHorrors 3)
    { cdCardTraits = singleton Hazard
    }

mesmerize :: CardDef
mesmerize =
  (treachery "82035" "Mesmerize" CarnevaleOfHorrors 2)
    { cdCardTraits = singleton Hex
    }

abduction :: CardDef
abduction =
  (treachery "82036" "Abduction" CarnevaleOfHorrors 2)
    { cdCardTraits = singleton Scheme
    }

acridMiasma :: CardDef
acridMiasma =
  (treachery "82037" "Acrid Miasma" CarnevaleOfHorrors 2)
    { cdCardTraits = singleton Hazard
    }

whatHaveYouDone :: CardDef
whatHaveYouDone =
  (weakness "84007" "What Have You Done?")
    { cdCardTraits = singleton Madness
    , cdEncounterSet = Just MurderAtTheExcelsiorHotel
    , cdEncounterSetQuantity = Just 1
    }

noxiousFumes :: CardDef
noxiousFumes =
  (treachery "84023" "Noxious Fumes" MurderAtTheExcelsiorHotel 2)
    { cdCardTraits = singleton Hazard
    }

drivenToMadness :: CardDef
drivenToMadness =
  (treachery "84024" "Driven to Madness" MurderAtTheExcelsiorHotel 3)
    { cdCardTraits = singleton Curse
    }

bloodOnYourHands :: CardDef
bloodOnYourHands =
  (treachery "84025" "Blood on Your Hands" MurderAtTheExcelsiorHotel 4)
    { cdCardTraits = singleton Terror
    }

incriminatingEvidence :: CardDef
incriminatingEvidence =
  (treachery "84026" "Incriminating Evidence" MurderAtTheExcelsiorHotel 2)
    { cdCardTraits = singleton Evidence
    }

violentOutburst :: CardDef
violentOutburst =
  (treachery "84027" "Violent Outburst" MurderAtTheExcelsiorHotel 3)
    { cdCardTraits = singleton Curse
    }

encephalonSignal :: CardDef
encephalonSignal =
  (treachery "84030" "Encephalon Signal" AlienInterference 3)
    { cdCardTraits = singleton Hazard
    , cdKeywords = setFromList [Keyword.Peril]
    }

harvestedBrain :: CardDef
harvestedBrain =
  (treachery "84038" "Harvested Brain" VileExperiments 1)
    { cdCardTraits = setFromList [Ancient, Science]
    , cdRevelation = NoRevelation
    }

morbidAwareness :: CardDef
morbidAwareness =
  (treachery "84039" "Morbid Awareness" VileExperiments 3)
    { cdCardTraits = singleton Hazard
    }

chillingPresence :: CardDef
chillingPresence =
  (treachery "84042" "Chilling Presence" SinsOfThePast 3)
    { cdCardTraits = singleton Terror
    }

inconvenientQuesitoningA :: CardDef
inconvenientQuesitoningA =
  (treachery "88038a" "Inconvenient Questioning" FortuneAndFolly 1)
    { cdCardTraits = singleton Scheme
    , cdMeta = mapFromList [("suit", String "spades"), ("value", String "nine")]
    }

inconvenientQuesitoningB :: CardDef
inconvenientQuesitoningB =
  (treachery "88038b" "Inconvenient Questioning" FortuneAndFolly 1)
    { cdCardTraits = singleton Scheme
    , cdMeta = mapFromList [("suit", String "diamonds"), ("value", String "nine")]
    }

inconvenientQuesitoningC :: CardDef
inconvenientQuesitoningC =
  (treachery "88038c" "Inconvenient Questioning" FortuneAndFolly 1)
    { cdCardTraits = singleton Scheme
    , cdMeta = mapFromList [("suit", String "spades"), ("value", String "eight")]
    }

inconvenientQuesitoningD :: CardDef
inconvenientQuesitoningD =
  (treachery "88038d" "Inconvenient Questioning" FortuneAndFolly 1)
    { cdCardTraits = singleton Scheme
    , cdMeta = mapFromList [("suit", String "diamonds"), ("value", String "eight")]
    }

arcaneSpotlightA :: CardDef
arcaneSpotlightA =
  (treachery "88039a" "Arcane Spotlight" FortuneAndFolly 1)
    { cdCardTraits = setFromList [Hex, Obstacle]
    , cdMeta = mapFromList [("suit", String "spades"), ("value", String "queen")]
    }

arcaneSpotlightB :: CardDef
arcaneSpotlightB =
  (treachery "88039b" "Arcane Spotlight" FortuneAndFolly 1)
    { cdCardTraits = setFromList [Hex, Obstacle]
    , cdMeta = mapFromList [("suit", String "spades"), ("value", String "jack")]
    }

arcaneSpotlightC :: CardDef
arcaneSpotlightC =
  (treachery "88039c" "Arcane Spotlight" FortuneAndFolly 1)
    { cdCardTraits = setFromList [Hex, Obstacle]
    , cdMeta = mapFromList [("suit", String "spades"), ("value", String "ten")]
    }

avariceCallsA :: CardDef
avariceCallsA =
  peril
    $ (treachery "88040a" "Avarice Calls" FortuneAndFolly 1)
      { cdCardTraits = singleton Curse
      , cdMeta = mapFromList [("suit", String "diamonds"), ("value", String "queen")]
      }

avariceCallsB :: CardDef
avariceCallsB =
  peril
    $ (treachery "88040b" "Avarice Calls" FortuneAndFolly 1)
      { cdCardTraits = singleton Curse
      , cdMeta = mapFromList [("suit", String "diamonds"), ("value", String "jack")]
      }

avariceCallsC :: CardDef
avariceCallsC =
  peril
    $ (treachery "88040c" "Avarice Calls" FortuneAndFolly 1)
      { cdCardTraits = singleton Curse
      , cdMeta = mapFromList [("suit", String "diamonds"), ("value", String "ten")]
      }

suspiciousGazeA :: CardDef
suspiciousGazeA =
  (treachery "88041a" "Suspicious Gaze" FortuneAndFolly 1)
    { cdCardTraits = singleton Scheme
    , cdMeta = mapFromList [("suit", String "clubs"), ("value", String "eight")]
    }

suspiciousGazeB :: CardDef
suspiciousGazeB =
  (treachery "88041b" "Suspicious Gaze" FortuneAndFolly 1)
    { cdCardTraits = singleton Scheme
    , cdMeta = mapFromList [("suit", String "spades"), ("value", String "seven")]
    }

suspiciousGazeC :: CardDef
suspiciousGazeC =
  (treachery "88041c" "Suspicious Gaze" FortuneAndFolly 1)
    { cdCardTraits = singleton Scheme
    , cdMeta = mapFromList [("suit", String "clubs"), ("value", String "seven")]
    }

obsessedGamblerA :: CardDef
obsessedGamblerA =
  (treachery "88042a" "Obsessed Gambler" FortuneAndFolly 1)
    { cdCardTraits = singleton Terror
    , cdMeta = mapFromList [("suit", String "hearts"), ("value", String "eight")]
    }

obsessedGamblerB :: CardDef
obsessedGamblerB =
  (treachery "88042b" "Obsessed Gambler" FortuneAndFolly 1)
    { cdCardTraits = singleton Terror
    , cdMeta = mapFromList [("suit", String "diamonds"), ("value", String "seven")]
    }

obsessedGamblerC :: CardDef
obsessedGamblerC =
  (treachery "88042c" "Obsessed Gambler" FortuneAndFolly 1)
    { cdCardTraits = singleton Terror
    , cdMeta = mapFromList [("suit", String "hearts"), ("value", String "seven")]
    }

dimensionalHypnosisA :: CardDef
dimensionalHypnosisA =
  (treachery "88051a" "Dimensional Hypnosis" PlanInShambles 1)
    { cdCardTraits = singleton Terror
    , cdMeta = mapFromList [("suit", String "spades"), ("value", String "five")]
    }

dimensionalHypnosisB :: CardDef
dimensionalHypnosisB =
  (treachery "88051b" "Dimensional Hypnosis" PlanInShambles 1)
    { cdCardTraits = singleton Terror
    , cdMeta = mapFromList [("suit", String "clubs"), ("value", String "five")]
    }

dimensionalHypnosisC :: CardDef
dimensionalHypnosisC =
  (treachery "88051c" "Dimensional Hypnosis" PlanInShambles 1)
    { cdCardTraits = singleton Terror
    , cdMeta = mapFromList [("suit", String "spades"), ("value", String "four")]
    }

gripOfTheBeyondA :: CardDef
gripOfTheBeyondA =
  (treachery "88052a" "Grip of the Beyond" PlanInShambles 1)
    { cdCardTraits = singleton Hazard
    , cdMeta = mapFromList [("suit", String "diamonds"), ("value", String "five")]
    }

gripOfTheBeyondB :: CardDef
gripOfTheBeyondB =
  (treachery "88052b" "Grip of the Beyond" PlanInShambles 1)
    { cdCardTraits = singleton Hazard
    , cdMeta = mapFromList [("suit", String "hearts"), ("value", String "five")]
    }

gripOfTheBeyondC :: CardDef
gripOfTheBeyondC =
  (treachery "88052c" "Grip of the Beyond" PlanInShambles 1)
    { cdCardTraits = singleton Hazard
    , cdMeta = mapFromList [("suit", String "diamonds"), ("value", String "four")]
    }

huntersHungerA :: CardDef
huntersHungerA =
  (treachery "88053a" "Hunter's Hunger" PlanInShambles 1)
    { cdCardTraits = singleton Hazard
    , cdMeta = mapFromList [("suit", String "clubs"), ("value", String "four")]
    }

huntersHungerB :: CardDef
huntersHungerB =
  (treachery "88053b" "Hunter's Hunger" PlanInShambles 1)
    { cdCardTraits = singleton Hazard
    , cdMeta = mapFromList [("suit", String "hearts"), ("value", String "four")]
    }

realityAcid5U21 :: CardDef
realityAcid5U21 =
  (weakness "89004" "Reality Acid")
    { cdCardTraits = setFromList [Power]
    }

hospitalDebtsAdvanced :: CardDef
hospitalDebtsAdvanced =
  (weakness "90010" "Hospital Debts")
    { cdCardTraits = setFromList [Task]
    , cdKeywords = singleton Keyword.Advanced
    }

coverUpAdvanced :: CardDef
coverUpAdvanced =
  (weakness "90031" "Cover Up")
    { cdCardTraits = setFromList [Task]
    , cdKeywords = singleton Keyword.Advanced
    }

abandonedAndAloneAdvanced :: CardDef
abandonedAndAloneAdvanced =
  (weakness "90040" "Abandoned and Alone")
    { cdCardTraits = setFromList [Madness]
    , cdKeywords = singleton Keyword.Advanced
    }

hardTimes :: CardDef
hardTimes =
  (weakness "90048" "Hard Times")
    { cdCardTraits = setFromList [Hardship]
    , cdKeywords = singleton Keyword.Replacement
    }

finalRhapsodyAdvanced :: CardDef
finalRhapsodyAdvanced =
  (weakness "90051" "Final Rhapsody")
    { cdCardTraits = setFromList [Endtimes]
    , cdKeywords = singleton Keyword.Advanced
    }

smiteTheWickedAdvanced :: CardDef
smiteTheWickedAdvanced =
  (weakness "90061" "Smite the Wicked")
    { cdCardTraits = setFromList [Task]
    , cdKeywords = singleton Keyword.Advanced
    }

buriedSecretsAdvanced :: CardDef
buriedSecretsAdvanced =
  (weakness "90064" "Buried Secrets")
    { cdCardTraits = setFromList [Mystery]
    , cdKeywords = singleton Keyword.Advanced
    }

rexsCurseAdvanced :: CardDef
rexsCurseAdvanced =
  (weakness "90080" "Rex's Curse")
    { cdCardTraits = setFromList [Curse]
    , cdKeywords = singleton Keyword.Advanced
    }

searchingForIzzieAdvanced :: CardDef
searchingForIzzieAdvanced =
  (weakness "90086" "Searching for Izzie")
    { cdCardTraits = setFromList [Task]
    , cdKeywords = singleton Keyword.Advanced
    }

unaware :: CardDef
unaware =
  (basicWeakness "60356" "Unaware")
    { cdCardTraits = singleton Flaw
    }

looseCannon :: CardDef
looseCannon =
  (weakness "60153" "Loose Cannon")
    { cdCardTraits = singleton Flaw
    , cdDeckRestrictions = [Signature ("60151" :: InvestigatorId)]
    , cdLevel = Nothing
    }

overconfident :: CardDef
overconfident =
  (basicWeakness "60154" "Overconfident")
    { cdCardTraits = singleton Flaw
    }

unbrokenWeb :: CardDef
unbrokenWeb =
  (weakness "60253" "Unbroken Web")
    { cdCardTraits = setFromList [Terror, Trait.Dreamlands]
    , cdDeckRestrictions = [Signature ("60251" :: InvestigatorId)]
    , cdLevel = Nothing
    }

calledToGuinee :: CardDef
calledToGuinee =
  (weakness "60453" "Called to Guin\233e")
    { cdCardTraits = setFromList [Curse, Pact]
    , cdDeckRestrictions = [Signature ("60451" :: InvestigatorId)]
    , cdLevel = Nothing
    }

hemophobia :: CardDef
hemophobia =
  (basicWeakness "60454" "Hemophobia")
    { cdCardTraits = singleton Terror
    }

confusion :: CardDef
confusion =
  surge
    $ (treachery "71026" "Confusion" TheMidwinterGala 1)
      { cdCardTraits = setFromList [Blunder, Rival]
      , cdRevelation = CannotBeCanceledRevelation
      }

coldStreak :: CardDef
coldStreak =
  surge
    $ (treachery "71032" "Cold Streak" TheMidwinterGala 1)
      { cdCardTraits = setFromList [Misfortune, Rival]
      , cdRevelation = CannotBeCanceledRevelation
      }

wardOfPreservation :: CardDef
wardOfPreservation =
  surge
    $ (treachery "71038" "Ward of Preservation" TheMidwinterGala 1)
      { cdCardTraits = setFromList [Spell, Rival]
      , cdRevelation = CannotBeCanceledRevelation
      }

unlucky :: CardDef
unlucky =
  surge
    $ (treachery "71044" "Unlucky" TheMidwinterGala 1)
      { cdCardTraits = setFromList [Misfortune, Rival]
      , cdRevelation = CannotBeCanceledRevelation
      }

viciousAmbush :: CardDef
viciousAmbush =
  (treachery "71050" "Vicious Ambush" TheMidwinterGala 2)
    { cdCardTraits = singleton Scheme
    }

bleedingWallsTheMidwinterGala :: CardDef
bleedingWallsTheMidwinterGala =
  (treachery "71054" "Bleeding Walls" TheMidwinterGala 2)
    { cdCardTraits = singleton Terror
    }

entrap :: CardDef
entrap =
  (treachery "71055" "Entrap" TheMidwinterGala 3)
    { cdCardTraits = singleton Hazard
    }

inexplicableColdTheMidwinterGala :: CardDef
inexplicableColdTheMidwinterGala =
  (treachery "71056" "Inexplicable Cold" TheMidwinterGala 1)
    { cdCardTraits = singleton Hazard
    }

mindExtraction :: CardDef
mindExtraction =
  (treachery "71057" "Mind Extraction" TheMidwinterGala 3)
    { cdCardTraits = singleton Power
    }

noxiousFumesTheMidwinterGala :: CardDef
noxiousFumesTheMidwinterGala =
  (treachery "71058" "Noxious Fumes" TheMidwinterGala 1)
    { cdCardTraits = singleton Hazard
    }

pushedIntoTheBeyondTheMidwinterGala :: CardDef
pushedIntoTheBeyondTheMidwinterGala =
  (treachery "71059" "Pushed into the Beyond" TheMidwinterGala 1)
    { cdCardTraits = singleton Hex
    }

secretDoorTheMidwinterGala :: CardDef
secretDoorTheMidwinterGala =
  (treachery "71060" "Secret Door" TheMidwinterGala 2)
    { cdCardTraits = singleton Obstacle
    }

terrorGate :: CardDef
terrorGate =
  (treachery "71061" "Terror Gate" TheMidwinterGala 4)
    { cdCardTraits = singleton Terror
    }

violentCommandsTheMidwinterGala :: CardDef
violentCommandsTheMidwinterGala =
  (treachery "71062" "Violent Commands" TheMidwinterGala 2)
    { cdCardTraits = singleton Terror
    }

flipTheScript :: CardDef
flipTheScript =
  (treachery "72012" "Flip the Script" FilmFatale 2)
    { cdCardTraits = singleton Paradox
    }

foundFootage :: CardDef
foundFootage =
  (treachery "72013" "Found Footage" FilmFatale 2)
    { cdCardTraits = singleton Terror
    }

action :: CardDef
action =
  peril
    $ (treachery "72014" "Action!" FilmFatale 2)
      { cdCardTraits = singleton Terror
      }

breakALeg :: CardDef
breakALeg =
  (treachery "72015" "\"Break a Leg!\"" FilmFatale 2)
    { cdCardTraits = singleton Hazard
    }

bleedingReality :: CardDef
bleedingReality =
  (treachery "72016" "Bleeding Reality" FilmFatale 3)
    { cdCardTraits = setFromList [Power, Extradimensional]
    }

lastLooks :: CardDef
lastLooks =
  (treachery "72017" "Last Looks" FilmFatale 2)
    { cdCardTraits = singleton Terror
    }

creatureFeature :: CardDef
creatureFeature =
  (treachery "72018" "Creature Feature" FilmFatale 2)
    { cdCardTraits = singleton Terror
    }

celestialShower :: CardDef
celestialShower =
  (treachery "72035" "Celestial Shower" CosmicJourney 2)
    { cdCardTraits = singleton Hazard
    }

primordialTerror :: CardDef
primordialTerror =
  (treachery "72046" "Primordial Terror" ForgottenIsland 2)
    { cdCardTraits = singleton Terror
    }

unexpectedTransformation :: CardDef
unexpectedTransformation =
  (treachery "72047" "Unexpected Transformation" ForgottenIsland 2)
    { cdCardTraits = singleton Power
    }

hellfire :: CardDef
hellfire =
  (treachery "72058" "Hellfire" AbominableContessa 2)
    { cdCardTraits = singleton Power
    }

vampiresKiss :: CardDef
vampiresKiss =
  (treachery "72059" "Vampire's Kiss" AbominableContessa 2)
    { cdCardTraits = singleton Scheme
    }

-- The Blob That Ate Everything

realityAcid :: CardDef
realityAcid =
  (treachery "85044" "Reality Acid" TheBlobThatAteEverything 4)
    { cdCardTraits = singleton Power
    }

devouringOoze :: CardDef
devouringOoze =
  (treachery "85045" "Devouring Ooze" TheBlobThatAteEverything 2)
    { cdCardTraits = singleton Attack
    }

corrosiveSlime :: CardDef
corrosiveSlime =
  (treachery "85046" "Corrosive Slime" TheBlobThatAteEverything 2)
    { cdCardTraits = singleton Attack
    }

consumingMaw :: CardDef
consumingMaw =
  (treachery "85047" "Consuming Maw" TheBlobThatAteEverything 3)
    { cdCardTraits = singleton Attack
    }

waveOfOoze :: CardDef
waveOfOoze =
  (treachery "85048" "Wave of Ooze" TheBlobThatAteEverything 3)
    { cdCardTraits = singleton Attack
    }

causticDissemination :: CardDef
causticDissemination =
  (treachery "85049" "Caustic Dissemination" TheBlobThatAteEverything 2)
    { cdCardTraits = setFromList [Ooze, Hazard]
    }

stickyFeet :: CardDef
stickyFeet =
  (treachery "85050" "Sticky Feet" TheBlobThatAteEverything 2)
    { cdCardTraits = setFromList [Ooze, Obstacle]
    }

replication :: CardDef
replication =
  (treachery "85051" "Replication" TheBlobThatAteEverything 2)
    { cdCardTraits = singleton Power
    }

itsGotMe :: CardDef
itsGotMe =
  (treachery "85052" "\"It's got me!\"" TheBlobThatAteEverything 2)
    { cdCardTraits = singleton Hazard
    }

alienFoodChain :: CardDef
alienFoodChain =
  (treachery "85053" "Alien Food Chain" TheBlobThatAteEverything 2)
    { cdCardTraits = setFromList [Ooze, Power]
    }

eclipse :: CardDef
eclipse =
  (treachery "83047" "Eclipse" SandsOfEgypt 3)
    { cdCardTraits = singleton Trait.Power
    }

sandstorm :: CardDef
sandstorm =
  (treachery "83048" "Sandstorm" SandsOfEgypt 3)
    { cdCardTraits = singleton Hazard
    }

terrorUnderThePyramids :: CardDef
terrorUnderThePyramids =
  (treachery "83049" "Terror Under the Pyramids" SandsOfEgypt 3)
    { cdCardTraits = singleton Scheme
    }

swarmOfLocusts :: CardDef
swarmOfLocusts =
  (treachery "83050" "Swarm of Locusts" SandsOfEgypt 3)
    { cdCardTraits = singleton Trait.Power
    }

slumber :: CardDef
slumber =
  (treachery "83051" "Slumber" SandsOfEgypt 2)
    { cdCardTraits = setFromList [Trait.Curse, Trait.Abyss]
    }

darkSacrifice :: CardDef
darkSacrifice =
  (treachery "83052" "Dark Sacrifice" SandsOfEgypt 2)
    { cdCardTraits = setFromList [Trait.Curse, Trait.Abyss]
    }

theBlackWind :: CardDef
theBlackWind =
  (treachery "83053" "The Black Wind" SandsOfEgypt 2)
    { cdCardTraits = singleton Trait.Power
    , cdKeywords = singleton Keyword.Peril
    , cdVictoryPoints = Just 1
    }

abyssalReach :: CardDef
abyssalReach =
  (treachery "83054" "Abyssal Reach" SandsOfEgypt 3)
    { cdCardTraits = setFromList [Trait.Curse, Trait.Abyss]
    }

deathAndDecay :: CardDef
deathAndDecay =
  (treachery "86026" "Death and Decay" WarOfTheOuterGods 2)
    { cdCardTraits = singleton Hex
    }

predatorsCall :: CardDef
predatorsCall =
  (treachery "86028" "Predator's Call" WarOfTheOuterGods 2)
    { cdCardTraits = singleton Scheme
    }

feastOfLocusts :: CardDef
feastOfLocusts =
  (treachery "86030" "Feast of Locusts" WarOfTheOuterGods 2)
    { cdCardTraits = singleton Hazard
    }

hellfireWarOfTheOuterGods :: CardDef
hellfireWarOfTheOuterGods =
  (treachery "86031" "Hellfire" WarOfTheOuterGods 3)
    { cdCardTraits = singleton Hazard
    }

ravagesOfWar :: CardDef
ravagesOfWar =
  (treachery "86032" "Ravages of War" WarOfTheOuterGods 2)
    { cdCardTraits = singleton Terror
    }

whileTheySleep :: CardDef
whileTheySleep =
  (treachery "86033" "While They Sleep" WarOfTheOuterGods 2)
    { cdCardTraits = singleton Omen
    }

inevitableEnd :: CardDef
inevitableEnd =
  (treachery "86039" "Inevitable End" DeathOfStars 3)
    { cdCardTraits = singleton Hex
    }

huntDown :: CardDef
huntDown =
  (treachery "86045" "Hunt Down" ChildrenOfParadise 3)
    { cdCardTraits = singleton Hazard
    }

transmogrify :: CardDef
transmogrify =
  (treachery "86050" "Transmogrify" SwarmOfAssimilation 2)
    { cdCardTraits = singleton Curse
    }
