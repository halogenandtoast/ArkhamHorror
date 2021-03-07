module Arkham.EncounterCard
  ( genEncounterCard
  , lookupEncounterCard
  , baseEncounterCard
  , allEncounterCards
  , placeholderEnemy
  , placeholderTreachery
  )
where

import Arkham.Prelude

import Arkham.Types.Card.CardCode
import Arkham.Types.Card.EncounterCard
import Arkham.Types.Card.Id
import qualified Arkham.Types.Keyword as Keyword
import Arkham.Types.Name
import Arkham.Types.Trait

genEncounterCard :: MonadRandom m => CardCode -> m EncounterCard
genEncounterCard cardCode = lookupEncounterCard cardCode <$> getRandom

lookupEncounterCard :: CardCode -> (CardId -> EncounterCard)
lookupEncounterCard cardCode =
  fromJustNote ("Unknown card: " <> show cardCode)
    $ lookup cardCode allEncounterCards

baseEncounterCard
  :: EncounterCardType -> CardId -> CardCode -> Name -> EncounterCard
baseEncounterCard encounterCardType cardId cardCode name = MkEncounterCard
  { ecCardCode = cardCode
  , ecId = cardId
  , ecName = name
  , ecTraits = mempty
  , ecKeywords = mempty
  , ecCardType = encounterCardType
  , ecVictoryPoints = Nothing
  }

enemy :: CardId -> CardCode -> Name -> EncounterCard
enemy = baseEncounterCard EnemyType

asset :: CardId -> CardCode -> Name -> EncounterCard
asset = baseEncounterCard EncounterAssetType

treachery :: CardId -> CardCode -> Name -> EncounterCard
treachery = baseEncounterCard TreacheryType

location :: CardId -> CardCode -> Name -> EncounterCard
location = baseEncounterCard LocationType


allEncounterCards :: HashMap CardCode (CardId -> EncounterCard)
allEncounterCards = mapFromList
  [ ("enemy", placeholderEnemy)
  , ("treachery", placeholderTreachery)
  , ("01116", ghoulPriest)
  , ("01118", fleshEater)
  , ("01119", icyGhoul)
  , ("01121b", theMaskedHunter)
  , ("01135", huntingShadow)
  , ("01136", falseLead)
  , ("01137", wolfManDrew)
  , ("01138", hermanCollins)
  , ("01139", peterWarren)
  , ("01140", victoriaDevereux)
  , ("01141", ruthTurner)
  , ("01157", umordhoth)
  , ("01158", umordhothsWrath)
  , ("01159", swarmOfRats)
  , ("01160", ghoulMinion)
  , ("01161", ravenousGhoul)
  , ("01162", graspingHands)
  , ("01163", rottingRemains)
  , ("01164", frozenInFear)
  , ("01165", dissonantVoices)
  , ("01166", ancientEvils)
  , ("01167", cryptChill)
  , ("01168", obscuringFog)
  , ("01169", acolyte)
  , ("01170", wizardOfTheOrder)
  , ("01171", mysteriousChanting)
  , ("01172", huntingNightgaunt)
  , ("01173", onWingsOfDarkness)
  , ("01174", lockedDoor)
  , ("01175", screechingByakhee)
  , ("01176", theYellowSign)
  , ("01177", yithianObserver)
  , ("01178", offerOfPower)
  , ("01179", relentlessDarkYoung)
  , ("01180", goatSpawn)
  , ("01181", youngDeepOne)
  , ("01182", dreamsOfRlyeh)
  , ("02058", theExperiment)
  , ("02060", jazzMulligan)
  , ("02078", cloverClubPitBoss)
  , ("02081", somethingInTheDrinks)
  , ("02082", arousingSuspicions)
  , ("02083", visionsOfFuturesPast)
  , ("02084", beyondTheVeil)
  , ("02085", lightOfAforgomon)
  , ("02086", thrall)
  , ("02087", wizardOfYogSothoth)
  , ("02088", unhallowedCountry)
  , ("02089", sordidAndSilent)
  , ("02090", whippoorwill)
  , ("02091", eagerForDeath)
  , ("02092", cursedLuck)
  , ("02093", twistOfFate)
  , ("02094", avianThrall)
  , ("02095", lupineThrall)
  , ("02096", alteredBeast)
  , ("02097", oBannionsThug)
  , ("02098", mobster)
  , ("02099", huntedDown)
  , ("02100", pushedIntoTheBeyond)
  , ("02101", terrorFromBeyond)
  , ("02102", arcaneBarrier)
  , ("02103", conglomerationOfSpheres)
  , ("02104", servantOfTheLurker)
  , ("02141", huntingHorror)
  , ("02142", shadowSpawned)
  , ("02143", stalkedInTheDark)
  , ("02144", passageIntoTheVeil)
  , ("02145", ephemeralExhibits)
  , ("02146", slitheringBehindYou)
  , ("02179", helplessPassenger)
  , ("02180", clawsOfSteam)
  , ("02181", brokenRails)
  , ("02182", grapplingHorror)
  , ("02183", emergentMonstrosity)
  , ("02214", theHiddenChamber)
  , ("02215", keyToTheChamber)
  , ("02216", silasBishop)
  , ("02220", kidnapped)
  , ("02221", psychopompsSong)
  , ("02222", strangeSigns)
  , ("02223", rottingRemainsBloodOnTheAltar)
  , ("02224", servantOfManyMouths)
  , ("02255", broodOfYogSothoth)
  , ("02256", toweringBeasts)
  , ("02257", ruinAndDestruction)
  , ("02258", attractingAttention)
  , ("02259", theCreaturesTracks)
  , ("02293", sethBishop)
  , ("02294", devoteeOfTheKey)
  , ("02295", crazedShoggoth)
  , ("02296", ritesHowled)
  , ("02297", spacesBetween)
  , ("02298", vortexOfTime)
  , ("02323", yogSothoth)
  , ("02324", tearThroughSpace)
  , ("02325", prismaticCascade)
  , ("02326", endlessBridge)
  , ("02327", stepsOfYhagharl)
  , ("02328", dimensionalDoorway)
  , ("02329", interstellarTraveler)
  , ("02330", yithianStarseeker)
  , ("02331", collapsingReality)
  , ("02332", wormhole)
  , ("02333", vastExpanse)
  , ("50022", corpseHungryGhoul)
  , ("50023", ghoulFromTheDepths)
  , ("50024", theZealotsSeal)
  , ("50026b", narogath)
  , ("50031", maskedHorrors)
  , ("50032b", vaultOfEarthlyDemise)
  , ("50037", umordhothsHunger)
  , ("50038", graveEater)
  , ("50039", acolyteOfUmordhoth)
  , ("50040", chillFromBelow)
  , ("50041", discipleOfTheDevourer)
  , ("50042", corpseTaker)
  , ("50043", maskOfUmordhoth)
  , ("50044", jeremiahPierce)
  , ("50045", billyCooper)
  , ("50046", almaHill)
  , ("81022", bogGator)
  , ("81023", swampLeech)
  , ("81024", cursedSwamp)
  , ("81025", spectralMist)
  , ("81026", draggedUnder)
  , ("81027", ripplesOnTheSurface)
  , ("81028", theRougarou)
  , ("81031", slimeCoveredDhole)
  , ("81032", marshGug)
  , ("81033", darkYoungHost)
  , ("81034", onTheProwl)
  , ("81035", beastOfTheBayou)
  , ("81036", insatiableBloodlust)
  ]

placeholderEnemy :: CardId -> EncounterCard
placeholderEnemy cardId =
  enemy cardId "enemy" (Name "Placeholder Enemy Card" Nothing)

placeholderTreachery :: CardId -> EncounterCard
placeholderTreachery cardId =
  treachery cardId "treachery" (Name "Placeholder Treachery Card" Nothing)

ghoulPriest :: CardId -> EncounterCard
ghoulPriest cardId = (enemy cardId "01116" (Name "Ghoul Priest" Nothing))
  { ecTraits = setFromList [Humanoid, Monster, Ghoul, Elite]
  , ecKeywords = setFromList [Keyword.Hunter, Keyword.Retaliate]
  , ecVictoryPoints = Just 2
  }

fleshEater :: CardId -> EncounterCard
fleshEater cardId = (enemy cardId "01118" (Name "Flesh-Eater" Nothing))
  { ecTraits = setFromList [Humanoid, Monster, Ghoul]
  , ecVictoryPoints = Just 1
  }

icyGhoul :: CardId -> EncounterCard
icyGhoul cardId = (enemy cardId "01119" (Name "Icy Ghoul" Nothing))
  { ecTraits = setFromList [Humanoid, Monster, Ghoul]
  , ecVictoryPoints = Just 1
  }

theMaskedHunter :: CardId -> EncounterCard
theMaskedHunter cardId =
  (enemy cardId "01121b" (Name "The Masked Hunter" Nothing))
    { ecTraits = setFromList [Humanoid, Cultist, Elite]
    , ecKeywords = setFromList [Keyword.Hunter]
    , ecVictoryPoints = Just 2
    }

huntingShadow :: CardId -> EncounterCard
huntingShadow cardId =
  (treachery cardId "01135" (Name "Hunting Shadow" Nothing))
    { ecTraits = setFromList [Curse]
    , ecKeywords = setFromList [Keyword.Peril]
    }

falseLead :: CardId -> EncounterCard
falseLead cardId = treachery cardId "01136" (Name "False Lead" Nothing)

wolfManDrew :: CardId -> EncounterCard
wolfManDrew cardId = (enemy cardId "01137" (Name "\"Wolf-Man\" Drew" Nothing))
  { ecTraits = setFromList [Humanoid, Cultist]
  , ecVictoryPoints = Just 1
  }

hermanCollins :: CardId -> EncounterCard
hermanCollins cardId = (enemy cardId "01138" (Name "Herman Collins" Nothing))
  { ecTraits = setFromList [Humanoid, Cultist]
  , ecVictoryPoints = Just 1
  }

peterWarren :: CardId -> EncounterCard
peterWarren cardId = (enemy cardId "01139" (Name "Peter Warren" Nothing))
  { ecTraits = setFromList [Humanoid, Cultist]
  , ecVictoryPoints = Just 1
  }

victoriaDevereux :: CardId -> EncounterCard
victoriaDevereux cardId =
  (enemy cardId "01140" (Name "Victoria Devereux" Nothing))
    { ecTraits = setFromList [Humanoid, Cultist]
    , ecVictoryPoints = Just 1
    }

ruthTurner :: CardId -> EncounterCard
ruthTurner cardId = (enemy cardId "01141" (Name "Ruth Turner" Nothing))
  { ecTraits = setFromList [Humanoid, Cultist]
  , ecVictoryPoints = Just 1
  }

umordhoth :: CardId -> EncounterCard
umordhoth cardId = (enemy cardId "01157" (Name "Umôrdhoth" Nothing))
  { ecTraits = setFromList [AncientOne, Elite]
  , ecKeywords = setFromList [Keyword.Hunter, Keyword.Massive]
  }

umordhothsWrath :: CardId -> EncounterCard
umordhothsWrath cardId =
  (treachery cardId "01158" (Name "Umôrdhoth's Wrath" Nothing))
    { ecTraits = setFromList [Curse]
    }

swarmOfRats :: CardId -> EncounterCard
swarmOfRats cardId = (enemy cardId "01159" (Name "Swarm of Rats" Nothing))
  { ecTraits = setFromList [Creature]
  , ecKeywords = setFromList [Keyword.Hunter]
  }

ghoulMinion :: CardId -> EncounterCard
ghoulMinion cardId = (enemy cardId "01160" (Name "Ghoul Minion" Nothing))
  { ecTraits = setFromList [Humanoid, Monster, Ghoul]
  }

ravenousGhoul :: CardId -> EncounterCard
ravenousGhoul cardId = (enemy cardId "01161" (Name "Ravenous Ghoul" Nothing))
  { ecTraits = setFromList [Humanoid, Monster, Ghoul]
  }

graspingHands :: CardId -> EncounterCard
graspingHands cardId =
  (treachery cardId "01162" (Name "Grasping Hands" Nothing))
    { ecTraits = setFromList [Hazard]
    }

rottingRemains :: CardId -> EncounterCard
rottingRemains cardId =
  (treachery cardId "01163" (Name "Rotting Remains" Nothing))
    { ecTraits = setFromList [Terror]
    }

frozenInFear :: CardId -> EncounterCard
frozenInFear cardId = (treachery cardId "01164" (Name "Frozen in Fear" Nothing)
                      )
  { ecTraits = setFromList [Terror]
  }

dissonantVoices :: CardId -> EncounterCard
dissonantVoices cardId =
  (treachery cardId "01165" (Name "Dissonant Voices" Nothing))
    { ecTraits = setFromList [Terror]
    }

ancientEvils :: CardId -> EncounterCard
ancientEvils cardId = (treachery cardId "01166" (Name "Ancient Evils" Nothing))
  { ecTraits = setFromList [Omen]
  }

cryptChill :: CardId -> EncounterCard
cryptChill cardId = (treachery cardId "01167" (Name "Crypt Chill" Nothing))
  { ecTraits = setFromList [Hazard]
  }

obscuringFog :: CardId -> EncounterCard
obscuringFog cardId = (treachery cardId "01168" (Name "Obscuring Fog" Nothing))
  { ecTraits = setFromList [Hazard]
  }

acolyte :: CardId -> EncounterCard
acolyte cardId = (enemy cardId "01169" (Name "Acolyte" Nothing))
  { ecTraits = setFromList [Humanoid, Cultist]
  }

wizardOfTheOrder :: CardId -> EncounterCard
wizardOfTheOrder cardId =
  (enemy cardId "01170" (Name "Wizard of the Order" Nothing))
    { ecTraits = setFromList [Humanoid, Cultist]
    , ecKeywords = setFromList [Keyword.Retaliate]
    }

mysteriousChanting :: CardId -> EncounterCard
mysteriousChanting cardId =
  (treachery cardId "01171" (Name "Mysterious Chanting" Nothing))
    { ecTraits = setFromList [Hex]
    }

huntingNightgaunt :: CardId -> EncounterCard
huntingNightgaunt cardId =
  (enemy cardId "01172" (Name "Hunting Nightgaunt" Nothing))
    { ecTraits = setFromList [Monster, Nightgaunt]
    , ecKeywords = setFromList [Keyword.Hunter]
    }

onWingsOfDarkness :: CardId -> EncounterCard
onWingsOfDarkness cardId =
  treachery cardId "01173" (Name "On Wings of Darkness" Nothing)

lockedDoor :: CardId -> EncounterCard
lockedDoor cardId = (treachery cardId "01174" (Name "Locked Door" Nothing))
  { ecTraits = setFromList [Obstacle]
  }

screechingByakhee :: CardId -> EncounterCard
screechingByakhee cardId =
  (enemy cardId "01175" (Name "Screeching Byakhee" Nothing))
    { ecTraits = setFromList [Monster, Byakhee]
    , ecKeywords = setFromList [Keyword.Hunter]
    }

theYellowSign :: CardId -> EncounterCard
theYellowSign cardId =
  (treachery cardId "01176" (Name "The Yellow Sign" Nothing))
    { ecTraits = setFromList [Omen]
    }

yithianObserver :: CardId -> EncounterCard
yithianObserver cardId =
  (enemy cardId "01177" (Name "Yithian Observer" Nothing))
    { ecTraits = setFromList [Monster, Yithian]
    , ecVictoryPoints = Just 1
    }

offerOfPower :: CardId -> EncounterCard
offerOfPower cardId = (treachery cardId "01178" (Name "Offer of Power" Nothing)
                      )
  { ecTraits = setFromList [Pact]
  , ecKeywords = setFromList [Keyword.Peril]
  }

relentlessDarkYoung :: CardId -> EncounterCard
relentlessDarkYoung cardId =
  (enemy cardId "01179" (Name "Relentless Dark Young" Nothing))
    { ecTraits = setFromList [Monster, DarkYoung]
    , ecVictoryPoints = Just 1
    }

goatSpawn :: CardId -> EncounterCard
goatSpawn cardId = (enemy cardId "01180" (Name "Goat Spawn" Nothing))
  { ecTraits = setFromList [Humanoid, Monster]
  , ecKeywords = setFromList [Keyword.Hunter, Keyword.Retaliate]
  }

youngDeepOne :: CardId -> EncounterCard
youngDeepOne cardId = (enemy cardId "01181" (Name "Young Deep One" Nothing))
  { ecTraits = setFromList [Humanoid, Monster, DeepOne]
  , ecKeywords = setFromList [Keyword.Hunter]
  }

dreamsOfRlyeh :: CardId -> EncounterCard
dreamsOfRlyeh cardId =
  (treachery cardId "01182" (Name "Dreams of R'lyeh" Nothing))
    { ecTraits = setFromList [Omen]
    }

theExperiment :: CardId -> EncounterCard
theExperiment cardId = (enemy cardId "02058" (Name "The Experiment" Nothing))
  { ecTraits = setFromList [Monster, Abomination, Elite]
  , ecKeywords = setFromList [Keyword.Massive]
  , ecVictoryPoints = Just 2
  }

jazzMulligan :: CardId -> EncounterCard
jazzMulligan cardId = (asset cardId "02060" (Name "\"Jazz\" Mulligan" Nothing))
  { ecTraits = setFromList [Ally, Miskatonic]
  }

cloverClubPitBoss :: CardId -> EncounterCard
cloverClubPitBoss cardId =
  (enemy cardId "02078" (Name "Clover Club Pit Boxx" Nothing))
    { ecTraits = setFromList [Criminal, Elite]
    , ecKeywords = setFromList [Keyword.Hunter]
    , ecVictoryPoints = Just 1
    }

somethingInTheDrinks :: CardId -> EncounterCard
somethingInTheDrinks cardId =
  (treachery cardId "02081" (Name "Something in the Drinks" Nothing))
    { ecTraits = setFromList [Poison, Illicit]
    , ecKeywords = setFromList [Keyword.Surge]
    }

arousingSuspicions :: CardId -> EncounterCard
arousingSuspicions cardId =
  treachery cardId "02082" (Name "Arousing Suspicions" Nothing)

visionsOfFuturesPast :: CardId -> EncounterCard
visionsOfFuturesPast cardId =
  (treachery cardId "02083" (Name "Visions of Futures Past" Nothing))
    { ecTraits = setFromList [Hex]
    }

beyondTheVeil :: CardId -> EncounterCard
beyondTheVeil cardId =
  (treachery cardId "02084" (Name "Beyond the Veil" Nothing))
    { ecTraits = setFromList [Hex]
    , ecKeywords = setFromList [Keyword.Surge]
    }

lightOfAforgomon :: CardId -> EncounterCard
lightOfAforgomon cardId =
  (treachery cardId "02085" (Name "Light of Aforgomon" Nothing))
    { ecTraits = setFromList [Pact, Power]
    , ecKeywords = setFromList [Keyword.Peril]
    }

thrall :: CardId -> EncounterCard
thrall cardId = (enemy cardId "02086" (Name "Thrall" Nothing))
  { ecTraits = setFromList [Humanoid, Monster, Abomination]
  , ecKeywords = setFromList [Keyword.Retaliate]
  }

wizardOfYogSothoth :: CardId -> EncounterCard
wizardOfYogSothoth cardId =
  (enemy cardId "02087" (Name "Wizard of Yog-Sothoth" Nothing))
    { ecTraits = setFromList [Humanoid, Sorcerer]
    , ecKeywords = setFromList [Keyword.Hunter]
    , ecVictoryPoints = Just 1
    }

unhallowedCountry :: CardId -> EncounterCard
unhallowedCountry cardId =
  (treachery cardId "02088" (Name "Unhallowed Country" Nothing))
    { ecTraits = setFromList [Terror]
    }

sordidAndSilent :: CardId -> EncounterCard
sordidAndSilent cardId =
  (treachery cardId "02089" (Name "Sordid and Silent" Nothing))
    { ecTraits = setFromList [Terror]
    }

whippoorwill :: CardId -> EncounterCard
whippoorwill cardId = (enemy cardId "02090" (Name "Whippoorwill" Nothing))
  { ecTraits = setFromList [Creature]
  , ecKeywords = setFromList [Keyword.Aloof, Keyword.Hunter]
  }

eagerForDeath :: CardId -> EncounterCard
eagerForDeath cardId =
  (treachery cardId "02091" (Name "Eager for Death" Nothing))
    { ecTraits = setFromList [Omen]
    }

cursedLuck :: CardId -> EncounterCard
cursedLuck cardId = (treachery cardId "02092" (Name "Cursed Luck" Nothing))
  { ecTraits = setFromList [Omen]
  }

twistOfFate :: CardId -> EncounterCard
twistOfFate cardId = (treachery cardId "02093" (Name "Twist of Fate" Nothing))
  { ecTraits = setFromList [Omen]
  }

avianThrall :: CardId -> EncounterCard
avianThrall cardId = (enemy cardId "02094" (Name "Avian Thrall" Nothing))
  { ecTraits = setFromList [Creature, Monster, Abomination]
  , ecKeywords = setFromList [Keyword.Hunter]
  }

lupineThrall :: CardId -> EncounterCard
lupineThrall cardId = (enemy cardId "02095" (Name "Lupine Thrall" Nothing))
  { ecTraits = setFromList [Creature, Monster, Abomination]
  , ecKeywords = setFromList [Keyword.Hunter, Keyword.Retaliate]
  }

alteredBeast :: CardId -> EncounterCard
alteredBeast cardId = (treachery cardId "02096" (Name "Altered Beast" Nothing))
  { ecTraits = setFromList [Power]
  }

oBannionsThug :: CardId -> EncounterCard
oBannionsThug cardId = (enemy cardId "02097" (Name "O'Bannion's Thug" Nothing))
  { ecTraits = setFromList [Humanoid, Criminal, Syndicate]
  }

mobster :: CardId -> EncounterCard
mobster cardId = (enemy cardId "02098" (Name "Mobster" Nothing))
  { ecTraits = setFromList [Humanoid, Criminal, Syndicate]
  , ecKeywords = setFromList [Keyword.Retaliate]
  }

huntedDown :: CardId -> EncounterCard
huntedDown cardId = (treachery cardId "02099" (Name "Hunted Down" Nothing))
  { ecTraits = setFromList [Tactic]
  }

pushedIntoTheBeyond :: CardId -> EncounterCard
pushedIntoTheBeyond cardId =
  (treachery cardId "02100" (Name "Pushed into the Beyond" Nothing))
    { ecTraits = setFromList [Hex]
    }

terrorFromBeyond :: CardId -> EncounterCard
terrorFromBeyond cardId =
  (treachery cardId "02101" (Name "Terror from Beyond" Nothing))
    { ecTraits = setFromList [Hex, Terror]
    , ecKeywords = setFromList [Keyword.Peril]
    }

arcaneBarrier :: CardId -> EncounterCard
arcaneBarrier cardId =
  (treachery cardId "02102" (Name "Arcane Barrier" Nothing))
    { ecTraits = setFromList [Hex, Obstacle]
    }

conglomerationOfSpheres :: CardId -> EncounterCard
conglomerationOfSpheres cardId =
  (enemy cardId "02103" (Name "Conglomeration of Spheres" Nothing))
    { ecTraits = setFromList [Monster, Abomination]
    , ecKeywords = setFromList [Keyword.Hunter]
    }

servantOfTheLurker :: CardId -> EncounterCard
servantOfTheLurker cardId =
  (enemy cardId "02104" (Name "Servant of the Lurker" Nothing))
    { ecTraits = setFromList [Monster, Abomination]
    , ecKeywords = setFromList [Keyword.Hunter]
    , ecVictoryPoints = Just 1
    }

huntingHorror :: CardId -> EncounterCard
huntingHorror cardId = (enemy cardId "02141" (Name "Hunting Horror" Nothing))
  { ecTraits = setFromList [Monster, Elite]
  , ecKeywords = setFromList [Keyword.Hunter, Keyword.Retaliate]
  }

shadowSpawned :: CardId -> EncounterCard
shadowSpawned cardId =
  (treachery cardId "02142" (Name "Shadow-spawned" Nothing))
    { ecTraits = singleton Power
    }

stalkedInTheDark :: CardId -> EncounterCard
stalkedInTheDark cardId =
  (treachery cardId "02143" (Name "Stalked in the Dark" Nothing))
    { ecTraits = singleton Tactic
    }

passageIntoTheVeil :: CardId -> EncounterCard
passageIntoTheVeil cardId =
  (treachery cardId "02144" (Name "Passage into the Veil" Nothing))
    { ecTraits = singleton Power
    }

ephemeralExhibits :: CardId -> EncounterCard
ephemeralExhibits cardId =
  (treachery cardId "02145" (Name "Ephemeral Exhibits" Nothing))
    { ecTraits = singleton Terror
    }

slitheringBehindYou :: CardId -> EncounterCard
slitheringBehindYou cardId =
  treachery cardId "02146" (Name "Slithering Behind You" Nothing)

helplessPassenger :: CardId -> EncounterCard
helplessPassenger cardId =
  (asset cardId "02179" (Name "Helpless Passenger" Nothing))
    { ecTraits = setFromList [Ally, Bystander]
    , ecKeywords = singleton Keyword.Surge
    }

clawsOfSteam :: CardId -> EncounterCard
clawsOfSteam cardId = (treachery cardId "02180" (Name "Claws of Steam" Nothing)
                      )
  { ecTraits = singleton Power
  }

brokenRails :: CardId -> EncounterCard
brokenRails cardId = (treachery cardId "02181" (Name "Broken Rails" Nothing))
  { ecTraits = singleton Hazard
  }

grapplingHorror :: CardId -> EncounterCard
grapplingHorror cardId =
  (enemy cardId "02182" (Name "Grappling Horror" Nothing))
    { ecTraits = setFromList [Monster, Abomination]
    , ecKeywords = setFromList [Keyword.Hunter]
    }

emergentMonstrosity :: CardId -> EncounterCard
emergentMonstrosity cardId =
  (enemy cardId "02183" (Name "Emergent Monstrosity" Nothing))
    { ecTraits = setFromList [Monster, Abomination]
    , ecVictoryPoints = Just 1
    }

theHiddenChamber :: CardId -> EncounterCard
theHiddenChamber cardId =
  (location cardId "02214" (Name "The Hidden Chamber" Nothing))
    { ecTraits = singleton Dunwich
    , ecVictoryPoints = Just 2
    }

keyToTheChamber :: CardId -> EncounterCard
keyToTheChamber cardId =
  (asset cardId "02215" (Name "Key to the Chamber" Nothing))
    { ecTraits = setFromList [Item, Key]
    }

silasBishop :: CardId -> EncounterCard
silasBishop cardId = (enemy cardId "02216" (Name "Silas Bishop" Nothing))
  { ecTraits = setFromList [Monster, Abomination, Elite]
  , ecKeywords = singleton Keyword.Massive
  , ecVictoryPoints = Just 2
  }

kidnapped :: CardId -> EncounterCard
kidnapped cardId = treachery cardId "02220" (Name "Kidnapped!" Nothing)

psychopompsSong :: CardId -> EncounterCard
psychopompsSong cardId =
  (treachery cardId "02221" (Name "Psychopomp's Song" Nothing))
    { ecTraits = singleton Omen
    , ecKeywords = setFromList [Keyword.Surge, Keyword.Peril]
    }

strangeSigns :: CardId -> EncounterCard
strangeSigns cardId = (treachery cardId "02222" (Name "Strange Signs" Nothing))
  { ecTraits = singleton Omen
  }

rottingRemainsBloodOnTheAltar :: CardId -> EncounterCard
rottingRemainsBloodOnTheAltar cardId =
  (treachery cardId "02223" (Name "Rotting Remains" Nothing))
    { ecTraits = singleton Terror
    }

servantOfManyMouths :: CardId -> EncounterCard
servantOfManyMouths cardId =
  (enemy cardId "02224" (Name "Servant of Many Mouths" Nothing))
    { ecTraits = singleton Humanoid
    , ecKeywords = singleton Keyword.Retaliate
    }

broodOfYogSothoth :: CardId -> EncounterCard
broodOfYogSothoth cardId =
  (enemy cardId "02255" (Name "Brood of Yog-Sothoth" Nothing))
    { ecTraits = setFromList [Monster, Abomination]
    , ecKeywords = singleton Keyword.Massive
    , ecVictoryPoints = Just 1
    }

toweringBeasts :: CardId -> EncounterCard
toweringBeasts cardId =
  (treachery cardId "02256" (Name "Towering Beasts" Nothing))
    { ecKeywords = singleton Keyword.Peril
    }

ruinAndDestruction :: CardId -> EncounterCard
ruinAndDestruction cardId =
  (treachery cardId "02257" (Name "Ruin and Destruction" Nothing))
    { ecTraits = singleton Hazard
    }

attractingAttention :: CardId -> EncounterCard
attractingAttention cardId =
  (treachery cardId "02258" (Name "Attracting Attention" Nothing))
    { ecKeywords = singleton Keyword.Surge
    }

theCreaturesTracks :: CardId -> EncounterCard
theCreaturesTracks cardId =
  (treachery cardId "02259" (Name "The Creatures' Tracks" Nothing))
    { ecTraits = singleton Terror
    , ecKeywords = singleton Keyword.Peril
    }

sethBishop :: CardId -> EncounterCard
sethBishop cardId =
  (enemy cardId "02293" (Name "Seth Bishop" (Just "Sorcerer of Dunwich")))
    { ecTraits = setFromList [Humanoid, Sorcerer, Elite]
    , ecKeywords = setFromList [Keyword.Retaliate]
    , ecVictoryPoints = Just 1
    }

devoteeOfTheKey :: CardId -> EncounterCard
devoteeOfTheKey cardId =
  (enemy cardId "02294" (Name "Devotee of the Key" Nothing))
    { ecTraits = setFromList [Humanoid, Sorcerer]
    }

crazedShoggoth :: CardId -> EncounterCard
crazedShoggoth cardId = (enemy cardId "02295" (Name "Crazed Shoggoth" Nothing))
  { ecTraits = setFromList [Monster, Shoggoth]
  , ecVictoryPoints = Just 1
  }

ritesHowled :: CardId -> EncounterCard
ritesHowled cardId = (treachery cardId "02296" (Name "Rites Howled" Nothing))
  { ecTraits = singleton Hex
  }

spacesBetween :: CardId -> EncounterCard
spacesBetween cardId =
  (treachery cardId "02297" (Name "Spaces Between" Nothing))
    { ecTraits = setFromList [Hex, Hazard]
    }

vortexOfTime :: CardId -> EncounterCard
vortexOfTime cardId = (treachery cardId "02298" (Name "Vortex of Time" Nothing)
                      )
  { ecTraits = setFromList [Hex, Hazard]
  }

yogSothoth :: CardId -> EncounterCard
yogSothoth cardId = (enemy
                      cardId
                      "02323"
                      (Name
                        "Yog-Sothoth"
                        (Just "The Lurker Beyond the Threshold")
                      )
                    )
  { ecTraits = setFromList [AncientOne, Elite]
  , ecKeywords = setFromList
    [Keyword.Massive, Keyword.Hunter, Keyword.Retaliate]
  }

tearThroughSpace :: CardId -> EncounterCard
tearThroughSpace cardId =
  (location cardId "02324" (Name "Tear Through Space" Nothing))
    { ecTraits = setFromList [Otherworld, Extradimensional]
    , ecKeywords = setFromList [Keyword.Surge]
    }

prismaticCascade :: CardId -> EncounterCard
prismaticCascade cardId =
  (location cardId "02325" (Name "Prismatic Cascade" Nothing))
    { ecTraits = setFromList [Otherworld, Extradimensional]
    }

endlessBridge :: CardId -> EncounterCard
endlessBridge cardId = (location cardId "02326" (Name "Endless Bridge" Nothing)
                       )
  { ecTraits = setFromList [Otherworld, Extradimensional]
  }

stepsOfYhagharl :: CardId -> EncounterCard
stepsOfYhagharl cardId =
  (location cardId "02327" (Name "Steps of Y'hagharl" Nothing))
    { ecTraits = setFromList [Otherworld, Extradimensional]
    }

dimensionalDoorway :: CardId -> EncounterCard
dimensionalDoorway cardId =
  (location cardId "02328" (Name "Dimensional Doorway" Nothing))
    { ecTraits = setFromList [Otherworld, Extradimensional]
    }

interstellarTraveler :: CardId -> EncounterCard
interstellarTraveler cardId =
  (enemy cardId "02329" (Name "Interstellar Traveler" Nothing))
    { ecTraits = setFromList [Monster, Yithian]
    , ecKeywords = setFromList [Keyword.Hunter]
    }

yithianStarseeker :: CardId -> EncounterCard
yithianStarseeker cardId =
  (enemy cardId "02330" (Name "Yithian Starseeker" Nothing))
    { ecTraits = setFromList [Monster, Yithian]
    , ecKeywords = setFromList [Keyword.Retaliate]
    }

collapsingReality :: CardId -> EncounterCard
collapsingReality cardId =
  (treachery cardId "02331" (Name "Collapsing Reality" Nothing))
    { ecTraits = setFromList [Hazard]
    }

wormhole :: CardId -> EncounterCard
wormhole cardId = (treachery cardId "02332" (Name "Wormhole" Nothing))
  { ecTraits = setFromList [Hazard]
  }

vastExpanse :: CardId -> EncounterCard
vastExpanse cardId = (treachery cardId "02333" (Name "Vast Expanse" Nothing))
  { ecTraits = setFromList [Terror]
  }

corpseHungryGhoul :: CardId -> EncounterCard
corpseHungryGhoul cardId =
  (enemy cardId "50022" (Name "Corpse-Hungry Ghoul" Nothing))
    { ecTraits = setFromList [Humanoid, Monster, Ghoul]
    , ecKeywords = setFromList [Keyword.Hunter]
    , ecVictoryPoints = Just 1
    }

ghoulFromTheDepths :: CardId -> EncounterCard
ghoulFromTheDepths cardId =
  (enemy cardId "50023" (Name "Ghoul from the Depths" Nothing))
    { ecTraits = setFromList [Humanoid, Monster, Ghoul]
    , ecKeywords = setFromList [Keyword.Retaliate]
    , ecVictoryPoints = Just 1
    }

theZealotsSeal :: CardId -> EncounterCard
theZealotsSeal cardId =
  (treachery cardId "50024" (Name "The Zealot's Seal" Nothing))
    { ecTraits = setFromList [Hex]
    }

narogath :: CardId -> EncounterCard
narogath cardId = (enemy cardId "50026b" (Name "Narôgath" Nothing))
  { ecTraits = setFromList [Humanoid, Monster, Cultist, Elite]
  , ecKeywords = setFromList [Keyword.Hunter]
  , ecVictoryPoints = Just 2
  }

maskedHorrors :: CardId -> EncounterCard
maskedHorrors cardId =
  (treachery cardId "50031" (Name "Masked Horrors" Nothing))
    { ecTraits = setFromList [Power, Scheme]
    }

vaultOfEarthlyDemise :: CardId -> EncounterCard
vaultOfEarthlyDemise cardId =
  (treachery cardId "50032a" (Name "Vault of Earthly Demise" Nothing))
    { ecTraits = setFromList [Eldritch, Otherworld]
    }

umordhothsHunger :: CardId -> EncounterCard
umordhothsHunger cardId =
  (treachery cardId "50037" (Name "Umôrdhoth's Hunger" Nothing))
    { ecTraits = setFromList [Power]
    }

graveEater :: CardId -> EncounterCard
graveEater cardId = (enemy cardId "50038" (Name "Grave-Eater" Nothing))
  { ecTraits = setFromList [Humanoid, Monster, Ghoul]
  }

acolyteOfUmordhoth :: CardId -> EncounterCard
acolyteOfUmordhoth cardId =
  (enemy cardId "50039" (Name "Acolyte of Umôrdhoth" Nothing))
    { ecTraits = setFromList [Humanoid, Monster, Ghoul]
    }

chillFromBelow :: CardId -> EncounterCard
chillFromBelow cardId =
  (treachery cardId "50040" (Name "Chill from Below" Nothing))
    { ecTraits = setFromList [Hazard]
    }

discipleOfTheDevourer :: CardId -> EncounterCard
discipleOfTheDevourer cardId =
  (enemy cardId "50041" (Name "Disciple of the Devourer" Nothing))
    { ecTraits = setFromList [Humanoid, Cultist]
    }

corpseTaker :: CardId -> EncounterCard
corpseTaker cardId = (enemy cardId "50042" (Name "Corpse-Taker" Nothing))
  { ecTraits = setFromList [Monster, Servitor, Cultist]
  }

maskOfUmordhoth :: CardId -> EncounterCard
maskOfUmordhoth cardId =
  (treachery cardId "50043" (Name "Mask of Umôrdhoth" Nothing))
    { ecTraits = setFromList [Item, Mask]
    }

jeremiahPierce :: CardId -> EncounterCard
jeremiahPierce cardId = (enemy cardId "50044" (Name "Jeremiah Pierce" Nothing))
  { ecTraits = setFromList [Humanoid, Cultist]
  , ecVictoryPoints = Just 1
  }

billyCooper :: CardId -> EncounterCard
billyCooper cardId = (enemy cardId "50045" (Name "Billy Cooper" Nothing))
  { ecTraits = setFromList [Humanoid, Cultist]
  , ecVictoryPoints = Just 1
  }

almaHill :: CardId -> EncounterCard
almaHill cardId = (enemy cardId "50046" (Name "Alma Hill" Nothing))
  { ecTraits = setFromList [Humanoid, Cultist]
  , ecVictoryPoints = Just 1
  }

bogGator :: CardId -> EncounterCard
bogGator cardId = (enemy cardId "81022" (Name "Bog Gator" Nothing))
  { ecTraits = setFromList [Creature]
  }

swampLeech :: CardId -> EncounterCard
swampLeech cardId = (enemy cardId "81023" (Name "Swamp Leech" Nothing))
  { ecTraits = setFromList [Creature]
  }

cursedSwamp :: CardId -> EncounterCard
cursedSwamp cardId = (treachery cardId "81024" (Name "Cursed Swamp" Nothing))
  { ecTraits = setFromList [Hazard]
  }

spectralMist :: CardId -> EncounterCard
spectralMist cardId = (treachery cardId "81025" (Name "Spectral Mist" Nothing))
  { ecTraits = setFromList [Hazard]
  }

draggedUnder :: CardId -> EncounterCard
draggedUnder cardId = (treachery cardId "81026" (Name "Dragged Under" Nothing))
  { ecTraits = setFromList [Hazard]
  }

ripplesOnTheSurface :: CardId -> EncounterCard
ripplesOnTheSurface cardId =
  (treachery cardId "81027" (Name "Ripples on the Surface" Nothing))
    { ecTraits = setFromList [Terror]
    }

theRougarou :: CardId -> EncounterCard
theRougarou cardId = (enemy cardId "81028" (Name "The Rougarou" Nothing))
  { ecTraits = setFromList [Monster, Creature, Elite]
  , ecKeywords = setFromList [Keyword.Aloof, Keyword.Retaliate]
  }

slimeCoveredDhole :: CardId -> EncounterCard
slimeCoveredDhole cardId =
  (enemy cardId "81031" (Name "Slime-Covered Dhole" Nothing))
    { ecTraits = setFromList [Monster, Dhole]
    , ecKeywords = setFromList [Keyword.Hunter]
    }

marshGug :: CardId -> EncounterCard
marshGug cardId = (enemy cardId "81032" (Name "Marsh Gug" Nothing))
  { ecTraits = setFromList [Monster, Gug]
  , ecKeywords = setFromList [Keyword.Hunter]
  }

darkYoungHost :: CardId -> EncounterCard
darkYoungHost cardId = (enemy cardId "81033" (Name "Dark Young Host" Nothing))
  { ecTraits = setFromList [Monster, DarkYoung]
  , ecVictoryPoints = Just 1
  }

onTheProwl :: CardId -> EncounterCard
onTheProwl cardId = (treachery cardId "81034" (Name "On the Prowl" Nothing))
  { ecKeywords = setFromList [Keyword.Surge]
  }

beastOfTheBayou :: CardId -> EncounterCard
beastOfTheBayou cardId =
  treachery cardId "81035" (Name "Beast of the Bayou" Nothing)

insatiableBloodlust :: CardId -> EncounterCard
insatiableBloodlust cardId =
  treachery cardId "81026" (Name "Insatiable Bloodlust" Nothing)
