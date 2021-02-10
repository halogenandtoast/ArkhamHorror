module Arkham.EncounterCard
  ( genEncounterCard
  , lookupEncounterCard
  , baseEncounterCard
  , allEncounterCards
  , placeholderEnemy
  , placeholderTreachery
  ) where

import Arkham.Prelude

import Arkham.Types.Card.CardCode
import Arkham.Types.Card.EncounterCard
import Arkham.Types.Card.Id
import qualified Arkham.Types.Keyword as Keyword
import Arkham.Types.Trait

genEncounterCard :: MonadRandom m => CardCode -> m EncounterCard
genEncounterCard cardCode = lookupEncounterCard cardCode <$> getRandom

lookupEncounterCard :: CardCode -> (CardId -> EncounterCard)
lookupEncounterCard cardCode =
  fromJustNote ("Unknown card: " <> show cardCode)
    $ lookup cardCode allEncounterCards

baseEncounterCard
  :: EncounterCardType -> CardId -> CardCode -> Text -> EncounterCard
baseEncounterCard encounterCardType cardId cardCode name = MkEncounterCard
  { ecCardCode = cardCode
  , ecId = cardId
  , ecName = name
  , ecTraits = mempty
  , ecKeywords = mempty
  , ecCardType = encounterCardType
  , ecVictoryPoints = Nothing
  }

enemy :: CardId -> CardCode -> Text -> EncounterCard
enemy = baseEncounterCard EnemyType

asset :: CardId -> CardCode -> Text -> EncounterCard
asset = baseEncounterCard EncounterAssetType

treachery :: CardId -> CardCode -> Text -> EncounterCard
treachery = baseEncounterCard TreacheryType

location :: CardId -> CardCode -> Text -> EncounterCard
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
placeholderEnemy cardId = enemy cardId "enemy" "Placeholder Enemy Card"

placeholderTreachery :: CardId -> EncounterCard
placeholderTreachery cardId =
  treachery cardId "treachery" "Placeholder Treachery Card"

ghoulPriest :: CardId -> EncounterCard
ghoulPriest cardId = (enemy cardId "01116" "Ghoul Priest")
  { ecTraits = setFromList [Humanoid, Monster, Ghoul, Elite]
  , ecKeywords = setFromList [Keyword.Hunter, Keyword.Retaliate]
  , ecVictoryPoints = Just 2
  }

fleshEater :: CardId -> EncounterCard
fleshEater cardId = (enemy cardId "01118" "Flesh-Eater")
  { ecTraits = setFromList [Humanoid, Monster, Ghoul]
  , ecVictoryPoints = Just 1
  }

icyGhoul :: CardId -> EncounterCard
icyGhoul cardId = (enemy cardId "01119" "Icy Ghoul")
  { ecTraits = setFromList [Humanoid, Monster, Ghoul]
  , ecVictoryPoints = Just 1
  }

theMaskedHunter :: CardId -> EncounterCard
theMaskedHunter cardId = (enemy cardId "01121b" "The Masked Hunter")
  { ecTraits = setFromList [Humanoid, Cultist, Elite]
  , ecKeywords = setFromList [Keyword.Hunter]
  , ecVictoryPoints = Just 2
  }

huntingShadow :: CardId -> EncounterCard
huntingShadow cardId = (treachery cardId "01135" "Hunting Shadow")
  { ecTraits = setFromList [Curse]
  , ecKeywords = setFromList [Keyword.Peril]
  }

falseLead :: CardId -> EncounterCard
falseLead cardId = treachery cardId "01136" "False Lead"

wolfManDrew :: CardId -> EncounterCard
wolfManDrew cardId = (enemy cardId "01137" "\"Wolf-Man\" Drew")
  { ecTraits = setFromList [Humanoid, Cultist]
  , ecVictoryPoints = Just 1
  }

hermanCollins :: CardId -> EncounterCard
hermanCollins cardId = (enemy cardId "01138" "Herman Collins")
  { ecTraits = setFromList [Humanoid, Cultist]
  , ecVictoryPoints = Just 1
  }

peterWarren :: CardId -> EncounterCard
peterWarren cardId = (enemy cardId "01139" "Peter Warren")
  { ecTraits = setFromList [Humanoid, Cultist]
  , ecVictoryPoints = Just 1
  }

victoriaDevereux :: CardId -> EncounterCard
victoriaDevereux cardId = (enemy cardId "01140" "Victoria Devereux")
  { ecTraits = setFromList [Humanoid, Cultist]
  , ecVictoryPoints = Just 1
  }

ruthTurner :: CardId -> EncounterCard
ruthTurner cardId = (enemy cardId "01141" "Ruth Turner")
  { ecTraits = setFromList [Humanoid, Cultist]
  , ecVictoryPoints = Just 1
  }

umordhoth :: CardId -> EncounterCard
umordhoth cardId = (enemy cardId "01157" "Umôrdhoth")
  { ecTraits = setFromList [AncientOne, Elite]
  , ecKeywords = setFromList [Keyword.Hunter, Keyword.Massive]
  }

umordhothsWrath :: CardId -> EncounterCard
umordhothsWrath cardId = (treachery cardId "01158" "Umôrdhoth's Wrath")
  { ecTraits = setFromList [Curse]
  }

swarmOfRats :: CardId -> EncounterCard
swarmOfRats cardId = (enemy cardId "01159" "Swarm of Rats")
  { ecTraits = setFromList [Creature]
  , ecKeywords = setFromList [Keyword.Hunter]
  }

ghoulMinion :: CardId -> EncounterCard
ghoulMinion cardId = (enemy cardId "01160" "Ghoul Minion")
  { ecTraits = setFromList [Humanoid, Monster, Ghoul]
  }

ravenousGhoul :: CardId -> EncounterCard
ravenousGhoul cardId = (enemy cardId "01161" "Ravenous Ghoul")
  { ecTraits = setFromList [Humanoid, Monster, Ghoul]
  }

graspingHands :: CardId -> EncounterCard
graspingHands cardId = (treachery cardId "01162" "Grasping Hands")
  { ecTraits = setFromList [Hazard]
  }

rottingRemains :: CardId -> EncounterCard
rottingRemains cardId = (treachery cardId "01163" "Rotting Remains")
  { ecTraits = setFromList [Terror]
  }

frozenInFear :: CardId -> EncounterCard
frozenInFear cardId = (treachery cardId "01164" "Frozen in Fear")
  { ecTraits = setFromList [Terror]
  }

dissonantVoices :: CardId -> EncounterCard
dissonantVoices cardId = (treachery cardId "01165" "Dissonant Voices")
  { ecTraits = setFromList [Terror]
  }

ancientEvils :: CardId -> EncounterCard
ancientEvils cardId =
  (treachery cardId "01166" "Ancient Evils") { ecTraits = setFromList [Omen] }

cryptChill :: CardId -> EncounterCard
cryptChill cardId =
  (treachery cardId "01167" "Crypt Chill") { ecTraits = setFromList [Hazard] }

obscuringFog :: CardId -> EncounterCard
obscuringFog cardId =
  (treachery cardId "01168" "Obscuring Fog") { ecTraits = setFromList [Hazard] }

acolyte :: CardId -> EncounterCard
acolyte cardId = (enemy cardId "01169" "Acolyte")
  { ecTraits = setFromList [Humanoid, Cultist]
  }

wizardOfTheOrder :: CardId -> EncounterCard
wizardOfTheOrder cardId = (enemy cardId "01170" "Wizard of the Order")
  { ecTraits = setFromList [Humanoid, Cultist]
  , ecKeywords = setFromList [Keyword.Retaliate]
  }

mysteriousChanting :: CardId -> EncounterCard
mysteriousChanting cardId = (treachery cardId "01171" "Mysterious Chanting")
  { ecTraits = setFromList [Hex]
  }

huntingNightgaunt :: CardId -> EncounterCard
huntingNightgaunt cardId = (enemy cardId "01172" "Hunting Nightgaunt")
  { ecTraits = setFromList [Monster, Nightgaunt]
  , ecKeywords = setFromList [Keyword.Hunter]
  }

onWingsOfDarkness :: CardId -> EncounterCard
onWingsOfDarkness cardId = treachery cardId "01173" "On Wings of Darkness"

lockedDoor :: CardId -> EncounterCard
lockedDoor cardId =
  (treachery cardId "01174" "Locked Door") { ecTraits = setFromList [Obstacle] }

screechingByakhee :: CardId -> EncounterCard
screechingByakhee cardId = (enemy cardId "01175" "Screeching Byakhee")
  { ecTraits = setFromList [Monster, Byakhee]
  , ecKeywords = setFromList [Keyword.Hunter]
  }

theYellowSign :: CardId -> EncounterCard
theYellowSign cardId =
  (treachery cardId "01176" "The Yellow Sign") { ecTraits = setFromList [Omen] }

yithianObserver :: CardId -> EncounterCard
yithianObserver cardId = (enemy cardId "01177" "Yithian Observer")
  { ecTraits = setFromList [Monster, Yithian]
  , ecVictoryPoints = Just 1
  }

offerOfPower :: CardId -> EncounterCard
offerOfPower cardId = (treachery cardId "01178" "Offer of Power")
  { ecTraits = setFromList [Pact]
  , ecKeywords = setFromList [Keyword.Peril]
  }

relentlessDarkYoung :: CardId -> EncounterCard
relentlessDarkYoung cardId = (enemy cardId "01179" "Relentless Dark Young")
  { ecTraits = setFromList [Monster, DarkYoung]
  , ecVictoryPoints = Just 1
  }

goatSpawn :: CardId -> EncounterCard
goatSpawn cardId = (enemy cardId "01180" "Goat Spawn")
  { ecTraits = setFromList [Humanoid, Monster]
  , ecKeywords = setFromList [Keyword.Hunter, Keyword.Retaliate]
  }

youngDeepOne :: CardId -> EncounterCard
youngDeepOne cardId = (enemy cardId "01181" "Young Deep One")
  { ecTraits = setFromList [Humanoid, Monster, DeepOne]
  , ecKeywords = setFromList [Keyword.Hunter]
  }

dreamsOfRlyeh :: CardId -> EncounterCard
dreamsOfRlyeh cardId = (treachery cardId "01182" "Dreams of R'lyeh")
  { ecTraits = setFromList [Omen]
  }

theExperiment :: CardId -> EncounterCard
theExperiment cardId = (enemy cardId "02058" "The Experiment")
  { ecTraits = setFromList [Monster, Abomination, Elite]
  , ecKeywords = setFromList [Keyword.Massive]
  , ecVictoryPoints = Just 2
  }

jazzMulligan :: CardId -> EncounterCard
jazzMulligan cardId = (asset cardId "02060" "\"Jazz\" Mulligan")
  { ecTraits = setFromList [Ally, Miskatonic]
  }

cloverClubPitBoss :: CardId -> EncounterCard
cloverClubPitBoss cardId = (enemy cardId "02078" "Clover Club Pit Boxx")
  { ecTraits = setFromList [Criminal, Elite]
  , ecKeywords = setFromList [Keyword.Hunter]
  , ecVictoryPoints = Just 1
  }

somethingInTheDrinks :: CardId -> EncounterCard
somethingInTheDrinks cardId =
  (treachery cardId "02081" "Something in the Drinks")
    { ecTraits = setFromList [Poison, Illicit]
    , ecKeywords = setFromList [Keyword.Surge]
    }

arousingSuspicions :: CardId -> EncounterCard
arousingSuspicions cardId = treachery cardId "02082" "Arousing Suspicions"

visionsOfFuturesPast :: CardId -> EncounterCard
visionsOfFuturesPast cardId =
  (treachery cardId "02083" "Visions of Futures Past")
    { ecTraits = setFromList [Hex]
    }

beyondTheVeil :: CardId -> EncounterCard
beyondTheVeil cardId = (treachery cardId "02084" "Beyond the Veil")
  { ecTraits = setFromList [Hex]
  , ecKeywords = setFromList [Keyword.Surge]
  }

lightOfAforgomon :: CardId -> EncounterCard
lightOfAforgomon cardId = (treachery cardId "02085" "Light of Aforgomon")
  { ecTraits = setFromList [Pact, Power]
  , ecKeywords = setFromList [Keyword.Peril]
  }

thrall :: CardId -> EncounterCard
thrall cardId = (enemy cardId "02086" "Thrall")
  { ecTraits = setFromList [Humanoid, Monster, Abomination]
  , ecKeywords = setFromList [Keyword.Retaliate]
  }

wizardOfYogSothoth :: CardId -> EncounterCard
wizardOfYogSothoth cardId = (enemy cardId "02087" "Wizard of Yog-Sothoth")
  { ecTraits = setFromList [Humanoid, Sorcerer]
  , ecKeywords = setFromList [Keyword.Hunter]
  , ecVictoryPoints = Just 1
  }

unhallowedCountry :: CardId -> EncounterCard
unhallowedCountry cardId = (treachery cardId "02088" "Unhallowed Country")
  { ecTraits = setFromList [Terror]
  }

sordidAndSilent :: CardId -> EncounterCard
sordidAndSilent cardId = (treachery cardId "02089" "Sordid and Silent")
  { ecTraits = setFromList [Terror]
  }

whippoorwill :: CardId -> EncounterCard
whippoorwill cardId = (enemy cardId "02090" "Whippoorwill")
  { ecTraits = setFromList [Creature]
  , ecKeywords = setFromList [Keyword.Aloof, Keyword.Hunter]
  }

eagerForDeath :: CardId -> EncounterCard
eagerForDeath cardId =
  (treachery cardId "02091" "Eager for Death") { ecTraits = setFromList [Omen] }

cursedLuck :: CardId -> EncounterCard
cursedLuck cardId =
  (treachery cardId "02092" "Cursed Luck") { ecTraits = setFromList [Omen] }

twistOfFate :: CardId -> EncounterCard
twistOfFate cardId =
  (treachery cardId "02093" "Twist of Fate") { ecTraits = setFromList [Omen] }

avianThrall :: CardId -> EncounterCard
avianThrall cardId = (enemy cardId "02094" "Avian Thrall")
  { ecTraits = setFromList [Creature, Monster, Abomination]
  , ecKeywords = setFromList [Keyword.Hunter]
  }

lupineThrall :: CardId -> EncounterCard
lupineThrall cardId = (enemy cardId "02095" "Lupine Thrall")
  { ecTraits = setFromList [Creature, Monster, Abomination]
  , ecKeywords = setFromList [Keyword.Hunter, Keyword.Retaliate]
  }

alteredBeast :: CardId -> EncounterCard
alteredBeast cardId =
  (treachery cardId "02096" "Altered Beast") { ecTraits = setFromList [Power] }

oBannionsThug :: CardId -> EncounterCard
oBannionsThug cardId = (enemy cardId "02097" "O'Bannion's Thug")
  { ecTraits = setFromList [Humanoid, Criminal, Syndicate]
  }

mobster :: CardId -> EncounterCard
mobster cardId = (enemy cardId "02098" "Mobster")
  { ecTraits = setFromList [Humanoid, Criminal, Syndicate]
  , ecKeywords = setFromList [Keyword.Retaliate]
  }

huntedDown :: CardId -> EncounterCard
huntedDown cardId =
  (treachery cardId "02099" "Hunted Down") { ecTraits = setFromList [Tactic] }

pushedIntoTheBeyond :: CardId -> EncounterCard
pushedIntoTheBeyond cardId = (treachery cardId "02100" "Pushed into the Beyond"
                             )
  { ecTraits = setFromList [Hex]
  }

terrorFromBeyond :: CardId -> EncounterCard
terrorFromBeyond cardId = (treachery cardId "02101" "Terror from Beyond")
  { ecTraits = setFromList [Hex, Terror]
  , ecKeywords = setFromList [Keyword.Peril]
  }

arcaneBarrier :: CardId -> EncounterCard
arcaneBarrier cardId = (treachery cardId "02102" "Arcane Barrier")
  { ecTraits = setFromList [Hex, Obstacle]
  }

conglomerationOfSpheres :: CardId -> EncounterCard
conglomerationOfSpheres cardId =
  (enemy cardId "02103" "Conglomeration of Spheres")
    { ecTraits = setFromList [Monster, Abomination]
    , ecKeywords = setFromList [Keyword.Hunter]
    }

servantOfTheLurker :: CardId -> EncounterCard
servantOfTheLurker cardId = (enemy cardId "02104" "Servant of the Lurker")
  { ecTraits = setFromList [Monster, Abomination]
  , ecKeywords = setFromList [Keyword.Hunter]
  , ecVictoryPoints = Just 1
  }

huntingHorror :: CardId -> EncounterCard
huntingHorror cardId = (enemy cardId "02141" "Hunting Horror")
  { ecTraits = setFromList [Monster, Elite]
  , ecKeywords = setFromList [Keyword.Hunter, Keyword.Retaliate]
  }

shadowSpawned :: CardId -> EncounterCard
shadowSpawned cardId =
  (treachery cardId "02142" "Shadow-spawned") { ecTraits = singleton Power }

stalkedInTheDark :: CardId -> EncounterCard
stalkedInTheDark cardId = (treachery cardId "02143" "Stalked in the Dark")
  { ecTraits = singleton Tactic
  }

passageIntoTheVeil :: CardId -> EncounterCard
passageIntoTheVeil cardId = (treachery cardId "02144" "Passage into the Veil")
  { ecTraits = singleton Power
  }

ephemeralExhibits :: CardId -> EncounterCard
ephemeralExhibits cardId = (treachery cardId "02145" "Ephemeral Exhibits")
  { ecTraits = singleton Terror
  }

slitheringBehindYou :: CardId -> EncounterCard
slitheringBehindYou cardId = treachery cardId "02146" "Slithering Behind You"

helplessPassenger :: CardId -> EncounterCard
helplessPassenger cardId = (asset cardId "02179" "Helpless Passenger")
  { ecTraits = setFromList [Ally, Bystander]
  , ecKeywords = singleton Keyword.Surge
  }

clawsOfSteam :: CardId -> EncounterCard
clawsOfSteam cardId =
  (treachery cardId "02180" "Claws of Steam") { ecTraits = singleton Power }

brokenRails :: CardId -> EncounterCard
brokenRails cardId =
  (treachery cardId "02181" "Broken Rails") { ecTraits = singleton Hazard }

grapplingHorror :: CardId -> EncounterCard
grapplingHorror cardId = (enemy cardId "02182" "Grappling Horror")
  { ecTraits = setFromList [Monster, Abomination]
  , ecKeywords = setFromList [Keyword.Hunter]
  }

emergentMonstrosity :: CardId -> EncounterCard
emergentMonstrosity cardId = (enemy cardId "02183" "Emergent Monstrosity")
  { ecTraits = setFromList [Monster, Abomination]
  , ecVictoryPoints = Just 1
  }

theHiddenChamber :: CardId -> EncounterCard
theHiddenChamber cardId = (location cardId "02214" "The Hidden Chamber")
  { ecTraits = singleton Dunwich
  , ecVictoryPoints = Just 2
  }

keyToTheChamber :: CardId -> EncounterCard
keyToTheChamber cardId = (asset cardId "02215" "Key to the Chamber")
  { ecTraits = setFromList [Item, Key]
  }

silasBishop :: CardId -> EncounterCard
silasBishop cardId = (enemy cardId "02216" "Silas Bishop")
  { ecTraits = setFromList [Monster, Abomination, Elite]
  , ecKeywords = singleton Keyword.Massive
  , ecVictoryPoints = Just 2
  }

kidnapped :: CardId -> EncounterCard
kidnapped cardId = treachery cardId "02220" "Kidnapped!"

psychopompsSong :: CardId -> EncounterCard
psychopompsSong cardId = (treachery cardId "02221" "Psychopomp's Song")
  { ecTraits = singleton Omen
  , ecKeywords = setFromList [Keyword.Surge, Keyword.Peril]
  }

strangeSigns :: CardId -> EncounterCard
strangeSigns cardId =
  (treachery cardId "02222" "Strange Signs") { ecTraits = singleton Omen }

rottingRemainsBloodOnTheAltar :: CardId -> EncounterCard
rottingRemainsBloodOnTheAltar cardId =
  (treachery cardId "02223" "Rotting Remains") { ecTraits = singleton Terror }

servantOfManyMouths :: CardId -> EncounterCard
servantOfManyMouths cardId = (enemy cardId "02224" "Servant of Many Mouths")
  { ecTraits = singleton Humanoid
  , ecKeywords = singleton Keyword.Retaliate
  }

corpseHungryGhoul :: CardId -> EncounterCard
corpseHungryGhoul cardId = (enemy cardId "50022" "Corpse-Hungry Ghoul")
  { ecTraits = setFromList [Humanoid, Monster, Ghoul]
  , ecKeywords = setFromList [Keyword.Hunter]
  , ecVictoryPoints = Just 1
  }

ghoulFromTheDepths :: CardId -> EncounterCard
ghoulFromTheDepths cardId = (enemy cardId "50023" "Ghoul from the Depths")
  { ecTraits = setFromList [Humanoid, Monster, Ghoul]
  , ecKeywords = setFromList [Keyword.Retaliate]
  , ecVictoryPoints = Just 1
  }

theZealotsSeal :: CardId -> EncounterCard
theZealotsSeal cardId = (treachery cardId "50024" "The Zealot's Seal")
  { ecTraits = setFromList [Hex]
  }

narogath :: CardId -> EncounterCard
narogath cardId = (enemy cardId "50026b" "Narôgath")
  { ecTraits = setFromList [Humanoid, Monster, Cultist, Elite]
  , ecKeywords = setFromList [Keyword.Hunter]
  , ecVictoryPoints = Just 2
  }

maskedHorrors :: CardId -> EncounterCard
maskedHorrors cardId = (treachery cardId "50031" "Masked Horrors")
  { ecTraits = setFromList [Power, Scheme]
  }

vaultOfEarthlyDemise :: CardId -> EncounterCard
vaultOfEarthlyDemise cardId =
  (treachery cardId "50032a" "Vault of Earthly Demise")
    { ecTraits = setFromList [Eldritch, Otherworld]
    }

umordhothsHunger :: CardId -> EncounterCard
umordhothsHunger cardId = (treachery cardId "50037" "Umôrdhoth's Hunger")
  { ecTraits = setFromList [Power]
  }

graveEater :: CardId -> EncounterCard
graveEater cardId = (enemy cardId "50038" "Grave-Eater")
  { ecTraits = setFromList [Humanoid, Monster, Ghoul]
  }

acolyteOfUmordhoth :: CardId -> EncounterCard
acolyteOfUmordhoth cardId = (enemy cardId "50039" "Acolyte of Umôrdhoth")
  { ecTraits = setFromList [Humanoid, Monster, Ghoul]
  }

chillFromBelow :: CardId -> EncounterCard
chillFromBelow cardId = (treachery cardId "50040" "Chill from Below")
  { ecTraits = setFromList [Hazard]
  }

discipleOfTheDevourer :: CardId -> EncounterCard
discipleOfTheDevourer cardId = (enemy cardId "50041" "Disciple of the Devourer"
                               )
  { ecTraits = setFromList [Humanoid, Cultist]
  }

corpseTaker :: CardId -> EncounterCard
corpseTaker cardId = (enemy cardId "50042" "Corpse-Taker")
  { ecTraits = setFromList [Monster, Servitor, Cultist]
  }

maskOfUmordhoth :: CardId -> EncounterCard
maskOfUmordhoth cardId = (treachery cardId "50043" "Mask of Umôrdhoth")
  { ecTraits = setFromList [Item, Mask]
  }

jeremiahPierce :: CardId -> EncounterCard
jeremiahPierce cardId = (enemy cardId "50044" "Jeremiah Pierce")
  { ecTraits = setFromList [Humanoid, Cultist]
  , ecVictoryPoints = Just 1
  }

billyCooper :: CardId -> EncounterCard
billyCooper cardId = (enemy cardId "50045" "Billy Cooper")
  { ecTraits = setFromList [Humanoid, Cultist]
  , ecVictoryPoints = Just 1
  }

almaHill :: CardId -> EncounterCard
almaHill cardId = (enemy cardId "50046" "Alma Hill")
  { ecTraits = setFromList [Humanoid, Cultist]
  , ecVictoryPoints = Just 1
  }

bogGator :: CardId -> EncounterCard
bogGator cardId =
  (enemy cardId "81022" "Bog Gator") { ecTraits = setFromList [Creature] }

swampLeech :: CardId -> EncounterCard
swampLeech cardId =
  (enemy cardId "81023" "Swamp Leech") { ecTraits = setFromList [Creature] }

cursedSwamp :: CardId -> EncounterCard
cursedSwamp cardId =
  (treachery cardId "81024" "Cursed Swamp") { ecTraits = setFromList [Hazard] }

spectralMist :: CardId -> EncounterCard
spectralMist cardId =
  (treachery cardId "81025" "Spectral Mist") { ecTraits = setFromList [Hazard] }

draggedUnder :: CardId -> EncounterCard
draggedUnder cardId =
  (treachery cardId "81026" "Dragged Under") { ecTraits = setFromList [Hazard] }

ripplesOnTheSurface :: CardId -> EncounterCard
ripplesOnTheSurface cardId = (treachery cardId "81027" "Ripples on the Surface"
                             )
  { ecTraits = setFromList [Terror]
  }

theRougarou :: CardId -> EncounterCard
theRougarou cardId = (enemy cardId "81028" "The Rougarou")
  { ecTraits = setFromList [Monster, Creature, Elite]
  , ecKeywords = setFromList [Keyword.Aloof, Keyword.Retaliate]
  }

slimeCoveredDhole :: CardId -> EncounterCard
slimeCoveredDhole cardId = (enemy cardId "81031" "Slime-Covered Dhole")
  { ecTraits = setFromList [Monster, Dhole]
  , ecKeywords = setFromList [Keyword.Hunter]
  }

marshGug :: CardId -> EncounterCard
marshGug cardId = (enemy cardId "81032" "Marsh Gug")
  { ecTraits = setFromList [Monster, Gug]
  , ecKeywords = setFromList [Keyword.Hunter]
  }

darkYoungHost :: CardId -> EncounterCard
darkYoungHost cardId = (enemy cardId "81033" "Dark Young Host")
  { ecTraits = setFromList [Monster, DarkYoung]
  , ecVictoryPoints = Just 1
  }

onTheProwl :: CardId -> EncounterCard
onTheProwl cardId = (treachery cardId "81034" "On the Prowl")
  { ecKeywords = setFromList [Keyword.Surge]
  }

beastOfTheBayou :: CardId -> EncounterCard
beastOfTheBayou cardId = treachery cardId "81035" "Beast of the Bayou"

insatiableBloodlust :: CardId -> EncounterCard
insatiableBloodlust cardId = treachery cardId "81026" "Insatiable Bloodlust"
