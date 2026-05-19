module Arkham.Treachery.CardDefs.TheCircleUndone where

import Arkham.Treachery.CardDefs.Import
import Arkham.Keyword qualified as Keyword

rationalThought :: CardDef
rationalThought =
  (weakness "05008" "Rational Thought")
    { cdCardTraits = singleton Flaw
    }

terribleSecret :: CardDef
terribleSecret =
  (weakness "05015" "Terrible Secret")
    { cdCardTraits = singleton Madness
    , cdRevelation = CannotBeCanceledRevelation
    }

the13thVision :: CardDef
the13thVision =
  (basicWeakness "05041" "The 13th Vision")
    { cdCardTraits = singleton Omen
    }

watchersGrasp :: CardDef
watchersGrasp =
  (treachery "05087" "Watcher's Grasp" TheWatcher 2)
    { cdCardTraits = setFromList [Power, Spectral]
    }

daemonicPiping :: CardDef
daemonicPiping =
  surge
    $ (treachery "05089" "Daemonic Piping" AgentsOfAzathoth 3)
      { cdCardTraits = setFromList [Power, Terror]
      }

diabolicVoices :: CardDef
diabolicVoices =
  (treachery "05092" "Diabolic Voices" Witchcraft 3)
    { cdCardTraits = singleton Curse
    }

wracked :: CardDef
wracked =
  (treachery "05093" "Wracked" Witchcraft 2)
    { cdCardTraits = singleton Hex
    }

bedeviled :: CardDef
bedeviled =
  (treachery "05094" "Bedeviled" Witchcraft 2)
    { cdCardTraits = singleton Hex
    }

mysteriesOfTheLodge :: CardDef
mysteriesOfTheLodge =
  (treachery "05097" "Mysteries of the Lodge" SilverTwilightLodge 2)
    { cdCardTraits = singleton Scheme
    }

evilPast :: CardDef
evilPast =
  (treachery "05098" "Evil Past" CityOfSins 2)
    { cdCardTraits = singleton Curse
    }

centuriesOfSecrets :: CardDef
centuriesOfSecrets =
  (treachery "05099" "Centuries of Secrets" CityOfSins 3)
    { cdCardTraits = singleton Curse
    }

whispersInTheDark :: CardDef
whispersInTheDark =
  (treachery "05102" "Whispers in the Dark" SpectralPredators 2)
    { cdCardTraits = setFromList [Omen, Spectral]
    }

trappedSpirits :: CardDef
trappedSpirits =
  (treachery "05104" "Trapped Spirits" TrappedSpirits 2)
    { cdCardTraits = setFromList [Terror, Spectral]
    }

realmOfTorment :: CardDef
realmOfTorment =
  (treachery "05105" "Realm of Torment" RealmOfDeath 2)
    { cdCardTraits = setFromList [Terror, Spectral]
    }

shapesInTheMist :: CardDef
shapesInTheMist =
  surge
    $ (treachery "05106" "Shapes in the Mist" RealmOfDeath 2)
      { cdCardTraits = setFromList [Terror, Spectral]
      }

terrorInTheNight :: CardDef
terrorInTheNight =
  (treachery "05107" "Terror in the Night" InexorableFate 3)
    { cdCardTraits = setFromList [Terror, Spectral]
    }

fateOfAllFools :: CardDef
fateOfAllFools =
  (treachery "05108" "Fate of All Fools" InexorableFate 3)
    { cdCardTraits = setFromList [Omen, Spectral]
    , cdKeywords = singleton Keyword.Peril
    }

meddlesomeFamiliar :: CardDef
meddlesomeFamiliar =
  (treachery "05143" "Meddlesome Familiar" TheSecretName 3)
    { cdCardTraits = singleton Curse
    }

ghostlyPresence :: CardDef
ghostlyPresence =
  (treachery "05144" "Ghostly Presence" TheSecretName 2)
    { cdCardTraits = singleton Omen
    }

extradimensionalVisions :: CardDef
extradimensionalVisions =
  (treachery "05145" "Extradimensional Visions" TheSecretName 2)
    { cdCardTraits = singleton Hex
    }

pulledByTheStars :: CardDef
pulledByTheStars =
  (treachery "05146" "Pulled by the Stars" TheSecretName 2)
    { cdCardTraits = singleton Hex
    }

disquietingDreams :: CardDef
disquietingDreams =
  (treachery "05147" "Disquieting Dreams" TheSecretName 2)
    { cdCardTraits = singleton Terror
    }

punishment :: CardDef
punishment =
  (treachery "05181" "Punishment" TheWagesOfSin 2)
    { cdCardTraits = singleton Hex
    }

burdensOfThePast :: CardDef
burdensOfThePast =
  (treachery "05182" "Burdens of the Past" TheWagesOfSin 2)
    { cdCardTraits = setFromList [Curse, Spectral]
    }

ominousPortents :: CardDef
ominousPortents =
  (treachery "05183" "Ominous Portents" TheWagesOfSin 2)
    { cdCardTraits = singleton Omen
    , cdKeywords = singleton Keyword.Peril
    }

graveLight :: CardDef
graveLight =
  (treachery "05184" "Grave-Light" TheWagesOfSin 2)
    { cdCardTraits = singleton Curse
    }

-- Gravelight is the only card that cares which encounter deck it is drawn
-- from, so instead we represent it as two cards for which deck it is in.
graveLightSpectral :: CardDef
graveLightSpectral =
  (treachery "x05184" "Grave-Light" TheWagesOfSin 0)
    { cdCardTraits = singleton Curse
    , cdArt = "05184"
    }

baneOfTheLiving :: CardDef
baneOfTheLiving =
  (treachery "05185" "Bane of the Living" TheWagesOfSin 2)
    { cdCardTraits = setFromList [Curse, Spectral]
    , cdKeywords = singleton Keyword.Peril
    }

callToOrder :: CardDef
callToOrder =
  (treachery "05223" "Call to Order" ForTheGreaterGood 2)
    { cdCardTraits = singleton Scheme
    }

expulsion :: CardDef
expulsion =
  (treachery "05224" "Expulsion" ForTheGreaterGood 2)
    { cdCardTraits = singleton Scheme
    }

beneathTheLodge :: CardDef
beneathTheLodge =
  (treachery "05225" "Beneath the Lodge" ForTheGreaterGood 2)
    { cdCardTraits = singleton Scheme
    }

markOfTheOrder :: CardDef
markOfTheOrder =
  (treachery "05226" "Mark of the Order" ForTheGreaterGood 2)
    { cdCardTraits = singleton Scheme
    , cdKeywords = singleton Keyword.Surge
    }

eagerForDeathUnionAndDisillusion :: CardDef
eagerForDeathUnionAndDisillusion =
  (treachery "05268" "Eager for Death" UnionAndDisillusion 2)
    { cdCardTraits = setFromList [Omen]
    }

psychopompsSongUnionAndDisillusion :: CardDef
psychopompsSongUnionAndDisillusion =
  (treachery "05269" "Psychopomp's Song" UnionAndDisillusion 2)
    { cdCardTraits = singleton Omen
    , cdKeywords = setFromList [Keyword.Surge, Keyword.Peril]
    }

deathApproaches :: CardDef
deathApproaches =
  (treachery "05270" "Death Approaches" UnionAndDisillusion 2)
    { cdCardTraits = singleton Terror
    , cdKeywords = setFromList [Keyword.Surge, Keyword.Peril]
    }

markedForDeath :: CardDef
markedForDeath =
  (treachery "05271" "Marked for Death" UnionAndDisillusion 2)
    { cdCardTraits = singleton Curse
    }

watchersGazeUnionAndDisillusion :: CardDef
watchersGazeUnionAndDisillusion =
  (treachery "05272" "Watcher's Gaze" UnionAndDisillusion 1)
    { cdCardTraits = singleton Curse
    }

chaosManifest :: CardDef
chaosManifest =
  (treachery "05306" "Chaos Manifest" InTheClutchesOfChaos 3)
    { cdCardTraits = singleton Power
    }

primordialGateway :: CardDef
primordialGateway =
  (treachery "05307" "Primordial Gateway" InTheClutchesOfChaos 2)
    { cdCardTraits = singleton Power
    }

terrorUnleashed :: CardDef
terrorUnleashed =
  (treachery "05308" "Terror Unleashed" InTheClutchesOfChaos 3)
    { cdCardTraits = singleton Curse
    }

secretsOfTheBeyond :: CardDef
secretsOfTheBeyond =
  (treachery "05310" "Secrets of the Beyond" SecretsOfTheUniverse 2)
    { cdCardTraits = singleton Hex
    }

toilAndTrouble :: CardDef
toilAndTrouble =
  (treachery "05312" "Toil and Trouble" MusicOfTheDamned 2)
    { cdCardTraits = singleton Hex
    , cdKeywords = singleton Keyword.Peril
    }

ultimateChaos :: CardDef
ultimateChaos =
  (treachery "05342" "Ultimate Chaos" BeforeTheBlackThrone 3)
    { cdCardTraits = singleton Power
    , cdRevelation = CannotBeCanceledRevelation
    }

whisperedBargain :: CardDef
whisperedBargain =
  (treachery "05343" "Whispered Bargain" BeforeTheBlackThrone 2)
    { cdCardTraits = singleton Pact
    , cdKeywords = singleton Keyword.Peril
    }

theEndIsNigh :: CardDef
theEndIsNigh =
  (treachery "05344" "The End is Nigh!" BeforeTheBlackThrone 2)
    { cdCardTraits = singleton Endtimes
    }

aWorldInDarkness :: CardDef
aWorldInDarkness =
  (treachery "05345" "A World in Darkness" BeforeTheBlackThrone 2)
    { cdCardTraits = singleton Endtimes
    }
