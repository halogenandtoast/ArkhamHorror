{-# LANGUAGE PatternSynonyms #-}

{- | Trait values contributed by homebrew content, living entirely in the
homebrew layer so core 'Arkham.Trait.Trait' never needs editing to add them.

Each trait is a bidirectional pattern synonym over the open
'Arkham.Trait.HomebrewTrait' escape hatch, so homebrew card code references a
named, typo-checked constructor exactly as it would a core trait — in both
construction (@[Tatterdemalion, Access]@) and matching. The 'Text' tag equals
the trait's name so serialization is identical to a plain enum constructor.

Adding a campaign's traits means adding synonyms here (and to 'homebrewTraits'),
never touching core. If a trait name collides with something in a card module's
scope (e.g. the @Moon@ location symbol), import this module qualified or with a
@hiding@ clause there, just as core modules already do for core traits.
-}
module Arkham.Homebrew.Traits (
  homebrewTraits,
  allTraits,
  pattern AI,
  pattern Access,
  pattern Alien,
  pattern AsteroidBelt,
  pattern Brain,
  pattern Camp,
  pattern Carcosa,
  pattern CircusTrain,
  pattern Clearing,
  pattern Colony,
  pattern Data,
  pattern Device,
  pattern Earth,
  pattern Elbrus,
  pattern FreightCar,
  pattern Interface,
  pattern LiberPater,
  pattern Liminal,
  pattern Machine,
  pattern Mars,
  pattern Medical,
  pattern Memory,
  pattern Moon,
  pattern NewMoonCircus,
  pattern Nightmare,
  pattern NostalgiaII,
  pattern Path,
  pattern Pluto,
  pattern Quantum,
  pattern School,
  pattern Simulation,
  pattern SpecialCar,
  pattern Starship,
  pattern Tainted,
  pattern Tatterdemalion,
  pattern Virtual,
) where

import Arkham.Prelude
import Arkham.Trait (Trait (HomebrewTrait), coreTraits)

pattern AI :: Trait
pattern AI = HomebrewTrait "AI"

pattern Access :: Trait
pattern Access = HomebrewTrait "Access"

pattern Alien :: Trait
pattern Alien = HomebrewTrait "Alien"

pattern AsteroidBelt :: Trait
pattern AsteroidBelt = HomebrewTrait "AsteroidBelt"

pattern Brain :: Trait
pattern Brain = HomebrewTrait "Brain"

pattern Camp :: Trait
pattern Camp = HomebrewTrait "Camp"

pattern Carcosa :: Trait
pattern Carcosa = HomebrewTrait "Carcosa"

pattern CircusTrain :: Trait
pattern CircusTrain = HomebrewTrait "CircusTrain"

pattern Clearing :: Trait
pattern Clearing = HomebrewTrait "Clearing"

pattern Colony :: Trait
pattern Colony = HomebrewTrait "Colony"

pattern Data :: Trait
pattern Data = HomebrewTrait "Data"

pattern Device :: Trait
pattern Device = HomebrewTrait "Device"

pattern Earth :: Trait
pattern Earth = HomebrewTrait "Earth"

pattern Elbrus :: Trait
pattern Elbrus = HomebrewTrait "Elbrus"

pattern FreightCar :: Trait
pattern FreightCar = HomebrewTrait "FreightCar"

pattern Interface :: Trait
pattern Interface = HomebrewTrait "Interface"

pattern LiberPater :: Trait
pattern LiberPater = HomebrewTrait "LiberPater"

pattern Liminal :: Trait
pattern Liminal = HomebrewTrait "Liminal"

pattern Machine :: Trait
pattern Machine = HomebrewTrait "Machine"

pattern Mars :: Trait
pattern Mars = HomebrewTrait "Mars"

pattern Medical :: Trait
pattern Medical = HomebrewTrait "Medical"

pattern Memory :: Trait
pattern Memory = HomebrewTrait "Memory"

pattern Moon :: Trait
pattern Moon = HomebrewTrait "Moon"

pattern NewMoonCircus :: Trait
pattern NewMoonCircus = HomebrewTrait "NewMoonCircus"

pattern Nightmare :: Trait
pattern Nightmare = HomebrewTrait "Nightmare"

pattern NostalgiaII :: Trait
pattern NostalgiaII = HomebrewTrait "NostalgiaII"

pattern Path :: Trait
pattern Path = HomebrewTrait "Path"

pattern Pluto :: Trait
pattern Pluto = HomebrewTrait "Pluto"

pattern Quantum :: Trait
pattern Quantum = HomebrewTrait "Quantum"

pattern School :: Trait
pattern School = HomebrewTrait "School"

pattern Simulation :: Trait
pattern Simulation = HomebrewTrait "Simulation"

pattern SpecialCar :: Trait
pattern SpecialCar = HomebrewTrait "SpecialCar"

pattern Starship :: Trait
pattern Starship = HomebrewTrait "Starship"

pattern Tainted :: Trait
pattern Tainted = HomebrewTrait "Tainted"

pattern Tatterdemalion :: Trait
pattern Tatterdemalion = HomebrewTrait "Tatterdemalion"

pattern Virtual :: Trait
pattern Virtual = HomebrewTrait "Virtual"

-- | Every homebrew trait. Extend this list when adding synonyms above so they
-- appear anywhere the full trait universe is enumerated ('allTraits').
homebrewTraits :: [Trait]
homebrewTraits =
  [ AI
  , Access
  , Alien
  , AsteroidBelt
  , Brain
  , Camp
  , Carcosa
  , CircusTrain
  , Clearing
  , Colony
  , Data
  , Device
  , Earth
  , Elbrus
  , FreightCar
  , Interface
  , LiberPater
  , Liminal
  , Machine
  , Mars
  , Medical
  , Memory
  , Moon
  , NewMoonCircus
  , Nightmare
  , NostalgiaII
  , Path
  , Pluto
  , Quantum
  , School
  , Simulation
  , SpecialCar
  , Starship
  , Tainted
  , Tatterdemalion
  , Virtual
  ]

-- | The full trait universe: core traits plus every homebrew trait. Use in
-- place of the old @[minBound .. maxBound] :: [Trait]@.
allTraits :: [Trait]
allTraits = coreTraits <> homebrewTraits
