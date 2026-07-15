module Arkham.Trait (displayTrait, Trait (..), EnemyTrait (..), HasTraits (..), coreTraits) where

import Arkham.Prelude
import Data.Data (dataTypeConstrs, dataTypeOf, fromConstr, showConstr)
import Data.Map.Strict qualified as Map

newtype EnemyTrait = EnemyTrait {unEnemyTrait :: Trait}

data Trait
  = Abandoned
  | Abomination
  | Abyss
  | Agency
  | Alchemy
  | Alexandria
  | Allied
  | Ally
  | Altered
  | Ancient
  | AncientOne
  | Apiary
  | Apparel
  | Arkham
  | ArkhamAsylum
  | Armor
  | Artifact
  | Artist
  | Assistant
  | Attack
  | Augury
  | Avatar
  | Bane
  | Basement
  | Bayou
  | Bazaar
  | Believer
  | Blessed
  | Blight
  | Blunder
  | Boat
  | Bog
  | Bold
  | Boon
  | Bridge
  | Broken
  | Brotherhood
  | BuenosAires
  | Byakhee
  | Bystander
  | Cairo
  | Campsite
  | Carnevale
  | Cart
  | Casino
  | Castle
  | Cave
  | Central
  | Charm
  | Chosen
  | Circle
  | City
  | Civic
  | Civilian
  | Clairvoyant
  | Clothing
  | CloverClub
  | Coastal
  | Colour
  | Completed
  | Composure
  | Condition
  | Connection
  | Conspirator
  | Construct
  | Corruption
  | Cosmos
  | Coterie
  | Courage
  | Covenant
  | Creature
  | Crew
  | CrimeScene
  | Criminal
  | Cthulhu
  | Cultist
  | Curse
  | Cursed
  | Dark
  | DarkYoung
  | DeepOne
  | Depths
  | Desert
  | Desperate
  | Detective
  | Developed
  | Dhole
  | Dilemma
  | Dinosaur
  | Distortion
  | Dormant
  | Double
  | Dreamer
  | Dreamlands
  | Drifter
  | Dunwich
  | Eidolon
  | ElderThing
  | Eldritch
  | Elite
  | Emissary
  | Enclave
  | Endtimes
  | Enraged
  | Entrepreneur
  | Evidence
  | Exhibit
  | Expedition
  | Expert
  | Extradimensional
  | Extraterrestrial
  | Eztli
  | FalconPoint
  | Familiar
  | Farm
  | Fated
  | Favor
  | Field
  | Firearm
  | Flaw
  | Flora
  | Footwear
  | Forbidden
  | Forest
  | Forgotten
  | Fortune
  | Future
  | Gambit
  | Game
  | Front
  | Geist
  | Ghast
  | Ghoul
  | Glacier
  | Glyph
  | Government
  | Grant
  | Graveyard
  | GroundFloor
  | Guest
  | Gug
  | Hall
  | Hardship
  | Haunted
  | Havana
  | Hazard
  | Headwear
  | HemlockVale
  | Hex
  | Hideout
  | HistoricalSociety
  | Human
  | Humanoid
  | Hunter
  | Hybrid
  | Illicit
  | Improvised
  | Inconspicious
  | Injury
  | Innate
  | Innocent
  | Innsmouth
  | Insect
  | Insight
  | Instrument
  | Island
  | Istanbul
  | Item
  | Job
  | Jungle
  | Kadath
  | Keeper
  | Key
  | Kingsport
  | KualaLumpur
  | Lair
  | LanternClub
  | Lead
  | Leader
  | Leng
  | Lift
  | Lit
  | LocusSite
  | Lodge
  | London
  | Lunatic
  | Machination
  | Madness
  | Mainland
  | Manifold
  | Manor
  | Marrakesh
  | Mask
  | Medic
  | Melee
  | MexicoCity
  | MiGo
  | Midtown
  | Mirage
  | Misfortune
  | Miskatonic
  | Mnar
  | Monster
  | Montreal
  | Mountain
  | Mountains
  | Mutated
  | Mutation
  | Mystery
  | Nest
  | NewOrleans
  | NewYorkCity
  | Nightgaunt
  | Obstacle
  | Occult
  | Ocean
  | Omen
  | OothNargai
  | Ooze
  | Oozified
  | Oriab
  | Otherworld
  | Outsider
  | Pact
  | Paradox
  | Paris
  | Part1
  | Passageway
  | Past
  | Patron
  | Performer
  | Plot
  | Pnakotus
  | Poison
  | Police
  | Port
  | Portal
  | Possessed
  | Power
  | Practiced
  | Present
  | Prison
  | Profession
  | PresentDay
  | Private
  | Prop
  | Providence
  | Public
  | Rail
  | Ranged
  | Relic
  | Reporter
  | Research
  | Resident
  | Resolute
  | Restricted
  | Risen
  | Rlyeh
  | Ritual
  | RitualSite
  | Rival
  | River
  | Riverside
  | Road
  | Role
  | Room
  | Rooftop
  | Rot
  | Ruined
  | Ruins
  | Salem
  | Sanctum
  | Satellite
  | Saturnite
  | Scheme
  | Scholar
  | Science
  | Scientist
  | Scion
  | Script
  | Seafloor
  | SecondFloor
  | SentinelHill
  | Serpent
  | Service
  | Servitor
  | Set
  | Sewer
  | Shantak
  | Shattered
  | Ship
  | Shoggoth
  | SilverTwilight
  | Skai
  | Socialite
  | Song
  | Sorcerer
  | Spectral
  | Spell
  | Spider
  | Spirit
  | StMarys
  | Stable
  | Staff
  | StarSpawn
  | Station
  | Steps
  | Stowaway
  | Summit
  | Summon
  | Sunken
  | Supply
  | Surface
  | Suspect
  | Syndicate
  | Synergy
  | Tactic
  | Talent
  | Tarot
  | Task
  | Incomplete
  | Temple
  | Tenochtitlan
  | Tentacle
  | Terror
  | ThirdFloor
  | Tindalos
  | Tome
  | Tool
  | Tower
  | Town
  | Train
  | Trap
  | Trick
  | Unbroken
  | Uncharted
  | Unhallowed
  | Unpracticed
  | Unstable
  | Upgrade
  | Vale
  | Vault
  | Vehicle
  | Venice
  | Veteran
  | Void
  | Walkway
  | Warden
  | Wastes
  | Wayfarer
  | Weapon
  | Wilderness
  | Witch
  | WitchHouse
  | Woods
  | Worker
  | Yhanthlei
  | Yithian
  | Yoth
  | Yuggoth
  | Zoog
  | -- | Open extension point for homebrew content. Do not use directly; each
    -- homebrew campaign owns and exposes named, bidirectional pattern synonyms
    -- over this (see its @Traits.hs@, e.g. @Arkham.Homebrew.DarkMatter.Traits@)
    -- so card code stays typo-checked. The 'Text' tag equals the trait's name,
    -- so serialization matches a plain enum constructor and needs no migration.
    HomebrewTrait Text
  deriving stock (Show, Eq, Generic, Ord, Read, Data)
  deriving anyclass (Hashable)

-- | Core traits serialize as their bare constructor name (as the derived
-- all-nullary encoding did); a 'HomebrewTrait' serializes as its tag, which by
-- construction equals the old constructor name, so existing saves round-trip.
instance ToJSON Trait where
  toJSON = \case
    HomebrewTrait t -> toJSON t
    t -> toJSON (tshow t)

instance FromJSON Trait where
  parseJSON = withText "Trait" $ \t ->
    pure $ Map.findWithDefault (HomebrewTrait t) t coreTraitsByName

-- | Every non-homebrew trait. Replaces @[minBound .. maxBound]@ now that 'Trait'
-- carries the open 'HomebrewTrait' constructor and can no longer derive 'Enum'.
-- For the full set including homebrew, use @Arkham.Homebrew.Defs.allTraits@.
coreTraits :: [Trait]
coreTraits =
  [ fromConstr con
  | con <- dataTypeConstrs (dataTypeOf (HomebrewTrait ""))
  , showConstr con /= "HomebrewTrait"
  ]

coreTraitsByName :: Map Text Trait
coreTraitsByName = Map.fromList [(tshow t, t) | t <- coreTraits]

class HasTraits a where
  toTraits :: a -> Set Trait

displayTrait :: Trait -> Text
displayTrait (HomebrewTrait t) = pack $ splitCamelCase $ unpack t
displayTrait t = pack $ splitCamelCase $ show t
