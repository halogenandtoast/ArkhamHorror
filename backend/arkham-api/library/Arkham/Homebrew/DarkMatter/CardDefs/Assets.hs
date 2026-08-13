module Arkham.Homebrew.DarkMatter.CardDefs.Assets where

import Arkham.Asset.Cards.Import
import Arkham.Homebrew.DarkMatter.Sets qualified as Set
import Arkham.Homebrew.DarkMatter.Traits
import Arkham.LocationSymbol qualified as LS

withScanIcons :: [LS.LocationSymbol] -> CardDef -> CardDef
withScanIcons icons def = def {cdMeta = insertMap "scanIcons" (toJSON icons) def.meta}

-- | The icon printed on the front of a card; see @Helpers.printedIcons@.
withPrintedIcons :: [LS.LocationSymbol] -> CardDef -> CardDef
withPrintedIcons icons def = def {cdMeta = insertMap "printedIcons" (toJSON icons) def.meta}

-- the_tatterdemalion
virtualAccessKey :: CardDef
virtualAccessKey =
  ( storyAsset
      ":dark-matter:021"
      ("Virtual Access Key" <:> "Key to the Gate of Dreams")
      2
      Set.TheTatterdemalion
  )
    { cdCardTraits = setFromList [Item, Charm, Relic]
    , cdSkills = [#willpower, #wild, #wild]
    , cdSlots = [#accessory]
    , cdUnique = True
    }

evaSuit :: CardDef
evaSuit =
  withScanIcons [LS.Square, LS.Moon]
    $ (encounterAsset_ ":dark-matter:030" "EVA Suit" Set.TheTatterdemalion)
      { cdCardTraits = setFromList [Armor, Item]
      , cdSlots = [#body]
      }

heirToCarcosa :: CardDef
heirToCarcosa =
  withScanIcons [LS.Equals]
    $ permanent
    -- player-backed: its revelation adds it to the drawing investigator's deck
    $ ( storyAsset_
          ":dark-matter:032"
          ("Heir to Carcosa" <:> "Untranslated Runes")
          Set.TheTatterdemalion
      )
      { cdCardTraits = setFromList [Tome]
      , cdUnique = True
      }

medicalFoam :: CardDef
medicalFoam =
  withScanIcons [LS.Heart]
    $ (encounterAsset_ ":dark-matter:037" "Medical Foam" Set.TheTatterdemalion)
      { cdCardTraits = setFromList [Medical, Science, Item]
      , cdSlots = [#hand]
      , cdUses = uses Supply 3
      }

mindMachineInterface :: CardDef
mindMachineInterface =
  withScanIcons [LS.Triangle, LS.Plus, LS.Hourglass, LS.T]
    $ (encounterAsset_ ":dark-matter:038" "Mind-Machine Interface" Set.TheTatterdemalion)
      { cdCardTraits = setFromList [Device]
      }

radiationTablets :: CardDef
radiationTablets =
  withScanIcons [LS.Heart]
    $ (encounterAsset_ ":dark-matter:039" "Radiation Tablets" Set.TheTatterdemalion)
      { cdCardTraits = setFromList [Medical, Science, Item]
      , cdUses = uses Supply 3
      }

-- electric_nightmare
maja :: CardDef
maja =
  (encounterAsset_ ":dark-matter:062" ("Maja" <:> "Information Archives") Set.ElectricNightmare)
    { cdCardTraits = setFromList [Avatar]
    , cdUnique = True
    }

alma :: CardDef
alma =
  otherSideIs ":dark-matter:063aa"
    $ (encounterAsset_ ":dark-matter:063ab" ("Alma" <:> "Environmental Controls") Set.ElectricNightmare)
      { cdCardTraits = setFromList [Avatar]
      , cdUnique = True
      }

david :: CardDef
david =
  otherSideIs ":dark-matter:063ba"
    $ (encounterAsset_ ":dark-matter:063bb" ("David" <:> "Engines and System Power") Set.ElectricNightmare)
      { cdCardTraits = setFromList [Avatar]
      , cdUnique = True
      }

tilde :: CardDef
tilde =
  otherSideIs ":dark-matter:063ca"
    $ (encounterAsset_ ":dark-matter:063cb" ("Tilde" <:> "Digital Mainframe") Set.ElectricNightmare)
      { cdCardTraits = setFromList [Avatar]
      , cdUnique = True
      }

william :: CardDef
william =
  otherSideIs ":dark-matter:063da"
    $ (encounterAsset_ ":dark-matter:063db" ("William" <:> "Intrasolar Navigations") Set.ElectricNightmare)
      { cdCardTraits = setFromList [Avatar]
      , cdUnique = True
      }

k2PS18725Functionality :: CardDef
k2PS18725Functionality =
  permanent
    $ (storyAsset_ ":dark-matter:065" ("K2-PS187" <:> "25% Functionality") Set.ElectricNightmare)
      { cdCardTraits = setFromList [AI]
      , cdUnique = True
      }

k2PS18750Functionality :: CardDef
k2PS18750Functionality =
  permanent
    $ (storyAsset_ ":dark-matter:066" ("K2-PS187" <:> "50% Functionality") Set.ElectricNightmare)
      { cdCardTraits = setFromList [AI]
      , cdUnique = True
      }

k2PS18775Functionality :: CardDef
k2PS18775Functionality =
  permanent
    $ (storyAsset_ ":dark-matter:067" ("K2-PS187" <:> "75% Functionality") Set.ElectricNightmare)
      { cdCardTraits = setFromList [AI]
      , cdUnique = True
      }

k2PS187100Functionality :: CardDef
k2PS187100Functionality =
  permanent
    $ (storyAsset_ ":dark-matter:068" ("K2-PS187" <:> "100% Functionality") Set.ElectricNightmare)
      { cdCardTraits = setFromList [AI]
      , cdUnique = True
      }

-- lost_quantum
erwinSimmonsFading :: CardDef
erwinSimmonsFading =
  (encounterAsset_ ":dark-matter:094" ("Erwin Simmons" <:> "Fading") Set.LostQuantum)
    { cdCardTraits = setFromList [Scientist, Human, Ally]
    , cdUnique = True
    }

erwinSimmonsQuantumPhysicist :: CardDef
erwinSimmonsQuantumPhysicist =
  (storyAsset ":dark-matter:095" ("Erwin Simmons" <:> "Quantum Physicist") 3 Set.LostQuantum)
    { cdCardTraits = setFromList [Scientist, Human, Ally]
    , cdSkills = [#wild, #wild]
    , cdSlots = [#ally]
    , cdUnique = True
    }

-- in_the_shadow_of_earth
spaceArtillery :: CardDef
spaceArtillery =
  (storyAsset ":dark-matter:120" "Space Artillery" 4 Set.InTheShadowOfEarth)
    { cdCardTraits = setFromList [NostalgiaII, Weapon, Ranged]
    , cdSkills = [#combat, #combat, #combat]
    , cdUnique = True
    , cdUses = uses Supply 2
    }

adamTanner :: CardDef
adamTanner =
  withScanIcons [LS.Diamond, LS.Equals]
    $ (encounterAsset_ ":dark-matter:130" "Adam Tanner" Set.InTheShadowOfEarth)
      { cdCardTraits = setFromList [Ally, Crew]
      , cdUnique = True
      }

captainBurr :: CardDef
captainBurr =
  withScanIcons [LS.T, LS.Hourglass]
    $ (encounterAsset_ ":dark-matter:131" "Captain Burr" Set.InTheShadowOfEarth)
      { cdCardTraits = setFromList [Ally, Crew]
      , cdUnique = True
      }

doctorFeng :: CardDef
doctorFeng =
  withScanIcons [LS.Trefoil, LS.Diamond]
    $ (encounterAsset_ ":dark-matter:132" "Doctor Feng" Set.InTheShadowOfEarth)
      { cdCardTraits = setFromList [Ally, Crew]
      , cdUnique = True
      }

ltArcherMichaels :: CardDef
ltArcherMichaels =
  withScanIcons [LS.Equals, LS.T]
    $ (encounterAsset_ ":dark-matter:133" "Lt. \"Archer\" Michaels" Set.InTheShadowOfEarth)
      { cdCardTraits = setFromList [Ally, Crew]
      , cdUnique = True
      }

muD12Mudbug :: CardDef
muD12Mudbug =
  withScanIcons [LS.Square, LS.Trefoil]
    $ (encounterAsset_ ":dark-matter:134" "MU-D12 \"Mudbug\"" Set.InTheShadowOfEarth)
      { cdCardTraits = setFromList [Ally, Crew]
      , cdUnique = True
      }

sophie :: CardDef
sophie =
  withScanIcons [LS.Hourglass, LS.Square]
    $ (encounterAsset_ ":dark-matter:135" "Sophie" Set.InTheShadowOfEarth)
      { cdCardTraits = setFromList [Ally, Crew]
      , cdUnique = True
      }

-- strange_moons
brainCylinder089 :: CardDef
brainCylinder089 =
  withPrintedIcons [LS.Square]
    $ (encounterAsset_ ":dark-matter:160" "Brain Cylinder 089" Set.StrangeMoons)
      { cdCardTraits = setFromList [Brain]
      }

brainCylinder114 :: CardDef
brainCylinder114 =
  withPrintedIcons [LS.Equals]
    $ (encounterAsset_ ":dark-matter:161" "Brain Cylinder 114" Set.StrangeMoons)
      { cdCardTraits = setFromList [Brain]
      }

brainCylinder367 :: CardDef
brainCylinder367 =
  withPrintedIcons [LS.Diamond]
    $ (encounterAsset_ ":dark-matter:162" "Brain Cylinder 367" Set.StrangeMoons)
      { cdCardTraits = setFromList [Brain]
      }

-- fragment_of_carcosa
bottleOfWhispers :: CardDef
bottleOfWhispers =
  ( storyAsset
      ":dark-matter:215"
      ("Bottle of Whispers" <:> "It Was Meant for You")
      1
      Set.FragmentOfCarcosa
  )
    { cdCardTraits = setFromList [Item]
    }

-- starfall
projectOrigami :: CardDef
projectOrigami =
  withScanIcons [LS.Diamond]
    $ encounterAsset_ ":dark-matter:267" "Project Origami" Set.Starfall

lastHope :: CardDef
lastHope =
  withScanIcons [LS.Circle, LS.Triangle]
    $ encounterAsset_ ":dark-matter:268" "Last Hope" Set.Starfall

repairingTheThreshold :: CardDef
repairingTheThreshold =
  withScanIcons [LS.Trefoil]
    $ encounterAsset_ ":dark-matter:269" "Repairing the Threshold" Set.Starfall

arNO :: CardDef
arNO =
  withScanIcons [LS.Diamond]
    $ (encounterAsset_ ":dark-matter:270" ("Ar-NO" <:> "Insufficient Data") Set.Starfall)
      { cdCardTraits = setFromList [AI]
      }

directorCixin :: CardDef
directorCixin =
  withScanIcons [LS.Circle, LS.Triangle]
    $ (encounterAsset_ ":dark-matter:271" ("Director Cixin" <:> "Hope is in Danger") Set.Starfall)
      { cdCardTraits = setFromList [Human]
      , cdUnique = True
      }

miGoCollector :: CardDef
miGoCollector =
  withScanIcons [LS.Trefoil]
    $ (encounterAsset_ ":dark-matter:272" ("Mi-Go Collector" <:> "Distrustful") Set.Starfall)
      { cdCardTraits = setFromList [MiGo]
      }

thePallidMask :: CardDef
thePallidMask =
  withScanIcons [LS.Droplet, LS.Plus]
    $ (encounterAsset_ ":dark-matter:276" "The Pallid Mask" Set.Starfall)
      { cdCardTraits = setFromList [Item, Relic]
      , cdUnique = True
      }

k11SurveyUnit :: CardDef
k11SurveyUnit =
  withScanIcons [LS.Equals, LS.Hourglass]
    $ (encounterAsset_ ":dark-matter:278" "K-11 Survey Unit" Set.Starfall)
      { cdCardTraits = setFromList [Ally, AI]
      }

shieldingDevice :: CardDef
shieldingDevice =
  withScanIcons [LS.Equals, LS.Diamond]
    $ (encounterAsset_ ":dark-matter:279" ("Shielding Device" <:> "Electromagnetic Barrier") Set.Starfall)
      { cdCardTraits = setFromList [Item]
      , cdUnique = True
      }

stasisCube :: CardDef
stasisCube =
  withScanIcons [LS.Square, LS.Droplet]
    $ (encounterAsset_ ":dark-matter:280" ("Stasis Cube" <:> "Timeless Artifact") Set.Starfall)
      { cdCardTraits = setFromList [Item, Relic]
      , cdUnique = True
      }

universalArchives :: CardDef
universalArchives =
  withScanIcons [LS.Moon, LS.Trefoil, LS.Hourglass]
    $ (encounterAsset_ ":dark-matter:281" ("Universal Archives" <:> "Theory of Everything") Set.Starfall)
      { cdCardTraits = setFromList [Data]
      , cdUnique = True
      }
