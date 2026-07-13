module Arkham.Asset.Cards.DarkMatter where

import Arkham.Asset.Cards.Import
import Arkham.LocationSymbol qualified as LS

withScanIcons :: [LS.LocationSymbol] -> CardDef -> CardDef
withScanIcons icons def = def {cdMeta = insertMap "scanIcons" (toJSON icons) def.meta}

-- the_tatterdemalion
virtualAccessKeyDarkMatter :: CardDef
virtualAccessKeyDarkMatter =
  (storyAsset "z-dark-matter-020" ("Virtual Access Key" <:> "Key to the Gate of Dreams") 2 DarkMatterTheTatterdemalion)
      { cdCardTraits = setFromList [Item, Charm, Relic]
      , cdSkills = [#willpower, #wild, #wild]
      , cdSlots = [#accessory]
      , cdUnique = True
      }

evaSuitDarkMatter :: CardDef
evaSuitDarkMatter =
  withScanIcons [LS.Square, LS.Moon]
    $ (storyAsset_ "z-dark-matter-029" "EVA Suit" DarkMatterTheTatterdemalion)
        { cdCardTraits = setFromList [Armor, Item]
        , cdSlots = [#body]
        }

heirToCarcosaDarkMatter :: CardDef
heirToCarcosaDarkMatter =
  withScanIcons [LS.Equals]
    $ permanent
      $ (storyAsset_ "z-dark-matter-031" ("Heir to Carcosa" <:> "Untranslated Runes") DarkMatterTheTatterdemalion)
        { cdCardTraits = setFromList [Tome]
        , cdUnique = True
        }

medicalFoamDarkMatter :: CardDef
medicalFoamDarkMatter =
  withScanIcons [LS.Heart]
    $ (storyAsset_ "z-dark-matter-036" "Medical Foam" DarkMatterTheTatterdemalion)
        { cdCardTraits = setFromList [Medical, Science, Item]
        , cdSlots = [#hand]
        }

mindMachineInterfaceDarkMatter :: CardDef
mindMachineInterfaceDarkMatter =
  withScanIcons [LS.Triangle, LS.Plus, LS.Hourglass, LS.T]
    $ (storyAsset_ "z-dark-matter-037" "Mind-Machine Interface" DarkMatterTheTatterdemalion) {cdCardTraits = setFromList [Device]}

radiationTabletsDarkMatter :: CardDef
radiationTabletsDarkMatter =
  withScanIcons [LS.Heart]
    $ (storyAsset_ "z-dark-matter-038" "Radiation Tablets" DarkMatterTheTatterdemalion) {cdCardTraits = setFromList [Medical, Science, Item]}

-- electric_nightmare
majaDarkMatter :: CardDef
majaDarkMatter =
  (storyAsset_ "z-dark-matter-061" ("Maja" <:> "Information Archives") DarkMatterElectricNightmare)
      { cdCardTraits = setFromList [Avatar]
      , cdUnique = True
      }

k2PS18725FunctionalityDarkMatter :: CardDef
k2PS18725FunctionalityDarkMatter =
  permanent
      $ (storyAsset_ "z-dark-matter-067" ("K2-PS187" <:> "25% Functionality") DarkMatterElectricNightmare)
        { cdCardTraits = setFromList [AI]
        , cdUnique = True
        }

k2PS18750FunctionalityDarkMatter :: CardDef
k2PS18750FunctionalityDarkMatter =
  permanent
      $ (storyAsset_ "z-dark-matter-068" ("K2-PS187" <:> "50% Functionality") DarkMatterElectricNightmare)
        { cdCardTraits = setFromList [AI]
        , cdUnique = True
        }

k2PS18775FunctionalityDarkMatter :: CardDef
k2PS18775FunctionalityDarkMatter =
  permanent
      $ (storyAsset_ "z-dark-matter-069" ("K2-PS187" <:> "75% Functionality") DarkMatterElectricNightmare)
        { cdCardTraits = setFromList [AI]
        , cdUnique = True
        }

k2PS187100FunctionalityDarkMatter :: CardDef
k2PS187100FunctionalityDarkMatter =
  permanent
      $ (storyAsset_ "z-dark-matter-070" ("K2-PS187" <:> "100% Functionality") DarkMatterElectricNightmare)
        { cdCardTraits = setFromList [AI]
        , cdUnique = True
        }

-- lost_quantum
erwinSimmonsFadingDarkMatter :: CardDef
erwinSimmonsFadingDarkMatter =
  (storyAsset_ "z-dark-matter-097" ("Erwin Simmons" <:> "Fading") DarkMatterLostQuantum)
      { cdCardTraits = setFromList [Scientist, Human, Ally]
      , cdUnique = True
      }

erwinSimmonsQuantumPhysicistDarkMatter :: CardDef
erwinSimmonsQuantumPhysicistDarkMatter =
  (storyAsset "z-dark-matter-098" ("Erwin Simmons" <:> "Quantum Physicist") 3 DarkMatterLostQuantum)
      { cdCardTraits = setFromList [Scientist, Human, Ally]
      , cdSkills = [#wild, #wild]
      , cdSlots = [#ally]
      , cdUnique = True
      }

-- in_the_shadow_of_earth
spaceArtilleryDarkMatter :: CardDef
spaceArtilleryDarkMatter =
  (storyAsset "z-dark-matter-123" "Space Artillery" 4 DarkMatterInTheShadowOfEarth)
      { cdCardTraits = setFromList [NostalgiaII, Weapon, Ranged]
      , cdSkills = [#combat, #combat, #combat]
      , cdUnique = True
      }

adamTannerDarkMatter :: CardDef
adamTannerDarkMatter =
  (storyAsset_ "z-dark-matter-133" "Adam Tanner" DarkMatterInTheShadowOfEarth)
      { cdCardTraits = setFromList [Ally, Crew]
      , cdUnique = True
      }

captainBurrDarkMatter :: CardDef
captainBurrDarkMatter =
  (storyAsset_ "z-dark-matter-134" "Captain Burr" DarkMatterInTheShadowOfEarth)
      { cdCardTraits = setFromList [Ally, Crew]
      , cdUnique = True
      }

doctorFengDarkMatter :: CardDef
doctorFengDarkMatter =
  (storyAsset_ "z-dark-matter-135" "Doctor Feng" DarkMatterInTheShadowOfEarth)
      { cdCardTraits = setFromList [Ally, Crew]
      , cdUnique = True
      }

ltArcherMichaelsDarkMatter :: CardDef
ltArcherMichaelsDarkMatter =
  (storyAsset_ "z-dark-matter-136" "Lt. \"Archer\" Michaels" DarkMatterInTheShadowOfEarth)
      { cdCardTraits = setFromList [Ally, Crew]
      , cdUnique = True
      }

muD12MudbugDarkMatter :: CardDef
muD12MudbugDarkMatter =
  (storyAsset_ "z-dark-matter-137" "MU-D12 \"Mudbug\"" DarkMatterInTheShadowOfEarth)
      { cdCardTraits = setFromList [Ally, Crew]
      , cdUnique = True
      }

sophieDarkMatter :: CardDef
sophieDarkMatter =
  (storyAsset_ "z-dark-matter-138" "Sophie" DarkMatterInTheShadowOfEarth)
      { cdCardTraits = setFromList [Ally, Crew]
      , cdUnique = True
      }

-- strange_moons
brainCylinder089DarkMatter :: CardDef
brainCylinder089DarkMatter =
  (storyAsset_ "z-dark-matter-163" "Brain Cylinder 089" DarkMatterStrangeMoons) {cdCardTraits = setFromList [Brain]}

brainCylinder114DarkMatter :: CardDef
brainCylinder114DarkMatter =
  (storyAsset_ "z-dark-matter-164" "Brain Cylinder 114" DarkMatterStrangeMoons) {cdCardTraits = setFromList [Brain]}

brainCylinder367DarkMatter :: CardDef
brainCylinder367DarkMatter =
  (storyAsset_ "z-dark-matter-165" "Brain Cylinder 367" DarkMatterStrangeMoons) {cdCardTraits = setFromList [Brain]}

-- fragment_of_carcosa
bottleOfWhispersDarkMatter :: CardDef
bottleOfWhispersDarkMatter =
  (storyAsset "z-dark-matter-218" ("Bottle of Whispers" <:> "It Was Meant for You") 1 DarkMatterFragmentOfCarcosa) {cdCardTraits = setFromList [Item]}

-- starfall
projectOrigamiDarkMatter :: CardDef
projectOrigamiDarkMatter = (storyAsset_ "z-dark-matter-270" "Project Origami" DarkMatterStarfall)

lastHopeDarkMatter :: CardDef
lastHopeDarkMatter = (storyAsset_ "z-dark-matter-271" "Last Hope" DarkMatterStarfall)

repairingTheThresholdDarkMatter :: CardDef
repairingTheThresholdDarkMatter =
  (storyAsset_ "z-dark-matter-272" "Repairing the Threshold" DarkMatterStarfall)

arNODarkMatter :: CardDef
arNODarkMatter =
  (storyAsset_ "z-dark-matter-273" ("Ar-NO" <:> "Insufficient Data") DarkMatterStarfall) {cdCardTraits = setFromList [AI]}

directorCixinDarkMatter :: CardDef
directorCixinDarkMatter =
  (storyAsset_ "z-dark-matter-274" ("Director Cixin" <:> "Hope is in Danger") DarkMatterStarfall)
      { cdCardTraits = setFromList [Human]
      , cdUnique = True
      }

miGoCollectorDarkMatter :: CardDef
miGoCollectorDarkMatter =
  (storyAsset_ "z-dark-matter-275" ("Mi-Go Collector" <:> "Distrustful") DarkMatterStarfall) {cdCardTraits = setFromList [MiGo]}

thePallidMaskDarkMatter :: CardDef
thePallidMaskDarkMatter =
  (storyAsset_ "z-dark-matter-279" "The Pallid Mask" DarkMatterStarfall)
      { cdCardTraits = setFromList [Item, Relic]
      , cdUnique = True
      }

k11SurveyUnitDarkMatter :: CardDef
k11SurveyUnitDarkMatter =
  (storyAsset_ "z-dark-matter-281" "K-11 Survey Unit" DarkMatterStarfall) {cdCardTraits = setFromList [Ally, AI]}

shieldingDeviceDarkMatter :: CardDef
shieldingDeviceDarkMatter =
  (storyAsset_ "z-dark-matter-282" ("Shielding Device" <:> "Electromagnetic Barrier") DarkMatterStarfall)
      { cdCardTraits = setFromList [Item]
      , cdUnique = True
      }

stasisCubeDarkMatter :: CardDef
stasisCubeDarkMatter =
  (storyAsset_ "z-dark-matter-283" ("Stasis Cube" <:> "Timeless Artifact") DarkMatterStarfall)
      { cdCardTraits = setFromList [Item, Relic]
      , cdUnique = True
      }

universalArchivesDarkMatter :: CardDef
universalArchivesDarkMatter =
  (storyAsset_ "z-dark-matter-284" ("Universal Archives" <:> "Theory of Everything") DarkMatterStarfall)
      { cdCardTraits = setFromList [Data]
      , cdUnique = True
      }
