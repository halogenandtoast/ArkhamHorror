{- | And Then There Were None's enemies.

Ten of them are the far side of a [[Victim]] story asset (the fronts live in
"Arkham.Homebrew.AndThenThereWereNone.CardDefs.Assets"); act 1b flips exactly
one, and only that one is ever an enemy. Each is [[Elite]], worth 1 victory
point, and has 10 health per investigator, so the whole of act 2 is spent
killing it while its own text bends the four copies of Antediluvian Sigils.

The five Restless Spirits share a name and differ in everything else, so each
module carries its collector number. Each spawns at one of two named guests'
locations, which is why they read the [[Victim]] cards rather than the map.
-}
module Arkham.Homebrew.AndThenThereWereNone.CardDefs.Enemies where

import Arkham.Enemy.CardDefs.Import
import Arkham.Homebrew.AndThenThereWereNone.Sets qualified as Set
import Arkham.Homebrew.AndThenThereWereNone.Traits (pattern Culprit)
import Arkham.Keyword qualified as Keyword

{- | A guest, unmasked. Every culprit shares its traits, its 10[per_investigator]
health, its Victory 1 and its uniqueness; only the numbers differ.
-}
culprit :: CardCode -> Name -> Int -> Int -> Int -> Int -> CardDef
culprit cardCode name fightValue evadeValue damageValue horrorValue =
  (enemy cardCode name Set.AndThenThereWereNone 1)
    { cdCardTraits = setFromList [Culprit, Geist, Monster, Elite]
    , cdFight = fight fightValue
    , cdHealth = healthPerInvestigator 10
    , cdEvade = evade evadeValue
    , cdHealthDamage = healthDamage damageValue
    , cdSanityDamage = sanityDamage horrorValue
    , cdUnique = True
    , cdDoubleSided = True
    , cdOtherSide = Just (flippedCardCode cardCode)
    , cdVictoryPoints = Just 1
    }

-- | One of the five spectres haunting the island. All [[Geist]], all Hunter.
restlessSpirit :: CardCode -> Int -> Int -> Int -> Int -> Int -> [Keyword.Keyword] -> CardDef
restlessSpirit cardCode fightValue healthValue evadeValue damageValue horrorValue keywords =
  (enemy cardCode "Restless Spirit" Set.AndThenThereWereNone 1)
    { cdCardTraits = setFromList [Monster, Geist]
    , cdFight = fight fightValue
    , cdHealth = health healthValue
    , cdEvade = evade evadeValue
    , cdHealthDamage = healthDamage damageValue
    , cdSanityDamage = sanityDamage horrorValue
    , cdKeywords = setFromList (Keyword.Hunter : keywords)
    }

anthonyMarstonDelusionsOfGrandeur :: CardDef
anthonyMarstonDelusionsOfGrandeur =
  culprit
    ":and-then-there-were-none:011b"
    ("Anthony Marston" <:> "Delusions of Grandeur")
    3
    2
    0
    2

edwardArmstrongDreamingDeeply :: CardDef
edwardArmstrongDreamingDeeply =
  culprit ":and-then-there-were-none:012b" ("Edward Armstrong" <:> "Dreaming Deeply") 3 3 0 2

emilyBrentInATangledWeb :: CardDef
emilyBrentInATangledWeb =
  culprit ":and-then-there-were-none:013b" ("Emily Brent" <:> "In a Tangled Web") 2 2 0 2

ethelRogersMaidenNameMarsh :: CardDef
ethelRogersMaidenNameMarsh =
  culprit ":and-then-there-were-none:014b" ("Ethel Rogers" <:> "Maiden Name Marsh") 3 3 1 1

johnMacArthurAmountedToNothing :: CardDef
johnMacArthurAmountedToNothing =
  culprit ":and-then-there-were-none:015b" ("John MacArthur" <:> "Amounted to Nothing") 2 5 0 0

lawrenceWargraveManOfManyFaces :: CardDef
lawrenceWargraveManOfManyFaces =
  culprit ":and-then-there-were-none:016b" ("Lawrence Wargrave" <:> "Man of Many Faces") 3 2 2 0

philipLombardSnakeInTheGrass :: CardDef
philipLombardSnakeInTheGrass =
  (culprit ":and-then-there-were-none:017b" ("Philip Lombard" <:> "Snake in the Grass") 4 2 2 0)
    { cdKeywords = singleton Keyword.Retaliate
    }

thomasRogersColdAndDistant :: CardDef
thomasRogersColdAndDistant =
  culprit ":and-then-there-were-none:018b" ("Thomas Rogers" <:> "Cold and Distant") 3 1 1 1

veraClaythorneDiscipliningTheYoung :: CardDef
veraClaythorneDiscipliningTheYoung =
  culprit
    ":and-then-there-were-none:019b"
    ("Vera Claythorne" <:> "Disciplining the Young")
    2
    2
    2
    0

williamBloreInTheBusinessOfSecrets :: CardDef
williamBloreInTheBusinessOfSecrets =
  culprit
    ":and-then-there-were-none:020b"
    ("William Blore" <:> "In the Business of Secrets")
    3
    3
    1
    1

restlessSpirit_038 :: CardDef
restlessSpirit_038 = restlessSpirit ":and-then-there-were-none:038" 3 4 2 2 0 []

restlessSpirit_039 :: CardDef
restlessSpirit_039 = restlessSpirit ":and-then-there-were-none:039" 4 3 1 1 1 [Keyword.Retaliate]

restlessSpirit_040 :: CardDef
restlessSpirit_040 = restlessSpirit ":and-then-there-were-none:040" 2 1 2 0 2 [Keyword.Aloof]

restlessSpirit_041 :: CardDef
restlessSpirit_041 = restlessSpirit ":and-then-there-were-none:041" 2 2 3 1 1 [Keyword.Elusive]

restlessSpirit_042 :: CardDef
restlessSpirit_042 = restlessSpirit ":and-then-there-were-none:042" 3 2 3 1 1 [Keyword.Alert]
