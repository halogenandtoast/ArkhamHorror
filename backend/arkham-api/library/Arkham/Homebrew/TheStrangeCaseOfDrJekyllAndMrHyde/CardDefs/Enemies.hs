{- | Two of these are printed on the back of another card: Sinister Chemist is
the far side of the Adapted Formula story, and Edward Hyde the far side of the
Henry Jekyll asset. Both keep the card code of the front with a 'b' suffix.
-}
module Arkham.Homebrew.TheStrangeCaseOfDrJekyllAndMrHyde.CardDefs.Enemies where

import Arkham.Enemy.CardDefs.Import
import Arkham.Homebrew.TheStrangeCaseOfDrJekyllAndMrHyde.Sets qualified as Set
import Arkham.Keyword qualified as Keyword

sinisterChemist :: CardDef
sinisterChemist =
  ( enemy
      ":the-strange-case-of-dr-jekyll-and-mr-hyde:006b"
      "Sinister Chemist"
      Set.TheStrangeCaseOfDrJekyllAndMrHyde
      0
  )
    { cdCardTraits = setFromList [Humanoid, Sorcerer, Elite]
    , cdFight = fight 3
    , cdHealth = healthPerInvestigator 2
    , cdEvade = evade 3
    , cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdKeywords = singleton Keyword.Retaliate
    , cdVictoryPoints = Just 1
    , cdOtherSide = Just ":the-strange-case-of-dr-jekyll-and-mr-hyde:006"
    , cdDoubleSided = True
    }

edwardHyde :: CardDef
edwardHyde =
  ( enemy
      ":the-strange-case-of-dr-jekyll-and-mr-hyde:020b"
      ("Edward Hyde" <:> "Stain Upon the Soul")
      Set.TheStrangeCaseOfDrJekyllAndMrHyde
      0
  )
    { cdCardTraits = setFromList [Humanoid, Sorcerer, Elite]
    , cdFight = fight 3
    , cdHealth = healthPerInvestigator 1
    , cdEvade = evade 4
    , cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdKeywords = singleton (Keyword.Patrol LocationWithoutInvestigators)
    , cdUnique = True
    , cdOtherSide = Just ":the-strange-case-of-dr-jekyll-and-mr-hyde:020"
    , cdDoubleSided = True
    }

fleshyCreation :: CardDef
fleshyCreation =
  ( enemy
      ":the-strange-case-of-dr-jekyll-and-mr-hyde:027"
      "Fleshy Creation"
      Set.TheStrangeCaseOfDrJekyllAndMrHyde
      3
  )
    { cdCardTraits = setFromList [Monster, Abomination]
    , cdFight = fight 2
    , cdHealth = health 2
    , cdEvade = evade 2
    , cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdKeywords = singleton Keyword.Hunter
    }

misshapenBeast :: CardDef
misshapenBeast =
  ( enemy
      ":the-strange-case-of-dr-jekyll-and-mr-hyde:029"
      "Misshapen Beast"
      Set.TheStrangeCaseOfDrJekyllAndMrHyde
      2
  )
    { cdCardTraits = setFromList [Creature, Monster, Abomination]
    , cdFight = fight 3
    , cdHealth = health 3
    , cdEvade = evade 3
    , cdHealthDamage = healthDamage 2
    , cdSanityDamage = sanityDamage 1
    , cdKeywords = singleton Keyword.Hunter
    }

profaneSculptor :: CardDef
profaneSculptor =
  ( enemy
      ":the-strange-case-of-dr-jekyll-and-mr-hyde:030"
      "Profane Sculptor"
      Set.TheStrangeCaseOfDrJekyllAndMrHyde
      2
  )
    { cdCardTraits = setFromList [Humanoid, Sorcerer]
    , cdFight = fight 4
    , cdHealth = health 2
    , cdEvade = evade 2
    , cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdKeywords = singleton Keyword.Hunter
    }

servantOfYogSothoth :: CardDef
servantOfYogSothoth =
  ( enemy
      ":the-strange-case-of-dr-jekyll-and-mr-hyde:031"
      "Servant of Yog-Sothoth"
      Set.TheStrangeCaseOfDrJekyllAndMrHyde
      3
  )
    { cdCardTraits = setFromList [Humanoid, Sorcerer]
    , cdFight = fight 3
    , cdHealth = health 2
    , cdEvade = evade 3
    , cdHealthDamage = healthDamage 2
    }
