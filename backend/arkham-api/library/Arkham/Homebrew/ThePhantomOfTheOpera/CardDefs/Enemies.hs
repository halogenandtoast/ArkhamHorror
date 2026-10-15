{- | Six of these are concealed: they sit in the shadows as mini-cards and only
become enemies once exposed. Terrible Guise and Violent Shade are the ordinary
encounter enemies, and each has one variable stat -- X is the number of
concealed mini-cards at its location, capped at 5.
-}
module Arkham.Homebrew.ThePhantomOfTheOpera.CardDefs.Enemies where

import Arkham.Enemy.CardDefs.Import
import Arkham.Homebrew.ThePhantomOfTheOpera.Kinds qualified as Kinds
import Arkham.Homebrew.ThePhantomOfTheOpera.Sets qualified as Set
import Arkham.Keyword qualified as Keyword

buriedSorrows :: CardDef
buriedSorrows =
  (enemy ":the-phantom-of-the-opera:028" "Buried Sorrows" Set.ThePhantomOfTheOpera 1)
    { cdCardTraits = setFromList [Humanoid, Conspirator]
    , cdFight = fight 3
    , cdHealth = health 2
    , cdEvade = evade 3
    , cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdKeywords = singleton $ Keyword.Concealed Kinds.BuriedSorrows (Static 1)
    }

chaoticMinionA :: CardDef
chaoticMinionA =
  (enemy ":the-phantom-of-the-opera:029" "Chaotic Minion (A)" Set.ThePhantomOfTheOpera 1)
    { cdCardTraits = setFromList [Humanoid, Monster]
    , cdFight = fight 2
    , cdHealth = health 1
    , cdEvade = evade 2
    , cdSanityDamage = sanityDamage 1
    , cdKeywords = singleton Keyword.Aloof
    }

chaoticMinionB :: CardDef
chaoticMinionB =
  (enemy ":the-phantom-of-the-opera:030" "Chaotic Minion (B)" Set.ThePhantomOfTheOpera 1)
    { cdCardTraits = setFromList [Humanoid, Monster]
    , cdFight = fight 2
    , cdHealth = health 1
    , cdEvade = evade 2
    , cdHealthDamage = healthDamage 1
    , cdKeywords = singleton Keyword.Aloof
    }

simmeringAnger :: CardDef
simmeringAnger =
  (enemy ":the-phantom-of-the-opera:031" "Simmering Anger" Set.ThePhantomOfTheOpera 1)
    { cdCardTraits = setFromList [Humanoid, Conspirator]
    , cdFight = fight 4
    , cdHealth = health 2
    , cdEvade = evade 2
    , cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdKeywords = singleton $ Keyword.Concealed Kinds.SimmeringAnger (Static 1)
    }

stifledLaughter :: CardDef
stifledLaughter =
  (enemy ":the-phantom-of-the-opera:032" "Stifled Laughter" Set.ThePhantomOfTheOpera 1)
    { cdCardTraits = setFromList [Humanoid, Conspirator]
    , cdFight = fight 2
    , cdHealth = health 2
    , cdEvade = evade 4
    , cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdKeywords = singleton $ Keyword.Concealed Kinds.StifledLaughter (Static 1)
    }

thePhantom :: CardDef
thePhantom =
  ( enemy
      ":the-phantom-of-the-opera:033"
      ("The Phantom" <:> "Lurking Mastermind")
      Set.ThePhantomOfTheOpera
      1
  )
    { cdCardTraits = setFromList [Humanoid, Conspirator, Elite]
    , cdFight = fight 4
    , cdHealth = healthPerInvestigator 4
    , cdEvade = evade 4
    , cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdKeywords = singleton $ Keyword.Concealed Kinds.ThePhantom (Static 1)
    , cdUnique = True
    , cdVictoryPoints = Just 1
    , cdOtherSide = Just ":the-phantom-of-the-opera:033b"
    }

{- | The Phantom's other side. He flips here when Christine's parley puts enough
clues on him, and act 4 reads him as the peaceful ending.
-}
thePhantomTragicFigure :: CardDef
thePhantomTragicFigure =
  ( enemy
      ":the-phantom-of-the-opera:033b"
      ("The Phantom" <:> "Tragic Figure")
      Set.ThePhantomOfTheOpera
      0
  )
    { cdCardTraits = setFromList [Humanoid, Conspirator, Elite]
    , cdFight = fight 0
    , cdEvade = evade 0
    , cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdKeywords = singleton Keyword.Aloof
    , cdUnique = True
    , cdOtherSide = Just ":the-phantom-of-the-opera:033"
    }

terribleGuise :: CardDef
terribleGuise =
  (enemy ":the-phantom-of-the-opera:043" "Terrible Guise" Set.ThePhantomOfTheOpera 2)
    { cdCardTraits = setFromList [Humanoid, Monster]
    , cdFight = fightX
    , cdHealth = health 2
    , cdRevelation = IsRevelation
    , cdEvade = evade 4
    , cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdKeywords = singleton Keyword.Hunter
    }

violentShade :: CardDef
violentShade =
  (enemy ":the-phantom-of-the-opera:045" "Violent Shade" Set.ThePhantomOfTheOpera 2)
    { cdCardTraits = setFromList [Humanoid, Conspirator]
    , cdFight = fight 4
    , cdEvade = evadeX
    , cdHealth = health 2
    , cdRevelation = IsRevelation
    , cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdKeywords = singleton Keyword.Hunter
    }
