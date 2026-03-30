module Arkham.Asset.Cards.CarolynFern2 where

import Arkham.Asset.Cards.Import
import Arkham.Keyword qualified as Keyword

experimentalPsychology :: CardDef
experimentalPsychology =
  signature "60251"
    $ (asset "60252" "Experimental Psychology" 2 Seeker)
      { cdCardTraits = setFromList [Item, Tome, Science]
      , cdSkills = [#willpower, #intellect, #wild]
      , cdSlots = [#hand]
      }

dreamersChronicle :: CardDef
dreamersChronicle =
  (asset "60255" "Dreamer's Chronicle" 4 Seeker)
    { cdCardTraits = setFromList [Item, Tome]
    , cdSkills = [#intellect]
    , cdSlots = [#hand]
    , cdUses = uses Secret 4
    }

occultRecords :: CardDef
occultRecords =
  (asset "60256" "Occult Records" 3 Seeker)
    { cdCardTraits = setFromList [Item, Tome, Occult]
    , cdSkills = [#willpower]
    , cdSlots = [#hand]
    , cdUses = uses Secret 3
    }

privatePractice :: CardDef
privatePractice =
  (asset "60257" "Private Practice" 1 Seeker)
    { cdCardTraits = singleton Profession
    , cdSkills = [#intellect]
    , cdLimits = [LimitPerInvestigator 1]
    }

psychologyStudent :: CardDef
psychologyStudent =
  (asset "60258" "Psychology Student" 2 Seeker)
    { cdCardTraits = setFromList [Ally, Miskatonic]
    , cdSkills = [#willpower]
    , cdSlots = [#ally]
    }

scrollOfThePharaohs :: CardDef
scrollOfThePharaohs =
  (asset "60259" ("Scroll of the Pharaohs" <:> "Untranslated") 3 Seeker)
    { cdCardTraits = setFromList [Item, Relic, Tome]
    , cdSkills = [#intellect]
    , cdSlots = [#hand]
    , cdUnique = True
    , cdUses = uses Secret 4
    }

universityArchivist :: CardDef
universityArchivist =
  (asset "60260" "University Archivist" 2 Seeker)
    { cdCardTraits = setFromList [Ally, Miskatonic]
    , cdSkills = [#agility]
    , cdSlots = [#ally]
    }

typewriter2 :: CardDef
typewriter2 =
  (asset "60272" "Typewriter" 2 Seeker)
    { cdCardTraits = setFromList [Item, Tool]
    , cdSkills = [#intellect, #willpower]
    , cdUses = uses Secret 3
    , cdLevel = Just 2
    }

autopsyReport3 :: CardDef
autopsyReport3 =
  (asset "60276" "Autopsy Report" 3 Seeker)
    { cdCardTraits = setFromList [Item, Tome, Science]
    , cdSkills = [#agility, #combat, #wild]
    , cdSlots = [#hand]
    , cdLevel = Just 3
    }

sharpRhetoric3 :: CardDef
sharpRhetoric3 =
  (asset "60277" "Sharp Rhetoric" 2 Seeker)
    { cdCardTraits = singleton Talent
    , cdSkills = [#intellect, #willpower, #wild]
    , cdKeywords = singleton Keyword.Starting
    , cdLevel = Just 3
    }

scrollOfThePharaohsWordsOfBast4 :: CardDef
scrollOfThePharaohsWordsOfBast4 =
  (asset "60279" ("Scroll of the Pharaohs" <:> "Words of Bast") 3 Seeker)
    { cdCardTraits = setFromList [Item, Relic, Tome]
    , cdSkills = [#wild, #wild, #willpower]
    , cdSlots = [#hand]
    , cdUnique = True
    , cdUses = uses Secret 4
    , cdKeywords = singleton $ Keyword.Researched YouHaveUnearthedTheSecretsOfThePharaohs
    , cdLevel = Just 4
    }

scrollOfThePharaohsWordsOfRa4 :: CardDef
scrollOfThePharaohsWordsOfRa4 =
  (asset "60280" ("Scroll of the Pharaohs" <:> "Words of Ra") 3 Seeker)
    { cdCardTraits = setFromList [Item, Relic, Tome]
    , cdSkills = [#intellect, #wild, #wild]
    , cdSlots = [#hand]
    , cdUnique = True
    , cdUses = uses Secret 4
    , cdKeywords = singleton $ Keyword.Researched YouHaveUnearthedTheSecretsOfThePharaohs
    , cdLevel = Just 4
    }

scrollOfThePharaohsWordsOfSet4 :: CardDef
scrollOfThePharaohsWordsOfSet4 =
  (asset "60281" ("Scroll of the Pharaohs" <:> "Words of Set") 3 Seeker)
    { cdCardTraits = setFromList [Item, Relic, Tome]
    , cdSkills = [#intellect, #wild, #wild]
    , cdSlots = [#hand]
    , cdUnique = True
    , cdUses = uses Secret 4
    , cdKeywords = singleton $ Keyword.Researched YouHaveUnearthedTheSecretsOfThePharaohs
    , cdLevel = Just 4
    }
