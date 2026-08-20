module Arkham.Story.CardDefs.GuardiansOfTheAbyss.BrotherhoodOfTheBeast where

import Arkham.Story.CardDefs.Import

theAssassinsEvidence :: CardDef
theAssassinsEvidence =
  victory 1
    $ addTrait Evidence
    $ otherSideIs "83035a"
    $ story "83035b" "The Assassin's Evidence" BrotherhoodOfTheBeast

thePriestesssEvidence :: CardDef
thePriestesssEvidence =
  victory 1
    $ addTrait Evidence
    $ otherSideIs "83033a"
    $ story "83033b" "The Priestess's Evidence" BrotherhoodOfTheBeast

theProfessorsEvidence :: CardDef
theProfessorsEvidence =
  victory 1
    $ addTrait Evidence
    $ otherSideIs "83036a"
    $ story "83036b" "The Professor's Evidence" BrotherhoodOfTheBeast

theSalesmansEvidence :: CardDef
theSalesmansEvidence =
  victory 1
    $ addTrait Evidence
    $ otherSideIs "83034a"
    $ story "83034b" "The Salesman's Evidence" BrotherhoodOfTheBeast

theSupplicantsEvidence :: CardDef
theSupplicantsEvidence =
  victory 1
    $ addTrait Evidence
    $ otherSideIs "83032a"
    $ story "83032b" "The Supplicant's Evidence" BrotherhoodOfTheBeast

theTranslatorsEvidence :: CardDef
theTranslatorsEvidence =
  victory 1
    $ addTrait Evidence
    $ otherSideIs "83031a"
    $ story "83031b" "The Translator's Evidence" BrotherhoodOfTheBeast
