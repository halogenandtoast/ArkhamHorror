module Arkham.Homebrew.AgesUnwound.Agendas.Hunted (hunted) where

import Arkham.Ability
import Arkham.Agenda.Import.Lifted
import Arkham.Helpers.Cost (getSpendableClueCount)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Helpers.Query (getInvestigators, getLead, getSetAsideCardsMatching)
import Arkham.Homebrew.AgesUnwound.CardDefs.Acts qualified as Acts
import Arkham.Homebrew.AgesUnwound.CardDefs.Agendas qualified as Cards
import Arkham.Homebrew.AgesUnwound.CardDefs.Enemies qualified as Enemies
import Arkham.Homebrew.AgesUnwound.Scenarios.NightOfFire.Helpers
import Arkham.Homebrew.AgesUnwound.Sets qualified as Set
import Arkham.I18n
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype Hunted = Hunted AgendaAttrs
  deriving anyclass IsAgenda
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hunted :: AgendaCard Hunted
hunted = agenda (1, A) Hunted Cards.hunted (Static 3)

instance HasModifiersFor Hunted where
  getModifiersFor (Hunted a) = modifySelf a [OtherDoomSubtracts]

instance HasAbilities Hunted where
  getAbilities (Hunted a) =
    [forcedAbility a 1 $ TurnEnds #when (You <> not_ InvestigatorThatMovedDuringTurn)]

instance RunMessage Hunted where
  runMessage msg a@(Hunted attrs) = runQueueT $ nightOfFireI18n $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 1) iid #agility (Fixed 2)
      pure a
    FailedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      assignDamage iid (attrs.ability 1) 1
      pure a
    AdvanceAgenda (isSide B attrs -> True) -> do
      {- "For each investigator, spawn a set-aside copy of Time Spirit engaged
      with that investigator unless the investigators spend 2 clues as a
      group." -}
      iids <- getInvestigators
      lead <- getLead
      clues <- getSpendableClueCount iids
      scope "hunted" $ chooseOneM lead do
        labeledValidate (clues >= 2) "spendTwoClues" $ spendCluesAsAGroup iids 2
        labeled "spawnTimeSpirits" do
          -- One distinct set-aside copy each: `createSetAsideEnemy_` resolves
          -- the def through `selectJust`, and nothing is flushed between the
          -- calls in this body, so looping the def would mint every Time Spirit
          -- from the same card.
          spirits <- getSetAsideCardsMatching (cardIs Enemies.timeSpirit)
          for_ (zip iids spirits) \(iid, spirit) -> createEnemyWith_ spirit iid id

      {- "Shuffle Myriad Assassin, Irregulars and the Agents of Aforgomon
      encounter set into the encounter deck, along with the encounter discard
      pile." -}
      shuffleSetAsideIntoEncounterDeck
        $ mapOneOf cardIs [Enemies.myriadAssassin, Enemies.irregulars]
      shuffleSetAsideEncounterSet Set.AgentsOfAforgomon
      shuffleEncounterDiscardBackIn

      -- "Advance to act 2a and agenda 2a."
      advanceToActA attrs Acts.gettingYourBearings
      advanceAgendaDeck attrs
      pure a
    _ -> Hunted <$> liftRunMessage msg attrs
