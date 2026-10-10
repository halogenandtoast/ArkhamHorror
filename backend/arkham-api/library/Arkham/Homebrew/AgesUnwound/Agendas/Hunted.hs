module Arkham.Homebrew.AgesUnwound.Agendas.Hunted (hunted) where

import Arkham.Ability
import Arkham.Agenda.Import.Lifted
import Arkham.Helpers.Cost (getSpendableClueCount)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Helpers.Query (getInvestigators, getLead)
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

{- | "Doom on cards other than this agenda subtracts from the total doom in play
instead of adding to it." Shared by all three Night of Fire agendas, and the
reason Eternity's Sentinel's self-inflicted doom buys the investigators time.
-}
instance HasModifiersFor Hunted where
  getModifiersFor (Hunted a) = modifySelf a [OtherDoomSubtracts]

instance HasAbilities Hunted where
  getAbilities (Hunted a) =
    [forcedAbility a 1 $ TurnEnds #when (You <> not_ InvestigatorThatMovedDuringTurn)]

instance RunMessage Hunted where
  runMessage msg a@(Hunted attrs) = runQueueT $ nightOfFireI18n $ case msg of
    -- "Forced - At the end of your turn, if you did not move at least once
    -- during your turn: Test [agility] (2). If you fail, take 1 damage."
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
        labeled "spawnTimeSpirits"
          $ for_ iids (createSetAsideEnemy_ Enemies.timeSpirit)

      {- "Shuffle Myriad Assassin, Irregulars and the Agents of Aforgomon
      encounter set into the encounter deck, along with the encounter discard
      pile." The guide's "Agents of Aforgomon" is the data's
      @agents_of_chronos@. -}
      shuffleSetAsideIntoEncounterDeck
        $ mapOneOf cardIs [Enemies.myriadAssassin, Enemies.irregulars]
      shuffleSetAsideEncounterSet Set.AgentsOfChronos
      shuffleEncounterDiscardBackIn

      -- "Advance to act 2a and agenda 2a."
      advanceToActA attrs Acts.gettingYourBearings
      advanceAgendaDeck attrs
      pure a
    _ -> Hunted <$> liftRunMessage msg attrs
