module Arkham.Agenda.Cards.WildFlames (wildFlames) where

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Deck qualified as Deck
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Query (getLead)
import Arkham.Helpers.Scenario (findTopOfDiscard)
import Arkham.Helpers.SkillTest.Lifted (parley)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Resolution
import Arkham.Scenarios.SpreadingFlames.Helpers
import Arkham.Treachery.Cards qualified as Treacheries

newtype WildFlames = WildFlames AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

wildFlames :: AgendaCard WildFlames
wildFlames = agenda (3, A) WildFlames Cards.wildFlames (Static 10)

instance HasAbilities WildFlames where
  getAbilities (WildFlames a) =
    [ skillTestAbility $ mkAbility a 1 parleyAction_
    , restricted a 2 (exists $ InEncounterDiscard <> basic (cardIs Treacheries.fire1))
        $ forced
        $ PlacedDoomCounter #after AnySource (targetIs a)
    ]

instance RunMessage WildFlames where
  runMessage msg a@(WildFlames attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      parley sid iid (attrs.ability 1) iid #intellect (Fixed 2)
      pure a
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      bystanders <- select $ enemyIs Enemies.bystander <> enemyAtLocationWith iid
      chooseTargetM iid bystanders $ toDiscardBy iid (attrs.ability 1)
      pure a
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      lead <- getLead
      findTopOfDiscard (cardIs Treacheries.fire1) >>= traverse_ (drawCardFrom lead Deck.EncounterDiscard)
      pure a
    AdvanceAgenda (isSide B attrs -> True) -> scenarioI18n do
      selectEach UneliminatedInvestigator \iid -> do
        investigatorDefeated attrs iid
        sufferPhysicalTrauma iid 1
      push $ ScenarioResolution NoResolution
      pure a
    _ -> WildFlames <$> liftRunMessage msg attrs
