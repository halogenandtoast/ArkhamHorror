module Arkham.Homebrew.DarkMatter.Treacheries.ReminiscencePledge (reminiscencePledge) where

import Arkham.Ability
import Arkham.Helpers.Window (defeatedEnemy)
import Arkham.Homebrew.DarkMatter.CardDefs.Treacheries qualified as Cards
import Arkham.Matcher
import Arkham.Placement
import Arkham.Treachery.Import.Lifted hiding (InvestigatorEliminated)

newtype ReminiscencePledge = ReminiscencePledge TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

reminiscencePledge :: TreacheryCard ReminiscencePledge
reminiscencePledge = treachery ReminiscencePledge Cards.reminiscencePledge

instance HasAbilities ReminiscencePledge where
  getAbilities (ReminiscencePledge a) = case a.placement of
    HiddenInHand iid ->
      [ mkAbility a 1 $ forced $ oneOf [GameEnds #when, InvestigatorEliminated #when You]
      , mkAbility a 2 $ freeReaction $ EnemyWouldBeDefeated #when (EnemyAt $ locationWithInvestigator iid)
      ]
    _ -> []

instance RunMessage ReminiscencePledge where
  runMessage msg t@(ReminiscencePledge attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      addHiddenToHand iid attrs
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      addToVictory iid attrs
      pure t
    UseCardAbility iid (isSource attrs -> True) 2 (defeatedEnemy -> enemy) _ -> do
      cancelEnemyDefeat enemy
      healAllDamage (attrs.ability 2) enemy
      toDiscardBy iid (attrs.ability 2) attrs
      pure t
    _ -> ReminiscencePledge <$> liftRunMessage msg attrs
