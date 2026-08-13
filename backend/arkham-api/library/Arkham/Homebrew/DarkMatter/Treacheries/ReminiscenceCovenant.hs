module Arkham.Homebrew.DarkMatter.Treacheries.ReminiscenceCovenant (reminiscenceCovenant) where

import Arkham.Ability
import Arkham.Helpers.Window.Enemy (evadingInvestigator, getEnemy)
import Arkham.Homebrew.DarkMatter.CardDefs.Treacheries qualified as Cards
import Arkham.Matcher
import Arkham.Placement
import Arkham.Treachery.Import.Lifted hiding (InvestigatorEliminated)

newtype ReminiscenceCovenant = ReminiscenceCovenant TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

reminiscenceCovenant :: TreacheryCard ReminiscenceCovenant
reminiscenceCovenant = treachery ReminiscenceCovenant Cards.reminiscenceCovenant

instance HasAbilities ReminiscenceCovenant where
  getAbilities (ReminiscenceCovenant a) = case a.placement of
    HiddenInHand iid ->
      [ mkAbility a 1 $ forced $ oneOf [GameEnds #when, InvestigatorEliminated #when You]
      , mkAbility a 2
          $ freeReaction
          $ EnemyWouldBeEvaded #when Anyone (EnemyAt $ locationWithInvestigator iid)
      ]
    _ -> []

instance RunMessage ReminiscenceCovenant where
  runMessage msg t@(ReminiscenceCovenant attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      addHiddenToHand iid attrs
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      addToVictory iid attrs
      pure t
    UseCardAbility iid (isSource attrs -> True) 2 ws _ -> do
      cancelWindowBatch ws
      initiateEnemyAttackEdit (getEnemy ws) (attrs.ability 2) (evadingInvestigator ws) despiteExhausted
      toDiscardBy iid (attrs.ability 2) attrs
      pure t
    _ -> ReminiscenceCovenant <$> liftRunMessage msg attrs
