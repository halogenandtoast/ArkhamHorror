module Arkham.Homebrew.DarkMatter.Treacheries.ReminiscenceCovenant (reminiscenceCovenant) where

import Arkham.Ability
import Arkham.Helpers.Window.Enemy (evadingInvestigator, getEnemy)
import Arkham.Homebrew.DarkMatter.CardDefs.Treacheries qualified as Cards
import Arkham.Matcher
import Arkham.Placement
import Arkham.Treachery.Import.Lifted hiding (InvestigatorEliminated)
import Arkham.Window (Window (..))
import Arkham.Window qualified as Window

newtype ReminiscenceCovenant = ReminiscenceCovenant TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

reminiscenceCovenant :: TreacheryCard ReminiscenceCovenant
reminiscenceCovenant = treachery ReminiscenceCovenant Cards.reminiscenceCovenant

instance HasAbilities ReminiscenceCovenant where
  getAbilities (ReminiscenceCovenant a) =
    [ mkAbility a 1 $ forced $ oneOf [GameEnds #when, InvestigatorEliminated #when You]
    , mkAbility a 2 $ freeReaction $ EnemyWouldBeEvaded #when Anyone (EnemyAt $ here a)
    ]

-- The card lives hidden in its holder's hand, so @inThreatAreaOf@ is Nothing and
-- scoping off it alone would leave the ability matching every location.
here :: TreacheryAttrs -> LocationMatcher
here a = case a.placement of
  HiddenInHand iid -> locationWithInvestigator iid
  InThreatArea iid -> locationWithInvestigator iid
  _ -> Anywhere

instance RunMessage ReminiscenceCovenant where
  runMessage msg t@(ReminiscenceCovenant attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      addHiddenToHand iid attrs
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      addToVictory iid attrs
      pure t
    UseCardAbility iid (isSource attrs -> True) 2 ws _ -> do
      -- "that enemy immediately attacks them instead" — cancelling the batch
      -- replaces the whole successful-evade cascade. Cancel the evade batch
      -- specifically rather than the first batch on the stack, which may belong
      -- to something enclosing this window.
      for_ [bId | Window _ (Window.EnemyWouldBeEvaded {}) (Just bId) <- ws] cancelBatch
      -- The attack replaces the evasion, so it happens even if the enemy was
      -- already exhausted when it was evaded.
      initiateEnemyAttackEdit (getEnemy ws) (attrs.ability 2) (evadingInvestigator ws) despiteExhausted
      toDiscardBy iid (attrs.ability 2) attrs
      pure t
    _ -> ReminiscenceCovenant <$> liftRunMessage msg attrs
