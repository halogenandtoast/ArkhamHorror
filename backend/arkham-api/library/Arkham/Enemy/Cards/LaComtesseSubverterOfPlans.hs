module Arkham.Enemy.Cards.LaComtesseSubverterOfPlans (laComtesseSubverterOfPlans) where

import Arkham.Ability
import Arkham.Card
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted hiding (EnemyDefeated)
import Arkham.Helpers.Modifiers (ModifierType (..), modified_)
import Arkham.Matcher
import Arkham.Message.Lifted.Placement

newtype LaComtesseSubverterOfPlans = LaComtesseSubverterOfPlans EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

laComtesseSubverterOfPlans :: EnemyCard LaComtesseSubverterOfPlans
laComtesseSubverterOfPlans =
  enemy LaComtesseSubverterOfPlans Cards.laComtesseSubverterOfPlans (1, Static 3, 3) (0, 1)
    & setSpawnAt "Balcony"

instance HasModifiersFor LaComtesseSubverterOfPlans where
  getModifiersFor (LaComtesseSubverterOfPlans a) = case a.placement of
    HiddenInHand _ -> modified_ a (toCard a) [HandSizeCardCount 4]
    _ -> pure ()

-- The Per Phase limit on the second ability is a bit incorrect, however it's
-- easier than batching discards currently
instance HasAbilities LaComtesseSubverterOfPlans where
  getAbilities (LaComtesseSubverterOfPlans a) =
    extend
      a
      [ mkAbility a 1 $ forced $ EnemyDefeated #after You ByAny (be a)
      , playerLimit PerPhase $ restricted a 2 (InYourHand <> DuringPhase #upkeep) $ forced $ DiscardedFromHand #after You #any #any
      ]

instance RunMessage LaComtesseSubverterOfPlans where
  runMessage msg e@(LaComtesseSubverterOfPlans attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      cancelEnemyDefeat attrs
      place attrs (HiddenInHand iid)
      pure $ LaComtesseSubverterOfPlans $ attrs & tokensL .~ mempty & defeatedL .~ False
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      assignHorror iid (attrs.ability 2) 1
      pure e
    _ -> LaComtesseSubverterOfPlans <$> liftRunMessage msg attrs
