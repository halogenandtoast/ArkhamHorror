module Arkham.Enemy.Cards.ReturnToThePathToCarcosa.ReturnToCurtainCall.LaComtesseSubverterOfPlans (laComtesseSubverterOfPlans) where

import Arkham.Ability
import Arkham.Card
import Arkham.Enemy.CardDefs.ReturnToThePathToCarcosa.ReturnToCurtainCall qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Enemy (insteadOfDiscarding)
import Arkham.Helpers.Modifiers (ModifierType (..), modified_)
import Arkham.Matcher
import Arkham.Message.Lifted.Placement

newtype LaComtesseSubverterOfPlans = LaComtesseSubverterOfPlans EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

laComtesseSubverterOfPlans :: EnemyCard LaComtesseSubverterOfPlans
laComtesseSubverterOfPlans =
  enemy LaComtesseSubverterOfPlans Cards.laComtesseSubverterOfPlans
    & setSpawnAt "Balcony"

instance HasModifiersFor LaComtesseSubverterOfPlans where
  getModifiersFor (LaComtesseSubverterOfPlans a) = case a.placement of
    HiddenInHand _ -> modified_ a (toCard a) [HandSizeCardCount 4]
    _ -> pure ()

-- Going to hand replaces the discard, so ability 1 has to land before disposal
-- (IfEnemyDefeated resolves after it). The Per Phase limit on the second
-- ability is a bit incorrect, however it's easier than batching discards
-- currently
instance HasAbilities LaComtesseSubverterOfPlans where
  getAbilities (LaComtesseSubverterOfPlans a) =
    extend
      a
      [ mkAbility a 1 $ forced $ EnemyDefeated #when You ByAny (be a)
      , playerLimit PerPhase
          $ restricted a 2 (InYourHand <> DuringPhase #upkeep)
          $ forced
          $ DiscardedFromHand #after You #any #any
      ]

instance RunMessage LaComtesseSubverterOfPlans where
  runMessage msg e@(LaComtesseSubverterOfPlans attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      insteadOfDiscarding attrs $ place attrs (HiddenInHand iid)
      pure $ LaComtesseSubverterOfPlans $ attrs & tokensL .~ mempty & defeatedL .~ False
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      assignHorror iid (attrs.ability 2) 1
      pure e
    _ -> LaComtesseSubverterOfPlans <$> liftRunMessage msg attrs
