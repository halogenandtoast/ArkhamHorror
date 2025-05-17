module Arkham.Enemy.Cards.LaComtesseSubverterOfPlans (laComtesseSubverterOfPlans) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted hiding (EnemyDefeated)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Matcher
import Arkham.Message.Lifted.Placement
import Arkham.Placement

newtype LaComtesseSubverterOfPlans = LaComtesseSubverterOfPlans EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

laComtesseSubverterOfPlans :: EnemyCard LaComtesseSubverterOfPlans
laComtesseSubverterOfPlans =
  enemy LaComtesseSubverterOfPlans Cards.laComtesseSubverterOfPlans (1, Static 3, 3) (0, 1)
    & setSpawnAt "Balcony"

instance HasModifiersFor LaComtesseSubverterOfPlans where
  getModifiersFor (LaComtesseSubverterOfPlans a) = case a.placement of
    HiddenInHand _ -> modifySelf a [HandSizeCardCount 4]
    _ -> pure ()

instance HasAbilities LaComtesseSubverterOfPlans where
  getAbilities (LaComtesseSubverterOfPlans a) =
    extend
      a
      [ mkAbility a 1 $ forced $ EnemyDefeated #after You ByAny (be a)
      , restricted a 2 (InYourHand <> DuringPhase #upkeep) $ forced $ DiscardedFromHand #when You #any #any
      ]

instance RunMessage LaComtesseSubverterOfPlans where
  runMessage msg e@(LaComtesseSubverterOfPlans attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      cancelEnemyDefeat attrs
      place attrs (HiddenInHand iid)
      pure e
    InHand _ (UseThisAbility iid (isSource attrs -> True) 2) -> do
      assignHorror iid (attrs.ability 2) 1
      pure e
    _ -> LaComtesseSubverterOfPlans <$> liftRunMessage msg attrs
