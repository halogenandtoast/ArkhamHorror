module Arkham.Treachery.Cards.ChildrenOfBlood.AgentsOfZburamoarte.UnnaturalStrength (unnaturalStrength) where

import Arkham.Ability
import Arkham.Helpers.Modifiers (ModifierType (..), modified_)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Trait (Trait (Monster))
import Arkham.Treachery.CardDefs.ChildrenOfBlood.AgentsOfZburamoarte qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype UnnaturalStrength = UnnaturalStrength TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

unnaturalStrength :: TreacheryCard UnnaturalStrength
unnaturalStrength = treachery UnnaturalStrength Cards.unnaturalStrength

instance HasModifiersFor UnnaturalStrength where
  getModifiersFor (UnnaturalStrength a) = for_ a.attached.enemy \enemy ->
    modified_ a enemy [EnemyFight 1, DamageDealt 1]

instance HasAbilities UnnaturalStrength where
  getAbilities (UnnaturalStrength a) = case a.attached.enemy of
    Nothing -> []
    Just enemy ->
      [ mkAbility a 1
          $ forced
          $ EnemyTakeDamage #after AnyDamageEffect (EnemyWithId enemy) AnyValue AnySource
      ]

instance RunMessage UnnaturalStrength where
  runMessage msg t@(UnnaturalStrength attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      monsters <- select $ NearestEnemyToFallback iid $ EnemyWithTrait Monster
      if null monsters
        then gainSurge attrs
        else chooseTargetM iid monsters $ attachTreachery attrs
      pure t
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      toDiscard (attrs.ability 1) attrs
      pure t
    _ -> UnnaturalStrength <$> liftRunMessage msg attrs
