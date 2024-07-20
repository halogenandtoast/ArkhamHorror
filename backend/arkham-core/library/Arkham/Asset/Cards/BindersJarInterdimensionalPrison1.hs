module Arkham.Asset.Cards.BindersJarInterdimensionalPrison1 (
  bindersJarInterdimensionalPrison1,
  BindersJarInterdimensionalPrison1 (..),
)
where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted hiding (EnemyAttacks, EnemyDefeated)
import Arkham.Attack.Types
import Arkham.Card
import Arkham.Enemy.Types (Field (..))
import Arkham.Helpers.Window (defeatedEnemy, getAttackDetails)
import Arkham.Matcher hiding (PlaceUnderneath)
import Arkham.Projection
import Arkham.Slot
import Arkham.Trait (toTraits)

newtype Meta = Meta {underneath :: [Card]}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype BindersJarInterdimensionalPrison1 = BindersJarInterdimensionalPrison1 (AssetAttrs `With` Meta)
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

bindersJarInterdimensionalPrison1 :: AssetCard BindersJarInterdimensionalPrison1
bindersJarInterdimensionalPrison1 =
  asset (BindersJarInterdimensionalPrison1 . (`with` Meta [])) Cards.bindersJarInterdimensionalPrison1

instance HasAbilities BindersJarInterdimensionalPrison1 where
  getAbilities (BindersJarInterdimensionalPrison1 (With a _)) =
    [ controlledAbility a 1 criteria1
        $ freeReaction
        $ EnemyDefeated #after Anyone ByAny
        $ NonEliteEnemy
        <> EnemyAt YourLocation
    , restrictedAbility a 2 ControlsThis
        $ ReactionAbility
          (EnemyAttacks #when You (CancelableEnemyAttack AnyEnemyAttack) $ mapOneOf EnemyWithTrait traits)
          (DiscardUnderneathCardCost a.id CardWithSharedTraitToAttackingEnemy)
    ]
   where
    criteria1 = if length a.cardsUnderneath >= 2 then Never else NoRestriction
    traits = toList $ concatMap toTraits a.cardsUnderneath

instance RunMessage BindersJarInterdimensionalPrison1 where
  runMessage msg a@(BindersJarInterdimensionalPrison1 (With attrs meta)) = runQueueT $ case msg of
    UseCardAbility _iid (isSource attrs -> True) 1 (defeatedEnemy -> eid) _ -> do
      card <- field EnemyCard eid
      push $ ObtainCard card
      push $ PlaceUnderneath (toTarget attrs) [card]
      pure a
    PlaceUnderneath _ [c] -> do
      attrs' <- liftRunMessage msg attrs
      for_ attrs.controller \iid -> do
        push $ AddSlot iid #arcane (Slot (toSource attrs) [])
      pure $ BindersJarInterdimensionalPrison1 $ attrs' `with` Meta (c : underneath meta)
    UseCardAbility _iid (isSource attrs -> True) 2 (getAttackDetails -> details) _ -> do
      when (attackCanBeCanceled details) do
        cancelAttack attrs details
      pure a
    _ -> do
      attrs' <- liftRunMessage msg attrs
      let remove = length (underneath meta) - length attrs.cardsUnderneath
      when (remove > 0) do
        for_ attrs.owner \iid ->
          replicateM_ remove $ push $ RemoveSlotFrom iid (toSource attrs) #arcane
      pure $ BindersJarInterdimensionalPrison1 $ attrs' `with` Meta attrs.cardsUnderneath
