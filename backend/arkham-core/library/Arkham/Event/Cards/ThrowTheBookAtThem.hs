module Arkham.Event.Cards.ThrowTheBookAtThem (throwTheBookAtThem, ThrowTheBookAtThem (..)) where

import Arkham.Ability
import Arkham.Asset.Types (Field (..))
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.SkillTest.Target
import Arkham.Id
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Modifier
import Arkham.Projection

newtype Meta = Meta {chosenTome :: Maybe AssetId}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype ThrowTheBookAtThem = ThrowTheBookAtThem (EventAttrs `With` Meta)
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

throwTheBookAtThem :: EventCard ThrowTheBookAtThem
throwTheBookAtThem = event (ThrowTheBookAtThem . (`with` Meta Nothing)) Cards.throwTheBookAtThem

instance RunMessage ThrowTheBookAtThem where
  runMessage msg e@(ThrowTheBookAtThem (With attrs meta)) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      selectOneToHandle iid attrs $ assetControlledBy iid <> #tome
      pure e
    HandleTargetChoice iid (isSource attrs -> True) (AssetTarget aid) -> do
      x <- field AssetCost aid
      sid <- getRandom
      when (x > 0) $ skillTestModifier sid attrs iid (SkillModifier #combat x)
      chooseFightEnemy sid iid attrs
      pure . ThrowTheBookAtThem $ attrs `with` Meta (Just aid)
    PassedThisSkillTest iid (isSource attrs -> True) -> do
      getSkillTestTarget >>= \case
        Just (EnemyTarget eid) -> do
          canEvade <- eid <=~> EnemyCanBeEvadedBy (toSource attrs)
          chooseOrRunOneM iid do
            when canEvade do
              labeled "Automatically evade the attacked enemy" $ automaticallyEvadeEnemy iid eid
            labeled
              "After the attack ends, you may resolve an {action} or {fast} ability on the chosen asset (ignoring its {action} cost, if any)"
              $ doStep 1 msg
        _ -> pure ()
      pure e
    DoStep 1 (PassedThisSkillTest iid (isSource attrs -> True)) -> do
      afterSkillTest do
        push $ ResolveEventChoice iid attrs.id 1 Nothing []
      pure e
    ResolveEventChoice iid eid _ _ _ | eid == attrs.id -> do
      for_ (chosenTome meta) \tome -> do
        abilities <-
          map (doesNotProvokeAttacksOfOpportunity . (`applyAbilityModifiers` [IgnoreActionCost]))
            <$> select
              ( AbilityOnAsset (AssetWithId tome)
                  <> oneOf [AbilityIsActionAbility, AbilityIsFastAbility]
                  <> PerformableAbility [IgnoreActionCost]
              )
        when (notNull abilities) do
          chooseOne iid $ Label "Do not use ability" [] : [AbilityLabel iid ab [] [] [] | ab <- abilities]
      pure e
    _ -> ThrowTheBookAtThem . (`with` meta) <$> liftRunMessage msg attrs
