module Arkham.Asset.Assets.KeyLocusDefensiveBarrier (keyLocusDefensiveBarrier) where

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted hiding (AssetDefeated)
import Arkham.Constants
import Arkham.DamageEffect
import Arkham.Fight
import Arkham.Helpers.Enemy
import Arkham.Helpers.GameValue
import Arkham.Helpers.Modifiers (ModifierType (..), getModifiers, modifySelf)
import Arkham.Helpers.SkillTest.Lifted (fight)
import Arkham.History
import Arkham.Matcher
import Arkham.Message qualified as Msg
import Arkham.Scenarios.DogsOfWar.Helpers (pattern IsKeyLocus)
import Arkham.Window qualified as Window

newtype KeyLocusDefensiveBarrier = KeyLocusDefensiveBarrier AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

keyLocusDefensiveBarrier :: AssetCard KeyLocusDefensiveBarrier
keyLocusDefensiveBarrier = assetWith KeyLocusDefensiveBarrier Cards.keyLocusDefensiveBarrier (healthL ?~ 3)

instance HasModifiersFor KeyLocusDefensiveBarrier where
  getModifiersFor (KeyLocusDefensiveBarrier a) = do
    modifySelf a [IsKeyLocus, CanBeAttackedAsIfEnemy]

instance HasAbilities KeyLocusDefensiveBarrier where
  getAbilities (KeyLocusDefensiveBarrier a) =
    [ basicAbility $ restricted a AbilityAttack OnSameLocation $ ActionAbility [#fight] #combat (ActionCost 1)
    , restricted a 1 OnSameLocation $ actionAbilityWithCost (ClueCost $ Static 1)
    , mkAbility a 2 $ forced $ AssetDefeated #when ByAny (be a)
    ]

instance RunMessage KeyLocusDefensiveBarrier where
  runMessage msg a@(KeyLocusDefensiveBarrier attrs) = runQueueT $ case msg of
    PlaceAsset aid _ | aid == attrs.id -> do
      attrs' <- liftRunMessage msg attrs
      n <- perPlayer 3
      pure $ KeyLocusDefensiveBarrier $ attrs' & healthL ?~ n
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      dealAssetDamage attrs.id (attrs.ability 1) 2
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      addToVictory iid attrs
      pure a
    UseThisAbility iid (isSource attrs -> True) AbilityAttack -> do
      sid <- getRandom
      push
        $ FightEnemy (coerce attrs.id)
        $ mkChooseFightPure sid iid (attrs.ability AbilityAttack)
      pure a
    AttackEnemy eid fchoose | coerce eid == attrs.id -> do
      let iid = fchoose.investigator
      let source = fchoose.source
      let sid = fchoose.skillTest
      let target = maybe (toTarget attrs) (ProxyTarget (toTarget attrs)) fchoose.target
      let skillType = fchoose.skillType
      let
        difficulty =
          case fchoose.difficulty of
            DefaultChooseFightDifficulty -> Fixed 4
            CalculatedChooseFightDifficulty c -> c

      fight sid iid source target skillType difficulty
      pure a
    PassedSkillTest iid (Just Action.Fight) source (Initiator target) _ n | isActionTarget attrs target -> do
      updateHistory iid (HistoryItem HistorySuccessfulAttacks 1)
      push $ Successful (Action.Fight, toProxyTarget target) iid source (toActionTarget target) n
      pure a
    Successful (Action.Fight, _) iid source target n | isTarget attrs target -> do
      mods <- getModifiers attrs
      let alternateSuccess = [t | AlternateSuccess t <- mods]
      pushWhen (null alternateSuccess) $ InvestigatorDamageEnemy iid (coerce attrs.id) source
      for_ alternateSuccess $ \target' ->
        push $ Successful (Action.Fight, toTarget attrs) iid source target' n
      pure a
    Msg.EnemyDamage eid damageAssignment | coerce eid == attrs.id -> do
      mods <- getModifiers attrs
      unless (CannotBeDamaged `elem` mods) do
        let
          source = damageAssignmentSource damageAssignment
          damageEffect = damageAssignmentDamageEffect damageAssignment
          damageAmount = damageAssignmentAmount damageAssignment
        checkWhen $ Window.DealtDamage source damageEffect (toTarget attrs) damageAmount
        checkAfter $ Window.DealtDamage source damageEffect (toTarget attrs) damageAmount
        checkWhen $ Window.TakeDamage source damageEffect (toTarget attrs) damageAmount
        push $ EnemyDamaged eid damageAssignment
        checkAfter $ Window.TakeDamage source damageEffect (toTarget attrs) damageAmount
      pure a
    EnemyDamaged eid damageAssignment | coerce eid == attrs.id -> do
      mods <- getModifiers attrs
      unless (CannotBeDamaged `elem` mods) do
        amount' <- getModifiedDamageAmount attrs damageAssignment
        dealAssetDamage attrs.id damageAssignment.source amount'
      pure a
    _ -> KeyLocusDefensiveBarrier <$> liftRunMessage msg attrs
