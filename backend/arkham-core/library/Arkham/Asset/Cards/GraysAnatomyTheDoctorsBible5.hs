module Arkham.Asset.Cards.GraysAnatomyTheDoctorsBible5 (
  graysAnatomyTheDoctorsBible5,
  graysAnatomyTheDoctorsBible5Effect,
  GraysAnatomyTheDoctorsBible5 (..),
)
where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Card
import Arkham.Effect.Import
import Arkham.Helpers.SkillTest qualified as Msg
import Arkham.Helpers.SkillTest.Target
import Arkham.Matcher
import Arkham.Message qualified as Msg
import Arkham.Modifier

newtype GraysAnatomyTheDoctorsBible5 = GraysAnatomyTheDoctorsBible5 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

graysAnatomyTheDoctorsBible5 :: AssetCard GraysAnatomyTheDoctorsBible5
graysAnatomyTheDoctorsBible5 = asset GraysAnatomyTheDoctorsBible5 Cards.graysAnatomyTheDoctorsBible5

instance HasAbilities GraysAnatomyTheDoctorsBible5 where
  getAbilities (GraysAnatomyTheDoctorsBible5 attrs) =
    [restrictedAbility attrs 1 ControlsThis actionAbility]

instance RunMessage GraysAnatomyTheDoctorsBible5 where
  runMessage msg a@(GraysAnatomyTheDoctorsBible5 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      targets <-
        mconcat
          <$> sequence
            [ selectTargets $ EnemyAt (locationWithInvestigator iid)
            , selectTargets
                $ AssetAt (locationWithInvestigator iid)
                <> oneOf [AssetOwnedBy (affectsOthers Anyone), UnownedAsset]
            , selectTargets $ affectsOthers $ colocatedWith iid
            ]

      chooseOne iid
        $ [targetLabel t [Msg.beginSkillTest iid (attrs.ability 1) t #intellect (Fixed 1)] | t <- targets]

      pure a
    PassedThisSkillTestBy _iid (isAbilitySource attrs 1 -> True) n -> do
      doStep n msg
      pure a
    DoStep n msg'@(PassedThisSkillTestBy iid (isAbilitySource attrs 1 -> True) _) | n > 0 -> do
      let cardCode = toCardCode attrs
      let
        handleTarget :: forall m a. (Targetable a, ReverseQueue m) => a -> m ()
        handleTarget tid = do
          healX <-
            selectCount $ EffectWithTarget (toTarget tid) <> EffectWithCardCode cardCode <> EffectWithMetaInt 1
          damageX <-
            selectCount $ EffectWithTarget (toTarget tid) <> EffectWithCardCode cardCode <> EffectWithMetaInt 1

          chooseOne
            iid
            [ Label
                "The next time that card would be healed this round, heal +1 damage/horror (to a maximum of +3"
                $ [ Msg.createCardEffect Cards.graysAnatomyTheDoctorsBible5 (effectInt 1) (attrs.ability 1) tid
                  | healX < 3
                  ]
                <> [DoStep (n - 1) msg']
            , Label
                "The next time that card would be dealt damage this round, deal +1 damage/horror (to a maximum of +3"
                $ [ Msg.createCardEffect Cards.graysAnatomyTheDoctorsBible5 (effectInt 2) (attrs.ability 1) tid
                  | damageX < 3
                  ]
                <> [DoStep (n - 1) msg']
            ]
      withSkillTestTarget \case
        AssetTarget aid -> handleTarget aid
        EnemyTarget eid -> handleTarget eid
        InvestigatorTarget iid' -> handleTarget iid'
        _ -> error "Unhandled"
      pure a
    _ -> GraysAnatomyTheDoctorsBible5 <$> liftRunMessage msg attrs

newtype GraysAnatomyTheDoctorsBible5Effect = GraysAnatomyTheDoctorsBible5Effect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

instance HasModifiersFor GraysAnatomyTheDoctorsBible5Effect where
  getModifiersFor target (GraysAnatomyTheDoctorsBible5Effect a) | a.target == target = do
    case a.metaInt of
      Just 1 -> modified a [HealingTaken 1]
      Just 2 -> modified a [DamageTaken 1]
      _ -> error "Unhandled"
  getModifiersFor _ _ = pure []

graysAnatomyTheDoctorsBible5Effect :: EffectArgs -> GraysAnatomyTheDoctorsBible5Effect
graysAnatomyTheDoctorsBible5Effect =
  cardEffect GraysAnatomyTheDoctorsBible5Effect Cards.graysAnatomyTheDoctorsBible5

instance RunMessage GraysAnatomyTheDoctorsBible5Effect where
  runMessage msg e@(GraysAnatomyTheDoctorsBible5Effect attrs) = runQueueT $ case msg of
    AssignedHealing target
      | attrs.target == target && attrs.metaInt == Just 1 ->
          disableReturn e
    AssignedDamage target
      | attrs.target == target && attrs.metaInt == Just 2 ->
          disableReturn e
    _ -> GraysAnatomyTheDoctorsBible5Effect <$> liftRunMessage msg attrs
