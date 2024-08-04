module Arkham.Asset.Cards.TidalMemento (tidalMemento, TidalMemento (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.ChaosBagStepState
import Arkham.ChaosToken
import Arkham.Enemy.Types (Field (..))
import Arkham.Helpers.Window (getThatEnemy)
import Arkham.Matcher
import Arkham.Message (pattern CancelNext)
import Arkham.Message.Type
import Arkham.Projection
import Arkham.Window (getBatchId)

newtype TidalMemento = TidalMemento AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

tidalMemento :: AssetCard TidalMemento
tidalMemento = asset TidalMemento Cards.tidalMemento

instance HasAbilities TidalMemento where
  getAbilities (TidalMemento a) =
    [ restrictedAbility a 1 ControlsThis
        $ ReactionAbility
          ( oneOf
              [ EnemyWouldReady #when AnyEnemy
              , WouldPlaceDoomCounter #when AnySource $ EnemyTargetMatches AnyEnemy
              ]
          )
          (ReleaseChaosTokensCost 1 $ SealedOnEnemy ThatEnemy $ oneOf [#curse, #bless])
    , controlledAbility
        a
        2
        ( DuringSkillTest (YourSkillTest #any)
            <> exists (EnemyWithSealedChaosTokens 1 $ oneOf [#curse, #bless])
        )
        $ freeReaction
          (WouldRevealChaosToken #when $ affectsOthers $ InvestigatorAt YourLocation)
    ]

instance RunMessage TidalMemento where
  runMessage msg a@(TidalMemento attrs) = runQueueT $ case msg of
    UseCardAbility _iid (isSource attrs -> True) 1 ws@(getThatEnemy -> Just _eid) _ -> do
      push $ CancelBatch $ getBatchId ws
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      selectOneToHandle iid (attrs.ability 2) $ EnemyWithSealedChaosTokens 1 $ oneOf [#curse, #bless]
      pure a
    HandleTargetChoice iid (isAbilitySource attrs 2 -> True) (EnemyTarget eid) -> do
      tokens <- filter ((`elem` [CurseToken, BlessToken]) . (.face)) <$> field EnemySealedChaosTokens eid
      push $ FocusChaosTokens tokens
      chooseOrRunOne
        iid
        [targetLabel token [UnfocusChaosTokens, ForTarget (toTarget token) msg] | token <- tokens]
      pure a
    ForTarget
      (ChaosTokenTarget token)
      (HandleTargetChoice iid (isAbilitySource attrs 2 -> True) _) -> do
        pushAll
          [ CancelNext (attrs.ability 2) RunWindowMessage
          , UnsealChaosToken token
          , ReplaceCurrentDraw (toSource attrs) iid
              $ Choose (toSource attrs) 1 ResolveChoice [Resolved [token]] []
          ]
        pure a
    _ -> TidalMemento <$> liftRunMessage msg attrs
