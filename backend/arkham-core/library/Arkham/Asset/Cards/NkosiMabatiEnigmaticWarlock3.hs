module Arkham.Asset.Cards.NkosiMabatiEnigmaticWarlock3 (
  nkosiMabatiEnigmaticWarlock3,
  NkosiMabatiEnigmaticWarlock3 (..),
)
where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted hiding (RevealChaosToken)
import Arkham.ChaosToken
import Arkham.Game.Helpers (cancelChaosToken)
import Arkham.Helpers.Message (handleTargetChoice)
import Arkham.Helpers.Window (getChaosToken)
import Arkham.Matcher
import Arkham.Message (MessageType (..))

newtype NkosiMabatiEnigmaticWarlock3 = NkosiMabatiEnigmaticWarlock3 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

nkosiMabatiEnigmaticWarlock3 :: AssetCard NkosiMabatiEnigmaticWarlock3
nkosiMabatiEnigmaticWarlock3 = ally NkosiMabatiEnigmaticWarlock3 Cards.nkosiMabatiEnigmaticWarlock3 (2, 2)

instance HasAbilities NkosiMabatiEnigmaticWarlock3 where
  getAbilities (NkosiMabatiEnigmaticWarlock3 a) =
    [ restrictedAbility a 1 ControlsThis (freeReaction $ AssetEntersPlay #when (be a))
    , controlledAbility a 2 tokenFaceCriteria
        $ ReactionAbility
          ( RevealChaosToken
              #when
              (affectsOthers $ InvestigatorAt YourLocation)
              (oneOf [#cultist, #tablet, #elderthing])
          )
          (exhaust a)
    ]
   where
    tokenFaceCriteria = case toResult a.meta of
      Nothing -> Never
      Just face -> (ChaosTokenCountIs (ChaosTokenFaceIs face) (atLeast 1))

instance RunMessage NkosiMabatiEnigmaticWarlock3 where
  runMessage msg a@(NkosiMabatiEnigmaticWarlock3 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let tokens = map (toSnd chaosTokenLabel) $ filter (/= ElderSign) allChaosTokenFaces
      chooseOne
        iid
        [Label lbl [handleTargetChoice iid attrs (ChaosTokenFaceTarget tkn)] | (tkn, lbl) <- tokens]
      pure a
    HandleTargetChoice _ (isSource attrs -> True) (ChaosTokenFaceTarget face) -> do
      pure . NkosiMabatiEnigmaticWarlock3 $ attrs & setMeta (Just face)
    UseCardAbility iid (isSource attrs -> True) 2 (getChaosToken -> token) _ -> do
      for_ (toResult @(Maybe ChaosTokenFace) attrs.meta) $ \face -> do
        let source = toAbilitySource attrs 2
        cancelChaosToken token
        pushAll
          [ CancelEachNext source [RunWindowMessage, DrawChaosTokenMessage, RevealChaosTokenMessage]
          , ReturnChaosTokens [token]
          , ForceChaosTokenDraw face
          , DrawAnotherChaosToken iid
          ]
      pure a
    _ -> NkosiMabatiEnigmaticWarlock3 <$> liftRunMessage msg attrs
