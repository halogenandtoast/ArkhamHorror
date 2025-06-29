module Arkham.Asset.Assets.RecallTheFuture2 (recallTheFuture2) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.ChaosToken
import Arkham.Helpers.Modifiers qualified as Msg
import Arkham.Helpers.SkillTest (withSkillTest)
import Arkham.Matcher
import Arkham.Message qualified as Msg
import Arkham.Message.Lifted.Choose
import Arkham.Modifier
import Arkham.Window qualified as Window

newtype Metadata = Metadata {chosenChaosToken :: Maybe ChaosTokenFace}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype RecallTheFuture2 = RecallTheFuture2 (AssetAttrs `With` Metadata)
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

recallTheFuture2 :: AssetCard RecallTheFuture2
recallTheFuture2 = asset (RecallTheFuture2 . (`with` Metadata Nothing)) Cards.recallTheFuture2

instance HasAbilities RecallTheFuture2 where
  getAbilities (RecallTheFuture2 a) =
    [restricted a 1 ControlsThis $ freeReaction $ InitiatedSkillTest #when You #any #any #any]

instance RunMessage RecallTheFuture2 where
  runMessage msg a@(RecallTheFuture2 (attrs `With` metadata)) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      tokens <- sort . nub . map (.face) <$> select (IncludeSealed $ IncludeTokenPool AnyChaosToken)
      chooseOneM iid do
        for_ tokens \t -> labeled (tshow t) $ handleTarget iid attrs t
      pure a
    HandleTargetChoice _ (isSource attrs -> True) (ChaosTokenFaceTarget t) -> do
      pure . RecallTheFuture2 $ attrs `with` Metadata (Just t)
    When (Msg.RevealChaosToken _ _ token) | Just token.face == chosenChaosToken metadata -> do
      unless (assetExhausted attrs) $ for_ (assetController attrs) $ \iid -> do
        withSkillTest \sid -> do
          enable <- Msg.skillTestModifier sid attrs iid (AnySkillValue 2)
          push
            $ If
              (Window.RevealChaosTokenAssetAbilityEffect iid [token] (toId attrs))
              [ExhaustThen (toTarget attrs) [enable]]
      pure a
    SkillTestEnds {} -> pure . RecallTheFuture2 $ attrs `with` Metadata Nothing
    _ -> RecallTheFuture2 . (`with` metadata) <$> liftRunMessage msg attrs
