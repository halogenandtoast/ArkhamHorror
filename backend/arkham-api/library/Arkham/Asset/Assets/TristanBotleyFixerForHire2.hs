module Arkham.Asset.Assets.TristanBotleyFixerForHire2 (
  tristanBotleyFixerForHire2,
  TristanBotleyFixerForHire2 (..),
)
where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted hiding (SkillTestEnded)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Modifier
import Arkham.SkillType

newtype TristanBotleyFixerForHire2 = TristanBotleyFixerForHire2 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

tristanBotleyFixerForHire2 :: AssetCard TristanBotleyFixerForHire2
tristanBotleyFixerForHire2 = ally TristanBotleyFixerForHire2 Cards.tristanBotleyFixerForHire2 (3, 2)

instance HasAbilities TristanBotleyFixerForHire2 where
  getAbilities (TristanBotleyFixerForHire2 a) =
    [ restrictedAbility a 1 ControlsThis $ freeReaction (TurnBegins #after You)
    , restrictedAbility a 2 InYourHand
        $ freeReaction
        $ SkillTestEnded #after Anyone
        $ SkillTestWithRevealedChaosTokenCount 3
        $ oneOf [#bless, #curse]
    ]

instance RunMessage TristanBotleyFixerForHire2 where
  runMessage msg a@(TristanBotleyFixerForHire2 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      chooseNM iid 2 $ for_ allSkills \s -> do
        skillLabeled s $ nextTurnModifier iid (attrs.ability 1) iid (SkillModifier s 1)
      pure a
    InHand iid (UseThisAbility iid' (isSource attrs -> True) 2) | iid == iid' -> do
      putCardIntoPlay iid attrs
      pure a
    _ -> TristanBotleyFixerForHire2 <$> liftRunMessage msg attrs
