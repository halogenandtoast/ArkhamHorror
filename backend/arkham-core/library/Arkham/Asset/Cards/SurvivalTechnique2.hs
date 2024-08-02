module Arkham.Asset.Cards.SurvivalTechnique2 (
  survivalTechnique2,
  SurvivalTechnique2 (..),
)
where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.SkillTest (withSkillTest)
import Arkham.Matcher
import Arkham.Modifier

newtype SurvivalTechnique2 = SurvivalTechnique2 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

survivalTechnique2 :: AssetCard SurvivalTechnique2
survivalTechnique2 = asset SurvivalTechnique2 Cards.survivalTechnique2

instance HasAbilities SurvivalTechnique2 where
  getAbilities (SurvivalTechnique2 a) =
    [ restrictedAbility a 1 ControlsThis
        $ FastAbility
          (ChooseExtendedCardCost (OwnedBy You <> CardIsAttachedToLocation YourLocation) <> exhaust a)
    , controlledAbility
        a
        2
        ( DuringSkillTest
            $ oneOf
              [SkillTestOnLocation YourLocation, SkillTestOnTreachery (TreacheryAttachedToLocation YourLocation)]
        )
        $ FastAbility (exhaust a)
    ]

instance RunMessage SurvivalTechnique2 where
  runMessage msg a@(SurvivalTechnique2 attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ (chosenCardPayment -> Just cardId) -> do
      selectOne (AssetWithCardId cardId) >>= traverse_ (returnToHand iid)
      selectOne (EventWithCardId cardId) >>= traverse_ (returnToHand iid)
      selectOne (SkillWithCardId cardId) >>= traverse_ (returnToHand iid)
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      withSkillTest \sid -> do
        skillTestModifier sid (attrs.ability 2) iid (AnySkillValue 2)
      pure a
    _ -> SurvivalTechnique2 <$> liftRunMessage msg attrs
