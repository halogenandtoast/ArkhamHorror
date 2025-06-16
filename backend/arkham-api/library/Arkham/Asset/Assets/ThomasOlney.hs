module Arkham.Asset.Assets.ThomasOlney (
  thomasOlney,
  ThomasOlney(..),
) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Matcher
import Arkham.Trait
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted (commitCard, discardTopOfDeckAndHandle)
import Data.Text (unpack)
import Text.Read (readMaybe)

newtype Meta = Meta { chosenTrait :: Maybe Trait }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype ThomasOlney = ThomasOlney (With AssetAttrs Meta)
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

thomasOlney :: AssetCard ThomasOlney
thomasOlney = ally (ThomasOlney . (`with` Meta Nothing)) Cards.thomasOlney (3, 1)

instance HasAbilities ThomasOlney where
  getAbilities (ThomasOlney (With a _)) =
    [ controlledAbility a 1 ControlsThis $ FastAbility (exhaust a)
    , restrictedAbility a 2 (ControlsThis <> DuringSkillTest SkillTestAtYourLocation) $ FastAbility (exhaust a)
    ]

instance RunMessage ThomasOlney where
  runMessage msg a@(ThomasOlney (With attrs meta)) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      chooseOneDropDown iid
        [ (tshow trait, HandleTargetChoice iid (attrs.ability 1) (LabeledTarget (tshow trait) (toTarget attrs)))
        | trait <- [minBound ..]
        ]
      pure a
    HandleTargetChoice iid (isAbilitySource attrs 1 -> True) (LabeledTarget t (isTarget attrs -> True)) -> do
      case readMaybe (unpack t) of
        Just trait -> do
          push $ DiscardTopOfEncounterDeck iid 1 (attrs.ability 1) (Just $ toTarget attrs)
          pure $ ThomasOlney $ With attrs (meta {chosenTrait = Just trait})
        Nothing -> pure a
    DiscardedTopOfEncounterDeck iid cards _ (isTarget attrs -> True) -> do
      for_ meta.chosenTrait \trait -> do
        when (any (`cardMatch` CardWithTrait trait) cards) $ gainResources iid (attrs.ability 1) 2
      pure $ ThomasOlney $ With attrs (meta {chosenTrait = Nothing})
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      discardTopOfDeckAndHandle iid (attrs.ability 2) 1 attrs
      pure a
    DiscardedTopOfDeck iid (card : _) _ (isTarget attrs -> True) -> do
      withSkillTest \_ -> commitCard iid (PlayerCard card)
      pure a
    _ -> ThomasOlney . (`with` meta) <$> liftRunMessage msg attrs
