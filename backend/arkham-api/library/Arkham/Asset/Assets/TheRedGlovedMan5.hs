module Arkham.Asset.Assets.TheRedGlovedMan5 (theRedGlovedMan5) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Modifiers
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.SkillType

newtype Metadata = Metadata {chosenSkills :: [SkillType]}
  deriving stock (Show, Generic, Eq)
  deriving anyclass (ToJSON, FromJSON)

newtype TheRedGlovedMan5 = TheRedGlovedMan5 (AssetAttrs `With` Metadata)
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theRedGlovedMan5 :: AssetCard TheRedGlovedMan5
theRedGlovedMan5 = ally (TheRedGlovedMan5 . (`with` Metadata [])) Cards.theRedGlovedMan5 (4, 4)

instance HasAbilities TheRedGlovedMan5 where
  getAbilities (TheRedGlovedMan5 (x `With` _)) =
    [ restricted x 1 ControlsThis $ freeReaction (AssetEntersPlay #when (AssetWithId $ toId x))
    , restricted x 2 ControlsThis $ ForcedAbility (PhaseEnds #when #mythos)
    ]

instance HasModifiersFor TheRedGlovedMan5 where
  getModifiersFor (TheRedGlovedMan5 (a `With` Metadata {..})) = controllerGets a $ map (`BaseSkillOf` 6) chosenSkills

instance RunMessage TheRedGlovedMan5 where
  runMessage msg a@(TheRedGlovedMan5 (attrs `With` metadata)) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 windows' p -> do
      chooseOneM iid do
        for_ [#willpower, #intellect, #combat, #agility] \kind ->
          skillLabeled kind
            $ push
            $ UseCardAbilityChoice iid (toSource attrs) 1 (SkillChoiceMetadata kind) windows' p
      pure a
    UseCardAbilityChoice iid (isSource attrs -> True) 1 (SkillChoiceMetadata c) windows' p -> do
      let source = toAbilitySource attrs 1
      case metadata of
        Metadata [] -> do
          chooseOneM iid do
            for_ (filter (/= c) [#willpower, #intellect, #combat, #agility]) \kind ->
              skillLabeled kind $ push $ UseCardAbilityChoice iid source 1 (SkillChoiceMetadata kind) windows' p
          pure $ TheRedGlovedMan5 $ attrs `with` Metadata [c]
        Metadata [x] -> pure $ TheRedGlovedMan5 $ attrs `with` Metadata [c, x]
        _ -> error "Only two skills for the red gloved man"
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      toDiscardBy iid (attrs.ability 2) attrs
      pure a
    _ -> TheRedGlovedMan5 . (`with` metadata) <$> liftRunMessage msg attrs
