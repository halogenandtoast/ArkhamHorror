module Arkham.Asset.Cards.TheRedGlovedMan5 (
  theRedGlovedMan5,
  TheRedGlovedMan5 (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Matcher
import Arkham.SkillType

newtype Metadata = Metadata {chosenSkills :: [SkillType]}
  deriving stock (Show, Generic, Eq)
  deriving anyclass (ToJSON, FromJSON, NoThunks, NFData)

newtype TheRedGlovedMan5 = TheRedGlovedMan5 (AssetAttrs `With` Metadata)
  deriving anyclass (IsAsset)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

theRedGlovedMan5 :: AssetCard TheRedGlovedMan5
theRedGlovedMan5 = ally (TheRedGlovedMan5 . (`with` Metadata [])) Cards.theRedGlovedMan5 (4, 4)

instance HasAbilities TheRedGlovedMan5 where
  getAbilities (TheRedGlovedMan5 (x `With` _)) =
    [ restrictedAbility x 1 ControlsThis $ freeReaction (AssetEntersPlay #when (AssetWithId $ toId x))
    , restrictedAbility x 2 ControlsThis $ ForcedAbility (PhaseEnds #when #mythos)
    ]

instance HasModifiersFor TheRedGlovedMan5 where
  getModifiersFor (InvestigatorTarget iid) (TheRedGlovedMan5 (a `With` Metadata {..})) | controlledBy a iid = do
    pure $ toModifiers a $ map (`BaseSkillOf` 6) chosenSkills
  getModifiersFor _ _ = pure []

skillTypes :: [(Text, SkillType)]
skillTypes =
  [ ("Willpower", #willpower)
  , ("Intellect", #intellect)
  , ("Combat", #combat)
  , ("Agility", #agility)
  ]

instance RunMessage TheRedGlovedMan5 where
  runMessage msg a@(TheRedGlovedMan5 (attrs `With` metadata)) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 windows' p -> do
      player <- getPlayer iid
      push
        $ chooseOne player
        $ [ Label label [UseCardAbilityChoice iid (toSource attrs) 1 (SkillChoiceMetadata s) windows' p]
          | (label, s) <- skillTypes
          ]
      pure a
    UseCardAbilityChoice iid (isSource attrs -> True) 1 (SkillChoiceMetadata c) windows' p -> do
      let source = toAbilitySource attrs 1
      case metadata of
        Metadata [] -> do
          player <- getPlayer iid
          push
            $ chooseOne player
            $ [ Label label [UseCardAbilityChoice iid source 1 (SkillChoiceMetadata s) windows' p]
              | (label, s) <- filter ((/= c) . snd) skillTypes
              ]
          pure $ TheRedGlovedMan5 $ attrs `with` Metadata [c]
        Metadata [x] -> pure $ TheRedGlovedMan5 $ attrs `with` Metadata [c, x]
        _ -> error "Only two skills for the red gloved man"
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      push $ toDiscardBy iid (toAbilitySource attrs 2) attrs
      pure a
    _ -> TheRedGlovedMan5 . (`with` metadata) <$> runMessage msg attrs
