module Arkham.Asset.Cards.TheRedGlovedMan5
  ( theRedGlovedMan5
  , TheRedGlovedMan5(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Cost
import Arkham.Criteria
import Arkham.Matcher
import Arkham.Phase
import Arkham.SkillType
import Arkham.Target
import Arkham.Timing qualified as Timing

newtype Metadata = Metadata { chosenSkills :: [SkillType] }
  deriving stock (Show, Generic, Eq)
  deriving anyclass (ToJSON, FromJSON)

newtype TheRedGlovedMan5 = TheRedGlovedMan5 (AssetAttrs `With` Metadata)
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theRedGlovedMan5 :: AssetCard TheRedGlovedMan5
theRedGlovedMan5 =
  ally (TheRedGlovedMan5 . (`with` Metadata [])) Cards.theRedGlovedMan5 (4, 4)

instance HasAbilities TheRedGlovedMan5 where
  getAbilities (TheRedGlovedMan5 (x `With` _)) =
    [ restrictedAbility x 1 ControlsThis
      $ ReactionAbility
          (AssetEntersPlay Timing.When (AssetWithId $ toId x))
          Free
    , restrictedAbility x 2 ControlsThis
      $ ForcedAbility (PhaseEnds Timing.When $ PhaseIs MythosPhase)
    ]

instance HasModifiersFor TheRedGlovedMan5 where
  getModifiersFor (InvestigatorTarget iid) (TheRedGlovedMan5 (a `With` Metadata {..}))
    | controlledBy a iid
    = pure $ toModifiers a $ map (`BaseSkillOf` 6) chosenSkills
  getModifiersFor _ _ = pure []

skillTypes :: [(Text, SkillType)]
skillTypes =
  [ ("Willpower", SkillWillpower)
  , ("Intellect", SkillIntellect)
  , ("Combat", SkillCombat)
  , ("Agility", SkillAgility)
  ]

instance RunMessage TheRedGlovedMan5 where
  runMessage msg a@(TheRedGlovedMan5 (attrs `With` metadata)) = case msg of
    UseCardAbility iid source windows' 1 p | isSource attrs source -> a <$ push
      (chooseOne
        iid
        [ Label
            label
            [ UseCardAbilityChoice
                iid
                source
                windows'
                1
                p
                (SkillChoiceMetadata s)
            ]
        | (label, s) <- skillTypes
        ]
      )
    UseCardAbilityChoice iid source windows' 1 p (SkillChoiceMetadata c)
      | isSource attrs source -> case metadata of
        Metadata [] -> do
          push
            (chooseOne
              iid
              [ Label
                  label
                  [ UseCardAbilityChoice
                      iid
                      source
                      windows'
                      1
                      p
                      (SkillChoiceMetadata s)
                  ]
              | (label, s) <- filter ((/= c) . snd) skillTypes
              ]
            )
          pure $ TheRedGlovedMan5 $ attrs `with` Metadata [c]
        Metadata [x] -> pure $ TheRedGlovedMan5 $ attrs `with` Metadata [c, x]
        _ -> error "Only two skills for the red gloved man"
    UseCardAbility _ source _ 2 _ | isSource attrs source ->
      a <$ push (Discard $ toTarget attrs)
    _ -> TheRedGlovedMan5 . (`with` metadata) <$> runMessage msg attrs
