module Arkham.Types.Asset.Cards.TheRedGlovedMan5
  ( theRedGlovedMan5
  , TheRedGlovedMan5(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Asset.Cards as Cards
import Arkham.Types.Ability
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Helpers
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Phase
import Arkham.Types.Restriction
import Arkham.Types.SkillType
import Arkham.Types.Target
import qualified Arkham.Types.Timing as Timing

newtype Metadata = Metadata { chosenSkills :: [SkillType] }
  deriving stock (Show, Generic, Eq)
  deriving anyclass (ToJSON, FromJSON)

newtype TheRedGlovedMan5 = TheRedGlovedMan5 (AssetAttrs `With` Metadata)
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theRedGlovedMan5 :: AssetCard TheRedGlovedMan5
theRedGlovedMan5 =
  ally (TheRedGlovedMan5 . (`with` Metadata [])) Cards.theRedGlovedMan5 (4, 4)

instance HasActions TheRedGlovedMan5 where
  getActions (TheRedGlovedMan5 (x `With` _)) =
    [ restrictedAbility
      x
      1
      OwnsThis
      (ReactionAbility (AssetEntersPlay Timing.When (AssetWithId $ toId x)) Free
      )
    , restrictedAbility x 2 OwnsThis
      $ ForcedAbility (PhaseEnds Timing.When $ PhaseIs MythosPhase)
    ]

instance HasModifiersFor env TheRedGlovedMan5 where
  getModifiersFor _ (InvestigatorTarget iid) (TheRedGlovedMan5 (a `With` Metadata {..}))
    | ownedBy a iid
    = pure $ toModifiers a $ map (`BaseSkillOf` 6) chosenSkills
  getModifiersFor _ _ _ = pure []

skillTypes :: [(Text, SkillType)]
skillTypes =
  [ ("Willpower", SkillWillpower)
  , ("Intellect", SkillIntellect)
  , ("Combat", SkillCombat)
  , ("Agility", SkillAgility)
  ]

instance (HasQueue env, HasModifiersFor env ()) => RunMessage env TheRedGlovedMan5 where
  runMessage msg a@(TheRedGlovedMan5 (attrs `With` metadata)) = case msg of
    UseCardAbility iid source windows 1 p | isSource attrs source -> a <$ push
      (chooseOne
        iid
        [ Label
            label
            [ UseCardAbilityChoice
                iid
                source
                windows
                1
                p
                (SkillChoiceMetadata s)
            ]
        | (label, s) <- skillTypes
        ]
      )
    UseCardAbilityChoice iid source windows 1 p (SkillChoiceMetadata c)
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
                      windows
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
