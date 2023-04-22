module Arkham.Asset.Cards.TreasureHunter1
  ( treasureHunter1
  , TreasureHunter1(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Matcher
import Arkham.Phase
import Arkham.SkillType
import Arkham.Timing qualified as Timing

newtype TreasureHunter1 = TreasureHunter1 AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

treasureHunter1 :: AssetCard TreasureHunter1
treasureHunter1 = ally TreasureHunter1 Cards.treasureHunter1 (2, 2)

instance HasModifiersFor TreasureHunter1 where
  getModifiersFor (InvestigatorTarget iid) (TreasureHunter1 a)
    | controlledBy a iid = pure $ toModifiers a [SkillModifier SkillIntellect 1]
  getModifiersFor _ _ = pure []

instance HasAbilities TreasureHunter1 where
  getAbilities (TreasureHunter1 x) =
    [ restrictedAbility x 1 ControlsThis
        $ ForcedAbility
        $ PhaseEnds Timing.When
        $ PhaseIs UpkeepPhase
    ]

instance RunMessage TreasureHunter1 where
  runMessage msg a@(TreasureHunter1 attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> a <$ push
      (chooseOne
        iid
        [ Label "Pay 1 Resource to Treasure Hunter" [SpendResources iid 1]
        , Label "Discard Treasure Hunter" [Discard (toAbilitySource attrs 1) $ toTarget attrs]
        ]
      )
    _ -> TreasureHunter1 <$> runMessage msg attrs
