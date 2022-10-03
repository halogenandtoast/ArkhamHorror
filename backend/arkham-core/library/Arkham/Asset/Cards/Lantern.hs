module Arkham.Asset.Cards.Lantern
  ( lantern
  , Lantern(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Action qualified as Action
import Arkham.Asset.Runner
import Arkham.Cost
import Arkham.Criteria
import Arkham.Investigator.Types (Field(..))
import Arkham.Matcher
import Arkham.Projection
import Arkham.SkillType
import Arkham.Target

newtype Lantern = Lantern AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lantern :: AssetCard Lantern
lantern = asset Lantern Cards.lantern

instance HasAbilities Lantern where
  getAbilities (Lantern x) =
    [ restrictedAbility x 1 ControlsThis
      $ ActionAbility (Just Action.Investigate)
      $ ActionCost 1
    , restrictedAbility
        x
        2
        (ControlsThis <> EnemyCriteria (EnemyExists $ EnemyAt YourLocation))
      $ ActionAbility Nothing
      $ DiscardCost
      $ toTarget x
    ]

instance RunMessage Lantern where
  runMessage msg a@(Lantern attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      lid <- fieldMap InvestigatorLocation (fromJustNote "must be at a location") iid
      a <$ pushAll
        [ skillTestModifier source (LocationTarget lid) (ShroudModifier (-1))
        , Investigate iid lid source Nothing SkillIntellect False
        ]
    InDiscard _ (UseCardAbility iid source 2 _ _) | isSource attrs source -> do
      targets <- selectListMap EnemyTarget $ EnemyAt YourLocation
      a <$ push
        (chooseOne
          iid
          [ TargetLabel target [Damage target source 1] | target <- targets ]
        )
    _ -> Lantern <$> runMessage msg attrs
