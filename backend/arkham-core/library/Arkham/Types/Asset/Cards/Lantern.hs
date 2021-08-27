module Arkham.Types.Asset.Cards.Lantern
  ( lantern
  , Lantern(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Asset.Cards as Cards
import Arkham.Types.Ability
import qualified Arkham.Types.Action as Action
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Helpers
import Arkham.Types.Asset.Runner
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Criteria
import Arkham.Types.Id
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.SkillType
import Arkham.Types.Target

newtype Lantern = Lantern AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lantern :: AssetCard Lantern
lantern = hand Lantern Cards.lantern

instance HasAbilities env Lantern where
  getAbilities _ _ (Lantern x) = pure
    [ restrictedAbility x 1 OwnsThis
    $ ActionAbility (Just Action.Investigate)
    $ ActionCost 1
    , restrictedAbility
      x
      2
      (OwnsThis <> EnemyCriteria (EnemyExists $ EnemyAt YourLocation))
    $ ActionAbility Nothing
    $ DiscardCost
    $ toTarget x
    ]

instance AssetRunner env => RunMessage env Lantern where
  runMessage msg a@(Lantern attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> do
      lid <- getId @LocationId iid
      a <$ pushAll
        [ skillTestModifier source (LocationTarget lid) (ShroudModifier (-1))
        , Investigate iid lid source SkillIntellect False
        ]
    UseCardAbility iid source _ 2 _ | isSource attrs source -> do
      targets <- selectListMap EnemyTarget $ EnemyAt YourLocation
      a <$ push
        (chooseOne
          iid
          [ TargetLabel target [Damage target source 1] | target <- targets ]
        )
    _ -> Lantern <$> runMessage msg attrs
