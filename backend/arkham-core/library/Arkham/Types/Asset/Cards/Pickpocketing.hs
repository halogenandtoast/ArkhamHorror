module Arkham.Types.Asset.Cards.Pickpocketing
  ( Pickpocketing(..)
  , pickpocketing
  ) where

import Arkham.Prelude

import Arkham.Asset.Cards qualified as Cards
import Arkham.Types.Ability
import Arkham.Types.Asset.Attrs
import Arkham.Types.Cost
import Arkham.Types.Criteria
import Arkham.Types.Matcher
import Arkham.Types.Matcher qualified as Matcher
import Arkham.Types.Timing qualified as Timing

newtype Pickpocketing = Pickpocketing AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

pickpocketing :: AssetCard Pickpocketing
pickpocketing = asset Pickpocketing Cards.pickpocketing

instance HasAbilities Pickpocketing where
  getAbilities (Pickpocketing a) =
    [ restrictedAbility a 1 OwnsThis $ ReactionAbility
        (Matcher.EnemyEvaded Timing.After You AnyEnemy)
        (ExhaustCost $ toTarget a)
    ]

instance AssetRunner env => RunMessage env Pickpocketing where
  runMessage msg a@(Pickpocketing attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      a <$ push (DrawCards iid 1 False)
    _ -> Pickpocketing <$> runMessage msg attrs
