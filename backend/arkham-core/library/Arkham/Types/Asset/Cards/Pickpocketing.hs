module Arkham.Types.Asset.Cards.Pickpocketing
  ( Pickpocketing(..)
  , pickpocketing
  ) where

import Arkham.Prelude

import qualified Arkham.Asset.Cards as Cards
import Arkham.Types.Ability
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Runner
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Criteria
import Arkham.Types.Matcher
import Arkham.Types.Message hiding (EnemyEvaded)
import qualified Arkham.Types.Timing as Timing

newtype Pickpocketing = Pickpocketing AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

pickpocketing :: AssetCard Pickpocketing
pickpocketing = asset Pickpocketing Cards.pickpocketing

instance HasAbilities Pickpocketing where
  getAbilities (Pickpocketing a) =
    [ restrictedAbility a 1 OwnsThis $ ReactionAbility
        (EnemyEvaded Timing.After You AnyEnemy)
        (ExhaustCost $ toTarget a)
    ]

instance AssetRunner env => RunMessage env Pickpocketing where
  runMessage msg a@(Pickpocketing attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      a <$ push (DrawCards iid 1 False)
    _ -> Pickpocketing <$> runMessage msg attrs
