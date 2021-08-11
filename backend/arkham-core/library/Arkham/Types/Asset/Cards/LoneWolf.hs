module Arkham.Types.Asset.Cards.LoneWolf
  ( loneWolf
  , LoneWolf(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Asset.Cards as Cards
import Arkham.Types.Ability
import Arkham.Types.Asset.Attrs
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Matcher
import Arkham.Types.Message hiding (When)
import Arkham.Types.Restriction
import Arkham.Types.Timing

newtype LoneWolf = LoneWolf AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

loneWolf :: AssetCard LoneWolf
loneWolf = asset LoneWolf Cards.loneWolf

instance HasActions LoneWolf where
  getActions (LoneWolf x) =
    [ restrictedAbility
        x
        1
        (OwnsThis <> InvestigatorIsAlone)
        (ReactionAbility (TurnBegins When You) Free)
    ]

instance HasModifiersFor env LoneWolf

instance (HasQueue env, HasModifiersFor env ()) => RunMessage env LoneWolf where
  runMessage msg a@(LoneWolf attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      a <$ push (TakeResources iid 1 False)
    _ -> LoneWolf <$> runMessage msg attrs
