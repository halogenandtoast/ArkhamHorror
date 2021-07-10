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
import Arkham.Types.Id
import Arkham.Types.Message
import Arkham.Types.WindowMatcher

newtype LoneWolf = LoneWolf AssetAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

loneWolf :: AssetCard LoneWolf
loneWolf = asset LoneWolf Cards.loneWolf

ability :: AssetAttrs -> Ability
ability x = (assetAbility x 1 (ReactionAbility (WhenTurnBegins You) Free))
  { abilityRestrictions = Just
    $ AllAbilityRestrictions [InvestigatorIsAlone, InvestigatorIsOwner]
  }

instance HasAbilities LoneWolf where
  getAbilities (LoneWolf x) = [ability x]

instance HasModifiersFor env LoneWolf

instance (HasSet InvestigatorId env (), HasQueue env, HasModifiersFor env ()) => RunMessage env LoneWolf where
  runMessage msg a@(LoneWolf attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      a <$ push (TakeResources iid 1 False)
    _ -> LoneWolf <$> runMessage msg attrs
