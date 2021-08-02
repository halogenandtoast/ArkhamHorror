module Arkham.Types.Asset.Cards.LoneWolf
  ( loneWolf
  , LoneWolf(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Asset.Cards as Cards
import Arkham.Types.Ability
import Arkham.Types.Asset.Attrs
import Arkham.Types.Card.CardDef
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Message
import Arkham.Types.Window

newtype LoneWolf = LoneWolf AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

loneWolf :: AssetCard LoneWolf
loneWolf = asset LoneWolf Cards.loneWolf

ability :: AssetAttrs -> Ability
ability x = (mkAbility (toSource x) 1 (ReactionAbility Free))
  { abilityRestrictions = Just InvestigatorIsAlone
  }

instance HasActions env LoneWolf where
  getActions i (WhenTurnBegins who) (LoneWolf x) | ownedBy x i && i == who =
    pure [UseAbility i (ability x)]
  getActions iid window (LoneWolf attrs) = getActions iid window attrs

instance HasModifiersFor env LoneWolf

instance (HasQueue env, HasModifiersFor env ()) => RunMessage env LoneWolf where
  runMessage msg a@(LoneWolf attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      a <$ push (TakeResources iid 1 False)
    _ -> LoneWolf <$> runMessage msg attrs
