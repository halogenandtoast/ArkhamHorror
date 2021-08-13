module Arkham.Types.Asset.Cards.TheNecronomiconOlausWormiusTranslation
  ( theNecronomiconOlausWormiusTranslation
  , TheNecronomiconOlausWormiusTranslation(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Asset.Cards as Cards
import Arkham.Types.Ability
import Arkham.Types.Asset.Attrs
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Game.Helpers
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Restriction
import Arkham.Types.SkillType
import Arkham.Types.Target

newtype TheNecronomiconOlausWormiusTranslation = TheNecronomiconOlausWormiusTranslation AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theNecronomiconOlausWormiusTranslation
  :: AssetCard TheNecronomiconOlausWormiusTranslation
theNecronomiconOlausWormiusTranslation = hand
  TheNecronomiconOlausWormiusTranslation
  Cards.theNecronomiconOlausWormiusTranslation

instance HasActions TheNecronomiconOlausWormiusTranslation where
  getActions (TheNecronomiconOlausWormiusTranslation a) =
    [restrictedAbility a 1 OwnsThis $ ActionAbility Nothing $ ActionCost 1]

instance HasModifiersFor env TheNecronomiconOlausWormiusTranslation where
  getModifiersFor _ (InvestigatorTarget iid) (TheNecronomiconOlausWormiusTranslation a)
    = pure $ toModifiers a [ SkillModifier SkillIntellect 1 | ownedBy a iid ]
  getModifiersFor _ _ _ = pure []

instance (HasQueue env, HasModifiersFor env ()) => RunMessage env TheNecronomiconOlausWormiusTranslation where
  runMessage msg a@(TheNecronomiconOlausWormiusTranslation attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      a <$ push (TakeResources iid 2 False)
    _ -> TheNecronomiconOlausWormiusTranslation <$> runMessage msg attrs
