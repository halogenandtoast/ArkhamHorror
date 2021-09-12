module Arkham.Types.Asset.Cards.DavidRenfield
  ( davidRenfield
  , DavidRenfield(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Asset.Cards as Cards
import Arkham.Types.Ability
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Helpers
import Arkham.Types.Asset.Runner
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Criteria
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.SkillType
import Arkham.Types.Target

newtype DavidRenfield = DavidRenfield AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

davidRenfield :: AssetCard DavidRenfield
davidRenfield = ally DavidRenfield Cards.davidRenfield (2, 1)

instance HasModifiersFor env DavidRenfield where
  getModifiersFor _ (InvestigatorTarget iid) (DavidRenfield attrs)
    | attrs `ownedBy` iid = pure $ toModifiers
      attrs
      [ SkillModifier SkillWillpower 1 | assetDoom attrs > 0 ]
  getModifiersFor _ _ _ = pure []

instance HasAbilities DavidRenfield where
  getAbilities (DavidRenfield a) =
    [restrictedAbility a 1 OwnsThis $ FastAbility $ ExhaustCost $ toTarget a]

instance AssetRunner env => RunMessage env DavidRenfield where
  runMessage msg a@(DavidRenfield attrs) = case msg of
    UseCardAbility iid source windows' 1 p | isSource attrs source -> do
      let
        resolveAbility =
          UseCardAbilityChoice iid source windows' 1 p NoAbilityMetadata
      a <$ push
        (chooseOne
          iid
          [ Label
            "Place doom on David Renfield"
            [PlaceDoom (toTarget attrs) 1, resolveAbility]
          , Label "Do not place doom on David Renfield" [resolveAbility]
          ]
        )
    UseCardAbilityChoice iid source _ 1 _ _ | isSource attrs source ->
      a <$ push (TakeResources iid (assetDoom attrs) False)
    _ -> DavidRenfield <$> runMessage msg attrs
