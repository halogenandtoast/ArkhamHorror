module Arkham.Asset.Cards.DavidRenfield (
  davidRenfield,
  DavidRenfield (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner

newtype DavidRenfield = DavidRenfield AssetAttrs
  deriving anyclass (IsAsset)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

davidRenfield :: AssetCard DavidRenfield
davidRenfield = ally DavidRenfield Cards.davidRenfield (2, 1)

instance HasModifiersFor DavidRenfield where
  getModifiersFor (InvestigatorTarget iid) (DavidRenfield attrs) | attrs `controlledBy` iid = do
    pure $ toModifiers attrs [SkillModifier #willpower 1 | assetDoom attrs > 0]
  getModifiersFor _ _ = pure []

instance HasAbilities DavidRenfield where
  getAbilities (DavidRenfield a) = [restrictedAbility a 1 ControlsThis $ FastAbility $ exhaust a]

instance RunMessage DavidRenfield where
  runMessage msg a@(DavidRenfield attrs) = case msg of
    UseCardAbility iid source 1 windows' p | isSource attrs source -> do
      let
        resolveAbility =
          UseCardAbilityChoice iid source 1 NoAbilityMetadata windows' p
      player <- getPlayer iid
      push
        $ chooseOne
          player
          [ Label
              "Place doom on David Renfield"
              [PlaceDoom (toAbilitySource attrs 1) (toTarget attrs) 1, resolveAbility]
          , Label "Do not place doom on David Renfield" [resolveAbility]
          ]
      pure a
    UseCardAbilityChoice iid source 1 _ _ _ | isSource attrs source -> do
      push $ TakeResources iid (assetDoom attrs) (toAbilitySource attrs 1) False
      pure a
    _ -> DavidRenfield <$> runMessage msg attrs
