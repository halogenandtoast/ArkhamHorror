module Arkham.Asset.Assets.WhittonGreene (whittonGreene, WhittonGreene (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Matcher
import Arkham.Matcher qualified as Matcher
import Arkham.Prelude
import Arkham.Trait

newtype WhittonGreene = WhittonGreene AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

whittonGreene :: AssetCard WhittonGreene
whittonGreene = ally WhittonGreene Cards.whittonGreene (2, 2)

instance HasAbilities WhittonGreene where
  getAbilities (WhittonGreene x) =
    [ restrictedAbility x 1 ControlsThis
        $ ReactionAbility
          (oneOf [Matcher.RevealLocation #after You Anywhere, PutLocationIntoPlay #after You Anywhere])
          (exhaust x)
    ]

instance HasModifiersFor WhittonGreene where
  getModifiersFor (WhittonGreene a) = case a.controller of
    Nothing -> pure mempty
    Just iid -> maybeModified_ a iid do
      liftGuardM $ selectAny $ assetControlledBy iid <> oneOf (withTrait <$> [Tome, Relic])
      pure [SkillModifier #intellect 1]

instance RunMessage WhittonGreene where
  runMessage msg a@(WhittonGreene attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let matcher = basic $ #asset <> oneOf (withTrait <$> [Tome, Relic])
      push $ search iid (attrs.ability 1) iid [fromTopOfDeck 6] matcher (DrawFound iid 1)
      pure a
    _ -> WhittonGreene <$> runMessage msg attrs
