module Arkham.Asset.Assets.WhittonGreene2 (whittonGreene2, WhittonGreene2 (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Matcher
import Arkham.Matcher qualified as Matcher
import Arkham.Prelude
import Arkham.Trait

newtype WhittonGreene2 = WhittonGreene2 AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

whittonGreene2 :: AssetCard WhittonGreene2
whittonGreene2 = ally WhittonGreene2 Cards.whittonGreene2 (2, 3)

instance HasAbilities WhittonGreene2 where
  getAbilities (WhittonGreene2 x) =
    [ restrictedAbility x 1 ControlsThis
        $ ReactionAbility
          (oneOf [Matcher.RevealLocation #after You Anywhere, PutLocationIntoPlay #after You Anywhere])
          (exhaust x)
    ]

instance HasModifiersFor WhittonGreene2 where
  getModifiersFor (WhittonGreene2 a) = case a.controller of
    Nothing -> pure mempty
    Just iid -> maybeModified_ a iid do
      liftGuardM $ selectAny $ assetControlledBy iid <> oneOf (withTrait <$> [Tome, Relic])
      pure [SkillModifier #willpower 1, SkillModifier #intellect 1]

instance RunMessage WhittonGreene2 where
  runMessage msg a@(WhittonGreene2 attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let matcher = basic $ #asset <> oneOf (withTrait <$> [Tome, Relic])
      push $ search iid (attrs.ability 1) iid [fromTopOfDeck 9] matcher (DrawFound iid 1)
      pure a
    _ -> WhittonGreene2 <$> runMessage msg attrs
