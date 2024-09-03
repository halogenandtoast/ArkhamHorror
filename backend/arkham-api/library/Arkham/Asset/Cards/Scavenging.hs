module Arkham.Asset.Cards.Scavenging (Scavenging (..), scavenging) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Matcher
import Arkham.Prelude

newtype Scavenging = Scavenging AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

scavenging :: AssetCard Scavenging
scavenging = asset Scavenging Cards.scavenging

instance HasAbilities Scavenging where
  getAbilities (Scavenging a) =
    [ controlledAbility a 1 (youExist $ DiscardWith #item <> noModifier CardsCannotLeaveYourDiscardPile)
        $ ReactionAbility (SkillTestResult #after You #investigating (SuccessResult $ atLeast 2)) (exhaust a)
    ]

instance RunMessage Scavenging where
  runMessage msg a@(Scavenging attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      push $ search iid (attrs.ability 1) iid [fromDiscard] #item (DrawFound iid 1)
      pure a
    _ -> Scavenging <$> runMessage msg attrs
