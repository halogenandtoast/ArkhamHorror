module Arkham.Asset.Cards.Scavenging (
  Scavenging (..),
  scavenging,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Matcher
import Arkham.Trait

newtype Scavenging = Scavenging AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

scavenging :: AssetCard Scavenging
scavenging = asset Scavenging Cards.scavenging

instance HasAbilities Scavenging where
  getAbilities (Scavenging a) =
    [ controlledAbility
        a
        1
        ( exists
            $ You
            <> DiscardWith (HasCard $ CardWithTrait Item)
            <> InvestigatorWithoutModifier CardsCannotLeaveYourDiscardPile
        )
        $ ReactionAbility
          (SkillTestResult #after You (WhileInvestigating Anywhere) (SuccessResult $ atLeast 2))
          (exhaust a)
    ]

instance RunMessage Scavenging where
  runMessage msg a@(Scavenging attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let source = toAbilitySource attrs 1
      push $ search iid source iid [fromDiscard] (CardWithTrait Item) $ DrawFound iid 1
      pure a
    _ -> Scavenging <$> runMessage msg attrs
