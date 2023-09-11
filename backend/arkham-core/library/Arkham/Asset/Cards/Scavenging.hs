module Arkham.Asset.Cards.Scavenging (
  Scavenging (..),
  scavenging,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Matcher
import Arkham.Timing qualified as Timing
import Arkham.Trait
import Arkham.Zone qualified as Zone

newtype Scavenging = Scavenging AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

scavenging :: AssetCard Scavenging
scavenging = asset Scavenging Cards.scavenging

instance HasAbilities Scavenging where
  getAbilities (Scavenging a) =
    [ restrictedAbility
        a
        1
        ( ControlsThis
            <> InvestigatorExists
              ( You
                  <> DiscardWith (HasCard $ CardWithTrait Item)
                  <> InvestigatorWithoutModifier CardsCannotLeaveYourDiscardPile
              )
        )
        $ ReactionAbility
          ( SkillTestResult
              Timing.After
              You
              (WhileInvestigating Anywhere)
              (SuccessResult $ AtLeast $ Static 2)
          )
          (exhaust a)
    ]

instance RunMessage Scavenging where
  runMessage msg a@(Scavenging attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      push
        $ Search
          iid
          (toAbilitySource attrs 1)
          (toTarget iid)
          [(Zone.FromDiscard, PutBack)]
          (CardWithTrait Item)
        $ DrawFound iid 1
      pure a
    _ -> Scavenging <$> runMessage msg attrs
