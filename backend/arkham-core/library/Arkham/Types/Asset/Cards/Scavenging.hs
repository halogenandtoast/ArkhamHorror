module Arkham.Types.Asset.Cards.Scavenging
  ( Scavenging(..)
  , scavenging
  ) where

import Arkham.Prelude

import Arkham.Asset.Cards qualified as Cards
import Arkham.Types.Ability
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Runner
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Criteria
import Arkham.Types.GameValue
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Target
import Arkham.Types.Timing qualified as Timing
import Arkham.Types.Trait
import Arkham.Types.Zone qualified as Zone

newtype Scavenging = Scavenging AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

scavenging :: AssetCard Scavenging
scavenging = asset Scavenging Cards.scavenging

instance HasAbilities Scavenging where
  getAbilities (Scavenging a) =
    [ restrictedAbility
          a
          1
          (OwnsThis <> InvestigatorExists
            (You
            <> DiscardWith (HasCard $ CardWithTrait Item)
            <> InvestigatorWithoutModifier CardsCannotLeaveYourDiscardPile
            )
          )
        $ ReactionAbility
            (SkillTestResult
              Timing.After
              You
              (WhileInvestigating Anywhere)
              (SuccessResult $ AtLeast $ Static 2)
            )
            (ExhaustCost $ toTarget a)
    ]

instance AssetRunner env => RunMessage env Scavenging where
  runMessage msg a@(Scavenging attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> a <$ push
      (Search
          iid
          source
          (InvestigatorTarget iid)
          [(Zone.FromDiscard, PutBack)]
          [Item]
      $ DrawFound iid 1
      )
    _ -> Scavenging <$> runMessage msg attrs
