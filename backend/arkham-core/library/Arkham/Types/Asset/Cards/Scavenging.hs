module Arkham.Types.Asset.Cards.Scavenging
  ( Scavenging(..)
  , scavenging
  ) where

import Arkham.Prelude

import qualified Arkham.Asset.Cards as Cards
import Arkham.Types.Ability
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Runner
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.GameValue
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Restriction
import Arkham.Types.Target
import qualified Arkham.Types.Timing as Timing
import Arkham.Types.Trait

newtype Scavenging = Scavenging AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

scavenging :: AssetCard Scavenging
scavenging = asset Scavenging Cards.scavenging

instance HasActions Scavenging where
  getActions (Scavenging a) =
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
              WhileInvestigating
              (SuccessResult $ AtLeast $ Static 2)
            )
            ExhaustThis
    ]

instance AssetRunner env => RunMessage env Scavenging where
  runMessage msg a@(Scavenging attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      a <$ push (SearchDiscard iid (InvestigatorTarget iid) [Item])
    _ -> Scavenging <$> runMessage msg attrs
