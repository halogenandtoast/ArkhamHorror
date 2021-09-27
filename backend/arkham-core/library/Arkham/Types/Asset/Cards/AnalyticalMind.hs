module Arkham.Types.Asset.Cards.AnalyticalMind
  ( analyticalMind
  , AnalyticalMind(..)
  ) where

import Arkham.Prelude

import Arkham.Asset.Cards qualified as Cards
import Arkham.Types.Ability
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Helpers
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Criteria
import Arkham.Types.GameValue
import Arkham.Types.Id
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Target
import Arkham.Types.Timing qualified as Timing

newtype AnalyticalMind = AnalyticalMind AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

analyticalMind :: AssetCard AnalyticalMind
analyticalMind = asset AnalyticalMind Cards.analyticalMind

instance HasAbilities AnalyticalMind where
  getAbilities (AnalyticalMind attrs) =
    [ restrictedAbility attrs 1 OwnsThis
        $ ReactionAbility
            (CommittedCards Timing.After You $ LengthIs $ EqualTo $ Static 1)
        $ ExhaustCost (toTarget attrs)
    ]

instance HasModifiersFor env AnalyticalMind where
  getModifiersFor _ (InvestigatorTarget iid) (AnalyticalMind attrs)
    | ownedBy attrs iid = pure $ toModifiers
      attrs
      [CanCommitToSkillTestPerformedByAnInvestigatorAtAnotherLocation 1]
  getModifiersFor _ _ _ = pure []

instance
  ( HasSet InvestigatorId env ()
  , HasQueue env
  , HasModifiersFor env ()
  )
  => RunMessage env AnalyticalMind where
  runMessage msg a@(AnalyticalMind attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      a <$ push (DrawCards iid 1 False)
    _ -> AnalyticalMind <$> runMessage msg attrs
