module Arkham.Types.Asset.Cards.AnalyticalMind
  ( analyticalMind
  , AnalyticalMind(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Asset.Cards as Cards
import Arkham.Types.Ability
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Helpers
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Restriction
import Arkham.Types.Target
import qualified Arkham.Types.Timing as Timing

newtype AnalyticalMind = AnalyticalMind AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

analyticalMind :: AssetCard AnalyticalMind
analyticalMind = asset AnalyticalMind Cards.analyticalMind

instance HasActions AnalyticalMind where
  getActions (AnalyticalMind x) =
    [ restrictedAbility x 1 OwnsThis
        $ ReactionAbility
            (CommittedCards Timing.After You ExactlyOne)
            ExhaustThis
    ]

instance HasModifiersFor env AnalyticalMind where
  getModifiersFor _ (InvestigatorTarget iid) (AnalyticalMind attrs)
    | ownedBy attrs iid = pure $ toModifiers
      attrs
      [CanCommitToSkillTestPerformedByAnInvestigatorAtAnotherLocation 1]
  getModifiersFor _ _ _ = pure []

instance (HasQueue env, HasModifiersFor env ()) => RunMessage env AnalyticalMind where
  runMessage msg a@(AnalyticalMind attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      a <$ push (DrawCards iid 1 False)
    _ -> AnalyticalMind <$> runMessage msg attrs
