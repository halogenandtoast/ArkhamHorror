module Arkham.Location.Cards.ArtGallery
  ( artGallery
  , ArtGallery(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Location.Cards qualified as Cards (artGallery)
import Arkham.Action qualified as Action
import Arkham.Classes
import Arkham.Criteria
import Arkham.Game.Helpers
import Arkham.GameValue
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Message
import Arkham.Target
import Arkham.Timing qualified as Timing

newtype ArtGallery = ArtGallery LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

artGallery :: LocationCard ArtGallery
artGallery = locationWith
  ArtGallery
  Cards.artGallery
  2
  (PerPlayer 1)
  T
  [Diamond]
  (revealedSymbolL .~ Hourglass)

instance HasAbilities ArtGallery where
  getAbilities (ArtGallery x) = withBaseAbilities
    x
    [ restrictedAbility x 1 Here $ ForcedAbility $ SkillTestResult
        Timing.After
        You
        (WhileInvestigating $ LocationWithId $ toId x)
        (FailureResult AnyValue)
    | locationRevealed x
    ]

instance LocationRunner env => RunMessage ArtGallery where
  runMessage msg l@(ArtGallery attrs) = case msg of
    After (FailedSkillTest iid (Just Action.Investigate) _ (SkillTestInitiatorTarget _) _ _)
      -> l <$ push (SpendResources iid 2)
    _ -> ArtGallery <$> runMessage msg attrs
