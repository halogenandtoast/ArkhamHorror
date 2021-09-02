module Arkham.Types.Location.Cards.ArtGallery
  ( artGallery
  , ArtGallery(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards (artGallery)
import Arkham.Types.Ability
import qualified Arkham.Types.Action as Action
import Arkham.Types.Classes
import Arkham.Types.Criteria
import Arkham.Types.Game.Helpers
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.Target
import qualified Arkham.Types.Timing as Timing

newtype ArtGallery = ArtGallery LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor env)
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

instance LocationRunner env => RunMessage env ArtGallery where
  runMessage msg l@(ArtGallery attrs) = case msg of
    After (FailedSkillTest iid (Just Action.Investigate) _ (SkillTestInitiatorTarget _) _ _)
      -> l <$ push (SpendResources iid 2)
    _ -> ArtGallery <$> runMessage msg attrs
