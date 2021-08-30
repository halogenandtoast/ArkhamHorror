module Arkham.Types.Location.Cards.ExhibitHallMedusaExhibit
  ( exhibitHallMedusaExhibit
  , ExhibitHallMedusaExhibit(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards (exhibitHallMedusaExhibit)
import Arkham.Types.Ability
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Helpers
import Arkham.Types.Matcher
import Arkham.Types.Message
import qualified Arkham.Types.Timing as Timing

newtype ExhibitHallMedusaExhibit = ExhibitHallMedusaExhibit LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

exhibitHallMedusaExhibit :: LocationCard ExhibitHallMedusaExhibit
exhibitHallMedusaExhibit = locationWithRevealedSideConnections
  ExhibitHallMedusaExhibit
  Cards.exhibitHallMedusaExhibit
  2
  (PerPlayer 1)
  NoSymbol
  [Square]
  T
  [Square, Moon]

instance HasAbilities env ExhibitHallMedusaExhibit where
  getAbilities i w (ExhibitHallMedusaExhibit x) = withBaseAbilities i w x $ do
    pure
      [ mkAbility x 1
        $ ForcedAbility
        $ SkillTestResult
            Timing.After
            You
            (WhileInvestigating $ LocationWithId $ toId x)
        $ FailureResult AnyValue
      | locationRevealed x
      ]

instance LocationRunner env => RunMessage env ExhibitHallMedusaExhibit where
  runMessage msg l@(ExhibitHallMedusaExhibit attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      l <$ push (ChooseAndDiscardAsset iid AnyAsset)
    _ -> ExhibitHallMedusaExhibit <$> runMessage msg attrs
