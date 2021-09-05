module Arkham.Types.Location.Cards.Backstage
  ( backstage
  , Backstage(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards
import Arkham.Types.Ability
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Criteria
import Arkham.Types.GameValue
import Arkham.Types.Keyword
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Helpers
import Arkham.Types.Matcher
import Arkham.Types.Message hiding (RevealLocation)
import Arkham.Types.Modifier
import Arkham.Types.Source
import Arkham.Types.Target
import qualified Arkham.Types.Timing as Timing

newtype Backstage = Backstage LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

backstage :: LocationCard Backstage
backstage =
  location Backstage Cards.backstage 3 (Static 1) Diamond [Circle, Circle]

instance HasModifiersFor env Backstage where
  getModifiersFor (InvestigatorSource iid) (CardTarget card) (Backstage attrs)
    | iid `on` attrs = pure $ toModifiers
      attrs
      [ HandSizeCardCount 3 | Hidden `elem` cdKeywords (toCardDef card) ]
  getModifiersFor _ _ _ = pure []

instance HasAbilities Backstage where
  getAbilities (Backstage attrs) =
    withBaseAbilities attrs $ if locationRevealed attrs
      then
        [ mkAbility attrs 1
        $ ForcedAbility
        $ RevealLocation Timing.When Anyone
        $ LocationWithId
        $ toId attrs
        , restrictedAbility attrs 1 Here $ ActionAbility Nothing $ ActionCost 2
        ]
      else []

instance LocationRunner env => RunMessage env Backstage where
  runMessage msg l@(Backstage attrs) = case msg of
    UseCardAbility _ source _ 1 _ | isSource attrs source -> do
      backstageDoorwayCount <- selectCount
        (LocationWithUnrevealedTitle "Backstage Doorway")
      backstageDoorways <-
        zip [backstageDoorwayCount ..]
        . take 2
        <$> (shuffleM =<< selectList
              (SetAsideCardMatch $ CardWithTitle "Backstage Doorway")
            )
      msgs <- concat <$> for
        backstageDoorways
        \(idx, backstageDoorway) -> do
          locationId <- getRandom
          pure
            [ PlaceLocation locationId (toCardDef backstageDoorway)
            , SetLocationLabel locationId $ "backstageDoorway" <> tshow
              (idx + 1)
            ]
      l <$ pushAll msgs
    _ -> Backstage <$> runMessage msg attrs
