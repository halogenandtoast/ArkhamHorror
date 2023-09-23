module Arkham.Location.Cards.Backstage (
  backstage,
  Backstage (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Card
import Arkham.Classes
import Arkham.GameValue
import Arkham.Keyword
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Helpers
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Timing qualified as Timing

newtype Backstage = Backstage LocationAttrs
  deriving anyclass (IsLocation)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

backstage :: LocationCard Backstage
backstage = location Backstage Cards.backstage 3 (Static 1)

instance HasModifiersFor Backstage where
  getModifiersFor (CardTarget card) (Backstage attrs)
    | maybe False (`on` attrs) (toCardOwner card) =
        pure
          $ toModifiers
            attrs
            [HandSizeCardCount 3 | Hidden `elem` cdKeywords (toCardDef card)]
  getModifiersFor _ _ = pure []

instance HasAbilities Backstage where
  getAbilities (Backstage attrs) =
    withBaseAbilities attrs
      $ if locationRevealed attrs
        then
          [ mkAbility attrs 1
              $ ForcedAbility
              $ RevealLocation Timing.When Anyone
              $ LocationWithId
              $ toId attrs
          , restrictedAbility attrs 1 Here $ ActionAbility Nothing $ ActionCost 2
          ]
        else []

instance RunMessage Backstage where
  runMessage msg l@(Backstage attrs) = case msg of
    UseCardAbility _ source 1 _ _ | isSource attrs source -> do
      backstageDoorwayCount <-
        selectCount
          (LocationWithUnrevealedTitle "Backstage Doorway")
      backstageDoorways <-
        zip [backstageDoorwayCount ..]
          . take 2
            <$> ( shuffleM
                    =<< selectList
                      (SetAsideCardMatch $ CardWithTitle "Backstage Doorway")
                )
      msgs <-
        concat <$> for
          backstageDoorways
          \(idx, backstageDoorway) -> do
            (locationId, placement) <- placeLocation backstageDoorway
            pure
              [ placement
              , SetLocationLabel locationId $ "backstageDoorway" <> tshow (idx + 1)
              ]
      l <$ pushAll msgs
    _ -> Backstage <$> runMessage msg attrs
