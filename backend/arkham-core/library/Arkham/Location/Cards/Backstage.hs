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

newtype Backstage = Backstage LocationAttrs
  deriving anyclass (IsLocation)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

backstage :: LocationCard Backstage
backstage = location Backstage Cards.backstage 3 (Static 1)

instance HasModifiersFor Backstage where
  getModifiersFor (CardTarget card) (Backstage attrs) = do
    here <- maybe (pure False) (`isAt` attrs) (toCardOwner card)
    pure $ toModifiers attrs [HandSizeCardCount 3 | here, Hidden `elem` cdKeywords (toCardDef card)]
  getModifiersFor _ _ = pure []

instance HasAbilities Backstage where
  getAbilities (Backstage attrs) =
    withRevealedAbilities
      attrs
      [ mkAbility attrs 1
          $ ForcedAbility
          $ RevealLocation #when Anyone
          $ LocationWithId
          $ toId attrs
      , restrictedAbility attrs 1 Here $ ActionAbility [] $ ActionCost 2
      ]

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
