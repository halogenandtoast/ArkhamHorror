module Arkham.Location.Cards.LodgeCatacombs (
  lodgeCatacombs,
  LodgeCatacombs (..),
)
where

import Arkham.Prelude

import Arkham.Game.Helpers
import Arkham.GameValue
import Arkham.Key
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Timing qualified as Timing

newtype LodgeCatacombs = LodgeCatacombs LocationAttrs
  deriving anyclass (IsLocation)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lodgeCatacombs :: LocationCard LodgeCatacombs
lodgeCatacombs = location LodgeCatacombs Cards.lodgeCatacombs 4 (Static 0)

instance HasModifiersFor LodgeCatacombs where
  getModifiersFor (InvestigatorTarget iid) (LodgeCatacombs attrs) = do
    hasAKey <-
      iid
        <=~> AnyInvestigator
          [ InvestigatorWithKey ElderThingKey
          , InvestigatorWithKey SkullKey
          , InvestigatorWithKey CultistKey
          , InvestigatorWithKey TabletKey
          ]
    pure $ toModifiers attrs [CannotEnter (toId attrs) | unrevealed attrs && not hasAKey]
  getModifiersFor _ _ = pure []

instance HasAbilities LodgeCatacombs where
  getAbilities (LodgeCatacombs attrs) =
    withRevealedAbilities
      attrs
      [mkAbility attrs 1 $ ForcedAbility $ RevealLocation Timing.After You $ LocationWithId $ toId attrs]

instance RunMessage LodgeCatacombs where
  runMessage msg l@(LodgeCatacombs attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ _ -> do
      sanctumDoorways <- shuffleM =<< getSetAsideCardsMatching (CardWithTitle "Sanctum Doorway")
      msgs <- for (withIndex1 sanctumDoorways) \(idx, sanctumDoorway) -> do
        (locationId, placement) <- placeLocation sanctumDoorway
        pure [placement, SetLocationLabel locationId $ "sanctumDoorway" <> tshow idx]
      pushAll $ PlaceLocationMatching (CardWithTitle "Inner Sanctum") : concat msgs
      pure l
    _ -> LodgeCatacombs <$> runMessage msg attrs
