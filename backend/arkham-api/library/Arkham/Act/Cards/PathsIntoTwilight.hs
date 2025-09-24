module Arkham.Act.Cards.PathsIntoTwilight (pathsIntoTwilight) where

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Card
import Arkham.Helpers.Modifiers
import Arkham.Helpers.Scenario
import Arkham.Location.Cards qualified as Locations
import Arkham.Location.Types (Field (..))
import Arkham.Matcher
import Arkham.Projection

newtype PathsIntoTwilight = PathsIntoTwilight ActAttrs
  deriving anyclass IsAct
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

pathsIntoTwilight :: ActCard PathsIntoTwilight
pathsIntoTwilight = act (3, A) PathsIntoTwilight Cards.pathsIntoTwilight (groupClueCost (PerPlayer 3))

instance HasModifiersFor PathsIntoTwilight where
  getModifiersFor (PathsIntoTwilight a) = do
    modifySelectMaybe a (LocationIsInFrontOf Anyone) \lid -> do
      iid <- MaybeT $ field LocationInFrontOf lid
      pure
        [ ConnectedToWhen (LocationWithId lid)
            $ not_ (LocationWithId lid)
            <> LocationIsInFrontOf (InvestigatorWithId iid)
        ]

instance RunMessage PathsIntoTwilight where
  runMessage msg a@(PathsIntoTwilight attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      witchesCircle <- placeLocationCard Locations.witchesCircle
      lead <- getLead
      push $ Revelation lead (toSource witchesCircle)
      returnTo <- getIsReturnTo
      if returnTo
        then do
          setActDeckN 1 [Cards.pathsIntoTwilight, Cards.aCircleUnbroken]
          returnToAct <- genCard Cards.returnToACircleUnbroken
          push $ AddAct 2 returnToAct
          setDecksLayout
            [ ".       act1"
            , "agenda1 act1"
            , "agenda1 act2"
            , ".       act2"
            ]
          advanceActDeck attrs
        else do
          advanceActDeck attrs
      pure a
    _ -> PathsIntoTwilight <$> liftRunMessage msg attrs
