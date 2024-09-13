module Arkham.Act.Cards.TheEscape (TheEscape (..), theEscape) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted hiding (RevealLocation)
import Arkham.Helpers.Ability
import Arkham.Helpers.Query
import Arkham.Helpers.Scenario
import Arkham.Helpers.Window
import Arkham.Location.Grid
import Arkham.Matcher
import Arkham.Scenario.Types (Field (..))

newtype TheEscape = TheEscape ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theEscape :: ActCard TheEscape
theEscape = act (2, A) TheEscape Cards.theEscape Nothing

instance HasAbilities TheEscape where
  getAbilities (TheEscape x) =
    extend x
      $ guard (onSide A x)
      *> [ mkAbility x 1 $ forced $ RevealLocation #after Anyone Anywhere
         , restrictedAbility x 2 AllUndefeatedInvestigatorsResigned
            $ Objective
            $ forced AnyWindow
         ]

instance RunMessage TheEscape where
  runMessage msg a@(TheEscape attrs) = runQueueT $ case msg of
    UseCardAbility _ (isSource attrs -> True) 1 (getRevealedLocation -> lid) _ -> do
      tidalTunnelDeck <- shuffleM =<< getSetAsideCardsMatching "Tidal Tunnel"
      grid <- scenarioField ScenarioGrid
      let
        locationPositions = case findInGrid lid grid of
          Nothing -> []
          Just (Pos x y) -> filter (\pos -> isNothing $ viewGrid pos grid) [Pos x (y - 1), Pos (x - 1) y, Pos (x + 1) y]

      for_ (zip locationPositions tidalTunnelDeck) (uncurry placeLocationInGrid)
      pure a
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      advancedWithOther attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      push R1
      pure a
    _ -> TheEscape <$> liftRunMessage msg attrs
