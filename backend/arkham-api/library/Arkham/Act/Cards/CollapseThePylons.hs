module Arkham.Act.Cards.CollapseThePylons (collapseThePylons) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted hiding (DiscoverClues)
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Query (getSetAsideCardsMatching)
import Arkham.Helpers.Window (discoveredCluesAt)
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Placement
import Arkham.Modifier
import Arkham.Placement

newtype CollapseThePylons = CollapseThePylons ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

collapseThePylons :: ActCard CollapseThePylons
collapseThePylons = act (3, A) CollapseThePylons Cards.collapseThePylons Nothing

instance HasAbilities CollapseThePylons where
  getAbilities (CollapseThePylons x) =
    extend
      x
      [ mkAbility x 1 $ triggered (DiscoverClues #when You "Mist Pylon" (atLeast 1)) DiscoveredCluesCost
      , restricted x 2 (LocationCount 5 (LocationWithModifier $ ScenarioModifier "collapsed"))
          $ Objective
          $ forced
          $ RoundEnds #when
      ]

instance RunMessage CollapseThePylons where
  runMessage msg a@(CollapseThePylons attrs) = runQueueT $ case msg of
    UseCardAbility _iid (isSource attrs -> True) 1 (discoveredCluesAt -> (lid, n)) _ -> do
      placeTokens (attrs.ability 1) lid #damage n
      pure a
    UseThisAbility _iid (isSource attrs -> True) 2 -> do
      advancedWithOther attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      nameless <- select $ enemyIs Enemies.theNamelessMadness
      lead <- getLead
      let x = max 0 (length nameless - 3)
      when (x > 3) do
        chooseNM lead x do
          questionLabeled $ "Set " <> tshow x <> " the nameless madness aside"
          targets nameless (`place` SetAsideZone)
      doStep 1 msg
      eachInvestigator (discardAllClues attrs)
      eachInvestigator (`place` Unplaced)
      selectEach (not_ $ locationIs Locations.theGateOfYquaa) removeLocation
      push $ SetLayout ["theGateOfYquaa titanicRamp1 titanicRamp2 titanicRamp3 titanicRamp4 hiddenTunnel"]
      placeRandomLocationGroup "titanicRamp" =<< getSetAsideCardsMatching "Titanic Ramp"
      placeSetAsideLocation_ Locations.hiddenTunnelAWayOut
      doStep 2 msg
      theFinalMirage <- getSetAsideCard Cards.theFinalMirage
      push $ SetCurrentActDeck 1 [theFinalMirage]
      push $ SetCurrentAgendaDeck 1 []
      toDiscard GameSource attrs
      pure a
    _ -> CollapseThePylons <$> liftRunMessage msg attrs
