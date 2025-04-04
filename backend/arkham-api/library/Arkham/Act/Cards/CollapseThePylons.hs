module Arkham.Act.Cards.CollapseThePylons (collapseThePylons) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted hiding (DiscoverClues)
import Arkham.Helpers.Window (discoveredCluesAt)
import Arkham.Matcher
import Arkham.Modifier

newtype CollapseThePylons = CollapseThePylons ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

collapseThePylons :: ActCard CollapseThePylons
collapseThePylons = act (1, A) CollapseThePylons Cards.collapseThePylons Nothing

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
      advanceActDeck attrs
      pure a
    _ -> CollapseThePylons <$> liftRunMessage msg attrs
