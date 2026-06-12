module Arkham.Act.Cards.BreakFreeFromThePast (breakFreeFromThePast) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Card
import Arkham.Deck qualified as Deck
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Scenario.Deck
import Arkham.Scenarios.EnthrallingEncore.Helpers
import Arkham.Trait (Trait (Private))

newtype BreakFreeFromThePast = BreakFreeFromThePast ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

breakFreeFromThePast :: ActCard BreakFreeFromThePast
breakFreeFromThePast = act (1, A) BreakFreeFromThePast Cards.breakFreeFromThePast Nothing

instance HasAbilities BreakFreeFromThePast where
  getAbilities = actAbilities \a ->
    [ restricted
        a
        1
        (exists (YourLocation <> LocationWithTrait Private) <> ScenarioDeckWithCard PropsDeck)
        $ FastAbility' (SameLocationGroupClueCost (PerPlayer 1) (LocationWithTrait Private)) #parley
    , restricted a 2 (exists $ VictoryDisplayCardMatch $ basic $ cardIs Enemies.sinisterSoloist)
        $ Objective
        $ forced AnyWindow
    ]

instance RunMessage BreakFreeFromThePast where
  runMessage msg a@(BreakFreeFromThePast attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      props <- take 3 <$> getPropsDeck
      when (notNull props) do
        investigators <- select UneliminatedInvestigator
        chooseOrRunOneM iid $ scenarioI18n do
          questionLabeled' "chooseInvestigator"
          targets investigators \iid' -> handleTarget iid attrs (toTarget iid')
      pure a
    HandleTargetChoice _ (isSource attrs -> True) (InvestigatorTarget iid') -> do
      props <- take 3 <$> getPropsDeck
      focusCards props do
        chooseOrRunOneM iid' $ scenarioI18n do
          questionLabeled' "chooseProp"
          targets props \card -> do
            unfocusCards
            push $ RemoveCardFromScenarioDeck PropsDeck card
            let card' = case card of
                  PlayerCard pc -> PlayerCard $ pc {pcOwner = pcOwner pc <|> Just iid'}
                  other -> other
            addToHand iid' (only card')
            rest <- shuffle (deleteFirst card props)
            for_ rest \c -> push $ PutCardOnBottomOfDeck iid' (Deck.ScenarioDeckByKey PropsDeck) c
      pure a
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      advancedWithOther attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      push R1
      pure a
    _ -> BreakFreeFromThePast <$> liftRunMessage msg attrs
