module Arkham.Act.Cards.PursuitOfTheUnknownV2 (pursuitOfTheUnknownV2) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Scenario.Deck
import Arkham.Strategy

newtype PursuitOfTheUnknownV2 = PursuitOfTheUnknownV2 ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

pursuitOfTheUnknownV2 :: ActCard PursuitOfTheUnknownV2
pursuitOfTheUnknownV2 = act (2, A) PursuitOfTheUnknownV2 Cards.pursuitOfTheUnknownV2 Nothing

instance HasAbilities PursuitOfTheUnknownV2 where
  getAbilities (PursuitOfTheUnknownV2 x) =
    [ restricted x 1 (exists $ DeckWith $ HasCard $ CardWithTitle "Tekeli-li")
        $ actionAbilityWithCost (SpendTokenKeyCost 2 #tablet)
    , restricted
        x
        2
        (EachUndefeatedInvestigator $ at_ $ "Hidden Tunnel" <> LocationWithoutClues)
        $ Objective
        $ forced AnyWindow
    ]

instance RunMessage PursuitOfTheUnknownV2 where
  runMessage msg a@(PursuitOfTheUnknownV2 attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      push R1
      pure a
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      eachInvestigator \iid -> do
        search
          iid
          (attrs.ability 1)
          attrs
          [fromDeck]
          (basic $ CardWithTitle "Tekeli-li")
          (defer attrs IsNotDraw)

      pure a
    SearchFound iid (isTarget attrs -> True) _ [] -> do
      chooseOneM iid $ labeled "No Cards Found" nothing
      pure a
    SearchFound iid (isTarget attrs -> True) _ cards -> do
      chooseTargetM iid cards $ shuffleCardsIntoDeck TekeliliDeck . only
      pure a
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      advancedWithOther attrs
      pure a
    _ -> PursuitOfTheUnknownV2 <$> liftRunMessage msg attrs
