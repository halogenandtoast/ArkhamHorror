module Arkham.Investigator.Cards.MontereyJack (montereyJack, MontereyJack (..)) where

import Arkham.Ability
import Arkham.Capability
import Arkham.Distance
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Helpers.Investigator (getMaybeLocation)
import Arkham.Helpers.Message qualified as Msg
import Arkham.Investigator.Cards qualified as Cards
import Arkham.Investigator.Import.Lifted
import Arkham.Matcher
import Arkham.Message (uiToRun)

newtype MontereyJack = MontereyJack InvestigatorAttrs
  deriving anyclass (IsInvestigator, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)
  deriving stock (Data)

montereyJack :: InvestigatorCard MontereyJack
montereyJack =
  investigator
    MontereyJack
    Cards.montereyJack
    Stats {health = 8, sanity = 6, willpower = 1, intellect = 4, combat = 2, agility = 5}

instance HasAbilities MontereyJack where
  getAbilities (MontereyJack attrs) =
    [ restrictedAbility
        attrs
        1
        (Self <> youExist (oneOf [can.draw.cards, can.gain.resources, DistanceFromRoundStart (atLeast 1)]))
        $ freeReaction (RoundEnds #when)
    ]

instance HasChaosTokenValue MontereyJack where
  getChaosTokenValue iid ElderSign (MontereyJack attrs) | iid == toId attrs = do
    pure $ ChaosTokenValue ElderSign (PositiveModifier 1)
  getChaosTokenValue _ token _ = pure $ ChaosTokenValue token mempty

instance RunMessage MontereyJack where
  runMessage msg i@(MontereyJack attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      void $ runMaybeT do
        startLocation <- hoistMaybe $ investigatorBeganRoundAt attrs
        current <- MaybeT $ getMaybeLocation iid
        distance <- MaybeT $ getDistance startLocation current
        mGainResources <- lift $ Msg.gainResourcesIfCan iid ElderSign 1
        mDrawCards <- lift $ Msg.drawCardsIfCan iid ElderSign 1
        lift
          $ (if distance >= 2 then (pushAll . map uiToRun) else chooseOrRunOne iid)
          $ [Label "Gain 1 resource" [x] | x <- toList mGainResources]
          <> [Label "Draw 1 card" [x] | x <- toList mDrawCards]
      pure i
    ElderSignEffect iid | attrs `is` iid -> do
      void $ runMaybeT do
        startLocation <- hoistMaybe $ investigatorBeganRoundAt attrs
        current <- MaybeT $ getMaybeLocation iid
        guardM $ MaybeT $ (> Distance 0) <$$> getDistance startLocation current
        mGainResources <- lift $ Msg.gainResourcesIfCan iid ElderSign 1
        mDrawCards <- lift $ Msg.drawCardsIfCan iid ElderSign 1
        lift
          $ chooseOrRunOne iid
          $ [Label "Gain 1 resource" [x] | x <- toList mGainResources]
          <> [Label "Draw 1 card" [x] | x <- toList mDrawCards]
      pure i
    _ -> MontereyJack <$> liftRunMessage msg attrs
