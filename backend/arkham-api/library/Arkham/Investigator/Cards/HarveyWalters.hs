module Arkham.Investigator.Cards.HarveyWalters (harveyWalters) where

import Arkham.Ability
import Arkham.Investigator.Cards qualified as Cards
import Arkham.Investigator.Import.Lifted
import Arkham.Matcher
import Arkham.Window (Window (..))
import Arkham.Window qualified as Window

newtype HarveyWalters = HarveyWalters InvestigatorAttrs
  deriving anyclass (IsInvestigator, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)
  deriving stock Data

harveyWalters :: InvestigatorCard HarveyWalters
harveyWalters =
  investigator HarveyWalters Cards.harveyWalters
    $ Stats {health = 7, sanity = 8, willpower = 4, intellect = 5, combat = 1, agility = 2}

instance HasAbilities HarveyWalters where
  getAbilities (HarveyWalters a) =
    [ playerLimit PerRound
        $ restricted a 1 (Self <> DuringPhase #investigation)
        $ freeReaction (DrawsCards #after (affectsOthersKnown a.id $ at_ YourLocation) AnyCards (atLeast 1))
    ]

instance HasChaosTokenValue HarveyWalters where
  getChaosTokenValue iid ElderSign (HarveyWalters attrs) | iid == toId attrs = do
    pure $ ChaosTokenValue ElderSign $ PositiveModifier 1
  getChaosTokenValue _ token _ = pure $ ChaosTokenValue token mempty

instance RunMessage HarveyWalters where
  runMessage msg i@(HarveyWalters attrs) = runQueueT $ case msg of
    UseCardAbility _ (isSource attrs -> True) 1 (map windowType -> [Window.DrawCards iid' _]) _ -> do
      drawCards iid' (attrs.ability 1) 1
      pure i
    ResolveChaosToken _drawnToken ElderSign iid | iid == toId attrs -> do
      drawCards iid ElderSign 1
      pure i
    _ -> HarveyWalters <$> liftRunMessage msg attrs
