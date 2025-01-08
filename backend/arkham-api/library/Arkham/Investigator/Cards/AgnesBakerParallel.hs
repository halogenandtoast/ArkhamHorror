module Arkham.Investigator.Cards.AgnesBakerParallel (agnesBakerParallel, AgnesBakerParallel (..)) where

import Arkham.ActiveCost.Base
import Arkham.Card
import Arkham.Cost
import {-# SOURCE #-} Arkham.GameEnv (findAllCards, getActiveCosts)
import Arkham.Helpers.Investigator (canHaveDamageHealed)
import Arkham.Helpers.Modifiers (ModifierType (..), modifyEachMaybe, modifySelf)
import Arkham.Investigator.Cards qualified as Cards
import Arkham.Investigator.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Projection
import Arkham.Strategy

newtype AgnesBakerParallel = AgnesBakerParallel InvestigatorAttrs
  deriving anyclass IsInvestigator
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)
  deriving stock Data

instance HasModifiersFor AgnesBakerParallel where
  getModifiersFor (AgnesBakerParallel a) = do
    self <- modifySelf a [ReduceCostOf (#spell <> #event) 2]
    validCards <- findAllCards (`cardMatch` (CardOwnedBy a.id <> card_ (#spell <> #event)))
    cards <- modifyEachMaybe a validCards \card -> do
      startingCost <- case card.cost of
        Just (StaticCost n) -> pure n
        Just DynamicCost -> pure 0
        Just (MaxDynamicCost _) -> pure 0
        Just DiscardAmountCost -> lift $ fieldMap InvestigatorDiscard (count ((== card.cardCode) . toCardCode)) a.id
        Nothing -> pure 0
      guard $ startingCost > 0
      pure
        [ AdditionalCost
            $ OrCost
              [ InvestigatorDamageCost (toSource a) (InvestigatorWithId a.id) DamageAny 1
              , ResourceCost (min startingCost 2)
              ]
        ]
    pure $ self <> cards

agnesBakerParallel :: InvestigatorCard AgnesBakerParallel
agnesBakerParallel =
  investigator AgnesBakerParallel Cards.agnesBakerParallel
    $ Stats {health = 8, sanity = 6, willpower = 5, intellect = 2, combat = 2, agility = 3}

instance HasAbilities AgnesBakerParallel where
  getAbilities (AgnesBakerParallel _) = []

instance HasChaosTokenValue AgnesBakerParallel where
  getChaosTokenValue iid ElderSign (AgnesBakerParallel attrs) | iid == toId attrs = do
    pure $ ChaosTokenValue ElderSign (PositiveModifier 1)
  getChaosTokenValue _ token _ = pure $ ChaosTokenValue token mempty

instance RunMessage AgnesBakerParallel where
  runMessage msg i@(AgnesBakerParallel attrs) = runQueueT $ case msg of
    ElderSignEffect (is attrs -> True) -> do
      whenM (canHaveDamageHealed attrs attrs.id) do
        chooseOne
          attrs.id
          [ Label "Heal 1 damage" [HealDamage (toTarget attrs) (ChaosTokenEffectSource #eldersign) 1]
          , Label "Do not heal" []
          ]
      pure i
    PayCost _ iid _ (InvestigatorDamageCost (isSource attrs -> True) _ _ _) -> do
      let go [] = error "No ForCard cost found"
          go (c : rest) = case c.target of
            ForCard _ card -> do
              chooseOneM iid do
                labeled "Shuffle event back in instead of discard?" do
                  cardResolutionModifier card attrs card (SetAfterPlay ShuffleThisBackIntoDeck)
                labeled "Resolve normally" nothing
              pure ()
            _ -> go rest
      go =<< getActiveCosts
      pure i
    _ -> AgnesBakerParallel <$> liftRunMessage msg attrs
