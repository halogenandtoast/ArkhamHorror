module Arkham.Investigator.Cards.AgnesBakerParallel (agnesBakerParallel, AgnesBakerParallel (..)) where

import Arkham.ActiveCost.Base
import Arkham.Card
import Arkham.Cost
import {-# SOURCE #-} Arkham.GameEnv (getActiveCosts, getCard)
import Arkham.Helpers.Investigator (canHaveDamageHealed)
import Arkham.Helpers.Modifiers (ModifierType (..), maybeModified, modified)
import Arkham.Investigator.Cards qualified as Cards
import Arkham.Investigator.Import.Lifted
import Arkham.Investigator.Types (Field (InvestigatorDiscard))
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Projection
import Arkham.Strategy

newtype AgnesBakerParallel = AgnesBakerParallel InvestigatorAttrs
  deriving anyclass IsInvestigator
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)
  deriving stock Data

instance HasModifiersFor AgnesBakerParallel where
  getModifiersFor target (AgnesBakerParallel attrs) | isTarget attrs target = do
    modified attrs [ReduceCostOf (#spell <> #event) 2]
  getModifiersFor (CardIdTarget cardId) (AgnesBakerParallel attrs) = do
    maybeModified attrs do
      card <- lift $ getCard cardId
      guard $ cardMatch card (card_ $ #spell <> #event)
      let pcDef = toCardDef card
      startingCost <- case cdCost pcDef of
        Just (StaticCost n) -> pure n
        Just DynamicCost -> pure 0
        Just (MaxDynamicCost _) -> pure 0
        Just DiscardAmountCost -> lift $ fieldMap InvestigatorDiscard (count ((== toCardCode card) . toCardCode)) attrs.id
        Nothing -> pure 0
      pure
        [ AdditionalCost
          $ OrCost
            [ InvestigatorDamageCost (toSource attrs) (InvestigatorWithId attrs.id) DamageAny 1
            , ResourceCost (min startingCost 2)
            ]
        | startingCost > 0
        ]
  getModifiersFor _ _ = pure []

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
      cs <- getActiveCosts
      case cs of
        [c] -> case c.target of
          ForCard _ card -> do
            chooseOneM iid do
              labeled "Shuffle event back in instead of discard?" do
                cardResolutionModifier card attrs card (SetAfterPlay ShuffleThisBackIntoDeck)
              labeled "Resolve normally" nothing
            pure ()
          _ -> error "Do not know how to handle non-card cost"
        _ -> error "do not know how to handle multiple active costs"
      pure i
    _ -> AgnesBakerParallel <$> liftRunMessage msg attrs
