module Arkham.Homebrew.DarkMatter.Treacheries.PredictiveAlgorithm (predictiveAlgorithm) where

import Arkham.Ability
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelectWhen)
import Arkham.Helpers.SkillTest (getSkillTestAction, getSkillTestTarget)
import Arkham.Homebrew.DarkMatter.CardDefs.Treacheries qualified as Cards
import Arkham.Homebrew.DarkMatter.Traits (pattern AI)
import Arkham.Matcher
import Arkham.Placement
import Arkham.Treachery.Import.Lifted

newtype PredictiveAlgorithm = PredictiveAlgorithm TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

predictiveAlgorithm :: TreacheryCard PredictiveAlgorithm
predictiveAlgorithm = treachery PredictiveAlgorithm Cards.predictiveAlgorithm

{- | "You get -1 to each of your skills while fighting or evading [[AI]]
enemies."
-}
instance HasModifiersFor PredictiveAlgorithm where
  getModifiersFor (PredictiveAlgorithm a) = case a.placement of
    InThreatArea iid -> do
      fightingOrEvadingAI <-
        getSkillTestAction >>= \case
          Just action
            | action `elem` [#fight, #evade] ->
                getSkillTestTarget >>= \case
                  Just (EnemyTarget eid) -> eid <=~> EnemyWithTrait AI
                  _ -> pure False
          _ -> pure False
      modifySelectWhen a fightingOrEvadingAI (InvestigatorWithId iid) [AnySkillValue (-1)]
    _ -> pure ()

-- "[action] Choose and discard 2 cards from your hand: Discard Predictive Algorithm."
instance HasAbilities PredictiveAlgorithm where
  getAbilities (PredictiveAlgorithm a) =
    [ restricted a 1 (InThreatAreaOf You)
        $ actionAbilityWithCost (HandDiscardCost 2 #any)
    ]

instance RunMessage PredictiveAlgorithm where
  runMessage msg t@(PredictiveAlgorithm attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      placeInThreatArea attrs iid
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      toDiscardBy iid (attrs.ability 1) attrs
      pure t
    _ -> PredictiveAlgorithm <$> liftRunMessage msg attrs
