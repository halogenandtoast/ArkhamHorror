module Arkham.Treachery.Cards.ArcaneBarrier (
  ArcaneBarrier (..),
  arcaneBarrier,
  arcaneBarrierEffect,
) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Effect.Runner
import Arkham.Investigator.Types
import Arkham.Matcher
import Arkham.Movement
import Arkham.Projection
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype ArcaneBarrier = ArcaneBarrier TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

arcaneBarrier :: TreacheryCard ArcaneBarrier
arcaneBarrier = treachery ArcaneBarrier Cards.arcaneBarrier

-- TODO: Move move to effect to a modifier...
instance RunMessage ArcaneBarrier where
  runMessage msg t@(ArcaneBarrier attrs) = case msg of
    Revelation iid source | isSource attrs source -> do
      mLocation <- field InvestigatorLocation iid
      for_ mLocation $ push . AttachTreachery (toId attrs) . toTarget
      pure t
    Will (MoveTo movement@(moveTarget -> InvestigatorTarget iid)) -> do
      shouldCostAdditional <-
        selectAny
          $ locationWithTreachery (toId attrs)
          <> LocationMatchAny
            [locationWithInvestigator iid, moveToLocationMatcher movement]
      when shouldCostAdditional $ do
        moveFromMessage <- fromJustNote "missing move from" <$> popMessage
        moveToMessage <- fromJustNote "missing move to" <$> popMessage
        push
          $ createCardEffect
            Cards.arcaneBarrier
            (Just $ EffectMessages [moveFromMessage, moveToMessage])
            (toSource attrs)
            (toTarget iid)
      pure t
    _ -> ArcaneBarrier <$> runMessage msg attrs

newtype ArcaneBarrierEffect = ArcaneBarrierEffect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

arcaneBarrierEffect :: EffectArgs -> ArcaneBarrierEffect
arcaneBarrierEffect = cardEffect ArcaneBarrierEffect Cards.arcaneBarrier

instance RunMessage ArcaneBarrierEffect where
  runMessage msg e@(ArcaneBarrierEffect attrs) = case msg of
    CreatedEffect eid _ _ (InvestigatorTarget iid) | eid == toId attrs -> do
      push $ beginSkillTest iid attrs iid #willpower 4
      pure e
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      let
        moveMessages = case effectMetadata attrs of
          Just (EffectMessages msgs) -> msgs
          _ -> error "messages must be supplied"
      player <- getPlayer iid
      pushAll
        [ disable attrs
        , chooseOne
            player
            [ Label "Cancel Move" []
            , Label
                "Discard top 5 cards of your deck"
                (DiscardTopOfDeck iid 5 (effectSource attrs) Nothing : moveMessages)
            ]
        ]
      pure e
    PassedThisSkillTest _ (isSource attrs -> True) -> do
      let
        moveMessages = case effectMetadata attrs of
          Just (EffectMessages msgs) -> msgs
          _ -> error "messages must be supplied"
      case effectSource attrs of
        TreacherySource tid ->
          pushAll $ disable attrs : Discard (effectSource attrs) (toTarget tid) : moveMessages
        _ -> error "Has to be a treachery source"
      pure e
    _ -> ArcaneBarrierEffect <$> runMessage msg attrs
