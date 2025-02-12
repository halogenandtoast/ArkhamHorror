module Arkham.Investigator.Cards.AgathaCrane where

import Arkham.Ability
import Arkham.Card
import Arkham.Game.Helpers (getIsPlayable)
import Arkham.GameT
import Arkham.Investigator.Import.Lifted
import Arkham.Investigator.Projection ()
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Modifier
import Arkham.Trait (Trait (Insight, Spell))
import Arkham.Window (defaultWindows)

agathaTokenValues
  :: Applicative m => InvestigatorId -> ChaosTokenFace -> InvestigatorAttrs -> m ChaosTokenValue
agathaTokenValues iid ElderSign attrs | iid == toId attrs = do
  pure $ ChaosTokenValue ElderSign (PositiveModifier 2)
agathaTokenValues _ token _ = pure $ ChaosTokenValue token mempty

agathaAbilities :: InvestigatorAttrs -> [Ability]
agathaAbilities a =
  [ restricted
      a
      1
      ( Self
          <> criteria
          <> exists
            ( InDiscardOf (be a.id)
                <> PlayableCard (UnpaidCost NoAction) (basic $ #event <> hasAnyTrait [Spell, Insight])
            )
      )
      $ freeReaction (TurnEnds #when You)
  ]
 where
  criteria =
    if lookupMetaKeyWithDefault "agathaTrigger" False a
      then NoRestriction
      else Never

agathaRunner :: (InvestigatorAttrs -> a) -> Message -> InvestigatorAttrs -> GameT a
agathaRunner f msg attrs = runQueueT $ case msg of
  BeginTurn iid | iid == attrs.id -> do
    result <- liftRunMessage msg attrs
    pure $ f $ setMetaKey "agathaTrigger" False result
  UseThisAbility iid (isSource attrs -> True) 1 -> do
    cards <-
      filterM (getIsPlayable iid (attrs.ability 1) (UnpaidCost NoAction) (defaultWindows iid))
        . map toCard
        . filterCards (card_ $ #event <> hasAnyTrait [Spell, Insight])
        =<< iid.discard
    focusCards cards $ chooseTargetM iid cards \card -> do
      cardResolutionModifier card (attrs.ability 1) card RemoveFromGameInsteadOfDiscard
      playCardPayingCost iid card
    pure $ f attrs
  ResolveChaosToken token ElderSign iid | iid == attrs.id -> do
    chooseOneM iid do
      labeled "Cancel this token and draw another. If you do, draw 1 card" do
        cancelChaosToken ElderSign iid token
        returnChaosTokens [token]
        unfocusChaosTokens
        drawAnotherChaosToken iid
        drawCards iid ElderSign 1
      labeled "Do not cancel this token" nothing
    pure $ f attrs
  ChaosTokenIgnored iid _ _ | iid == attrs.id -> do
    result <- liftRunMessage msg attrs
    pure $ f $ setMetaKey "agathaTrigger" True result
  ChaosTokenCanceled iid _ _ | iid == attrs.id -> do
    result <- liftRunMessage msg attrs
    pure $ f $ setMetaKey "agathaTrigger" True result
  SealedChaosToken _ (Just iid) _ | iid == attrs.id -> do
    result <- liftRunMessage msg attrs
    pure $ f $ setMetaKey "agathaTrigger" True result
  _ -> f <$> liftRunMessage msg attrs
