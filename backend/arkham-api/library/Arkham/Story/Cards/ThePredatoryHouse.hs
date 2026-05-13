{-# OPTIONS_GHC -Wno-deprecations #-}

module Arkham.Story.Cards.ThePredatoryHouse (thePredatoryHouse) where

import Arkham.Campaigns.TheFeastOfHemlockVale.Helpers
import Arkham.ChaosBag.RevealStrategy
import Arkham.ChaosToken
import Arkham.Helpers.FlavorText
import Arkham.Helpers.Log (remembered)
import Arkham.Helpers.Modifiers
import Arkham.Helpers.Query (getLead)
import Arkham.Location.Types (Field (..))
import Arkham.Matcher hiding (LocationCard)
import Arkham.Message.Lifted.Log (forget)
import Arkham.Projection
import Arkham.ScenarioLogKey (ScenarioLogKey (CancelNextPredation))
import Arkham.Scenarios.HemlockHouse.Helpers (scenarioI18n)
import Arkham.Scenarios.HemlockHouse.PredationBag
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted
import Arkham.Story.Types (StoryAttrs (..))
import Arkham.Trait (Trait (Dormant))
import Arkham.Window qualified as Window
import Data.Aeson.KeyMap ((!?))

newtype ThePredatoryHouse = ThePredatoryHouse StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

thePredatoryHouse :: StoryCard ThePredatoryHouse
thePredatoryHouse = storyWith ThePredatoryHouse Cards.thePredatoryHouse (flippedL .~ True)

instance RunMessage ThePredatoryHouse where
  runMessage msg (ThePredatoryHouse attrs) = runQueueT $ case msg of
    ResolveThisStory _ (is attrs -> True) -> do
      bag <- initPredationBag
      day <- getCampaignDay
      bag' <-
        case day of
          Day1 -> do
            tokenId1 <- getRandom
            tokenId2 <- getRandom
            pure
              $ bag
                { predationTokens = PredationToken tokenId1 #cultist : PredationToken tokenId2 #cultist : bag.tokens
                }
          Day2 -> do
            tokenId <- getRandom
            pure $ bag {predationTokens = PredationToken tokenId #cultist : bag.tokens}
          Day3 -> pure bag
      -- Honor any cancel-next-predation requests from Codex 6 (Gideon) that
      -- fired while this story was still set aside.
      pendingCancel <- remembered CancelNextPredation
      when pendingCancel $ forget CancelNextPredation
      let bag'' = bag' {predationCancelNext = predationCancelNext bag' || pendingCancel}
      pure
        $ ThePredatoryHouse
        $ attrs {storyFlipped = False, storyMeta = toJSON bag'', storyRemoveAfterResolution = False}
    SendMessage (isTarget attrs -> True) (RequestChaosTokens _ _ (Reveal 1) _) | (predationBag attrs).cancelNext -> do
      -- Codex 6 (Gideon Mizrah): cancel the next predation that would take place.
      let bag = predationBag attrs
      let bag' = bag {predationCancelNext = False}
      send "Predation test canceled (Gideon Mizrah)"
      pure $ ThePredatoryHouse $ attrs {storyMeta = toJSON bag'}
    SendMessage (isTarget attrs -> True) (RequestChaosTokens _ _ (Reveal 1) _) -> do
      let bag = predationBag attrs
      lead <- getLead
      (tokens, rest) <- splitAt 1 <$> shuffleM (predationTokens bag)
      let token = fromJustNote "invalid predation token" $ headMay tokens
      let bag' = bag {predationTokens = rest, predationCurrentToken = Just token}
      focusChaosTokens [asChaosToken token] \_unfocus -> do
        checkWhen $ Window.RevealChaosToken lead $ asChaosToken token
        checkWhen
          $ Window.ScenarioEvent
            ("revealPredationToken:" <> tshow token.face)
            (Just lead)
            (toJSON $ asChaosToken token)
        sendMessage attrs $ ResolveChaosToken (asChaosToken token) token.face lead

      pure
        $ ThePredatoryHouse
        $ attrs {storyMeta = toJSON bag'}
    SendMessage (isTarget attrs -> True) (ResolveChaosToken {}) | Just token <- attrs.predationBag.currentToken -> do
      let tokenFace = token.face
      send $ format (asChaosToken token) <> " drawn during predation Test"
      mods <- getModifiers attrs
      let bag =
            attrs.predationBag
              { predationCurrentToken = Nothing
              , predationSetAside = attrs.predationBag.setAside <> [token]
              }
      let
        enabled = \case
          MetaModifier (Object o) -> o !? "treatTabletAsSkill" == Just (Bool True)
          _ -> False
      let treatTabletAsSkull = any enabled mods
      let swapToken = \case
            Tablet | treatTabletAsSkull -> Skull
            t -> t

      lead <- getLead
      let resolvedFace = swapToken tokenFace
      let outcomeKey = case resolvedFace of
            Cultist -> "cultist"
            Tablet -> "tablet"
            ElderThing -> "elderthing"
            _ -> "unknown"
      -- Order matters: modal first, then effects, then unfocus — so the chaos
      -- token stays visible through the modal and the resulting effects.
      send $ "Predation modal push (outcome=" <> outcomeKey <> ")"
      scenarioI18n $ scope "predationTest" $ flavor do
        setTitle "title"
        img Cards.thePredatoryHouse
        p outcomeKey
      send "Predation modal pushed"
      bag' <- case resolvedFace of
        Cultist -> pure bag
        Tablet -> do
          push $ ScenarioSpecific "predationTablet" (toJSON lead)
          pure $ bag {predationTokens = predationTokens bag <> predationSetAside bag, predationSetAside = []}
        ElderThing -> do
          mLid <- selectOne $ NearestLocationTo lead (LocationWithTrait Dormant)
          for_ mLid \lid -> do
            card <- field LocationCard lid
            push $ FlipToEnemyLocation lid card
          pure bag
        _ -> error "Invalid predation token"
      unfocusChaosTokens
      pure $ ThePredatoryHouse $ attrs {storyMeta = toJSON bag'}
    SendMessage (isTarget attrs -> True) (AddChaosToken face) -> do
      let bag = predationBag attrs
      tokenId <- getRandom
      let bag' = bag {predationTokens = PredationToken tokenId face : bag.tokens}
      pure $ ThePredatoryHouse $ attrs {storyMeta = toJSON bag'}
    SendMessage (isTarget attrs -> True) (ChaosTokenCanceled {}) -> do
      let bag = predationBag attrs
      let bag' =
            bag
              { predationTokens = bag.tokens <> maybeToList bag.currentToken
              , predationCurrentToken = Nothing
              }
      unfocusChaosTokens
      pure $ ThePredatoryHouse $ attrs {storyMeta = toJSON bag'}
    SendMessage (isTarget attrs -> True) (ScenarioSpecific "predationCleanup" _) -> do
      -- Resolution 2: "Remove 1 [tablet] token from the predation bag. Return
      -- any remaining [tablet] tokens in the predation bag to the chaos bag for
      -- the remainder of the campaign."
      let bag = predationBag attrs
      let allTablets = filter (\t -> t.face == Tablet) (allBagTokens bag)
      let extraTablets = drop 1 allTablets
      for_ extraTablets $ \_ -> addChaosToken Tablet
      pure $ ThePredatoryHouse attrs
    SendMessage (isTarget attrs -> True) (ScenarioSpecific "cancelNextPredation" _) -> do
      let bag = predationBag attrs
      pure $ ThePredatoryHouse $ attrs {storyMeta = toJSON bag {predationCancelNext = True}}
    _ -> ThePredatoryHouse <$> liftRunMessage msg attrs
