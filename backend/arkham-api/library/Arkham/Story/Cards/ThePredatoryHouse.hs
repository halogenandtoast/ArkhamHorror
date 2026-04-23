{-# OPTIONS_GHC -Wno-deprecations #-}

module Arkham.Story.Cards.ThePredatoryHouse (thePredatoryHouse) where

import Arkham.Campaigns.TheFeastOfHemlockVale.Helpers
import Arkham.Card (forceFlipCard)
import Arkham.ChaosBag.RevealStrategy
import Arkham.ChaosToken
import Arkham.Helpers.Modifiers
import Arkham.Helpers.Query (getLead)
import Arkham.Location.Types (Field (..))
import Arkham.Matcher hiding (LocationCard)
import Arkham.Message.Lifted.Choose
import Arkham.Projection
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
      pure
        $ ThePredatoryHouse
        $ attrs {storyFlipped = False, storyMeta = toJSON bag', storyRemoveAfterResolution = False}
    SendMessage (isTarget attrs -> True) (RequestChaosTokens _ _ (Reveal 1) _) -> do
      let bag = predationBag attrs
      lead <- getLead
      (tokens, rest) <- splitAt 1 <$> shuffleM (predationTokens bag)
      let token = fromJustNote "invalid predation token" $ headMay tokens
      let bag' = bag {predationTokens = rest, predationCurrentToken = Just token}
      focusChaosTokens [asChaosToken token] \unfocus -> do
        checkWhen $ Window.RevealChaosToken lead $ asChaosToken token
        checkWhen
          $ Window.ScenarioEvent
            ("revealPredationToken:" <> tshow token.face)
            (Just lead)
            (toJSON $ asChaosToken token)
        push unfocus
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
      bag' <- case swapToken tokenFace of
        Cultist -> pure bag
        Tablet -> do
          push $ ScenarioSpecific "predationTablet" (toJSON lead)
          pure $ bag {predationTokens = predationTokens bag <> predationSetAside bag, predationSetAside = []}
        ElderThing -> do
          mLid <- selectOne $ NearestLocationTo lead (LocationWithTrait Dormant)
          for_ mLid \lid -> do
            card <- traceShowId <$> field LocationCard lid
            push $ FlipToEnemyLocation lid (forceFlipCard card)
          pure bag
        _ -> error "Invalid predation token"

      leadChooseOneM $ labeled "Continue" nothing
      pure $ ThePredatoryHouse $ attrs {storyMeta = toJSON bag'}

    -- if count ((== Cultist) . predationTokenFace) (predationSetAside bag) == 2
    --   then do
    --     bag' <- initPredationBag
    --     pure
    --       $ ThepredationBegins
    --       $ attrs {storyMeta = toJSON bag'}
    --   else pure $ ThepredationBegins $ attrs {storyMeta = toJSON bag}
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
      pure $ ThePredatoryHouse $ attrs {storyMeta = toJSON bag'}
    _ -> ThePredatoryHouse <$> liftRunMessage msg attrs
