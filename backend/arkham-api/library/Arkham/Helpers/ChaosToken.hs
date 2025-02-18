module Arkham.Helpers.ChaosToken where

import Arkham.ChaosToken.Types
import Arkham.Classes.HasGame
import Arkham.Classes.HasQueue
import Arkham.Classes.Query
import {-# SOURCE #-} Arkham.Game ()
import Arkham.Helpers.Modifiers (ModifierType (..), effectModifiers, getModifiers)
import Arkham.Id
import Arkham.Matcher qualified as Matcher
import Arkham.Matcher.ChaosToken
import Arkham.Message
import Arkham.Prelude
import Arkham.Source
import Arkham.Target
import Arkham.Window (Window (..))
import Arkham.Window qualified as Window

matchChaosToken
  :: HasGame m => InvestigatorId -> ChaosToken -> Matcher.ChaosTokenMatcher -> m Bool
matchChaosToken _ = (<=~>)

cancelChaosToken :: HasQueue Message m => ChaosToken -> m ()
cancelChaosToken token = withQueue_ $ \queue ->
  mapMaybe
    ( \case
        When (RevealChaosToken _ _ token') | token == token' -> Nothing
        RevealChaosToken _ _ token' | token == token' -> Nothing
        After (RevealChaosToken _ _ token') | token == token' -> Nothing
        CheckWindows ws -> case filter (not . isRevealChaosToken) ws of
          [] -> Nothing
          ws' -> Just $ CheckWindows ws'
        Do (CheckWindows ws) -> case filter (not . isRevealChaosToken) ws of
          [] -> Nothing
          ws' -> Just $ Do (CheckWindows ws')
        RequestedChaosTokens s miid ts -> Just $ RequestedChaosTokens s miid (filter (/= token) ts)
        msg -> Just msg
    )
    queue
 where
  isRevealChaosToken w = case windowType w of
    Window.RevealChaosToken _ token' -> token == token'
    _ -> False

getModifiedChaosTokenFaces :: HasGame m => [ChaosToken] -> m [ChaosTokenFace]
getModifiedChaosTokenFaces tokens = concatMapM getModifiedChaosTokenFace tokens

getModifiedChaosTokenFace :: HasGame m => ChaosToken -> m [ChaosTokenFace]
getModifiedChaosTokenFace token = do
  modifiers' <- getModifiers (ChaosTokenTarget token)
  pure $ foldl' applyModifier [chaosTokenFace token] modifiers'
 where
  applyModifier _ (ChaosTokenFaceModifier fs') = fs'
  applyModifier [f'] (ForcedChaosTokenChange f fs) | f == f' = fs
  applyModifier fs _ = fs

chaosTokenEffect
  :: (HasGame m, Sourceable source) => source -> ChaosToken -> ModifierType -> m Message
chaosTokenEffect (toSource -> source) token modifier = do
  ems <- effectModifiers source [modifier]
  pure $ CreateChaosTokenEffect ems source token

onRevealChaosTokenEffect
  :: (Sourceable source, Targetable target)
  => SkillTestId
  -> ChaosTokenMatcher
  -> source
  -> target
  -> [Message]
  -> Message
onRevealChaosTokenEffect sid matchr source target msgs = CreateOnRevealChaosTokenEffect sid matchr (toSource source) (toTarget target) msgs
