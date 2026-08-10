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
cancelChaosToken token = withQueue_ $ mapMaybe \case
  When (RevealChaosToken _ _ token') | token == token' -> Nothing
  RevealChaosToken _ _ token' | token == token' -> Nothing
  After (RevealChaosToken _ _ token') | token == token' -> Nothing
  Will (ResolveChaosToken drawnToken _ _) | drawnToken == token -> Nothing
  ResolveChaosToken drawnToken _ _ | drawnToken == token -> Nothing
  CheckWindows ws -> case filter (not . isRevealChaosToken) ws of
    [] -> Nothing
    ws' -> Just $ CheckWindows ws'
  Do (CheckWindows ws) -> case filter (not . isRevealChaosToken) ws of
    [] -> Nothing
    ws' -> Just $ Do (CheckWindows ws')
  RequestedChaosTokens s miid ts -> Just $ RequestedChaosTokens s miid (filter (/= token) ts)
  msg -> Just msg
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

{- | Whether the effects printed on a revealed symbol are suppressed, either
because the token is being ignored outright or because something is resolving
in its place (see The Black Cat (5)). Scenario and investigator handlers for
@PassedSkillTest@/@FailedSkillTest@ keyed on a 'ChaosTokenTarget' must check
this before resolving.
-}
chaosTokenSymbolEffectsIgnored :: HasGame m => ChaosToken -> m Bool
chaosTokenSymbolEffectsIgnored token = do
  modifiers' <- foldMapM getModifiers [toTarget token.face, toTarget token]
  pure
    $ any (`elem` modifiers') [IgnoreChaosTokenEffects, IgnoreChaosToken, IgnoreChaosTokenSymbolEffects]

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
