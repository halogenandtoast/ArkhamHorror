{-# OPTIONS_GHC -Wno-dodgy-imports #-}
{-# OPTIONs_GHC -Wno-orphans #-}
module Arkham.ChaosBag
  ( ChaosBag
  , emptyChaosBag
  , tokensL
  ) where

import Arkham.Prelude

import Arkham.ChaosBag.Base
import Arkham.ChaosBag.RevealStrategy
import Arkham.ChaosBagStepState
import Arkham.Classes
import Arkham.Game.Helpers
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Id
import Arkham.Matcher (TokenMatcher(TokenFaceIsNot, AnyToken))
import Arkham.Message
import Arkham.RequestedTokenStrategy
import Arkham.Source
import Arkham.Timing qualified as Timing
import Arkham.Token
import Arkham.Window ( Window (..) )
import Arkham.Window qualified as Window
import Control.Monad.State hiding (filterM)

isUndecided :: ChaosBagStepState -> Bool
isUndecided (Undecided _) = True
isUndecided (Deciding _) = True
isUndecided _ = False

isResolved :: ChaosBagStepState -> Bool
isResolved (Resolved _) = True
isResolved _ = False

toDecided :: ChaosBagStepState -> ChaosBagStepState
toDecided (Undecided x) = Deciding x
toDecided (Deciding x) = Decided x
toDecided other = other

toTokens :: ChaosBagStepState -> [Token]
toTokens (Resolved tokens') = tokens'
toTokens _ = []

toGroups
  :: HasCallStack => [ChaosBagStepState] -> [[Token]] -> [([ChaosBagStepState], [[Token]])]
toGroups !steps !tokens' = go steps []
 where
  go [] _ = []
  go (step : rest) prev = case step of
    Resolved group' -> (prev <> rest, group' : tokens') : go rest (step : prev)
    _ -> error "must be resolved"

replaceFirstChoice
  :: HasCallStack
  => Source
  -> InvestigatorId
  -> RequestedTokenStrategy
  -> ChaosBagStep
  -> ChaosBagStepState
  -> ChaosBagStepState
replaceFirstChoice source iid strategy replacement = \case
  Undecided s -> error $ "should not be ran with undecided: " <> show s
  Deciding _ -> error "should not be ran with deciding"
  Resolved tokens' -> Resolved tokens'
  Decided step -> case step of
    Draw -> Decided Draw
    Choose n tokenStrategy steps tokens' -> if all isResolved steps
      then Decided replacement
      else Decided $ Choose
        n
        tokenStrategy
        (replaceFirstChooseChoice source iid strategy replacement steps)
        tokens'
    ChooseMatch n tokenStrategy steps tokens' matcher -> if all isResolved steps
      then Decided replacement
      else Decided $ ChooseMatch
        n
        tokenStrategy
        (replaceFirstChooseChoice source iid strategy replacement steps)
        tokens'
        matcher
    ChooseMatchChoice steps tokens' matchers -> if all isResolved steps
      then Decided replacement
      else Decided $ ChooseMatchChoice
        (replaceFirstChooseChoice source iid strategy replacement steps)
        tokens'
        matchers

replaceFirstChooseChoice
  :: HasCallStack
  => Source
  -> InvestigatorId
  -> RequestedTokenStrategy
  -> ChaosBagStep
  -> [ChaosBagStepState]
  -> [ChaosBagStepState]
replaceFirstChooseChoice source iid strategy replacement = \case
  [] -> []
  (Undecided _ : _) -> error "should not be ran with undecided"
  (Deciding _ : _) -> error "should not be ran with deciding"
  (Resolved tokens' : rest) ->
    Resolved tokens'
      : replaceFirstChooseChoice source iid strategy replacement rest
  (Decided step : rest) ->
    replaceFirstChoice source iid strategy replacement (Decided step) : rest

resolveFirstUnresolved
  :: HasCallStack
  => Source
  -> InvestigatorId
  -> RequestedTokenStrategy
  -> ChaosBagStepState
  -> StateT ChaosBag GameT (ChaosBagStepState, [Message])
resolveFirstUnresolved source iid strategy = \case
  Undecided _ -> error "should not be ran with undecided"
  Deciding _ -> error "should not be ran with undecided"
  Resolved tokens' -> pure (Resolved tokens', [])
  Decided step -> case step of
    Draw -> do
      bagTokens <- gets chaosBagTokens
      forceDraw <- gets chaosBagForceDraw
      case forceDraw of
        Just face -> do
          case find ((== face) . tokenFace) bagTokens of
            Nothing -> do
              (drawn, remaining) <- splitAt 1 <$> shuffleM bagTokens
              modify'
                ((forceDrawL .~ Nothing)
                . (tokensL .~ remaining)
                . (setAsideTokensL %~ (drawn <>))
                )
              pure (Resolved drawn, [])
            Just drawn -> do
              let remaining = delete drawn bagTokens
              modify'
                ((forceDrawL .~ Nothing)
                . (tokensL .~ remaining)
                . (setAsideTokensL %~ ([drawn] <>))
                )
              pure (Resolved [drawn], [])
        Nothing -> do
          (drawn, remaining) <- splitAt 1 <$> shuffleM bagTokens
          modify' ((tokensL .~ remaining) . (setAsideTokensL %~ (drawn <>)))
          pure (Resolved drawn, [])
    Choose n tokenStrategy steps tokens' ->
      pure (Decided $ ChooseMatch n tokenStrategy steps tokens' AnyToken, [])
    ChooseMatch 0 CancelChoice steps tokenGroups _ -> pure (Resolved $ concatMap toTokens steps, concatMap (map TokenCanceledOrIgnored) tokenGroups)
    ChooseMatch 0 IgnoreChoice steps tokenGroups _ -> pure (Resolved $ concatMap toTokens steps, concatMap (map TokenCanceledOrIgnored) tokenGroups)
    ChooseMatch n tokenStrategy steps tokens' matcher -> if length tokens' >= n && tokenStrategy == ResolveChoice
      then pure (Resolved $ concat tokens', concatMap (map TokenCanceledOrIgnored . toTokens) steps)
      else if all isResolved steps
        then do
            modifiers' <- lift $ getModifiers iid
            let
              collectTokens xs (CannotCancelOrIgnoreToken t) = t : xs
              collectTokens xs _ = xs
              tokensThatCannotBeIgnored = foldl' collectTokens [] modifiers'
              uncanceleableSteps = filter (\s -> any (`elem` tokensThatCannotBeIgnored) (map tokenFace $ toTokens s)) steps
              remainingSteps = filter (`notElem` uncanceleableSteps) steps
            if (notNull uncanceleableSteps && tokenStrategy == ResolveChoice) || (null remainingSteps && tokenStrategy /= ResolveChoice)
               then do
                 let
                   tokens'' = map toTokens uncanceleableSteps
                   n' = if tokenStrategy == ResolveChoice then n else max 0 (n - length uncanceleableSteps)
                 pure $ if tokenStrategy == ResolveChoice
                    then ( Decided (ChooseMatch n' tokenStrategy remainingSteps (tokens' <> tokens'') matcher) , [])
                    else ( Resolved $ concatMap toTokens steps, [])
               else if (length steps + length tokens') <= n && tokenStrategy == ResolveChoice
                then pure (Resolved $ concat tokens' <> concatMap toTokens steps, [])
                else do
                  let
                    groups = toGroups steps tokens'
                    chooseFunction :: Monad m => (a -> m Bool) -> [a] -> m Bool
                    chooseFunction = if tokenStrategy == ResolveChoice then anyM else allM
                    matcher' = if tokenStrategy == ResolveChoice || null tokensThatCannotBeIgnored then matcher else matcher <> foldMap TokenFaceIsNot tokensThatCannotBeIgnored

                  matchedGroups <- filterM
                    (\(_, chosen) ->
                      chooseFunction (chooseFunction (\t -> lift $ matchToken iid t matcher')) chosen
                    )
                    groups
                  let
                    groups' = if null matchedGroups then groups else matchedGroups
                    nFunc = if tokenStrategy == ResolveChoice then id else subtract 1

                  pure $ if tokenStrategy /= ResolveChoice && null matchedGroups
                    then ( Resolved $ concatMap toTokens steps, [])
                    else
                      ( Decided (ChooseMatch n tokenStrategy steps tokens' matcher)
                      , [ chooseOne
                            iid
                            [ TokenGroupChoice
                                source
                                iid
                                (ChooseMatch (nFunc n) tokenStrategy remaining chosen matcher)
                            | (remaining, chosen) <- groups'
                            ]
                        ]
                      )
        else do
          (steps', msgs) <- resolveFirstChooseUnresolved
            source
            iid
            strategy
            steps
          pure (Decided $ ChooseMatch n tokenStrategy steps' tokens' matcher, msgs)
    ChooseMatchChoice steps tokens' choices -> do
      if all isResolved steps
        then do
          modifiers' <- lift $ getModifiers iid
          let
            collectTokens xs (CannotCancelOrIgnoreToken t) = t : xs
            collectTokens xs _ = xs
            tokensThatCannotBeIgnored = foldl' collectTokens [] modifiers'
            allTokens = concatMap toTokens steps 
            toStrategy = \case
              Draw -> ResolveChoice
              Choose _ st _ _ -> st
              ChooseMatch _ st _ _ _ -> st
              ChooseMatchChoice{} -> error "Do not nest these"
            isValidMatcher tokenStrategy matcher =
              let matcher' = if tokenStrategy == ResolveChoice || null tokensThatCannotBeIgnored then matcher else matcher <> foldMap TokenFaceIsNot tokensThatCannotBeIgnored
              in anyM (\t -> lift $ matchToken iid t matcher') allTokens
          choices' <- map snd <$> filterM (\(m, (_, s)) -> isValidMatcher (toStrategy s) m) choices
          let
            fixStep = \case
              Draw -> Draw
              Choose n tokenStrategy _ _ -> Choose n tokenStrategy steps []
              ChooseMatch n tokenStrategy _ _ matcher -> ChooseMatch n tokenStrategy steps [] matcher
              ChooseMatchChoice _ _ matchers -> ChooseMatchChoice steps [] matchers

          pure $ (Decided $ ChooseMatchChoice steps tokens' choices, [chooseOrRunOne iid [Label label [SetChaosBagChoice source iid (fixStep step')] | (label, step') <- choices']])
        else do
          (steps', msgs) <- resolveFirstChooseUnresolved
            source
            iid
            strategy
            steps
          pure (Decided $ ChooseMatchChoice steps' tokens' choices, msgs)

resolveFirstChooseUnresolved
  :: HasCallStack
  => Source
  -> InvestigatorId
  -> RequestedTokenStrategy
  -> [ChaosBagStepState]
  -> StateT ChaosBag GameT ([ChaosBagStepState], [Message])
resolveFirstChooseUnresolved source iid strategy = \case
  [] -> pure ([], [])
  (Undecided s : _) -> error $ "should not be called with undecided: "  <> show s
  (Deciding _ : _) -> error "should not be called with deciding"
  (Resolved tokens' : rest) -> do
    (rest', msgs) <- resolveFirstChooseUnresolved source iid strategy rest
    pure (Resolved tokens' : rest', msgs)
  (Decided step : rest) -> do
    (step', msgs) <- resolveFirstUnresolved source iid strategy (Decided step)
    pure (step' : rest, msgs)

decideFirstUndecided
  :: Source
  -> InvestigatorId
  -> RequestedTokenStrategy
  -> (ChaosBagStepState -> ChaosBagStepState)
  -> ChaosBagStepState
  -> (ChaosBagStepState, [Message])
decideFirstUndecided source iid strategy f = \case
  Decided step -> (Decided step, [])
  Resolved tokens' -> (Resolved tokens', [])
  Undecided step -> case step of
    Draw ->
      ( f $ Undecided Draw
      , [ CheckWindow
          [iid]
          [Window Timing.When (Window.WouldRevealChaosToken source iid)]
        , NextChaosBagStep source (Just iid) strategy
        ]
      )
    Choose n tokenStrategy steps tokens' -> if any isUndecided steps
      then
        let
          (steps', msgs) =
            decideFirstChooseUndecided source iid strategy f steps
        in (Deciding $ Choose n tokenStrategy steps' tokens', msgs)
      else (f $ Deciding (Choose n tokenStrategy steps tokens'), [])
    ChooseMatch n tokenStrategy steps tokens' matcher -> if any isUndecided steps
      then
        let
          (steps', msgs) =
            decideFirstChooseUndecided source iid strategy f steps
        in (Deciding $ ChooseMatch n tokenStrategy steps' tokens' matcher, msgs)
      else (f $ Deciding (ChooseMatch n tokenStrategy steps tokens' matcher), [])
    ChooseMatchChoice steps tokens' matchers -> if any isUndecided steps
      then
        let
          (steps', msgs) =
            decideFirstChooseUndecided source iid strategy f steps
        in (Deciding $ ChooseMatchChoice steps' tokens' matchers, msgs)
      else (f $ Deciding (ChooseMatchChoice steps tokens' matchers), [])
  Deciding step -> case step of
    Draw -> (f $ Deciding Draw, [NextChaosBagStep source (Just iid) strategy])
    Choose n tokenStrategy steps tokens' -> if any isUndecided steps
      then
        let
          (steps', msgs) =
            decideFirstChooseUndecided source iid strategy f steps
        in (Deciding $ Choose n tokenStrategy steps' tokens', msgs)
      else (f $ Deciding (Choose n tokenStrategy steps tokens'), [])
    ChooseMatch n tokenStrategy steps tokens' matcher -> if any isUndecided steps
      then
        let
          (steps', msgs) =
            decideFirstChooseUndecided source iid strategy f steps
        in (Deciding $ ChooseMatch n tokenStrategy steps' tokens' matcher, msgs)
      else (f $ Deciding (ChooseMatch n tokenStrategy steps tokens' matcher), [])
    ChooseMatchChoice steps tokens' matchers -> if any isUndecided steps
      then
        let
          (steps', msgs) =
            decideFirstChooseUndecided source iid strategy f steps
        in (Deciding $ ChooseMatchChoice steps' tokens' matchers, msgs)
      else (f $ Deciding (ChooseMatchChoice steps tokens' matchers), [])

decideFirstChooseUndecided
  :: Source
  -> InvestigatorId
  -> RequestedTokenStrategy
  -> (ChaosBagStepState -> ChaosBagStepState)
  -> [ChaosBagStepState]
  -> ([ChaosBagStepState], [Message])
decideFirstChooseUndecided source iid strategy f = \case
  [] -> ([], [])
  (Decided step : rest) ->
    let (rest', msgs) = decideFirstChooseUndecided source iid strategy f rest
    in (Decided step : rest', msgs)
  (Resolved tokens' : rest) ->
    let (rest', msgs) = decideFirstChooseUndecided source iid strategy f rest
    in (Resolved tokens' : rest', msgs)
  (Undecided step : rest) ->
    let
      (step', msgs) =
        decideFirstUndecided source iid strategy f (Undecided step)
    in (step' : rest, msgs)
  (Deciding step : rest) ->
    let
      (step', msgs) =
        decideFirstUndecided source iid strategy f (Deciding step)
    in (step' : rest, msgs)

replaceDeciding :: ChaosBagStepState -> ChaosBagStepState -> ChaosBagStepState
replaceDeciding current replacement = case current of
  Deciding step -> case step of
    Draw -> replacement
    ChooseMatch n tokenStrategy steps tokens' matcher -> Deciding
      $ ChooseMatch n tokenStrategy (replaceDecidingList steps replacement) tokens' matcher
    ChooseMatchChoice steps tokens' matchers -> Deciding
      $ ChooseMatchChoice (replaceDecidingList steps replacement) tokens' matchers
    Choose n tokenStrategy steps tokens' ->
      Deciding $ Choose n tokenStrategy (replaceDecidingList steps replacement) tokens'
  _ -> error $ "should be impossible, seen: " <> show current

replaceDecidingList
  :: [ChaosBagStepState] -> ChaosBagStepState -> [ChaosBagStepState]
replaceDecidingList steps replacement = case steps of
  [] -> []
  (Deciding step : xs) -> case step of
    Draw -> replacement : xs
    ChooseMatch{} -> replaceDeciding (Deciding step) replacement : xs
    ChooseMatchChoice{} -> replaceDeciding (Deciding step) replacement : xs
    Choose{} -> replaceDeciding (Deciding step) replacement : xs
  (stepState : xs) -> stepState : replaceDecidingList xs replacement

replaceChooseMatchChoice :: ChaosBagStepState -> ChaosBagStepState -> ChaosBagStepState
replaceChooseMatchChoice current replacement = case current of
  Decided step -> case step of
    Draw -> Decided Draw
    Choose n tokenStrategy steps tokens' ->
      Decided $ Choose n tokenStrategy (replaceChooseMatchChoiceList steps replacement) tokens'
    ChooseMatch n tokenStrategy steps tokens' matcher -> Decided
      $ ChooseMatch n tokenStrategy (replaceChooseMatchChoiceList steps replacement) tokens' matcher
    ChooseMatchChoice steps tokens' matchers ->
      let candidate = replaceChooseMatchChoiceList steps replacement
      in if candidate == steps
        then replacement
        else Decided $ ChooseMatchChoice candidate tokens' matchers
  _ -> current

replaceChooseMatchChoiceList
  :: [ChaosBagStepState] -> ChaosBagStepState -> [ChaosBagStepState]
replaceChooseMatchChoiceList steps replacement = case steps of
  [] -> []
  (Decided step : xs) -> case step of
    ChooseMatchChoice{} -> replaceChooseMatchChoice (Decided step) replacement : xs
    Draw -> Decided Draw : replaceChooseMatchChoiceList xs replacement

    ChooseMatch n tokenStrategy steps' tokens'  matcher -> let candidate = replaceChooseMatchChoiceList steps' replacement in if candidate == steps' then Decided step : replaceChooseMatchChoiceList xs replacement else Decided (ChooseMatch n tokenStrategy candidate tokens' matcher) : xs

    Choose n tokenStrategy steps' tokens' ->
      let candidate = replaceChooseMatchChoiceList steps' replacement
      in if candidate == steps' then Decided step : replaceChooseMatchChoiceList xs replacement else Decided (Choose n tokenStrategy candidate tokens') : xs
  (stepState : xs) -> stepState : replaceChooseMatchChoiceList xs replacement

instance RunMessage ChaosBag where
  runMessage msg c@ChaosBag {..} = case msg of
    ForceTokenDraw face -> do
      activeInvestigatorId <- getActiveInvestigatorId
      push $ StartSkillTest activeInvestigatorId
      pure $ c & forceDrawL ?~ face
    SetTokens tokens' -> do
      tokens'' <- traverse createToken tokens'
      pure $ c & tokensL .~ tokens'' & setAsideTokensL .~ mempty
    ResetTokens _source ->
      pure
        $ c
        & (tokensL <>~ chaosBagSetAsideTokens)
        & (setAsideTokensL .~ mempty)
        & (choiceL .~ Nothing)
    RequestTokens source miid revealStrategy strategy -> do
      push (RunBag source miid strategy)
      case revealStrategy of
        Reveal n -> case n of
          0 -> pure $ c & revealedTokensL .~ []
          1 -> pure $ c & choiceL ?~ Undecided Draw & revealedTokensL .~ []
          x ->
            pure
              $ c
              & (choiceL
                ?~ Undecided (Choose x ResolveChoice (replicate x (Undecided Draw)) [])
                )
              & (revealedTokensL .~ [])
        RevealAndChoose n m -> case n of
          0 -> error "should be more than 1"
          1 -> error "should be more than 1"
          x ->
            pure
              $ c
              & (choiceL
                ?~ Undecided (Choose m ResolveChoice (replicate x (Undecided Draw)) [])
                )
              & (revealedTokensL .~ [])
    RunBag source miid strategy -> case chaosBagChoice of
      Nothing -> error "unexpected"
      Just choice' -> if isUndecided choice'
        then do
          iid <- maybe getLeadInvestigatorId pure miid
          let
            (choice'', msgs) =
              decideFirstUndecided source iid strategy toDecided choice'
          push (RunBag source miid strategy)
          pushAll msgs
          pure $ c & choiceL ?~ choice''
        else c
          <$ pushAll [BeforeRevealTokens, RunDrawFromBag source miid strategy]
    NextChaosBagStep source miid strategy -> case chaosBagChoice of
      Nothing -> error "unexpected"
      Just choice' -> do
        iid <- maybe getLeadInvestigatorId pure miid
        let
          (updatedChoice, messages) =
            decideFirstUndecided source iid strategy toDecided choice'
        unless (null messages) $ pushAll messages
        pure $ c & choiceL ?~ updatedChoice
    SetChaosBagChoice _ _ step -> case chaosBagChoice of
      Nothing -> error "unexpected"
      Just choice' -> do
        -- When we replace we need to remove the original would reveal chaos
        -- token message as it will still be on the stack even though that
        -- token draw is gone
        removeAllMessagesMatching $ \case
          RunWindow _ [Window Timing.When (Window.WouldRevealChaosToken{})] -> True
          _ -> False

        let
          choice'' = replaceChooseMatchChoice choice' (Decided step)
        pure $ c & choiceL ?~ choice''
    ReplaceCurrentDraw source iid step -> case chaosBagChoice of
      Nothing -> error "unexpected"
      Just choice' -> do
        -- When we replace we need to remove the original would reveal chaos
        -- token message as it will still be on the stack even though that
        -- token draw is gone
        removeAllMessagesMatching $ \case
          RunWindow _ [Window Timing.When (Window.WouldRevealChaosToken{})] -> True
          _ -> False

        -- if we have not decided we can use const to replace
        let
          choice'' = replaceDeciding choice' (Undecided step)
          (updatedChoice, messages) =
            decideFirstUndecided source iid SetAside toDecided choice''
        unless (null messages) $ pushAll messages
        pure $ c & choiceL ?~ updatedChoice
    RunDrawFromBag source miid strategy -> case chaosBagChoice of
      Nothing -> error "unexpected"
      Just choice' -> case choice' of
        Resolved tokenFaces' -> do
          checkWindowMsgs <- case miid of
            Nothing -> pure []
            Just iid -> pure <$> checkWindows
              [ Window Timing.When (Window.RevealToken iid token)
              | token <- tokenFaces'
              ]
          pushAll
            (FocusTokens tokenFaces'
            : checkWindowMsgs
            <> [RequestedTokens source miid tokenFaces', UnfocusTokens]
            )
          pure $ c & choiceL .~ Nothing
        _ -> do
          iid <- maybe getLeadInvestigatorId pure miid
          ((choice'', msgs), c') <- runStateT
            (resolveFirstUnresolved source iid strategy choice')
            c
          push (RunDrawFromBag source miid strategy)
          pushAll msgs
          pure $ c' & choiceL ?~ choice''
    ChooseTokenGroups source iid groupChoice -> case chaosBagChoice of
      Nothing -> error "unexpected"
      Just choice' -> do
        let
          updatedChoice =
            replaceFirstChoice source iid SetAside groupChoice choice'
        pure $ c & choiceL ?~ updatedChoice
    RevealToken _source _iid token ->
      -- TODO: we may need a map of source to tokens here
      pure $ c & revealedTokensL %~ (token :)
    ReturnTokens tokens' ->
      pure
        $ c
        & (tokensL %~ (<> tokens'))
        & (setAsideTokensL %~ (\\ tokens'))
        & (choiceL .~ Nothing)
    AddToken tokenFace -> do
      token <- createToken tokenFace
      pure $ c & tokensL %~ (token :)
    SealToken token -> pure $ c & tokensL %~ filter (/= token) & setAsideTokensL %~ filter (/= token) & revealedTokensL %~ filter (/= token)
    UnsealToken token -> pure $ c & tokensL %~ (token :)
    _ -> pure c
