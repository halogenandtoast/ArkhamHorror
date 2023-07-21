{-# OPTIONS_GHC -Wno-dodgy-imports #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Arkham.ChaosBag (
  ChaosBag,
  emptyChaosBag,
  chaosTokensL,
) where

import Arkham.Prelude

import Arkham.ChaosBag.Base
import Arkham.ChaosBag.RevealStrategy
import Arkham.ChaosBagStepState
import Arkham.ChaosToken
import Arkham.Classes
import Arkham.Game.Helpers
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Id
import Arkham.Matcher (ChaosTokenMatcher (AnyChaosToken, ChaosTokenFaceIsNot))
import Arkham.Message
import Arkham.RequestedChaosTokenStrategy
import Arkham.Source
import Arkham.Timing qualified as Timing
import Arkham.Window (Window (..))
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

toChaosTokens :: ChaosBagStepState -> [ChaosToken]
toChaosTokens (Resolved tokens') = tokens'
toChaosTokens _ = []

toGroups
  :: HasCallStack => [ChaosBagStepState] -> [[ChaosToken]] -> [([ChaosBagStepState], [[ChaosToken]])]
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
  -> RequestedChaosTokenStrategy
  -> ChaosBagStep
  -> ChaosBagStepState
  -> ChaosBagStepState
replaceFirstChoice source iid strategy replacement = \case
  Undecided s -> error $ "should not be ran with undecided: " <> show s
  Deciding _ -> error "should not be ran with deciding"
  Resolved tokens' -> Resolved tokens'
  Decided step -> case step of
    Draw -> Decided Draw
    Choose chooseSource n tokenStrategy steps tokens' ->
      if all isResolved steps
        then Decided replacement
        else
          Decided $
            Choose
              chooseSource
              n
              tokenStrategy
              (replaceFirstChooseChoice source iid strategy replacement steps)
              tokens'
    ChooseMatch chooseSource n tokenStrategy steps tokens' matcher ->
      if all isResolved steps
        then Decided replacement
        else
          Decided $
            ChooseMatch
              chooseSource
              n
              tokenStrategy
              (replaceFirstChooseChoice source iid strategy replacement steps)
              tokens'
              matcher
    ChooseMatchChoice steps tokens' matchers ->
      if all isResolved steps
        then Decided replacement
        else
          Decided $
            ChooseMatchChoice
              (replaceFirstChooseChoice source iid strategy replacement steps)
              tokens'
              matchers

replaceFirstChooseChoice
  :: HasCallStack
  => Source
  -> InvestigatorId
  -> RequestedChaosTokenStrategy
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
  -> RequestedChaosTokenStrategy
  -> ChaosBagStepState
  -> StateT ChaosBag GameT (ChaosBagStepState, [Message])
resolveFirstUnresolved source iid strategy = \case
  Undecided _ -> error "should not be ran with undecided"
  Deciding _ -> error "should not be ran with undecided"
  Resolved tokens' -> pure (Resolved tokens', [])
  Decided step -> case step of
    Draw -> do
      bagChaosTokens <- gets chaosBagChaosTokens
      forceDraw <- gets chaosBagForceDraw
      case forceDraw of
        Just face -> do
          case find ((== face) . chaosTokenFace) bagChaosTokens of
            Nothing -> do
              (drawn, remaining) <- splitAt 1 <$> shuffleM bagChaosTokens
              modify'
                ( (forceDrawL .~ Nothing)
                    . (chaosTokensL .~ remaining)
                    . (setAsideChaosTokensL %~ (drawn <>))
                )
              pure (Resolved drawn, [])
            Just drawn -> do
              let remaining = delete drawn bagChaosTokens
              modify'
                ( (forceDrawL .~ Nothing)
                    . (chaosTokensL .~ remaining)
                    . (setAsideChaosTokensL %~ ([drawn] <>))
                )
              pure (Resolved [drawn], [])
        Nothing -> do
          (drawn, remaining) <- splitAt 1 <$> shuffleM bagChaosTokens
          modify' ((chaosTokensL .~ remaining) . (setAsideChaosTokensL %~ (drawn <>)))
          pure (Resolved drawn, [])
    Choose chooseSource n tokenStrategy steps tokens' ->
      pure (Decided $ ChooseMatch chooseSource n tokenStrategy steps tokens' AnyChaosToken, [])
    ChooseMatch chooseSource 0 CancelChoice steps tokenGroups _ ->
      pure
        ( Resolved $ concatMap toChaosTokens steps
        , concatMap (map (ChaosTokenCanceled iid chooseSource)) tokenGroups
        )
    ChooseMatch chooseSource 0 IgnoreChoice steps tokenGroups _ ->
      pure
        ( Resolved $ concatMap toChaosTokens steps
        , concatMap (map (ChaosTokenIgnored iid chooseSource)) tokenGroups
        )
    ChooseMatch chooseSource n tokenStrategy steps tokens' matcher ->
      if length tokens' >= n && tokenStrategy == ResolveChoice
        then
          pure
            ( Resolved $ concat tokens'
            , concatMap (map (ChaosTokenIgnored iid chooseSource) . toChaosTokens) steps
            )
        else
          if all isResolved steps
            then do
              modifiers' <- lift $ getModifiers iid
              let
                collectChaosTokens xs (CannotCancelOrIgnoreChaosToken t) = t : xs
                collectChaosTokens xs _ = xs
                tokensThatCannotBeIgnored = foldl' collectChaosTokens [] modifiers'
                uncanceleableSteps =
                  filter (\s -> any (`elem` tokensThatCannotBeIgnored) (map chaosTokenFace $ toChaosTokens s)) steps
                remainingSteps = filter (`notElem` uncanceleableSteps) steps
              if (notNull uncanceleableSteps && tokenStrategy == ResolveChoice)
                || (null remainingSteps && tokenStrategy /= ResolveChoice)
                then do
                  let
                    tokens'' = map toChaosTokens uncanceleableSteps
                    n' = if tokenStrategy == ResolveChoice then n else max 0 (n - length uncanceleableSteps)
                  pure $
                    if tokenStrategy == ResolveChoice
                      then
                        ( Decided (ChooseMatch chooseSource n' tokenStrategy remainingSteps (tokens' <> tokens'') matcher)
                        , []
                        )
                      else (Resolved $ concatMap toChaosTokens steps, [])
                else
                  if (length steps + length tokens') <= n && tokenStrategy == ResolveChoice
                    then pure (Resolved $ concat tokens' <> concatMap toChaosTokens steps, [])
                    else do
                      let
                        groups = toGroups steps tokens'
                        chooseFunction :: Monad m => (a -> m Bool) -> [a] -> m Bool
                        chooseFunction = if tokenStrategy == ResolveChoice then anyM else allM
                        matcher' =
                          if tokenStrategy == ResolveChoice || null tokensThatCannotBeIgnored
                            then matcher
                            else matcher <> foldMap ChaosTokenFaceIsNot tokensThatCannotBeIgnored

                      matchedGroups <-
                        filterM
                          ( \(_, chosen) ->
                              chooseFunction (chooseFunction (\t -> lift $ matchChaosToken iid t matcher')) chosen
                          )
                          groups
                      let
                        groups' = if null matchedGroups then groups else matchedGroups
                        nFunc = if tokenStrategy == ResolveChoice then id else subtract 1

                      pure $
                        if tokenStrategy /= ResolveChoice && null matchedGroups
                          then (Resolved $ concatMap toChaosTokens steps, [])
                          else
                            ( Decided (ChooseMatch chooseSource n tokenStrategy steps tokens' matcher)
                            ,
                              [ chooseOne
                                  iid
                                  [ ChaosTokenGroupChoice
                                    source
                                    iid
                                    (ChooseMatch chooseSource (nFunc n) tokenStrategy remaining chosen matcher)
                                  | (remaining, chosen) <- groups'
                                  ]
                              ]
                            )
            else do
              (steps', msgs) <-
                resolveFirstChooseUnresolved
                  source
                  iid
                  strategy
                  steps
              pure (Decided $ ChooseMatch chooseSource n tokenStrategy steps' tokens' matcher, msgs)
    ChooseMatchChoice steps tokens' choices -> do
      if all isResolved steps
        then do
          modifiers' <- lift $ getModifiers iid
          let
            collectChaosTokens xs (CannotCancelOrIgnoreChaosToken t) = t : xs
            collectChaosTokens xs _ = xs
            tokensThatCannotBeIgnored = foldl' collectChaosTokens [] modifiers'
            allChaosTokens = concatMap toChaosTokens steps
            toStrategy = \case
              Draw -> ResolveChoice
              Choose _ _ st _ _ -> st
              ChooseMatch _ _ st _ _ _ -> st
              ChooseMatchChoice {} -> error "Do not nest these"
            isValidMatcher tokenStrategy matcher =
              let matcher' =
                    if tokenStrategy == ResolveChoice || null tokensThatCannotBeIgnored
                      then matcher
                      else matcher <> foldMap ChaosTokenFaceIsNot tokensThatCannotBeIgnored
              in  anyM (\t -> lift $ matchChaosToken iid t matcher') allChaosTokens
          choices' <- map snd <$> filterM (\(m, (_, s)) -> isValidMatcher (toStrategy s) m) choices
          let
            fixStep = \case
              Draw -> Draw
              Choose chooseSource n tokenStrategy _ _ -> Choose chooseSource n tokenStrategy steps []
              ChooseMatch chooseSource n tokenStrategy _ _ matcher -> ChooseMatch chooseSource n tokenStrategy steps [] matcher
              ChooseMatchChoice _ _ matchers -> ChooseMatchChoice steps [] matchers

          pure $
            ( Decided $ ChooseMatchChoice steps tokens' choices
            ,
              [ chooseOrRunOne
                  iid
                  [Label label [SetChaosBagChoice source iid (fixStep step')] | (label, step') <- choices']
              ]
            )
        else do
          (steps', msgs) <-
            resolveFirstChooseUnresolved
              source
              iid
              strategy
              steps
          pure (Decided $ ChooseMatchChoice steps' tokens' choices, msgs)

resolveFirstChooseUnresolved
  :: HasCallStack
  => Source
  -> InvestigatorId
  -> RequestedChaosTokenStrategy
  -> [ChaosBagStepState]
  -> StateT ChaosBag GameT ([ChaosBagStepState], [Message])
resolveFirstChooseUnresolved source iid strategy = \case
  [] -> pure ([], [])
  (Undecided s : _) -> error $ "should not be called with undecided: " <> show s
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
  -> RequestedChaosTokenStrategy
  -> (ChaosBagStepState -> ChaosBagStepState)
  -> ChaosBagStepState
  -> (ChaosBagStepState, [Message])
decideFirstUndecided source iid strategy f = \case
  Decided step -> (Decided step, [])
  Resolved tokens' -> (Resolved tokens', [])
  Undecided step -> case step of
    Draw ->
      ( f $ Undecided Draw
      ,
        [ CheckWindow
            [iid]
            [Window Timing.When (Window.WouldRevealChaosToken source iid)]
        , NextChaosBagStep source (Just iid) strategy
        ]
      )
    Choose chooseSource n tokenStrategy steps tokens' ->
      if any isUndecided steps
        then
          let
            (steps', msgs) =
              decideFirstChooseUndecided source iid strategy f steps
          in
            (Deciding $ Choose chooseSource n tokenStrategy steps' tokens', msgs)
        else (f $ Deciding (Choose chooseSource n tokenStrategy steps tokens'), [])
    ChooseMatch chooseSource n tokenStrategy steps tokens' matcher ->
      if any isUndecided steps
        then
          let
            (steps', msgs) =
              decideFirstChooseUndecided source iid strategy f steps
          in
            (Deciding $ ChooseMatch chooseSource n tokenStrategy steps' tokens' matcher, msgs)
        else (f $ Deciding (ChooseMatch chooseSource n tokenStrategy steps tokens' matcher), [])
    ChooseMatchChoice steps tokens' matchers ->
      if any isUndecided steps
        then
          let
            (steps', msgs) =
              decideFirstChooseUndecided source iid strategy f steps
          in
            (Deciding $ ChooseMatchChoice steps' tokens' matchers, msgs)
        else (f $ Deciding (ChooseMatchChoice steps tokens' matchers), [])
  Deciding step -> case step of
    Draw -> (f $ Deciding Draw, [NextChaosBagStep source (Just iid) strategy])
    Choose chooseSource n tokenStrategy steps tokens' ->
      if any isUndecided steps
        then
          let
            (steps', msgs) =
              decideFirstChooseUndecided source iid strategy f steps
          in
            (Deciding $ Choose chooseSource n tokenStrategy steps' tokens', msgs)
        else (f $ Deciding (Choose chooseSource n tokenStrategy steps tokens'), [])
    ChooseMatch chooseSource n tokenStrategy steps tokens' matcher ->
      if any isUndecided steps
        then
          let
            (steps', msgs) =
              decideFirstChooseUndecided source iid strategy f steps
          in
            (Deciding $ ChooseMatch chooseSource n tokenStrategy steps' tokens' matcher, msgs)
        else (f $ Deciding (ChooseMatch chooseSource n tokenStrategy steps tokens' matcher), [])
    ChooseMatchChoice steps tokens' matchers ->
      if any isUndecided steps
        then
          let
            (steps', msgs) =
              decideFirstChooseUndecided source iid strategy f steps
          in
            (Deciding $ ChooseMatchChoice steps' tokens' matchers, msgs)
        else (f $ Deciding (ChooseMatchChoice steps tokens' matchers), [])

decideFirstChooseUndecided
  :: Source
  -> InvestigatorId
  -> RequestedChaosTokenStrategy
  -> (ChaosBagStepState -> ChaosBagStepState)
  -> [ChaosBagStepState]
  -> ([ChaosBagStepState], [Message])
decideFirstChooseUndecided source iid strategy f = \case
  [] -> ([], [])
  (Decided step : rest) ->
    let (rest', msgs) = decideFirstChooseUndecided source iid strategy f rest
    in  (Decided step : rest', msgs)
  (Resolved tokens' : rest) ->
    let (rest', msgs) = decideFirstChooseUndecided source iid strategy f rest
    in  (Resolved tokens' : rest', msgs)
  (Undecided step : rest) ->
    let
      (step', msgs) =
        decideFirstUndecided source iid strategy f (Undecided step)
    in
      (step' : rest, msgs)
  (Deciding step : rest) ->
    let
      (step', msgs) =
        decideFirstUndecided source iid strategy f (Deciding step)
    in
      (step' : rest, msgs)

replaceDeciding :: ChaosBagStepState -> ChaosBagStepState -> ChaosBagStepState
replaceDeciding current replacement = case current of
  Deciding step -> case step of
    Draw -> replacement
    ChooseMatch chooseSource n tokenStrategy steps tokens' matcher ->
      Deciding $
        ChooseMatch chooseSource n tokenStrategy (replaceDecidingList steps replacement) tokens' matcher
    ChooseMatchChoice steps tokens' matchers ->
      Deciding $
        ChooseMatchChoice (replaceDecidingList steps replacement) tokens' matchers
    Choose chooseSource n tokenStrategy steps tokens' ->
      Deciding $ Choose chooseSource n tokenStrategy (replaceDecidingList steps replacement) tokens'
  _ -> error $ "should be impossible, seen: " <> show current

replaceDecidingList
  :: [ChaosBagStepState] -> ChaosBagStepState -> [ChaosBagStepState]
replaceDecidingList steps replacement = case steps of
  [] -> []
  (Deciding step : xs) -> case step of
    Draw -> replacement : xs
    ChooseMatch {} -> replaceDeciding (Deciding step) replacement : xs
    ChooseMatchChoice {} -> replaceDeciding (Deciding step) replacement : xs
    Choose {} -> replaceDeciding (Deciding step) replacement : xs
  (stepState : xs) -> stepState : replaceDecidingList xs replacement

replaceChooseMatchChoice :: ChaosBagStepState -> ChaosBagStepState -> ChaosBagStepState
replaceChooseMatchChoice current replacement = case current of
  Decided step -> case step of
    Draw -> Decided Draw
    Choose chooseSource n tokenStrategy steps tokens' ->
      Decided $
        Choose chooseSource n tokenStrategy (replaceChooseMatchChoiceList steps replacement) tokens'
    ChooseMatch chooseSource n tokenStrategy steps tokens' matcher ->
      Decided $
        ChooseMatch
          chooseSource
          n
          tokenStrategy
          (replaceChooseMatchChoiceList steps replacement)
          tokens'
          matcher
    ChooseMatchChoice steps tokens' matchers ->
      let candidate = replaceChooseMatchChoiceList steps replacement
      in  if candidate == steps
            then replacement
            else Decided $ ChooseMatchChoice candidate tokens' matchers
  _ -> current

replaceChooseMatchChoiceList
  :: [ChaosBagStepState] -> ChaosBagStepState -> [ChaosBagStepState]
replaceChooseMatchChoiceList steps replacement = case steps of
  [] -> []
  (Decided step : xs) -> case step of
    ChooseMatchChoice {} -> replaceChooseMatchChoice (Decided step) replacement : xs
    Draw -> Decided Draw : replaceChooseMatchChoiceList xs replacement
    ChooseMatch chooseSource n tokenStrategy steps' tokens' matcher ->
      let candidate = replaceChooseMatchChoiceList steps' replacement
      in  if candidate == steps'
            then Decided step : replaceChooseMatchChoiceList xs replacement
            else Decided (ChooseMatch chooseSource n tokenStrategy candidate tokens' matcher) : xs
    Choose chooseSource n tokenStrategy steps' tokens' ->
      let candidate = replaceChooseMatchChoiceList steps' replacement
      in  if candidate == steps'
            then Decided step : replaceChooseMatchChoiceList xs replacement
            else Decided (Choose chooseSource n tokenStrategy candidate tokens') : xs
  (stepState : xs) -> stepState : replaceChooseMatchChoiceList xs replacement

instance RunMessage ChaosBag where
  runMessage msg c@ChaosBag {..} = case msg of
    ForceChaosTokenDraw face -> do
      activeInvestigatorId <- getActiveInvestigatorId
      push $ StartSkillTest activeInvestigatorId
      pure $ c & forceDrawL ?~ face
    SetChaosTokens tokens' -> do
      tokens'' <- traverse createChaosToken tokens'
      pure $ c & chaosTokensL .~ tokens'' & setAsideChaosTokensL .~ mempty
    ResetChaosTokens _source ->
      pure $
        c
          & (chaosTokensL <>~ chaosBagSetAsideChaosTokens)
          & (setAsideChaosTokensL .~ mempty)
          & (choiceL .~ Nothing)
    RequestChaosTokens source miid revealStrategy strategy -> do
      push (RunBag source miid strategy)
      case revealStrategy of
        Reveal n -> case n of
          0 -> pure $ c & revealedChaosTokensL .~ []
          1 -> pure $ c & choiceL ?~ Undecided Draw & revealedChaosTokensL .~ []
          x ->
            pure $
              c
                & ( choiceL
                      ?~ Undecided (Choose source x ResolveChoice (replicate x (Undecided Draw)) [])
                  )
                & (revealedChaosTokensL .~ [])
        RevealAndChoose n m -> case n of
          0 -> error "should be more than 1"
          1 -> error "should be more than 1"
          x ->
            pure $
              c
                & ( choiceL
                      ?~ Undecided (Choose source m ResolveChoice (replicate x (Undecided Draw)) [])
                  )
                & (revealedChaosTokensL .~ [])
    RunBag source miid strategy -> case chaosBagChoice of
      Nothing -> error "unexpected"
      Just choice' ->
        if isUndecided choice'
          then do
            iid <- maybe getLeadInvestigatorId pure miid
            let
              (choice'', msgs) =
                decideFirstUndecided source iid strategy toDecided choice'
            push (RunBag source miid strategy)
            pushAll msgs
            pure $ c & choiceL ?~ choice''
          else
            c
              <$ pushAll [BeforeRevealChaosTokens, RunDrawFromBag source miid strategy]
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
          RunWindow _ [Window Timing.When (Window.WouldRevealChaosToken {})] -> True
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
          RunWindow _ [Window Timing.When (Window.WouldRevealChaosToken {})] -> True
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
        Resolved chaosTokenFaces' -> do
          checkWindowMsgs <- case miid of
            Nothing -> pure []
            Just iid ->
              pure
                <$> checkWindows
                  [ Window Timing.When (Window.RevealChaosToken iid token)
                  | token <- chaosTokenFaces'
                  ]
          pushAll
            ( FocusChaosTokens chaosTokenFaces'
                : checkWindowMsgs
                  <> [RequestedChaosTokens source miid chaosTokenFaces', UnfocusChaosTokens]
            )
          pure $ c & choiceL .~ Nothing
        _ -> do
          iid <- maybe getLeadInvestigatorId pure miid
          ((choice'', msgs), c') <-
            runStateT
              (resolveFirstUnresolved source iid strategy choice')
              c
          push (RunDrawFromBag source miid strategy)
          pushAll msgs
          pure $ c' & choiceL ?~ choice''
    ChooseChaosTokenGroups source iid groupChoice -> case chaosBagChoice of
      Nothing -> error "unexpected"
      Just choice' -> do
        let
          updatedChoice =
            replaceFirstChoice source iid SetAside groupChoice choice'
        pure $ c & choiceL ?~ updatedChoice
    RevealChaosToken _source _iid token ->
      -- TODO: we may need a map of source to tokens here
      pure $ c & revealedChaosTokensL %~ (token :)
    ReturnChaosTokens tokens' ->
      pure $
        c
          & (chaosTokensL %~ (<> tokens'))
          & (setAsideChaosTokensL %~ (\\ tokens'))
          & (choiceL .~ Nothing)
    AddChaosToken chaosTokenFace -> do
      token <- createChaosToken chaosTokenFace
      pure $ c & chaosTokensL %~ (token :)
    SealChaosToken token ->
      pure $
        c
          & chaosTokensL
          %~ filter (/= token)
          & setAsideChaosTokensL
          %~ filter (/= token)
          & revealedChaosTokensL
          %~ filter (/= token)
    UnsealChaosToken token -> pure $ c & chaosTokensL %~ (token :)
    _ -> pure c
