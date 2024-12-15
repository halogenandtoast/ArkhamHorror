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
import Arkham.Classes.HasGame
import Arkham.Game.Helpers
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Helpers.Message
import Arkham.Id
import Arkham.Investigator.Types (Investigator)
import Arkham.Matcher (ChaosTokenMatcher (AnyChaosToken, ChaosTokenFaceIsNot))
import Arkham.Projection
import Arkham.RequestedChaosTokenStrategy
import Arkham.Source
import Arkham.Target
import Arkham.Timing qualified as Timing
import Arkham.Window (Window (..), mkAfter, mkWhen)
import Arkham.Window qualified as Window
import Control.Monad.State.Strict (StateT, execStateT, gets, modify', put, runStateT)

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
    Choose chooseSource n tokenStrategy steps tokens' nested ->
      if all isResolved steps
        then Decided replacement
        else
          Decided
            $ Choose
              chooseSource
              n
              tokenStrategy
              (replaceFirstChooseChoice source iid strategy replacement steps)
              tokens'
              nested
    ChooseMatch chooseSource n tokenStrategy steps tokens' matcher nested ->
      if all isResolved steps
        then Decided replacement
        else
          Decided
            $ ChooseMatch
              chooseSource
              n
              tokenStrategy
              (replaceFirstChooseChoice source iid strategy replacement steps)
              tokens'
              matcher
              nested
    ChooseMatchChoice steps tokens' matchers ->
      if all isResolved steps
        then Decided replacement
        else
          Decided
            $ ChooseMatchChoice
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
  :: (HasCallStack, HasGame m, MonadRandom m)
  => Source
  -> InvestigatorId
  -> RequestedChaosTokenStrategy
  -> ChaosBagStepState
  -> StateT ChaosBag m (ChaosBagStepState, [Message])
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
                    . (setAsideChaosTokensL %~ (<> drawn))
                )
              pure (Resolved drawn, [])
            Just drawn -> do
              let remaining = delete drawn bagChaosTokens
              modify'
                ( (forceDrawL .~ Nothing)
                    . (chaosTokensL .~ remaining)
                    . (setAsideChaosTokensL %~ (<> [drawn]))
                )
              pure (Resolved [drawn], [])
        Nothing -> do
          (drawn, remaining) <- splitAt 1 <$> shuffleM bagChaosTokens
          modify' ((chaosTokensL .~ remaining) . (setAsideChaosTokensL %~ (<> drawn)))
          pure (Resolved drawn, [])
    Choose chooseSource n tokenStrategy steps tokens' nested ->
      pure (Decided $ ChooseMatch chooseSource n tokenStrategy steps tokens' AnyChaosToken nested, [])
    ChooseMatch chooseSource 0 CancelChoice steps tokenGroups _ nested ->
      case nested of
        Nothing ->
          pure
            ( Resolved $ concatMap toChaosTokens steps
            , concatMap (map (ChaosTokenCanceled iid chooseSource)) tokenGroups
            )
        Just inner ->
          let
            replaceSteps sts = \case
              Choose cSource cAmount cTokenStrategy _ cTokens cNested -> Choose cSource cAmount cTokenStrategy sts cTokens cNested
              ChooseMatch cSource cAmount cTokenStrategy _ cTokens cMatcher cNested -> ChooseMatch cSource cAmount cTokenStrategy sts cTokens cMatcher cNested
              other -> other
           in
            pure
              ( Decided $ replaceSteps steps inner
              , concatMap (map (ChaosTokenCanceled iid chooseSource)) tokenGroups
              )
    ChooseMatch chooseSource 0 IgnoreChoice steps tokenGroups _ nested ->
      case nested of
        Nothing ->
          pure
            ( Resolved $ concatMap toChaosTokens steps
            , concatMap (map (ChaosTokenIgnored iid chooseSource)) tokenGroups
            )
        Just inner ->
          let
            replaceSteps sts = \case
              Choose cSource cAmount cTokenStrategy _ cTokens cNested -> Choose cSource cAmount cTokenStrategy sts cTokens cNested
              ChooseMatch cSource cAmount cTokenStrategy _ cTokens cMatcher cNested -> ChooseMatch cSource cAmount cTokenStrategy sts cTokens cMatcher cNested
              other -> other
           in
            pure
              ( Decided $ replaceSteps steps inner
              , concatMap (map (ChaosTokenIgnored iid chooseSource)) tokenGroups
              )
    ChooseMatch chooseSource n tokenStrategy steps tokens' matcher nested ->
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
                  pure
                    $ if tokenStrategy == ResolveChoice
                      then
                        ( Decided
                            ( ChooseMatch chooseSource n' tokenStrategy remainingSteps (tokens' <> tokens'') matcher nested
                            )
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

                      player <- lift $ getPlayer iid
                      pure
                        $ if tokenStrategy /= ResolveChoice && null matchedGroups
                          then (Resolved $ concatMap toChaosTokens steps, [])
                          else
                            ( Decided (ChooseMatch chooseSource n tokenStrategy steps tokens' matcher nested)
                            ,
                              [ chooseOne
                                  player
                                  [ ChaosTokenGroupChoice
                                    source
                                    iid
                                    (ChooseMatch chooseSource (nFunc n) tokenStrategy remaining chosen matcher nested)
                                  | (remaining, chosen) <- groups'
                                  ]
                              ]
                            )
            else do
              (steps', msgs) <- resolveFirstChooseUnresolved source iid strategy steps
              pure (Decided $ ChooseMatch chooseSource n tokenStrategy steps' tokens' matcher nested, msgs)
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
              Choose _ _ st _ _ _ -> st
              ChooseMatch _ _ st _ _ _ _ -> st
              ChooseMatchChoice {} -> error "Do not nest these"
            isValidMatcher tokenStrategy matcher =
              let matcher' =
                    if tokenStrategy == ResolveChoice || null tokensThatCannotBeIgnored
                      then matcher
                      else matcher <> foldMap ChaosTokenFaceIsNot tokensThatCannotBeIgnored
               in anyM (\t -> lift $ matchChaosToken iid t matcher') allChaosTokens
          choices' <-
            map snd <$> filterM (\(m, (_, s)) -> isValidMatcher (toStrategy s) m) choices
          let
            fixStep = \case
              Draw -> Draw
              Choose chooseSource n tokenStrategy _ _ nested -> Choose chooseSource n tokenStrategy steps [] nested
              ChooseMatch chooseSource n tokenStrategy _ _ matcher nested -> ChooseMatch chooseSource n tokenStrategy steps [] matcher nested
              ChooseMatchChoice _ _ matchers -> ChooseMatchChoice steps [] matchers

          player <- lift $ getPlayer iid

          pure
            $ if null choices'
              then (Resolved $ concatMap toChaosTokens steps <> concat tokens', [])
              else
                ( Decided $ ChooseMatchChoice steps tokens' choices
                ,
                  [ chooseOrRunOne
                      player
                      [Label label [SetChaosBagChoice source iid (fixStep step')] | (label, step') <- choices']
                  ]
                )
        else do
          (steps', msgs) <- resolveFirstChooseUnresolved source iid strategy steps
          pure (Decided $ ChooseMatchChoice steps' tokens' choices, msgs)

resolveFirstChooseUnresolved
  :: (HasCallStack, HasGame m, MonadRandom m)
  => Source
  -> InvestigatorId
  -> RequestedChaosTokenStrategy
  -> [ChaosBagStepState]
  -> StateT ChaosBag m ([ChaosBagStepState], [Message])
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
  -> [InvestigatorId]
  -> RequestedChaosTokenStrategy
  -> (ChaosBagStepState -> ChaosBagStepState)
  -> ChaosBagStepState
  -> (ChaosBagStepState, [Message])
decideFirstUndecided source iid iids strategy f = \case
  Decided step -> (Decided step, [])
  Resolved tokens' -> (Resolved tokens', [])
  Undecided step -> case step of
    Draw ->
      ( f $ Undecided Draw
      ,
        [ CheckWindows
            [mkWhen (Window.WouldRevealChaosToken source iid)]
        , NextChaosBagStep source (Just iid) strategy
        ]
      )
    Choose chooseSource n tokenStrategy steps tokens' nested ->
      if any isUndecided steps
        then
          let
            (steps', msgs) =
              decideFirstChooseUndecided source iid iids strategy f steps
           in
            (Deciding $ Choose chooseSource n tokenStrategy steps' tokens' nested, msgs)
        else (f $ Deciding (Choose chooseSource n tokenStrategy steps tokens' nested), [])
    ChooseMatch chooseSource n tokenStrategy steps tokens' matcher nested ->
      if any isUndecided steps
        then
          let
            (steps', msgs) =
              decideFirstChooseUndecided source iid iids strategy f steps
           in
            (Deciding $ ChooseMatch chooseSource n tokenStrategy steps' tokens' matcher nested, msgs)
        else (f $ Deciding (ChooseMatch chooseSource n tokenStrategy steps tokens' matcher nested), [])
    ChooseMatchChoice steps tokens' matchers ->
      if any isUndecided steps
        then
          let
            (steps', msgs) =
              decideFirstChooseUndecided source iid iids strategy f steps
           in
            (Deciding $ ChooseMatchChoice steps' tokens' matchers, msgs)
        else (f $ Deciding (ChooseMatchChoice steps tokens' matchers), [])
  Deciding step -> case step of
    Draw -> (f $ Deciding Draw, [NextChaosBagStep source (Just iid) strategy])
    Choose chooseSource n tokenStrategy steps tokens' nested ->
      if any isUndecided steps
        then
          let
            (steps', msgs) =
              decideFirstChooseUndecided source iid iids strategy f steps
           in
            (Deciding $ Choose chooseSource n tokenStrategy steps' tokens' nested, msgs)
        else (f $ Deciding (Choose chooseSource n tokenStrategy steps tokens' nested), [])
    ChooseMatch chooseSource n tokenStrategy steps tokens' matcher nested ->
      if any isUndecided steps
        then
          let
            (steps', msgs) =
              decideFirstChooseUndecided source iid iids strategy f steps
           in
            (Deciding $ ChooseMatch chooseSource n tokenStrategy steps' tokens' matcher nested, msgs)
        else (f $ Deciding (ChooseMatch chooseSource n tokenStrategy steps tokens' matcher nested), [])
    ChooseMatchChoice steps tokens' matchers ->
      if any isUndecided steps
        then
          let
            (steps', msgs) =
              decideFirstChooseUndecided source iid iids strategy f steps
           in
            (Deciding $ ChooseMatchChoice steps' tokens' matchers, msgs)
        else (f $ Deciding (ChooseMatchChoice steps tokens' matchers), [])

decideFirstChooseUndecided
  :: Source
  -> InvestigatorId
  -> [InvestigatorId]
  -> RequestedChaosTokenStrategy
  -> (ChaosBagStepState -> ChaosBagStepState)
  -> [ChaosBagStepState]
  -> ([ChaosBagStepState], [Message])
decideFirstChooseUndecided source iid iids strategy f = \case
  [] -> ([], [])
  (Decided step : rest) ->
    let (rest', msgs) = decideFirstChooseUndecided source iid iids strategy f rest
     in (Decided step : rest', msgs)
  (Resolved tokens' : rest) ->
    let (rest', msgs) = decideFirstChooseUndecided source iid iids strategy f rest
     in (Resolved tokens' : rest', msgs)
  (Undecided step : rest) ->
    let
      (step', msgs) =
        decideFirstUndecided source iid iids strategy f (Undecided step)
     in
      (step' : rest, msgs)
  (Deciding step : rest) ->
    let
      (step', msgs) =
        decideFirstUndecided source iid iids strategy f (Deciding step)
     in
      (step' : rest, msgs)

replaceDeciding :: ChaosBagStepState -> ChaosBagStepState -> ChaosBagStepState
replaceDeciding current replacement = case current of
  Deciding step -> case step of
    Draw -> replacement
    ChooseMatch chooseSource n tokenStrategy steps tokens' matcher nested ->
      Deciding
        $ ChooseMatch
          chooseSource
          n
          tokenStrategy
          (replaceDecidingList steps replacement)
          tokens'
          matcher
          nested
    ChooseMatchChoice steps tokens' matchers ->
      Deciding
        $ ChooseMatchChoice (replaceDecidingList steps replacement) tokens' matchers
    Choose chooseSource n tokenStrategy steps tokens' nested ->
      Deciding
        $ Choose chooseSource n tokenStrategy (replaceDecidingList steps replacement) tokens' nested
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
    Choose chooseSource n tokenStrategy steps tokens' nested ->
      Decided
        $ Choose chooseSource n tokenStrategy (replaceChooseMatchChoiceList steps replacement) tokens' nested
    ChooseMatch chooseSource n tokenStrategy steps tokens' matcher nested ->
      Decided
        $ ChooseMatch
          chooseSource
          n
          tokenStrategy
          (replaceChooseMatchChoiceList steps replacement)
          tokens'
          matcher
          nested
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
    ChooseMatchChoice {} -> replaceChooseMatchChoice (Decided step) replacement : xs
    Draw -> Decided Draw : replaceChooseMatchChoiceList xs replacement
    ChooseMatch chooseSource n tokenStrategy steps' tokens' matcher nested ->
      let candidate = replaceChooseMatchChoiceList steps' replacement
       in if candidate == steps'
            then Decided step : replaceChooseMatchChoiceList xs replacement
            else Decided (ChooseMatch chooseSource n tokenStrategy candidate tokens' matcher nested) : xs
    Choose chooseSource n tokenStrategy steps' tokens' nested ->
      let candidate = replaceChooseMatchChoiceList steps' replacement
       in if candidate == steps'
            then Decided step : replaceChooseMatchChoiceList xs replacement
            else Decided (Choose chooseSource n tokenStrategy candidate tokens' nested) : xs
  (stepState : xs) -> stepState : replaceChooseMatchChoiceList xs replacement

instance RunMessage ChaosBag where
  runMessage msg c@ChaosBag {..} = case msg of
    ForceChaosTokenDraw face -> do
      activeInvestigatorId <- getActiveInvestigatorId
      push $ StartSkillTest activeInvestigatorId
      pure $ c & forceDrawL ?~ face
    SetChaosTokens tokens' -> do
      tokens'' <- traverse createChaosToken tokens'
      blessTokens <- replicateM 10 $ createChaosToken #bless
      curseTokens <- replicateM 10 $ createChaosToken #curse
      frostTokens <- replicateM (8 - count (== #frost) tokens') $ createChaosToken #frost
      pure
        $ c
        & (chaosTokensL .~ tokens'')
        & (setAsideChaosTokensL .~ mempty)
        & (tokenPoolL .~ blessTokens <> curseTokens <> frostTokens)
    ReturnChaosTokensToPool tokensToPool -> do
      pure
        $ c
        & (chaosTokensL %~ filter (`notElem` tokensToPool))
        & (setAsideChaosTokensL %~ filter (`notElem` tokensToPool))
        & (tokenPoolL <>~ filter ((`elem` [#bless, #curse, #frost]) . (.face)) tokensToPool)
    PassSkillTest -> do
      removeAllMessagesMatching \case
        NextChaosBagStep {} -> True
        RunBag {} -> True
        _ -> False
      runMessage (ResetChaosTokens GameSource) c
    FailSkillTest -> do
      removeAllMessagesMatching \case
        NextChaosBagStep {} -> True
        RunBag {} -> True
        _ -> False
      runMessage (ResetChaosTokens GameSource) c
    ResetChaosTokens _source -> do
      returnAllBlessed <-
        getSkillTestId >>= \case
          Just sid -> hasModifier (SkillTestTarget sid) ReturnBlessedToChaosBag
          Nothing -> pure True

      returnAllCursed <-
        getSkillTestId >>= \case
          Just sid -> hasModifier (SkillTestTarget sid) ReturnCursedToChaosBag
          Nothing -> pure True

      -- TODO: We need to decide which tokens to keep, i.e. Blessed Blade (4)
      (tokensToReturn, tokensToPool) <- flip partitionM chaosBagSetAsideChaosTokens \token -> do
        if
          | token.face == #bless -> do
              if returnAllBlessed then pure True else hasModifier token ReturnBlessedToChaosBag
          | token.face == #curse -> do
              if returnAllCursed then pure True else hasModifier token ReturnCursedToChaosBag
          | token.face == #frost -> pure False
          | otherwise -> pure True

      let removeWindow ts = checkWindows [mkWhen $ Window.TokensWouldBeRemovedFromChaosBag ts]
      s <- flip execStateT True do
        when (notNull tokensToPool) do
          for_ tokensToPool \token -> do
            mods <- lift $ getModifiers token
            let iids = [iid | MayChooseToRemoveChaosToken iid <- mods]
            case iids of
              [] -> pure ()
              (iid : _) -> do
                put False
                lift do
                  player <- getPlayer iid
                  removeWindowMessage <- removeWindow [token]
                  pushAll
                    [ FocusChaosTokens [token]
                    , chooseOne
                        player
                        [ Label "Remove to Token Pool" [UnfocusChaosTokens, removeWindowMessage]
                        , Label "Return to Bag" [UnfocusChaosTokens, ReturnChaosTokens [token]]
                        ]
                    ]

      when (s && notNull tokensToPool) do
        push =<< removeWindow tokensToPool

      pure
        $ c
        & (chaosTokensL <>~ map (\token -> token {chaosTokenRevealedBy = Nothing}) tokensToReturn)
        & (setAsideChaosTokensL .~ mempty)
        & (tokenPoolL <>~ map (\token -> token {chaosTokenRevealedBy = Nothing}) tokensToPool)
        & (choiceL .~ Nothing)
    RequestChaosTokens source miid revealStrategy strategy -> do
      case revealStrategy of
        MultiReveal a b -> do
          pushAll [RequestChaosTokens source miid a strategy, RequestChaosTokens source miid b strategy]
          pure c
        Reveal n -> do
          push (RunBag source miid strategy)
          case n of
            0 -> pure $ c & revealedChaosTokensL .~ []
            1 -> pure $ c & choiceL ?~ Undecided Draw & revealedChaosTokensL .~ []
            x ->
              pure
                $ c
                & ( choiceL
                      ?~ Undecided (Choose source x ResolveChoice (replicate x (Undecided Draw)) [] Nothing)
                  )
                & (revealedChaosTokensL .~ [])
        RevealAndChoose n m -> do
          push (RunBag source miid strategy)
          case n of
            0 -> error "should be more than 1"
            1 -> error "should be more than 1"
            x ->
              pure
                $ c
                & ( choiceL
                      ?~ Undecided (Choose source m ResolveChoice (replicate x (Undecided Draw)) [] Nothing)
                  )
                & (revealedChaosTokensL .~ [])
    RunBag source miid strategy -> case chaosBagChoice of
      Nothing -> error "unexpected"
      Just choice' ->
        if isUndecided choice'
          then do
            iid <- maybe getLead pure miid
            iids <- getInvestigators
            let
              (choice'', msgs) =
                decideFirstUndecided source iid iids strategy toDecided choice'
            push (RunBag source miid strategy)
            pushAll msgs
            pure $ c & choiceL ?~ choice''
          else
            c
              <$ pushAll [BeforeRevealChaosTokens, RunDrawFromBag source miid strategy]
    NextChaosBagStep source miid strategy -> case chaosBagChoice of
      Nothing -> error "unexpected"
      Just choice' -> do
        iid <- maybe getLead pure miid
        iids <- getInvestigators
        let
          (updatedChoice, messages) =
            decideFirstUndecided source iid iids strategy toDecided choice'
        unless (null messages) $ pushAll messages
        pure $ c & choiceL ?~ updatedChoice
    SetChaosBagChoice _ _ step -> case chaosBagChoice of
      Nothing -> error "unexpected"
      Just choice' -> do
        -- When we replace we need to remove the original would reveal chaos
        -- token message as it will still be on the stack even though that
        -- token draw is gone
        removeAllMessagesMatching $ \case
          CheckWindows [Window Timing.When (Window.WouldRevealChaosToken {}) _] -> True
          Do (CheckWindows [Window Timing.When (Window.WouldRevealChaosToken {}) _]) -> True
          _ -> False

        let
          choice'' = replaceChooseMatchChoice choice' (Decided step)
        pure $ c & choiceL ?~ choice''
    ObtainChaosToken token -> do
      pure
        $ c
        & (setAsideChaosTokensL %~ filter (/= token))
        & (revealedChaosTokensL %~ filter (/= token))
        & (chaosTokensL %~ filter (/= token))
    ReplaceEntireDraw source iid step -> do
      removeAllMessagesMatching $ \case
        CheckWindows [Window Timing.When (Window.WouldRevealChaosToken {}) _] -> True
        Do (CheckWindows [Window Timing.When (Window.WouldRevealChaosToken {}) _]) -> True
        _ -> False

      iids <- getInvestigators
      -- if we have not decided we can use const to replace
      let
        choice'' = Undecided step
        (updatedChoice, messages) = decideFirstUndecided source iid iids SetAside toDecided choice''
      unless (null messages) $ pushAll messages
      pure $ c & choiceL ?~ updatedChoice
    ReplaceCurrentDraw source iid step -> case chaosBagChoice of
      Nothing -> error "unexpected"
      Just choice' -> do
        -- When we replace we need to remove the original would reveal chaos
        -- token message as it will still be on the stack even though that
        -- token draw is gone
        removeAllMessagesMatching $ \case
          CheckWindows [Window Timing.When (Window.WouldRevealChaosToken {}) _] -> True
          Do (CheckWindows [Window Timing.When (Window.WouldRevealChaosToken {}) _]) -> True
          _ -> False

        iids <- getInvestigators
        -- if we have not decided we can use const to replace
        let
          choice'' = replaceDeciding choice' (Undecided step)
          (updatedChoice, messages) =
            decideFirstUndecided source iid iids SetAside toDecided choice''
        unless (null messages) $ pushAll messages
        pure $ c & choiceL ?~ updatedChoice
    RunDrawFromBag source miid strategy -> case chaosBagChoice of
      Nothing -> error "unexpected"
      Just choice' -> case choice' of
        Resolved tokens -> do
          let tokens' = map (\token -> token {chaosTokenRevealedBy = miid}) tokens
          checkWindowMsgs <- case miid of
            Nothing -> pure []
            Just iid ->
              (\x y -> [x, y])
                <$> checkWindows
                  [ mkWhen (Window.RevealChaosToken iid token)
                  | token <- tokens'
                  ]
                <*> checkWindows
                  [ mkAfter (Window.RevealChaosToken iid token)
                  | token <- tokens'
                  ]
          for_ miid $ \iid -> do
            investigator <- getAttrs @Investigator iid
            send
              $ format investigator
              <> " draws "
              <> formatAsSentence tokens'
              <> " chaos "
              <> (if length tokens' == 1 then "token" else "tokens")

          -- the skill test handles revealing its own tokens so we only reveal
          -- here if the source was something else and we have an investigator
          -- to reveal "to"
          let
            revealF =
              case (source, miid) of
                (SkillTestSource _, _) -> const []
                (_, Nothing) -> const []
                (_, Just iid) -> map (RevealChaosToken source iid)

          pushAll
            ( FocusChaosTokens tokens'
                : checkWindowMsgs
                  <> revealF tokens'
                  <> [RequestedChaosTokens source miid tokens', UnfocusChaosTokens]
            )
          pure $ c & choiceL .~ Nothing
        _ -> do
          iid <- maybe getLead pure miid
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
    RevealChaosToken SkillTestSource {} _ token ->
      pure
        $ c
        & setAsideChaosTokensL
        %~ (nub . (<> [token]))
        & revealedChaosTokensL
        %~ (nub . (<> [token]))
        & chaosTokensL
        %~ filter (/= token)
    RevealChaosToken _source _iid token ->
      -- TODO: we may need a map of source to tokens here
      pure $ c & revealedChaosTokensL %~ (<> [token])
    ReturnChaosTokens tokens' ->
      pure
        $ c
        & (chaosTokensL %~ (<> tokens') . filter (`notElem` tokens'))
        & (setAsideChaosTokensL %~ (\\ tokens'))
        & (choiceL .~ Nothing)
        & (tokenPoolL %~ (\\ tokens'))
    AddChaosToken chaosTokenFace -> do
      token <- case chaosTokenFace of
        BlessToken -> pure $ fromMaybe (error "no more bless tokens") $ find ((== #bless) . (.face)) chaosBagTokenPool
        CurseToken -> pure $ fromMaybe (error "no more curse tokens") $ find ((== #curse) . (.face)) chaosBagTokenPool
        FrostToken -> pure $ fromMaybe (error "no more frost tokens") $ find ((== #frost) . (.face)) chaosBagTokenPool
        _ -> createChaosToken chaosTokenFace
      pure $ c & chaosTokensL %~ (token :) & tokenPoolL %~ delete token
    SwapChaosToken originalFace newFace -> do
      let
        replaceToken [] = []
        replaceToken (token : rest) | chaosTokenFace token == originalFace = token {chaosTokenFace = newFace} : rest
        replaceToken (token : rest) = token : replaceToken rest

      pure $ c & chaosTokensL %~ replaceToken
    SealChaosToken token ->
      pure
        $ c
        & chaosTokensL
        %~ filter (/= token)
        & setAsideChaosTokensL
        %~ filter (/= token)
        & revealedChaosTokensL
        %~ filter (/= token)
    SetChaosTokenAside token -> do
      pure $ c & setAsideChaosTokensL %~ (<> [token])
    UnsealChaosToken token -> do
      pure
        $ c
        & chaosTokensL
        %~ (token :)
        & setAsideChaosTokensL
        %~ filter (/= token)
        & revealedChaosTokensL
        %~ filter (/= token)
    RemoveAllChaosTokens face ->
      pure
        $ c
        & chaosTokensL
        %~ filter ((/= face) . chaosTokenFace)
        & setAsideChaosTokensL
        %~ filter ((/= face) . chaosTokenFace)
        & revealedChaosTokensL
        %~ filter ((/= face) . chaosTokenFace)
    _ -> pure c
