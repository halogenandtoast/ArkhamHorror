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
import Arkham.Message
import Arkham.RequestedTokenStrategy
import Arkham.Source
import Arkham.Timing qualified as Timing
import Arkham.Token
import Arkham.Window ( Window (..) )
import Arkham.Window qualified as Window
import Control.Monad.State hiding ( filterM )

isUndecided :: ChaosBagStepState -> Bool
isUndecided (Undecided _) = True
isUndecided _ = False

isResolved :: ChaosBagStepState -> Bool
isResolved (Resolved _) = True
isResolved _ = False

toDecided :: ChaosBagStepState -> ChaosBagStepState
toDecided (Undecided x) = Decided x
toDecided other = other

toTokens :: ChaosBagStepState -> [Token]
toTokens (Resolved tokens') = tokens'
toTokens _ = []

toGroups
  :: [ChaosBagStepState] -> [[Token]] -> [([ChaosBagStepState], [[Token]])]
toGroups !steps !tokens' = go steps []
 where
  go [] _ = []
  go (step : rest) prev = case step of
    Resolved group' -> (prev <> rest, group' : tokens') : go rest (step : prev)
    _ -> error "must be resolved"

replaceFirstChoice
  :: Source
  -> InvestigatorId
  -> RequestedTokenStrategy
  -> ChaosBagStep
  -> ChaosBagStepState
  -> ChaosBagStepState
replaceFirstChoice source iid strategy replacement = \case
  Undecided _ -> error "should not be ran with undecided"
  Resolved tokens' -> Resolved tokens'
  Decided step -> case step of
    Draw -> Decided Draw
    Choose n steps tokens' -> if all isResolved steps
      then Decided replacement
      else Decided $ Choose
        n
        (replaceFirstChooseChoice source iid strategy replacement steps)
        tokens'
    ChooseMatch n steps tokens' matcher -> if all isResolved steps
      then Decided replacement
      else Decided $ ChooseMatch
        n
        (replaceFirstChooseChoice source iid strategy replacement steps)
        tokens'
        matcher

replaceFirstChooseChoice
  :: Source
  -> InvestigatorId
  -> RequestedTokenStrategy
  -> ChaosBagStep
  -> [ChaosBagStepState]
  -> [ChaosBagStepState]
replaceFirstChooseChoice source iid strategy replacement = \case
  [] -> []
  (Undecided _ : _) -> error "should not be ran with undecided"
  (Resolved tokens' : rest) ->
    Resolved tokens'
      : replaceFirstChooseChoice source iid strategy replacement rest
  (Decided step : rest) ->
    replaceFirstChoice source iid strategy replacement (Decided step) : rest

resolveFirstUnresolved
  :: Source
  -> InvestigatorId
  -> RequestedTokenStrategy
  -> ChaosBagStepState
  -> StateT ChaosBag GameT (ChaosBagStepState, [Message])
resolveFirstUnresolved source iid strategy = \case
  Undecided _ -> error "should not be ran with undecided"
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
    Choose n steps tokens' -> if length tokens' == n
      then pure (Resolved $ concat tokens', [])
      else if all isResolved steps
        then if length steps == n
          then pure (Resolved $ concatMap toTokens steps, [])
          else do
            pure
              ( Decided (Choose n steps tokens')
              , [ chooseOne
                    iid
                    [ ChooseTokenGroups source iid (Choose n remaining chosen)
                    | (remaining, chosen) <- toGroups steps tokens'
                    ]
                ]
              )
        else do
          (steps', msgs) <- resolveFirstChooseUnresolved
            source
            iid
            strategy
            steps
          pure (Decided $ Choose n steps' tokens', msgs)
    ChooseMatch n steps tokens' matcher -> if length tokens' == n
      then pure (Resolved $ concat tokens', [])
      else if all isResolved steps
        then if length steps == n
          then pure (Resolved $ concatMap toTokens steps, [])
          else do
            pure
              ( Decided (ChooseMatch n steps tokens' matcher)
              , [ chooseOne
                    iid
                    [ ChooseTokenGroups
                        source
                        iid
                        (ChooseMatch n remaining chosen matcher)
                    | (remaining, chosen) <- toGroups steps tokens'
                    ]
                ]
              )
        else do
          (steps', msgs) <- resolveFirstChooseUnresolved
            source
            iid
            strategy
            steps
          pure (Decided $ ChooseMatch n steps' tokens' matcher, msgs)

resolveFirstChooseUnresolved
  :: Source
  -> InvestigatorId
  -> RequestedTokenStrategy
  -> [ChaosBagStepState]
  -> StateT ChaosBag GameT ([ChaosBagStepState], [Message])
resolveFirstChooseUnresolved source iid strategy = \case
  [] -> pure ([], [])
  (Undecided _ : _) -> error "should not be called with undecided"
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
  -> Bool -- trigger window?
  -> ChaosBagStepState
  -> (ChaosBagStepState, [Message])
decideFirstUndecided source iid strategy f shouldDecide stepState = case traceShowId stepState of
  Decided step -> (Decided step, [])
  Resolved tokens' -> (Resolved tokens', [])
  Undecided step -> case step of
    Draw ->
      ( f $ Undecided Draw
      , [ CheckWindow
            [iid]
            [Window Timing.When (Window.WouldRevealChaosToken source iid)]
        | not shouldDecide
        ]
      <> [NextChaosBagStep source (Just iid) strategy (not shouldDecide)]
      )
    Choose n steps tokens' -> if any isUndecided steps
      then
        let
          (steps', msgs) =
            decideFirstChooseUndecided source iid strategy f shouldDecide steps
        in (Undecided $ Choose n steps' tokens', msgs)
      else (f $ Undecided (Choose n steps tokens'), [])
    ChooseMatch n steps tokens' matcher -> if any isUndecided steps
      then
        let
          (steps', msgs) =
            decideFirstChooseUndecided source iid strategy f shouldDecide steps
        in (Undecided $ ChooseMatch n steps' tokens' matcher, msgs)
      else (f $ Undecided (ChooseMatch n steps tokens' matcher), [])

decideFirstChooseUndecided
  :: Source
  -> InvestigatorId
  -> RequestedTokenStrategy
  -> (ChaosBagStepState -> ChaosBagStepState)
  -> Bool
  -> [ChaosBagStepState]
  -> ([ChaosBagStepState], [Message])
decideFirstChooseUndecided source iid strategy f shouldDecide = \case
  [] -> ([], [])
  (Decided step : rest) ->
    let
      (rest', msgs) =
        decideFirstChooseUndecided source iid strategy f shouldDecide rest
    in (Decided step : rest', msgs)
  (Resolved tokens' : rest) ->
    let
      (rest', msgs) =
        decideFirstChooseUndecided source iid strategy f shouldDecide rest
    in (Resolved tokens' : rest', msgs)
  (Undecided step : rest) ->
    let
      (step', msgs) =
        decideFirstUndecided source iid strategy f shouldDecide (Undecided step)
    in (step' : rest, msgs)

instance RunMessage ChaosBag where
  runMessage msg c@ChaosBag {..} = case msg of
    ForceTokenDraw face -> do
      leadInvestigatorIdL <- getLeadInvestigatorId -- TODO: active
      push $ StartSkillTest leadInvestigatorIdL
      pure $ c & forceDrawL ?~ face
    SetTokens tokens' -> do
      tokens'' <- traverse createToken tokens'
      pure $ c & tokensL .~ tokens'' & setAsideTokensL .~ mempty
    ResetTokens _source ->
      pure
        $ c
        & tokensL
        %~ (<> chaosBagSetAsideTokens)
        & setAsideTokensL
        .~ mempty
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
                ?~ Undecided (Choose x (replicate x (Undecided Draw)) [])
                )
              & (revealedTokensL .~ [])
        RevealAndChoose n m -> case n of
          0 -> error "should be more than 1"
          1 -> error "should be more than 1"
          x ->
            pure
              $ c
              & (choiceL
                ?~ Undecided (Choose m (replicate x (Undecided Draw)) [])
                )
              & (revealedTokensL .~ [])
    RunBag source miid strategy -> case traceShowId chaosBagChoice of
      Nothing -> error "unexpected"
      Just choice' -> if isUndecided choice'
        then do
          iid <- maybe getLeadInvestigatorId pure miid
          let
            (choice'', msgs) =
              decideFirstUndecided source iid strategy toDecided False choice'
          push (RunBag source miid strategy)
          pushAll msgs
          pure $ c & choiceL ?~ choice''
        else c
          <$ pushAll [BeforeRevealTokens, RunDrawFromBag source miid strategy]
    NextChaosBagStep source miid strategy shouldDecide ->
      case traceShowId chaosBagChoice of
        Nothing -> error "unexpected"
        Just choice' -> do
          iid <- maybe getLeadInvestigatorId pure miid
          let
            updateF = if shouldDecide then toDecided else id
            (updatedChoice, messages) = decideFirstUndecided
              source
              iid
              strategy
              updateF
              shouldDecide
              choice'
          unless (null messages) $ pushAll messages
          pure $ c & choiceL ?~ updatedChoice
    ReplaceCurrentDraw source iid step -> case chaosBagChoice of
      Nothing -> error "unexpected"
      Just (Decided Draw) -> do
        -- if we have already decided, we can't use the function, this means we
        -- have to replace directly
        let
          (updatedChoice, messages) =
            decideFirstUndecided source iid SetAside id False (Undecided step)
        unless (null messages) $ pushAll messages
        pure $ c & choiceL ?~ updatedChoice
      Just choice' -> do
        -- if we have not decided we can use const to replace
        let
          (updatedChoice, messages) = decideFirstUndecided
            source
            iid
            SetAside
            (const (Undecided step))
            True -- hmmm
            choice'
        unless (null messages) $ pushAll messages
        pure $ c & choiceL ?~ updatedChoice
    RunDrawFromBag source miid strategy -> case traceShowId chaosBagChoice of
      Nothing -> error "unexpected"
      Just choice' -> case choice' of
        Resolved tokenFaces' -> do
          checkWindowMsgs <- case miid of
            Nothing -> pure []
            Just iid -> pure <$> checkWindows
              [ Window Timing.When (Window.RevealToken iid token)
              | token <- tokenFaces'
              ]
          c <$ pushAll
            (FocusTokens tokenFaces'
            : checkWindowMsgs
            <> [RequestedTokens source miid tokenFaces', UnfocusTokens]
            )
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
      pure $ c & tokensL %~ (<> tokens') & setAsideTokensL %~ (\\ tokens')
    AddToken tokenFace -> do
      token <- createToken tokenFace
      pure $ c & tokensL %~ (token :)
    SealToken token -> do
      pure $ c & tokensL %~ filter (/= token)
    UnsealToken token -> do
      pure $ c & tokensL %~ (token :)
    _ -> pure c
