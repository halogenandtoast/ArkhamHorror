module Arkham.Types.ChaosBag
  ( ChaosBag
  , emptyChaosBag
  , tokensL
  )
where

import ClassyPrelude

import Arkham.Json
import Arkham.Types.ChaosBagStepState
import Arkham.Types.Classes
import Arkham.Types.InvestigatorId
import Arkham.Types.Message
import Arkham.Types.RequestedTokenStrategy
import Arkham.Types.Source
import Arkham.Types.Token
import Arkham.Types.Window
import Control.Lens hiding (chosen)
import Control.Monad.State
import qualified Data.List as L
import System.Random.Shuffle

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
toGroups steps tokens' = go steps []
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
  :: (MonadIO m, MonadState ChaosBag m)
  => Source
  -> InvestigatorId
  -> RequestedTokenStrategy
  -> ChaosBagStepState
  -> m (ChaosBagStepState, [Message])
resolveFirstUnresolved source iid strategy = \case
  Undecided _ -> error "should not be ran with undecided"
  Resolved tokens' -> pure (Resolved tokens', [])
  Decided step -> case step of
    Draw -> do
      bagTokens <- gets chaosBagTokens
      (drawn, remaining) <- splitAt 1 <$> liftIO (shuffleM bagTokens)
      modify' ((tokensL .~ remaining) . (setAsideTokensL %~ (drawn <>)))
      pure (Resolved drawn, [])
    Choose n steps tokens' -> if length tokens' == n
      then pure (Resolved $ concat tokens', [])
      else if all isResolved steps
        then if length steps == n
          then pure (Resolved $ concatMap toTokens steps, [])
          else pure
            ( Decided (Choose n steps tokens')
            , [ Ask
                  iid
                  (ChooseOne
                    [ ChooseTokenGroups source iid (Choose n remaining chosen)
                    | (remaining, chosen) <- toGroups steps tokens'
                    ]
                  )
              ]
            )
        else do
          (steps', msgs) <- resolveFirstChooseUnresolved
            source
            iid
            strategy
            steps
          pure (Decided $ Choose n steps' tokens', msgs)

resolveFirstChooseUnresolved
  :: (MonadIO m, MonadState ChaosBag m)
  => Source
  -> InvestigatorId
  -> RequestedTokenStrategy
  -> [ChaosBagStepState]
  -> m ([ChaosBagStepState], [Message])
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
  -> ChaosBagStepState
  -> (ChaosBagStepState, [Message])
decideFirstUndecided source iid strategy f = \case
  Decided step -> (Decided step, [])
  Resolved tokens' -> (Resolved tokens', [])
  Undecided step -> case step of
    Draw ->
      ( f $ Undecided Draw
      , [ CheckWindow iid [WhenWouldRevealChaosToken source You]
        , NextChaosBagStep source iid strategy
        ]
      )
    Choose n steps tokens' -> if any isUndecided steps
      then
        let
          (steps', msgs) =
            decideFirstChooseUndecided source iid strategy f steps
        in (Undecided $ Choose n steps' tokens', msgs)
      else (f $ Undecided (Choose n steps tokens'), [])

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

data ChaosBag = ChaosBag
  { chaosBagTokens :: [Token]
  , chaosBagSetAsideTokens :: [Token]
  , chaosBagRevealedTokens :: [Token]
  , chaosBagChoice :: Maybe ChaosBagStepState
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON ChaosBag where
  toJSON = genericToJSON $ aesonOptions $ Just "chaosBag"
  toEncoding = genericToEncoding $ aesonOptions $ Just "chaosBag"

instance FromJSON ChaosBag where
  parseJSON = genericParseJSON $ aesonOptions $ Just "chaosBag"

emptyChaosBag :: ChaosBag
emptyChaosBag = ChaosBag
  { chaosBagTokens = []
  , chaosBagSetAsideTokens = []
  , chaosBagRevealedTokens = []
  , chaosBagChoice = Nothing
  }

tokensL :: Lens' ChaosBag [Token]
tokensL = lens chaosBagTokens $ \m x -> m { chaosBagTokens = x }

setAsideTokensL :: Lens' ChaosBag [Token]
setAsideTokensL =
  lens chaosBagSetAsideTokens $ \m x -> m { chaosBagSetAsideTokens = x }

revealedTokensL :: Lens' ChaosBag [Token]
revealedTokensL =
  lens chaosBagRevealedTokens $ \m x -> m { chaosBagRevealedTokens = x }

choiceL :: Lens' ChaosBag (Maybe ChaosBagStepState)
choiceL = lens chaosBagChoice $ \m x -> m { chaosBagChoice = x }

instance HasList Token env ChaosBag where
  getList = pure . chaosBagTokens

instance HasQueue env => RunMessage env ChaosBag where
  runMessage msg c@ChaosBag {..} = case msg of
    SetTokens tokens' ->
      pure $ c & tokensL .~ tokens' & setAsideTokensL .~ mempty
    ResetTokens _source ->
      pure
        $ c
        & tokensL
        %~ (<> chaosBagSetAsideTokens)
        & setAsideTokensL
        .~ mempty
    RequestTokens source iid n strategy -> do
      unshiftMessage (RunBag source iid strategy)
      case n of
        0 -> pure $ c & revealedTokensL .~ []
        1 -> pure $ c & choiceL ?~ Undecided Draw & revealedTokensL .~ []
        x ->
          pure
            $ c
            & (choiceL ?~ Undecided (Choose x (replicate x (Undecided Draw)) [])
              )
            & (revealedTokensL .~ [])
    RunBag source iid strategy -> do
      case chaosBagChoice of
        Nothing -> error "unexpected"
        Just choice' -> do
          if isUndecided choice'
            then do
              let
                (choice'', msgs) =
                  decideFirstUndecided source iid strategy id choice'
              unshiftMessage (RunBag source iid strategy)
              unshiftMessages msgs
              pure $ c & choiceL ?~ choice''
            else c <$ unshiftMessage (RunDrawFromBag source iid strategy)
    NextChaosBagStep source iid strategy -> do
      case chaosBagChoice of
        Nothing -> error "unexpected"
        Just choice' -> do
          let
            (updatedChoice, messages) =
              decideFirstUndecided source iid strategy toDecided choice'
          unless (null messages) $ unshiftMessages messages
          pure $ c & choiceL ?~ updatedChoice
    ReplaceCurrentDraw source iid step -> do
      case chaosBagChoice of
        Nothing -> error "unexpected"
        Just choice' -> do
          let
            (updatedChoice, messages) = decideFirstUndecided
              source
              iid
              SetAside
              (const (Undecided step))
              choice'
          unless (null messages) $ unshiftMessages messages
          pure $ c & choiceL ?~ updatedChoice
    RunDrawFromBag source iid strategy -> do
      case chaosBagChoice of
        Nothing -> error "unexpected"
        Just choice' -> do
          case choice' of
            Resolved tokenFaces' ->
              c <$ unshiftMessage (RequestedTokens source iid tokenFaces')
            _ -> do
              ((choice'', msgs), c') <- runStateT
                (resolveFirstUnresolved source iid strategy choice')
                c
              unshiftMessage (RunDrawFromBag source iid strategy)
              unshiftMessages msgs
              pure $ c' & choiceL ?~ choice''
    ChooseTokenGroups source iid groupChoice -> do
      case chaosBagChoice of
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
      pure $ c & tokensL %~ (<> tokens') & setAsideTokensL %~ (L.\\ tokens')
    AddToken token -> pure $ c & tokensL %~ (token :)
    _ -> pure c
