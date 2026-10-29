{- | Shared keyword machinery. Jinxed is a presence condition, never a count.
Printed Jinx clauses stay on their own act/agenda as 'jinxAbility' entries.
-}
module Arkham.Homebrew.NightOfVespers.Jinx where

import Arkham.Ability
import Arkham.Asset.Types (Field (AssetCard))
import Arkham.Calculation (GameCalculation)
import Arkham.Card
import Arkham.Classes.HasGame
import Arkham.Classes.HasQueue (push)
import Arkham.Classes.Query
import Arkham.Helpers.Enemy (getModifiedKeywords)
import Arkham.Helpers.Modifiers hiding (skillTestModifier)
import Arkham.Helpers.Ref (sourceToTarget)
import Arkham.Helpers.SkillTest (getSkillTest, getSkillTestResultWithResultModifiers)
import Arkham.Id
import Arkham.Keyword (Keyword (..))
import Arkham.Location.Types (Field (LocationKeywords))
import Arkham.Matcher hiding (AssetCard)
import Arkham.Message (Message (..), pattern FailedSkillTest)
import Arkham.Message.Lifted
import Arkham.Prelude
import Arkham.Projection
import Arkham.SkillTestResult
import Arkham.SkillType (SkillType)
import Arkham.Source
import Arkham.Target
import Arkham.Treachery.Types (Field (TreacheryCard))
import Arkham.Window qualified as Window
import Data.Aeson.Types (parseMaybe)

pattern Jinxed :: Keyword
pattern Jinxed = CustomKeyword "nightOfVespers.jinxed"

jinxEvent, jinxResolvedEvent :: Text
jinxEvent = "nightOfVespers.jinx"
jinxResolvedEvent = "nightOfVespers.jinxResolved"

-- | De Castries' Grand Cipher can suppress either the act or the agenda alone.
ignoreJinx :: ModifierType
ignoreJinx = ScenarioModifier "nightOfVespers.ignoreJinx"

data JinxContext = JinxContext
  { investigator :: InvestigatorId
  , failedBy :: Int
  , failTests :: Bool
  , effect :: Maybe (Source, Int)
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

jinxWindow :: WindowMatcher
jinxWindow = ScenarioEvent #when (Just You) jinxEvent

{- | Jinx is keyword text, not a printed "Forced" ability. Period limits belong
on this entry (groupLimit PerRound/PerPhase), rather than on the keyword hook.
-}
jinxAbility :: (HasCardCode a, Sourceable a) => a -> Int -> Ability
jinxAbility a n = restricted a n (not_ $ SelfHasModifier ignoreJinx) $ silent jinxWindow

jinxContext :: [Window.Window] -> Maybe JinxContext
jinxContext =
  listToMaybe . mapMaybe \w -> case Window.windowType w of
    Window.ScenarioEvent name _ value
      | name == jinxEvent || name == jinxResolvedEvent -> parseMaybe parseJSON value
    _ -> Nothing

{- | Includes controlled assets and threat-area cards through their projected
location. Read modifiers as well as printed keywords: several scenario
effects grant Jinxed temporarily. RemoveKeyword wins over AddKeyword.
-}
hasJinxedCardAt :: HasGame m => LocationId -> m Bool
hasJinxedCardAt lid = do
  locationKeywords <- field LocationKeywords lid
  enemies <- select $ EnemyAt (LocationWithId lid)
  assets <- select $ AssetAt (LocationWithId lid)
  treacheries <- select $ TreacheryAt (LocationWithId lid)
  orM
    [ pure $ Jinxed `member` locationKeywords
    , anyM (fmap (member Jinxed) . getModifiedKeywords) enemies
    , anyM (\aid -> field AssetCard aid >>= effectiveJinxed (toTarget aid)) assets
    , anyM (\tid -> field TreacheryCard tid >>= effectiveJinxed (toTarget tid)) treacheries
    ]
 where
  effectiveJinxed target card = do
    mods <- foldMapM getModifiers [target, CardIdTarget (toCardId card)]
    pure $ effectiveKeyword Jinxed (cdKeywords $ toCardDef card) mods

effectiveKeyword :: Keyword -> Set Keyword -> [ModifierType] -> Bool
effectiveKeyword keyword printed mods =
  RemoveKeyword keyword
    `notElem` mods
    && (keyword `member` printed || AddKeyword keyword `elem` mods)

{- | Direct triggers (Favor, Haunting, costs, Sabbath) do not require a Jinxed
card or a failed fight/evade/investigate test. Preserve a parent failure's
margin for effects such as The Danse Macabre.
-}
triggerJinx :: ReverseQueue m => InvestigatorId -> m ()
triggerJinx iid = do
  result <- getSkillTestResultWithResultModifiers
  let n = case result of
        Just (FailedBy _ amount) -> amount
        _ -> 0
  triggerJinxWith $ JinxContext iid n False Nothing

triggerJinxWith :: ReverseQueue m => JinxContext -> m ()
triggerJinxWith context = do
  clauses <- select $ AbilityWindow jinxWindow
  active <- filterM (withoutModifiersForClause . abilitySource) clauses
  -- No current Jinx means no trigger, including no "after a Jinx" effects.
  unless (null active) do
    checkWindows
      [Window.mkWhen $ Window.ScenarioEvent jinxEvent (Just context.investigator) (toJSON context)]
 where
  withoutModifiersForClause = \case
    source@ActSource {} -> withoutModifiers (sourceToTarget source) [ignoreJinx]
    source@AgendaSource {} -> withoutModifiers (sourceToTarget source) [ignoreJinx]
    _ -> pure False

{- | Wrap each clause's body so Benevento/Bridge of Sighs/One For Sorrow can
observe the actual resolution, after all its nested tests and choices.
-}
resolveJinx :: (ReverseQueue m, Sourceable a) => a -> Int -> JinxContext -> m () -> m ()
resolveJinx source clauseIndex context body = do
  body
  let resolved = context {effect = Just (toSource source, clauseIndex)}
  checkWindows
    [ Window.mkAfter
        $ Window.ScenarioEvent jinxResolvedEvent (Just context.investigator) (toJSON resolved)
    ]

{- | Re-resolve the same effects (Benevento/Haunting), without re-paying the
clause's trigger limit or accidentally selecting a different current Jinx.
-}
repeatJinx :: ReverseQueue m => JinxContext -> m ()
repeatJinx context = for_ context.effect \(source, clauseIndex) ->
  push
    $ UseCardAbility
      context.investigator
      source
      clauseIndex
      [Window.mkWhen $ Window.ScenarioEvent jinxEvent (Just context.investigator) (toJSON context)]
      NoPayment

-- | Beckoning can make every test performed by its Jinx automatically fail.
beginJinxSkillTest
  :: (ReverseQueue m, Sourceable a, Targetable a)
  => JinxContext -> SkillTestId -> a -> SkillType -> GameCalculation -> m ()
beginJinxSkillTest context sid source skill difficulty = do
  when context.failTests
    $ skillTestModifier sid source sid SkillTestAutomaticallyFails
  beginSkillTest sid context.investigator source source skill difficulty

{- | Called by every scenario, including standalone play. The initiator's
single failure notification is settled at ST.7, before its consequences can
move the investigator or remove a Jinxed card. Other subscriber notifications,
result windows and test-end windows must not trigger the keyword again.
-}
runJinx :: ReverseQueue m => Message -> m ()
runJinx = \case
  When (FailedSkillTest iid action _ (Initiator _) _ n)
    | action `elem` [Just #fight, Just #evade, Just #investigate] -> do
        skillTest <- getSkillTest
        cancelled <- case skillTest of
          Nothing -> pure False
          Just st -> do
            ignored <- hasModifier st.id CancelEffects
            failureCannotBeCancelled <- hasModifier st.target EffectsCannotBeCanceled
            pure $ ignored && not failureCannotBeCancelled
        unless cancelled do
          selectOne (LocationWithInvestigator $ InvestigatorWithId iid) >>= traverse_ \lid ->
            whenM (hasJinxedCardAt lid) $ triggerJinxWith (JinxContext iid n False Nothing)
  _ -> pure ()
