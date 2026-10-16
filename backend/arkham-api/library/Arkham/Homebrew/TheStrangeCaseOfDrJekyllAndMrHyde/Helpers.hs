{- | Shared rules for The Strange Case of Dr. Jekyll and Mr. Hyde.

Two things run through the whole scenario. Henry Jekyll and Edward Hyde are one
card: half the encounter set reads which face is showing, and the horror on Hyde
is what the investigators are really spending the scenario buying.

And the /arrangement/: act 1 lays the five set-aside duplicate locations out in
a line beneath the act deck, and act 2 and Adapted Formula read "the card in the
arrangement that matches your location" and count resources by position. The
cards are real locations, so they are placed at the labels @arrangement1@ ..
@arrangement5@ and the scenario isolates them -- "investigators cannot move to
or reveal these locations, but they are considered to be in play".
-}
module Arkham.Homebrew.TheStrangeCaseOfDrJekyllAndMrHyde.Helpers where

import Arkham.Ability
import Arkham.Asset.Types (Field (AssetCardId, AssetHorror, AssetLocation))
import Arkham.Card (lookupCard)
import Arkham.Classes.HasGame
import Arkham.Classes.HasQueue (push)
import Arkham.Classes.Query
import Arkham.Enemy.Types (Field (EnemyCardId, EnemyLocation, EnemyTokens))
import Arkham.Helpers.Ability (getAbility)
import Arkham.Helpers.SkillTest (getSkillTestAbilitySource)
import Arkham.Helpers.Source (sourceMatches)
import Arkham.Homebrew.TheStrangeCaseOfDrJekyllAndMrHyde.CardDefs.Assets qualified as Assets
import Arkham.Homebrew.TheStrangeCaseOfDrJekyllAndMrHyde.CardDefs.Enemies qualified as Enemies
import Arkham.I18n
import Arkham.Id
import Arkham.Label (Label (..))
import Arkham.Location.Types (Field (LocationName, LocationTokens))
import Arkham.Matcher
import Arkham.Message (Message (Flipped))
import Arkham.Message.Lifted
import Arkham.Message.Lifted.Placement
import Arkham.Prelude
import Arkham.Projection

-- 'Arkham.Matcher' has its own 'EncounterCardSource'; 'sourceMatches' wants that one.
import Arkham.Source hiding (EncounterCardSource)
import Arkham.Target
import Arkham.Token qualified as Token
import Arkham.Window qualified as Window

henryJekyll :: AssetMatcher
henryJekyll = assetIs Assets.henryJekyll

edwardHyde :: EnemyMatcher
edwardHyde = enemyIs Enemies.edwardHyde

-- | Hyde is only ever on the board while Jekyll's side is face down.
edwardHydeInPlay :: HasGame m => m Bool
edwardHydeInPlay = selectAny edwardHyde

-- * Flipping the Jekyll/Hyde card

{- The horror is kept as real tokens on whichever face is in play, the way the
table does it, rather than as a count in scenario meta: the player can see it,
and 'hydeHorror' is one read either way.

That is only safe because Henry Jekyll's def prints no sanity.
@AssetRemainingSanity@ answers 'Nothing' without one, so @Asset.Runner@'s
@defeated@ can never return @DefeatedByHorror@, and its @PlaceTokens@ arm only
asks for a defeat check when @isJust assetSanity@. Horror on the enemy face is
inert for the matching reason -- enemies are defeated by damage alone. If Henry
Jekyll ever gains printed sanity, this has to move to meta.

Both directions go through 'Flipped': the central handler in @Game/Runner.hs@
does @replaceCard@ plus @RemoveAsset@/@RemoveEnemy@, so the two faces keep one
card id. The asset-to-enemy half is
'Arkham.Asset.Assets.MaskedCarnevaleGoer_17'; the enemy-to-asset half is its
mirror and has no other precedent.
-}

-- | The face of the Jekyll/Hyde card currently on the table.
jekyllHydeTarget :: HasGame m => m (Maybe Target)
jekyllHydeTarget = do
  jekyll <- selectOne henryJekyll
  hyde <- selectOne edwardHyde
  pure $ (toTarget <$> jekyll) <|> (toTarget <$> hyde)

-- | The horror on the Jekyll/Hyde card, whichever face is showing. 0 if neither is in play.
hydeHorror :: HasGame m => m Int
hydeHorror = do
  onJekyll <- traverse (field AssetHorror) =<< selectOne henryJekyll
  onHyde <- traverse (fieldMap EnemyTokens (Token.countTokens Token.Horror)) =<< selectOne edwardHyde
  pure $ fromMaybe 0 (onJekyll <|> onHyde)

{- | "Add N horror to Edward Hyde", on whichever face is showing.

Resolves the face eagerly, so it must not be called in the same handler as a
flip: the flip is only queued, so this would read the face that is on its way
out and place the horror after that entity is gone. Defer it behind a 'doStep'.
-}
placeHorrorOnHyde :: (ReverseQueue m, Sourceable source) => source -> Int -> m ()
placeHorrorOnHyde source n = when (n > 0) do
  target <- jekyllHydeTarget
  for_ target \t -> placeTokens source t #horror n

-- | "Place horror on him until there is N horror on him" -- act 2b's top-up.
placeHorrorOnHydeUpTo :: (ReverseQueue m, Sourceable source) => source -> Int -> m ()
placeHorrorOnHydeUpTo source n = do
  current <- hydeHorror
  placeHorrorOnHyde source (n - current)

{- | A card attached to one face belongs to the other after the flip -- it is one
card turning over. Festering Evil is the only one in the set that attaches here.
-}
carryFlipAttachments :: (ReverseQueue m, IsPlacement p) => Target -> p -> m ()
carryFlipAttachments oldFace newFace = do
  attached <- select $ TreacheryIsAttachedTo oldFace
  for_ attached (`place` newFace)

{- | "Flip Henry Jekyll." The asset leaves, Edward Hyde arrives at the same
location, and the horror on the card comes with him.
-}
flipToHyde :: (ReverseQueue m, Sourceable source) => source -> m ()
flipToHyde source = runMaybeT_ do
  jekyll <- MaybeT $ selectOne henryJekyll
  lid <- MaybeT $ field AssetLocation jekyll
  horror <- lift $ field AssetHorror jekyll
  cardId <- lift $ field AssetCardId jekyll
  lift do
    let card = lookupCard Enemies.edwardHyde cardId
    hyde <- createEnemyAt card lid
    carryFlipAttachments (toTarget jekyll) (AttachedToEnemy hyde)
    placeTokens source hyde #horror horror
    push $ Flipped (toSource jekyll) card
    checkAfter $ Window.EnemyFlipped hyde

-- | "Flip Edward Hyde." The mirror of 'flipToHyde'; see this section's note.
flipToJekyll :: (ReverseQueue m, Sourceable source) => source -> m ()
flipToJekyll source = runMaybeT_ do
  hyde <- MaybeT $ selectOne edwardHyde
  lid <- MaybeT $ field EnemyLocation hyde
  horror <- lift $ fieldMap EnemyTokens (Token.countTokens Token.Horror) hyde
  cardId <- lift $ field EnemyCardId hyde
  lift do
    let card = lookupCard Assets.henryJekyll cardId
    jekyll <- createAssetAt card (AtLocation lid)
    carryFlipAttachments (toTarget hyde) (AttachedToAsset jekyll Nothing)
    placeTokens source jekyll #horror horror
    push $ Flipped (toSource hyde) card
    -- Raised after the removal, so "when the attached card flips" reactions see
    -- exactly one face. The departed enemy is still an entity at
    -- @OutOfPlay RemovedZone@ here, which is why a matcher for this window has
    -- to say 'IncludeOutOfPlayEnemy'.
    checkAfter $ Window.EnemyFlipped hyde

-- * The arrangement

-- | "Positions are numbered from left to right."
arrangementPositions :: [Int]
arrangementPositions = [1 .. 5]

arrangementLabel :: Int -> Text
arrangementLabel n = "arrangement" <> tshow n

-- | Every card in the arrangement, whatever its position.
inArrangement :: LocationMatcher
inArrangement = mapOneOf (LocationWithLabel . Label . arrangementLabel) arrangementPositions

-- | The card in the arrangement at a given position, if act 1 has laid it out.
arrangementAt :: HasGame m => Int -> m (Maybe LocationId)
arrangementAt = selectOne . LocationWithLabel . Label . arrangementLabel

-- | How many resources sit on the card at a position; 0 before act 1 advances.
arrangementResources :: HasGame m => Int -> m Int
arrangementResources n =
  arrangementAt n >>= \case
    Nothing -> pure 0
    Just lid -> fieldMap LocationTokens (Token.countTokens Token.Resource) lid

{- | "the card in the arrangement that matches your location". The arrangement
holds the other printing of a place already on the board, so the two match by
name.
-}
arrangementMatching :: HasGame m => LocationId -> m (Maybe LocationId)
arrangementMatching lid = do
  name <- field LocationName lid
  selectOne $ inArrangement <> LocationWithTitle name.title

-- * Scenario-card abilities

{- | "a test as part of an {action} or {fast} ability printed on a scenario
card" -- the chaos tokens, Servant of Yog-Sothoth and Supply and Demand all ask
some version of this.
-}
testIsFromScenarioAbility :: HasGame m => (Ability -> Bool) -> m Bool
testIsFromScenarioAbility isKind =
  getSkillTestAbilitySource >>= \case
    Just source@(AbilitySource inner idx) ->
      andM
        [ sourceMatches source EncounterCardSource
        , maybe False isKind <$> getAbility (AbilityRef inner idx)
        ]
    _ -> pure False

-- | An {action} or {fast} ability printed on a scenario card.
testIsFromScenarioActionOrFastAbility :: HasGame m => m Bool
testIsFromScenarioActionOrFastAbility =
  testIsFromScenarioAbility \a -> isActionAbility a || isFastAbility a

-- | An {action} ability printed on a scenario card.
testIsFromScenarioActionAbility :: HasGame m => m Bool
testIsFromScenarioActionAbility = testIsFromScenarioAbility isActionAbility

campaignI18n :: (HasI18n => a) -> a
campaignI18n a = withI18n $ scope "theStrangeCaseOfDrJekyllAndMrHyde" a

scenarioI18n :: (HasI18n => a) -> a
scenarioI18n a = campaignI18n $ scope "scenario" a
