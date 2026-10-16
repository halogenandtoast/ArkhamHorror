{- | Shared rules for And Then There Were None.

Two things run through the whole scenario.

The /tally/: fifteen [[Method]] cards exist, three bearing each of the five
skill icons, and setup removes five of them face down, so the ten that reach
the table carry a spread nobody has seen. Act 1b convicts the [[Victim]] whose
own two icons match the fewest of those ten. Because the ten victims between
them print every unordered pair of the five icons, and because "{wild} icons on
Method and Victim cards do not count as {willpower}, {intellect}, {combat}, or
{agility} icons" (agenda 1a) leaves {wild} matching only itself, exactly one
pair is lowest and the verdict is never a tie. That same agenda clause is why
every comparison here is icon equality and nothing in the set ever asks the
engine whether a {wild} icon "counts as" another.

And the /offerings/: act 1b hangs up to four copies of Antediluvian Sigils on
the locations around the culprit, and every culprit's text reads the running
total across all of them. The sigils are the dial act 2 is played on.
-}
module Arkham.Homebrew.AndThenThereWereNone.Helpers where

import Arkham.Asset.Types (AssetAttrs, Field (AssetCard, AssetUses))
import Arkham.Card (CardCode, lookupCard, toCardDef, toCardId)
import Arkham.Card.CardDef (CardDef, cdCardCode, cdSkills)
import Arkham.Classes.HasGame
import Arkham.Classes.HasQueue (push)
import Arkham.Classes.Query
import Arkham.Distance (unDistance)
import Arkham.GameEnv (getDistance)
import Arkham.Helpers.Location (withLocationOf)
import Arkham.Helpers.SkillTest (getSkillTestSkillTypes, getSkillTestTargetedEnemy)
import Arkham.Homebrew.AndThenThereWereNone.CardDefs.Assets qualified as Assets
import Arkham.Homebrew.AndThenThereWereNone.CardDefs.Enemies qualified as Enemies
import Arkham.Homebrew.AndThenThereWereNone.Traits (pattern Culprit, pattern Method, pattern Victim)
import Arkham.I18n
import Arkham.Id
import Arkham.Matcher hiding (AssetCard)
import Arkham.Message (Message (Flipped))
import Arkham.Message.Lifted
import Arkham.Message.Lifted.Choose (chooseOrRunOneM, targets)
import Arkham.Message.Lifted.Placement (place)
import Arkham.Modifier (ModifierType (ScenarioModifier))
import Arkham.Placement (Placement (AttachedToEnemy))
import Arkham.Prelude
import Arkham.Projection
import Arkham.SkillType (SkillIcon (..))
import Arkham.Source (Sourceable, toSource)
import Arkham.Token qualified as Token
import Arkham.Trait (Trait (Geist))

-- * The cast

-- | The ten [[Victim]] story assets, in collector order. Setup deals two to each outer room.
victimDefs :: [CardDef]
victimDefs =
  [ Assets.anthonyMarston
  , Assets.edwardArmstrong
  , Assets.emilyBrent
  , Assets.ethelRogers
  , Assets.johnMacArthur
  , Assets.lawrenceWargrave
  , Assets.philipLombard
  , Assets.thomasRogers
  , Assets.veraClaythorne
  , Assets.williamBlore
  ]

{- | The fifteen murder weapons, in collector order.

Three carry each icon, which is what lets setup "separate them by skill icons
and make five piles" without the players learning which pile is which.
-}
methodDefs :: [CardDef]
methodDefs =
  [ Assets.blowpipe
  , Assets.chloralHydrateOverdose
  , Assets.crushedByDebris
  , Assets.cyanideInjection
  , Assets.drownedInTheSea
  , Assets.electrocution
  , Assets.fellFromAHeight
  , Assets.hatchet
  , Assets.improvisedItem
  , Assets.knife
  , Assets.poisonedDrink
  , Assets.ropeNoose
  , Assets.serviceRevolver
  , Assets.smotheredInSleep
  , Assets.straightRazor
  ]

{- | Each [[Victim]] story asset paired with the [[Culprit]] enemy on its back.

Act 1b flips exactly one of them, so a guest another card names by name is a
[[Victim]] asset for most of the scenario and a [[Culprit]] enemy after the
verdict. The two faces share a card id but not a card code, so the pairing is
listed here rather than read off the board.
-}
victimCulpritPairs :: [(CardDef, CardDef)]
victimCulpritPairs =
  [ (Assets.anthonyMarston, Enemies.anthonyMarstonDelusionsOfGrandeur)
  , (Assets.edwardArmstrong, Enemies.edwardArmstrongDreamingDeeply)
  , (Assets.emilyBrent, Enemies.emilyBrentInATangledWeb)
  , (Assets.ethelRogers, Enemies.ethelRogersMaidenNameMarsh)
  , (Assets.johnMacArthur, Enemies.johnMacArthurAmountedToNothing)
  , (Assets.lawrenceWargrave, Enemies.lawrenceWargraveManOfManyFaces)
  , (Assets.philipLombard, Enemies.philipLombardSnakeInTheGrass)
  , (Assets.thomasRogers, Enemies.thomasRogersColdAndDistant)
  , (Assets.veraClaythorne, Enemies.veraClaythorneDiscipliningTheYoung)
  , (Assets.williamBlore, Enemies.williamBloreInTheBusinessOfSecrets)
  ]

-- | The enemy back of one guest, keyed on card code so a def edit cannot break it.
culpritFor :: CardDef -> Maybe CardDef
culpritFor victimDef =
  lookup (cdCardCode victimDef) [(cdCardCode v, c) | (v, c) <- victimCulpritPairs]

{- | "Spawn - X's or Y's location" -- the five Restless Spirits each name two guests.

A guest stands wherever their card stands, whichever face is up, so both faces
are searched: a spirit drawn before act 1b finds only victims, one drawn after
finds the culprit where their victim used to be.
-}
guestsLocation :: [CardDef] -> LocationMatcher
guestsLocation = oneOf . concatMap faces
 where
  faces victimDef =
    LocationWithAsset (assetIs victimDef)
      : [LocationWithEnemy (enemyIs c) | c <- toList (culpritFor victimDef)]

-- | A [[Victim]] story asset, however many are still face up.
victimAsset :: AssetMatcher
victimAsset = AssetWithTrait Victim

-- | A revealed murder weapon. A facedown one is a card beneath a location, not an asset.
methodAsset :: AssetMatcher
methodAsset = AssetWithTrait Method

-- | The unmasked guest. At most one exists, and only from act 1b onward.
culpritEnemy :: EnemyMatcher
culpritEnemy = EnemyWithTrait Culprit

-- | Every copy of Antediluvian Sigils still in play.
antediluvianSigils :: AssetMatcher
antediluvianSigils = assetIs Assets.antediluvianSigils

-- * Methods and victims

-- | The [[Method]] cards attached to one [[Victim]].
methodsAttachedTo :: AssetId -> AssetMatcher
methodsAttachedTo vid = methodAsset <> assetAttachedToAsset vid

{- | "a Victim card at that location without a Method card attached" -- act 1a's
target, and the thing the end-of-scenario experience bonus counts the other way
round.
-}
victimsWithoutMethod :: HasGame m => LocationMatcher -> m [AssetId]
victimsWithoutMethod lm = do
  candidates <- select $ victimAsset <> AssetAt lm
  filterM (fmap not . selectAny . methodsAttachedTo) candidates

{- | "the furthest Victim asset without a Method card attached" -- Grim
Implications, measured from the investigator who drew it.

A list, because two rooms are often the same distance away and the tie is the
player's to settle.
-}
farthestVictimsWithoutMethod :: HasGame m => InvestigatorId -> m [AssetId]
farthestVictimsWithoutMethod iid = do
  candidates <- victimsWithoutMethod Anywhere
  if null candidates
    then pure []
    else do
      lids <-
        select
          $ FarthestLocationFromInvestigator (InvestigatorWithId iid)
          $ LocationWithAsset (oneOf $ map AssetWithId candidates)
      filterM (<=~> AssetAt (oneOf $ map LocationWithId lids)) candidates

-- | The skill icons printed on a card in play.
assetIcons :: HasGame m => AssetId -> m [SkillIcon]
assetIcons = fieldMap AssetCard (cdSkills . toCardDef)

-- | "the skill icons among all ten Method cards in play"
methodIconsInPlay :: HasGame m => m [SkillIcon]
methodIconsInPlay = concatMapM assetIcons =<< select methodAsset

{- | A victim's "total matching icons": how many of the tallied [[Method]] icons
match one of their own. {wild} matches {wild} and nothing else, so the ten
victims are scored on ten distinct pairs of the five icons.
-}
victimMatchingIcons :: HasGame m => [SkillIcon] -> AssetId -> m Int
victimMatchingIcons tally vid = do
  icons <- assetIcons vid
  pure $ count (`elem` icons) tally

{- | "The Victim story asset with the least total matching icons is the true
killer!"

A list because a tie is conceivable off the printed setup -- it is not, with
all ten victims and all ten methods in play, but a scenario resumed from an odd
state should ask rather than pick.
-}
trueKiller :: HasGame m => m [AssetId]
trueKiller = do
  tally <- methodIconsInPlay
  scored <- traverse (\vid -> (,vid) <$> victimMatchingIcons tally vid) =<< select victimAsset
  pure $ case sortOn fst scored of
    [] -> []
    (lowest, _) : _ -> [vid | (n, vid) <- scored, n == lowest]

{- | "If there is an icon on a Method card at your location that matches the
skill for this test" -- the {tablet} rider.

A [[Method]] rides the [[Victim]] it is attached to, so it is at that victim's
location and @AssetAt@ finds it.
-}
methodIconMatchesTestAt :: HasGame m => LocationId -> m Bool
methodIconMatchesTestAt lid = do
  icons <- concatMapM assetIcons =<< select (methodAsset <> AssetAt (LocationWithId lid))
  any ((`elem` icons) . SkillIcon) <$> getSkillTestSkillTypes

-- * Offerings

-- | How many offerings sit on one copy of Antediluvian Sigils.
offeringsOn :: HasGame m => AssetId -> m Int
offeringsOn = fieldMap AssetUses (findWithDefault 0 Token.Offering)

-- | "the total amount of offerings among all copies of Antediluvian Sigils"
totalOfferings :: HasGame m => m Int
totalOfferings = sum <$> (traverse offeringsOn =<< select antediluvianSigils)

-- | The offerings on each copy, for the culprits that compare the copies to each other.
offeringsPerSigil :: HasGame m => m [Int]
offeringsPerSigil = traverse offeringsOn =<< select antediluvianSigils

{- | "Add 1 offering to a copy of Antediluvian Sigils (without investigators at
its location, if possible)."

Anthony Marston and Lawrence Wargrave both place one this way. A sigil hangs on
a location, so "its location" is the location it is attached to, and the
preference is only a preference: with no empty one, any copy will do.
-}
addOfferingToSigil :: (ReverseQueue m, Sourceable source) => source -> InvestigatorId -> m ()
addOfferingToSigil source iid = do
  unoccupied <- select $ antediluvianSigils <> AssetAt LocationWithoutInvestigators
  candidates <- if null unoccupied then select antediluvianSigils else pure unoccupied
  chooseOrRunOneM iid $ targets candidates \aid -> placeTokens source aid Token.Offering 1

-- | Marks a copy of Antediluvian Sigils as having been chosen this round.
sigilChosenThisRound :: ModifierType
sigilChosenThisRound = ScenarioModifier "andThenThereWereNone.sigilChosenThisRound"

{- | "a copy of Antediluvian Sigils that has not been chosen this round"

Emily Brent and Thomas Rogers each walk the table in player order, and no copy
may serve twice in one round, so each choice marks its copy for the round. A
copy with no offering left is no candidate either: there is nothing to remove
from it.
-}
unchosenSigils :: HasGame m => m [AssetId]
unchosenSigils = do
  candidates <- select $ antediluvianSigils <> AssetWithoutModifier sigilChosenThisRound
  filterM (fmap (> 0) . offeringsOn) candidates

-- | "remove 1 offering from a copy of Antediluvian Sigils that has not been chosen this round"
removeOfferingFromSigil :: (ReverseQueue m, Sourceable source) => source -> AssetId -> m ()
removeOfferingFromSigil source aid = do
  removeTokens source aid Token.Offering 1
  roundModifier source aid sigilChosenThisRound

-- * Tests

-- | "if this is a test against a _Geist_ enemy" -- the {elderThing} rider.
isTestAgainstGeist :: HasGame m => m Bool
isTestAgainstGeist =
  getSkillTestTargetedEnemy >>= \case
    Nothing -> pure False
    Just eid -> eid <=~> EnemyWithTrait Geist

-- * Flipping a victim

{- | "Flip that story asset to its enemy side."

Each [[Victim]] owns its own back, exactly as Masque's guests do: the asset
creates its culprit where it stands and 'Flipped' takes the asset out of play,
so the two faces keep one card id. Any [[Method]] attached to the victim is the
same physical card turning over, so it rides along to the enemy -- where it is
no longer attached to a [[Victim]], which is what the experience bonus counts.
-}
flipToCulprit :: ReverseQueue m => CardDef -> AssetAttrs -> m ()
flipToCulprit culpritDef attrs = withLocationOf attrs \lid -> do
  let culprit = lookupCard culpritDef (toCardId attrs)
  eid <- createEnemyAt culprit lid
  attached <- select $ assetAttachedToAsset attrs.id
  for_ attached (`place` AttachedToEnemy eid)
  push $ Flipped (toSource attrs) culprit

-- * The accusation

{- | "a Victim card at that location without a Method card attached" as a
matcher, for act 1a's availability criteria. 'victimsWithoutMethod' is the same
question asked of one location.
-}
victimWithoutMethod :: AssetMatcher
victimWithoutMethod = victimAsset <> not_ (AssetWithAttachedAsset methodAsset)

{- | "the investigators may name the killer and advance" -- act 1's objective.

The accusation is kept in act 1's own meta as the named [[Victim]]'s card code,
so 1b can compare it with 'trueKiller' after the tally and the record survives
the flip that takes the victim's asset side out of play.
-}
namedKiller :: Value -> Maybe CardCode
namedKiller = toResultDefault Nothing

-- * Distance

{- | Every location matching @lm@ paired with its connection distance from
@from@, nearest first. @from@ itself is distance 0; a location it cannot reach
is left out.

Act 1b's "the 1{perPlayer} nearest locations to the Culprit enemy (including its
location)" reads off the front of this list.
-}
locationsByDistanceFrom
  :: HasGame m => LocationId -> LocationMatcher -> m [(Int, LocationId)]
locationsByDistanceFrom from lm = do
  candidates <- select lm
  ranked <- forMaybeM candidates \lid ->
    if lid == from
      then pure $ Just (0, lid)
      else fmap ((,lid) . unDistance) <$> getDistance from lid
  pure $ sortOn fst ranked

campaignI18n :: (HasI18n => a) -> a
campaignI18n a = withI18n $ scope "andThenThereWereNone" a

scenarioI18n :: (HasI18n => a) -> a
scenarioI18n a = campaignI18n $ scope "scenario" a
