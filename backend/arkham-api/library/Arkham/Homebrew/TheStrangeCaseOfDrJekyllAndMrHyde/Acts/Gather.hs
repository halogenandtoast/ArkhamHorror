{- | The gathering ability, printed word for word on act 2 and on Adapted Formula.

"{action} Test any skill (3). This test gets +1{perPlayer} difficulty.
Investigators at your location may spend any amount of clues to give +2 skill
value for each clue spent. If you succeed, place 1 resource on the card in the
arrangement that matches your location."

Two cards print it, so it lives here once. The clue spend belongs to the ability
rather than being one of its own, and it happens with the test already under
way, so it is a 'SilentForcedAbility' on that ability's 'InitiatedSkillTest' --
the shape Under Lock and Key uses for the same sentence.

Nothing printed limits where the action may be taken, so an investigator
standing where no card in the arrangement matches still tests and simply places
nothing on a success.
-}
module Arkham.Homebrew.TheStrangeCaseOfDrJekyllAndMrHyde.Acts.Gather (
  arrangementWith,
  askCluesToSpend,
  beginGatherTest,
  gatherAbility,
  gatherClueAbility,
  gatherDifficulty,
  placeGatheredResource,
  spendCluesForSkillValue,
) where

import Arkham.Ability
import Arkham.Calculation (GameCalculation (Fixed))
import Arkham.Capability
import Arkham.Card.CardCode (HasCardCode)
import Arkham.Classes.HasModifiersFor (HasModifiersM)
import Arkham.GameValue (GameValue (Static))
import Arkham.Helpers.Cost (getSpendableClueCountOf)
import Arkham.Helpers.GameValue (perPlayer)
import Arkham.Helpers.Location (getLocationOf)
import Arkham.Helpers.Modifiers (ModifierType (..), maybeModified_)
import Arkham.Helpers.SkillTest (getSkillTest, withSkillTest)
import Arkham.Homebrew.TheStrangeCaseOfDrJekyllAndMrHyde.Helpers
import Arkham.I18n
import Arkham.Id
import Arkham.Label (Label (Label))
import Arkham.Matcher
import Arkham.Message.Lifted
import Arkham.Message.Lifted.Choose (chooseBeginSkillTest)
import Arkham.Prelude
import Arkham.Source
import Arkham.Target
import Arkham.Token qualified as Token

-- | The card at one position in the arrangement, with that many resources on it.
arrangementWith :: Int -> ValueMatcher -> LocationMatcher
arrangementWith n v = LocationWithLabel (Label $ arrangementLabel n) <> LocationWithResources v

-- | "{action} Test any skill (3)."
gatherAbility :: (HasCardCode a, Sourceable a) => Text -> a -> Int -> Ability
gatherAbility tip a n =
  scenarioI18n $ withI18nTooltip tip $ skillTestAbility $ mkAbility a n actionAbility

{- | "Investigators at your location may spend any amount of clues." Silent: it is
part of the printed ability it rides on, and resolves with no prompt of its own.
-}
gatherClueAbility :: (HasCardCode a, Sourceable a) => a -> Int -> Int -> Ability
gatherClueAbility a n gather =
  restricted a n (exists $ InvestigatorAt YourLocation <> can.spend.clues)
    $ SilentForcedAbility
    $ InitiatedSkillTest #after You AnySkillType AnySkillTestValue
    $ SkillTestSourceMatches
    $ SourceIs
    $ toAbilitySource a gather

-- | "This test gets +1{perPlayer} difficulty."
gatherDifficulty :: (HasModifiersM m, Sourceable a) => a -> Int -> m ()
gatherDifficulty a n = whenJustM getSkillTest \st ->
  maybeModified_ a (SkillTestTarget st.id) do
    guard $ isAbilitySource a n st.source
    extra <- lift $ perPlayer 1
    pure [Difficulty extra]

-- | "Test any skill": the investigator picks which.
beginGatherTest
  :: (ReverseQueue m, Sourceable source, Targetable target)
  => source -> target -> InvestigatorId -> m ()
beginGatherTest source target iid = do
  sid <- getRandom
  chooseBeginSkillTest sid iid source target [#willpower, #intellect, #combat, #agility] (Fixed 3)

-- | How many clues the investigators at that location are willing to put in.
askCluesToSpend :: (ReverseQueue m, Targetable target) => Text -> target -> InvestigatorId -> m ()
askCluesToSpend cardScope target iid = do
  clues <- getSpendableClueCountOf $ InvestigatorAt (locationWithInvestigator iid)
  when (clues > 0)
    $ scenarioI18n
    $ scope cardScope
    $ countVar clues
    $ chooseAmount iid "cluesToSpend" "$clues" 0 clues target

-- | "+2 skill value for each clue spent", paid as a group from that location.
spendCluesForSkillValue
  :: (ReverseQueue m, Sourceable source) => source -> InvestigatorId -> Int -> m ()
spendCluesForSkillValue source iid n = when (n > 0) do
  withCost iid (GroupClueCost (Static n) (locationWithInvestigator iid))
    $ withSkillTest \sid -> skillTestModifiers sid source iid [AnySkillValue (2 * n)]

-- | "place 1 resource on the card in the arrangement that matches your location"
placeGatheredResource :: (ReverseQueue m, Sourceable source) => source -> InvestigatorId -> m ()
placeGatheredResource source iid = runMaybeT_ do
  lid <- MaybeT $ getLocationOf iid
  match <- MaybeT $ arrangementMatching lid
  lift $ placeTokens source match Token.Resource 1
