module Arkham.Homebrew.AgesUnwound.Treacheries.Exposition (exposition) where

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Helpers.SkillTest (getSkillTestTarget, getSkillTestTargetedLocation, withSkillTest)
import Arkham.Helpers.Story (readStory)
import Arkham.Homebrew.AgesUnwound.CardDefs.Locations qualified as Locations
import Arkham.Homebrew.AgesUnwound.CardDefs.Stories qualified as Stories
import Arkham.Homebrew.AgesUnwound.CardDefs.Treacheries qualified as Cards
import Arkham.Matcher
import Arkham.Modifier
import Arkham.Treachery.Import.Lifted
import Arkham.Treachery.Types (treacheryResources)

newtype Exposition = Exposition TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

exposition :: TreacheryCard Exposition
exposition = treachery Exposition Cards.exposition

-- | The three cities /Higher Powers/ seeds with a resource.
myriadOperations :: LocationMatcher
myriadOperations =
  mapOneOf locationIs [Locations.mexicoCity, Locations.istanbul, Locations.shanghai]

{- | "[reaction] When you successfully investigate Mexico City, Istanbul or
Shanghai: Instead of discovering clues, move 1 resource from that location to
this card. Investigators at any location may trigger this ability. /
[action] If there are 3 resources on this card: Flip this card over and resolve
its text."

Ability 1 deliberately carries no location criterion -- the card spells out that
it reaches past San Francisco, where it is attached. Ability 2 says nothing of
the sort, so it stays on-location.
-}
instance HasAbilities Exposition where
  getAbilities (Exposition a) =
    [ mkAbility a 1
        $ freeReaction
        $ SkillTestResult #when You (WhileInvestigating myriadOperations) #success
    , restricted a 2 (if treacheryResources a >= 3 then OnSameLocation else Never) actionAbility
    ]

{- | "Instead of discovering clues" is 'AlternateSuccessfullInvestigation' aimed
at this treachery: the successful investigation resolves against the card
instead of the location, so no clue is discovered and the @Successful@ message
arrives here. Same shape as /Prismatic Phenomenon/, which is also a treachery
replacing someone else's discovery.
-}
instance RunMessage Exposition where
  runMessage msg t@(Exposition attrs) = runQueueT $ case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      whenJustM getSkillTestTarget \target ->
        withSkillTest \sid ->
          for_ (individualInvestigationTargets target) \target' ->
            skillTestModifier
              sid
              (attrs.ability 1)
              target'
              (AlternateSuccessfullInvestigation $ toTarget attrs)
      pure t
    Successful (Action.Investigate, _) _iid _ (isTarget attrs -> True) _ -> do
      whenJustM getSkillTestTargetedLocation \lid ->
        moveTokens (attrs.ability 1) lid attrs #resource 1
      pure t
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      readStory iid attrs Stories.grudgingAssistance
      pure t
    _ -> Exposition <$> liftRunMessage msg attrs

{- | An investigation can be aimed at two locations at once (@BothTarget@), and
the modifier has to land on each half.
-}
individualInvestigationTargets :: Target -> [Target]
individualInvestigationTargets = \case
  BothTarget left right -> individualInvestigationTargets left <> individualInvestigationTargets right
  target -> [target]
