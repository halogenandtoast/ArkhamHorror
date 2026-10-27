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

myriadOperations :: LocationMatcher
myriadOperations =
  mapOneOf locationIs [Locations.mexicoCity, Locations.istanbul, Locations.shanghai]
    <> LocationWithToken #resource

instance HasAbilities Exposition where
  getAbilities (Exposition a) =
    [ mkAbility a 1
        $ freeReaction
        $ SkillTestResult #when You (WhileInvestigating myriadOperations) #success
    , restricted a 2 (if treacheryResources a >= 3 then OnSameLocation else Never) actionAbility
    ]

instance RunMessage Exposition where
  runMessage msg t@(Exposition attrs) = runQueueT $ case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      whenJustM getSkillTestTarget \target ->
        withSkillTest \sid ->
          for_ (individualInvestigationTargets target) \target' ->
            skillTestModifier sid (attrs.ability 1) target'
              $ AlternateSuccessfullInvestigation (toTarget attrs)
      pure t
    Successful (Action.Investigate, _) _iid _ (isTarget attrs -> True) _ -> do
      whenJustM getSkillTestTargetedLocation \lid ->
        moveTokens (attrs.ability 1) lid attrs #resource 1
      pure t
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      readStory iid attrs Stories.grudgingAssistance
      pure t
    _ -> Exposition <$> liftRunMessage msg attrs

individualInvestigationTargets :: Target -> [Target]
individualInvestigationTargets = \case
  BothTarget left right -> individualInvestigationTargets left <> individualInvestigationTargets right
  target -> [target]
