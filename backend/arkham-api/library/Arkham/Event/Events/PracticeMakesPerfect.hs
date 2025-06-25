module Arkham.Event.Events.PracticeMakesPerfect where

import Arkham.Event.Cards qualified as Cards
import Arkham.Asset.Cards qualified as Assets
import Arkham.Event.Import.Lifted
import Arkham.Helpers.Modifiers (
  ModifierType (..),
  getAdditionalSearchTargets,
  ignoreCommitOneRestriction,
 )
import Arkham.Helpers.SkillTest (getIsCommittable, withSkillTest)
import Arkham.Matcher
import Arkham.Strategy
import Arkham.Trait (Trait (Practiced))

newtype PracticeMakesPerfect = PracticeMakesPerfect EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

practiceMakesPerfect :: EventCard PracticeMakesPerfect
practiceMakesPerfect = event PracticeMakesPerfect Cards.practiceMakesPerfect

instance RunMessage PracticeMakesPerfect where
  runMessage msg e@(PracticeMakesPerfect attrs) = runQueueT $ case msg of
    PlayThisEvent iid eid | eid == toId attrs -> do
      search iid attrs iid [fromTopOfDeck 9] (basic $ #skill <> withTrait Practiced)
        $ defer attrs IsNotDraw
      pure e
    SearchFound iid (isTarget attrs -> True) _ cards | notNull cards -> do
      hasKingInYellow <- selectAny $ assetControlledBy iid <> assetIs Assets.theKingInYellow
      additionalTargets <- getAdditionalSearchTargets iid
      committable <- if hasKingInYellow
        then pure []
        else ignoreCommitOneRestriction iid $ filterM (getIsCommittable iid) cards
      withSkillTest \sid -> do
        chooseNM iid (1 + additionalTargets) do
          when (null committable) $ labeled "No cards found" nothing
          targets committable \card -> do
            skillTestModifiers sid attrs card [IfSuccessfulModifier ReturnToHandAfterTest, MustBeCommitted]
            push $ SkillTestCommitCard iid card
      pure e
    SearchFound iid (isTarget attrs -> True) _ cards | null cards -> do
      continue_ iid
      pure e
    _ -> PracticeMakesPerfect <$> liftRunMessage msg attrs
