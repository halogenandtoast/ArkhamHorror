module Arkham.Homebrew.AgesUnwound.Treacheries.TemporalStutter (temporalStutter) where

import Arkham.Ability
import Arkham.Helpers.SkillTest (withSkillTest)
import Arkham.Homebrew.AgesUnwound.CardDefs.Treacheries qualified as Cards
import Arkham.Matcher
import Arkham.Message.Lifted.Move (moveTowardsMatching)
import Arkham.Treachery.Import.Lifted

newtype TemporalStutter = TemporalStutter TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

temporalStutter :: TreacheryCard TemporalStutter
temporalStutter = treachery TemporalStutter Cards.temporalStutter

{- | "Forced - After you fail a skill test: After this test ends, place 1 of your
clues on your location, then move once towards Front Gates. /
Forced - At the end of the round: Discard Temporal Stutter."
-}
instance HasAbilities TemporalStutter where
  getAbilities (TemporalStutter a) =
    [ restricted a 1 (InThreatAreaOf You) $ forced $ SkillTestResult #after You AnySkillTest #failure
    , forcedAbility a 2 $ RoundEnds #when
    ]

instance RunMessage TemporalStutter where
  runMessage msg t@(TemporalStutter attrs) = runQueueT $ case msg of
    -- "Revelation - Add Temporal Stutter to your threat area."
    Revelation iid (isSource attrs -> True) -> do
      placeInThreatArea attrs iid
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      withSkillTest \sid -> afterThisTestResolves sid do
        placeCluesOnLocation iid (attrs.ability 1) 1
        {- Matched by title: Scenario VI gathers a Front Gates of its own
        (@:ages-unwound:...@ in @a_world_torn_down_again@), and this scenario's
        other entry costs are title-matched for the same reason. -}
        moveTowardsMatching (attrs.ability 1) iid "Front Gates"
      pure t
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      toDiscard (attrs.ability 2) attrs
      pure t
    _ -> TemporalStutter <$> liftRunMessage msg attrs
