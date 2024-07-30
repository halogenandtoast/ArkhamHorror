module Arkham.Event.Cards.GuidedByFaith (guidedByFaith, GuidedByFaith (..)) where

import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.ChaosBag (getRemainingBlessTokens)
import Arkham.Helpers.SkillTest (getSkillTestSource)
import Arkham.Matcher
import Arkham.Modifier

newtype GuidedByFaith = GuidedByFaith EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

guidedByFaith :: EventCard GuidedByFaith
guidedByFaith = event GuidedByFaith Cards.guidedByFaith

instance RunMessage GuidedByFaith where
  runMessage msg e@(GuidedByFaith attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      sid <- getRandom
      onRevealChaosTokenEffect sid (oneOf [#bless, #eldersign]) attrs attrs do
        skillTestModifier sid attrs iid (DiscoveredClues 1)
      investigateWithSkillChoice sid iid attrs [#combat, #intellect]
      pure e
    BeforeRevealChaosTokens -> do
      n <- getRemainingBlessTokens
      void $ runMaybeT $ do
        source <- MaybeT getSkillTestSource
        guard $ isSource attrs source
        lift $ replicateM_ (min n 2) $ push $ AddChaosToken #bless
      pure e
    _ -> GuidedByFaith <$> liftRunMessage msg attrs
