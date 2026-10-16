module Arkham.Homebrew.TheLegendOfSleepyHollow.Treacheries.LoomingSpirit (loomingSpirit) where

import Arkham.Helpers.Location (withLocationOf)
import Arkham.Helpers.Message.Discard.Lifted (chooseAndDiscardCard)
import Arkham.Homebrew.TheLegendOfSleepyHollow.CardDefs.Treacheries qualified as Cards
import Arkham.Homebrew.TheLegendOfSleepyHollow.Helpers
import Arkham.I18n
import Arkham.Investigator.Types (Field (InvestigatorHand))
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Projection
import Arkham.Treachery.Import.Lifted

newtype LoomingSpirit = LoomingSpirit TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

loomingSpirit :: TreacheryCard LoomingSpirit
loomingSpirit = treachery LoomingSpirit Cards.loomingSpirit

{- | "Resolve that haunted ability at 0 skill value" -- there is no test here, so
the ability's own read of how far you failed by answers 0 ('amountFailedBy').
-}
instance RunMessage LoomingSpirit where
  runMessage msg t@(LoomingSpirit attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      withLocationOf iid \lid -> do
        haunted <- select $ HauntedAbility <> AbilityOnLocation (LocationWithId lid)
        if null haunted
          then gainSurge attrs
          else do
            for_ haunted \ab -> do
              hasCards <- fieldMap InvestigatorHand notNull iid
              chooseOrRunOneM iid $ scenarioI18n $ scope "loomingSpirit" do
                abilityLabeled iid ab nothing
                when hasCards $ labeled "discard" $ chooseAndDiscardCard iid attrs
            doStep 1 msg
      pure t
    DoStep 1 (Revelation iid (isSource attrs -> True)) -> do
      emptyHanded <- fieldMap InvestigatorHand ((<= 2) . length) iid
      when emptyHanded $ assignHorror iid attrs 1
      pure t
    _ -> LoomingSpirit <$> liftRunMessage msg attrs
