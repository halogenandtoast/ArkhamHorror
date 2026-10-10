module Arkham.Homebrew.AgesUnwound.Treacheries.FinalPreparations (finalPreparations) where

import Arkham.Ability
import Arkham.Act.Types (Field (ActResources))
import Arkham.GameValue
import Arkham.Homebrew.AgesUnwound.CardDefs.Treacheries qualified as Cards
import Arkham.Homebrew.AgesUnwound.Missions.Helpers (completeTaskId)
import Arkham.Matcher
import Arkham.Projection
import Arkham.Treachery.Import.Lifted

newtype FinalPreparations = FinalPreparations TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

{- | Spring's back puts this into play next to the act deck. It prints no
__Revelation__, so there is nothing to resolve when it arrives.
-}
finalPreparations :: TreacheryCard FinalPreparations
finalPreparations = treachery FinalPreparations Cards.finalPreparations

{- | "[free] The investigators spend 2[per_investigator] clues, as a group: Place
1 resource on the current act."
-}
instance HasAbilities FinalPreparations where
  getAbilities (FinalPreparations a) =
    [ restricted a 1 (ActExists AnyAct)
        $ FastAbility (GroupClueCost (PerPlayer 2) Anywhere)
    ]

{- | "__Task__ - Place as many resources on the current act as you can. If there
are 3 resources on the current act, complete Final Preparations."

The objective is read off the act after the resource lands, which is why the
count is a second step rather than a read inside the same handler. Scenario V
has a single act deck, so @AnyAct@ names it unambiguously.
-}
instance RunMessage FinalPreparations where
  runMessage msg t@(FinalPreparations attrs) = runQueueT $ case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      selectForMaybeM AnyAct \act -> placeTokens (attrs.ability 1) act #resource 1
      doStep 1 msg
      pure t
    DoStep 1 (UseThisAbility _ (isSource attrs -> True) 1) -> do
      resources <- selectMaybeM 0 AnyAct (field ActResources)
      when (resources >= 3) $ completeTaskId attrs
      pure t
    _ -> FinalPreparations <$> liftRunMessage msg attrs
