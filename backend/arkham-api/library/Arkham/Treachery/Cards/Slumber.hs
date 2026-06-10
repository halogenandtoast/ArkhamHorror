module Arkham.Treachery.Cards.Slumber (slumber) where

import Arkham.Ability
import Arkham.Matcher
import Arkham.ScenarioLogKey
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype Slumber = Slumber TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

slumber :: TreacheryCard Slumber
slumber = treachery Slumber Cards.slumber

instance HasAbilities Slumber where
  getAbilities (Slumber a) =
    [ restricted a 1 (InThreatAreaOf You) $ forced $ TurnBegins #when You
    , restricted a 2 (InThreatAreaOf You)
        $ freeReaction
        $ ScenarioCountDecremented #after StrengthOfTheAbyss
    ]

instance RunMessage Slumber where
  runMessage msg t@(Slumber attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      hasCopy <- selectAny $ treacheryIs Cards.slumber <> TreacheryInThreatAreaOf (InvestigatorWithId iid)
      if hasCopy
        then do
          gainSurge attrs
          toDiscard attrs attrs
        else placeInThreatArea attrs iid
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 1) iid #willpower (Fixed 2)
      pure t
    FailedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      loseActions iid (attrs.ability 1) 1
      pure t
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      toDiscardBy iid (attrs.ability 2) attrs
      pure t
    _ -> Slumber <$> liftRunMessage msg attrs
