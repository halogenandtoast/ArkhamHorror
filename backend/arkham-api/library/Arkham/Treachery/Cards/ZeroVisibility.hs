module Arkham.Treachery.Cards.ZeroVisibility (zeroVisibility, ZeroVisibility (..)) where

import Arkham.Ability
import Arkham.Helpers.Investigator (getMaybeLocation)
import Arkham.Helpers.Modifiers (ModifierType (..), maybeModified)
import Arkham.Matcher
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype ZeroVisibility = ZeroVisibility TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

zeroVisibility :: TreacheryCard ZeroVisibility
zeroVisibility = treachery ZeroVisibility Cards.zeroVisibility

instance HasModifiersFor ZeroVisibility where
  getModifiersFor (InvestigatorTarget iid) (ZeroVisibility a) = maybeModified a do
    guard $ treacheryInThreatArea iid a
    lid <- MaybeT $ getMaybeLocation iid
    liftGuardM $ lid <=~> LocationWithTreachery AnyTreachery
    pure [AdditionalCostToEnterMatching Anywhere $ ActionCost 1]
  getModifiersFor _ _ = pure []

instance HasAbilities ZeroVisibility where
  getAbilities (ZeroVisibility a) =
    [skillTestAbility $ restricted a 1 (InThreatAreaOf You) $ forced $ TurnEnds #when You]

instance RunMessage ZeroVisibility where
  runMessage msg t@(ZeroVisibility attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      placeInThreatArea attrs iid
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- genId
      beginSkillTest sid iid (attrs.ability 1) iid #agility (Fixed 2)
      pure t
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      toDiscardBy iid (attrs.ability 1) attrs
      pure t
    _ -> ZeroVisibility <$> liftRunMessage msg attrs
