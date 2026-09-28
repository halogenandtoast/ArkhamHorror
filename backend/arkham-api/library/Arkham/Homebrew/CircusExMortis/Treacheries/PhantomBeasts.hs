module Arkham.Homebrew.CircusExMortis.Treacheries.PhantomBeasts (phantomBeasts) where

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Helpers.Location (withLocationOf)
import Arkham.Helpers.Modifiers (ModifierType (AlternateSuccessfullInvestigation))
import Arkham.Helpers.SkillTest (withSkillTest)
import Arkham.Homebrew.CircusExMortis.CardDefs.Treacheries qualified as Cards
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Treachery.Import.Lifted

newtype PhantomBeasts = PhantomBeasts TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

phantomBeasts :: TreacheryCard PhantomBeasts
phantomBeasts = treachery PhantomBeasts Cards.phantomBeasts

instance HasAbilities PhantomBeasts where
  getAbilities (PhantomBeasts a) =
    [mkAbility a 1 $ forced $ RoundEnds #when]
      <> [ mkAbility a 2 $ freeReaction $ SuccessfulInvestigation #when You (be lid)
         | lid <- toList a.attached.location
         ]

instance RunMessage PhantomBeasts where
  runMessage msg t@(PhantomBeasts attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      withLocationOf iid $ attachTreachery attrs
      pure t
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      for_ attrs.attached.location \lid ->
        selectEach (investigatorAt lid) \iid -> do
          sid <- getRandom
          chooseBeginSkillTest sid iid (attrs.ability 1) iid [#willpower, #agility] (Fixed 4)
      pure t
    FailedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      chooseAndDiscardAsset iid (attrs.ability 1)
      pure t
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      -- the discovery is replaced, so redirect it here and discard instead
      for_ attrs.attached.location \lid -> withSkillTest \sid ->
        skillTestModifier sid (attrs.ability 2) lid (AlternateSuccessfullInvestigation $ toTarget attrs)
      pure t
    Successful (Action.Investigate, _) iid _ (isTarget attrs -> True) _ -> do
      toDiscardBy iid (attrs.ability 2) attrs
      pure t
    _ -> PhantomBeasts <$> liftRunMessage msg attrs
