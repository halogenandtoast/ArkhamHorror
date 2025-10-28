module Arkham.Treachery.Cards.SearchingForIzzieAdvanced (searchingForIzzieAdvanced) where

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Helpers.Modifiers (ModifierType (..), modified_)
import Arkham.Helpers.SkillTest.Lifted (investigateLocationEdit_)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype SearchingForIzzieAdvanced = SearchingForIzzieAdvanced TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

searchingForIzzieAdvanced :: TreacheryCard SearchingForIzzieAdvanced
searchingForIzzieAdvanced = treachery SearchingForIzzieAdvanced Cards.searchingForIzzieAdvanced

instance HasModifiersFor SearchingForIzzieAdvanced where
  getModifiersFor (SearchingForIzzieAdvanced a) = for_ a.attached.location \lid ->
    modified_ a lid [ShroudModifier 2]

instance HasAbilities SearchingForIzzieAdvanced where
  getAbilities (SearchingForIzzieAdvanced x) =
    investigateAbility x 1 (ActionCost 1) OnSameLocation
      : [mkAbility x 2 $ forcedOnElimination iid | iid <- toList x.owner]

instance RunMessage SearchingForIzzieAdvanced where
  runMessage msg t@(SearchingForIzzieAdvanced attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      locations <-
        selectTargets $ FarthestLocationFromInvestigator (InvestigatorWithId iid) LocationCanHaveAttachments
      chooseTargetM iid locations $ attachTreachery attrs
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      for_ attrs.attached.location \locationId -> do
        sid <- getRandom
        investigateLocationEdit_ sid iid (attrs.ability 1) locationId (setTarget attrs)
      pure t
    Successful (Action.Investigate, _) iid _ target _ | isTarget attrs target -> do
      toDiscardBy iid (attrs.ability 1) target
      pure t
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      let investigator = fromJustNote "missing investigator" attrs.owner
      sufferMentalTrauma investigator 1
      pure t
    _ -> SearchingForIzzieAdvanced <$> liftRunMessage msg attrs
