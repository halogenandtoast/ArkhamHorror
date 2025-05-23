module Arkham.Treachery.Cards.SearchingForIzzie (searchingForIzzie) where

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Helpers.Location
import Arkham.Helpers.SkillTest.Lifted (investigateLocationEdit_)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype SearchingForIzzie = SearchingForIzzie TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

searchingForIzzie :: TreacheryCard SearchingForIzzie
searchingForIzzie = treachery SearchingForIzzie Cards.searchingForIzzie

instance HasAbilities SearchingForIzzie where
  getAbilities (SearchingForIzzie x) =
    investigateAbility x 1 (ActionCost 1) OnSameLocation
      : [mkAbility x 2 $ forcedOnElimination iid | iid <- toList x.owner]

instance RunMessage SearchingForIzzie where
  runMessage msg t@(SearchingForIzzie attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      locations <-
        selectTargets $ FarthestLocationFromInvestigator (InvestigatorWithId iid) LocationCanHaveAttachments
      chooseOrRunOneM iid $ targets locations (attachTreachery attrs)
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      withLocationOf attrs \loc -> do
        sid <- getRandom
        investigateLocationEdit_ sid iid (attrs.ability 1) loc (setTarget attrs)
      pure t
    Successful (Action.Investigate, _) iid _ (isTarget attrs -> True) _ -> do
      toDiscardBy iid (attrs.ability 1) attrs
      pure t
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      let investigator = fromJustNote "missing investigator" attrs.owner
      sufferMentalTrauma investigator 1
      pure t
    _ -> SearchingForIzzie <$> liftRunMessage msg attrs
