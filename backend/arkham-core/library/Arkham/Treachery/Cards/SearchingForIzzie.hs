module Arkham.Treachery.Cards.SearchingForIzzie (
  SearchingForIzzie (..),
  searchingForIzzie,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Classes
import Arkham.Investigate
import Arkham.Matcher
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

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
  runMessage msg t@(SearchingForIzzie attrs) = case msg of
    Revelation iid (isSource attrs -> True) -> do
      targets <- selectTargets $ FarthestLocationFromYou Anywhere
      player <- getPlayer iid
      pushIfAny targets
        $ chooseOrRunOne player
        $ targetLabels targets (only . AttachTreachery (toId attrs))
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      withTreacheryLocation attrs $ \locationId -> do
        pushM $ mkInvestigateLocation iid (toAbilitySource attrs 1) locationId <&> setTarget attrs
        pure t
    Successful (Action.Investigate, _) _ _ target _ | isTarget attrs target -> do
      push $ Discard (toAbilitySource attrs 1) target
      pure t
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      let investigator = fromJustNote "missing investigator" attrs.owner
      push $ SufferTrauma investigator 0 1
      pure t
    _ -> SearchingForIzzie <$> runMessage msg attrs
