module Arkham.Treachery.Cards.SearchingForIzzieAdvanced (SearchingForIzzieAdvanced (..), searchingForIzzieAdvanced) where

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Classes
import Arkham.Investigate
import Arkham.Matcher
import Arkham.Modifier
import Arkham.Placement
import Arkham.Prelude
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype SearchingForIzzieAdvanced = SearchingForIzzieAdvanced TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

searchingForIzzieAdvanced :: TreacheryCard SearchingForIzzieAdvanced
searchingForIzzieAdvanced = treachery SearchingForIzzieAdvanced Cards.searchingForIzzieAdvanced

instance HasModifiersFor SearchingForIzzieAdvanced where
  getModifiersFor (SearchingForIzzieAdvanced a) = case a.placement of
    AttachedToLocation lid -> modified_ a lid [ShroudModifier 2]
    _ -> pure mempty

instance HasAbilities SearchingForIzzieAdvanced where
  getAbilities (SearchingForIzzieAdvanced x) =
    investigateAbility x 1 (ActionCost 1) OnSameLocation
      : [mkAbility x 2 $ forcedOnElimination iid | iid <- toList x.owner]

instance RunMessage SearchingForIzzieAdvanced where
  runMessage msg t@(SearchingForIzzieAdvanced attrs) = case msg of
    Revelation iid (isSource attrs -> True) -> do
      targets <- selectTargets $ FarthestLocationFromInvestigator (InvestigatorWithId iid) Anywhere
      player <- getPlayer iid
      pushIfAny targets $ chooseOrRunOne player $ targetLabels targets (only . attachTreachery attrs)
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      withTreacheryLocation attrs $ \locationId -> do
        sid <- getRandom
        pushM $ mkInvestigateLocation sid iid (toAbilitySource attrs 1) locationId <&> setTarget attrs
        pure t
    Successful (Action.Investigate, _) iid _ target _ | isTarget attrs target -> do
      push $ toDiscardBy iid (toAbilitySource attrs 1) target
      pure t
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      let investigator = fromJustNote "missing investigator" attrs.owner
      push $ SufferTrauma investigator 0 1
      pure t
    _ -> SearchingForIzzieAdvanced <$> runMessage msg attrs
