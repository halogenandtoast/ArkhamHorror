module Arkham.Act.Cards.CloseThePortal (closeThePortal) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Investigator.Types (Field (InvestigatorClues))
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Projection
import Arkham.ScenarioLogKey
import Arkham.Scenarios.WarOfTheOuterGods.Helpers
import Arkham.Trait (Trait (RitualSite))

newtype CloseThePortal = CloseThePortal ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

closeThePortal :: ActCard CloseThePortal
closeThePortal = act (2, A) CloseThePortal Cards.closeThePortal Nothing

instance HasAbilities CloseThePortal where
  getAbilities (CloseThePortal a) =
    [ restricted
        a
        1
        (youExist $ InvestigatorAt (LocationWithTrait RitualSite) <> InvestigatorWithAnyClues)
        actionAbility
    , mkAbility a 2 $ forced $ PlacedDoomCounter #after AnySource (TargetControlledBy Anyone)
    , onlyOnce
        $ restricted a 3 (HasScenarioCount CluesAroundHubDimension $ GreaterThanOrEqualTo $ PerPlayer 6)
        $ Objective
        $ forced AnyWindow
    ]

instance RunMessage CloseThePortal where
  runMessage msg a@(CloseThePortal attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      spendClues iid 1
      placeCluesAroundHubDimension 1
      sid <- getRandom
      scenarioI18n $ chooseOneM iid do
        labeled' "skipTest" nothing
        skillLabeled #willpower $ beginSkillTest sid iid (attrs.ability 1) iid #willpower (Fixed 2)
        skillLabeled #intellect $ beginSkillTest sid iid (attrs.ability 1) iid #intellect (Fixed 2)
      pure a
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      whenM (fieldMap InvestigatorClues (> 0) iid) do
        spendClues iid 1
        placeCluesAroundHubDimension 1
      pure a
    UseCardAbility _ (isSource attrs -> True) 2 (getPlacedDoomAmount -> n) _ -> do
      lead <- getLead
      chooseSelectM lead AnyAgenda \agenda -> placeDoom (attrs.ability 2) agenda n
      pure a
    UseThisAbility _iid (isSource attrs -> True) 3 -> do
      advancedWithOther attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      push R1
      pure a
    _ -> CloseThePortal <$> liftRunMessage msg attrs
