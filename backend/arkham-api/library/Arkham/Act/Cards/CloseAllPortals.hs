module Arkham.Act.Cards.CloseAllPortals (closeAllPortals) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Investigator.Types (Field (InvestigatorClues))
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Projection
import Arkham.Scenarios.WarOfTheOuterGods.Helpers
import Arkham.Trait (Trait (RitualSite))

-- Only used in Epic Multiplayer Mode; in Single Group Mode the scenario ends
-- at Close the Portal.
newtype CloseAllPortals = CloseAllPortals ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

closeAllPortals :: ActCard CloseAllPortals
closeAllPortals = act (3, A) CloseAllPortals Cards.closeAllPortals Nothing

instance HasAbilities CloseAllPortals where
  getAbilities (CloseAllPortals a) =
    [ restricted a 1 (youExist $ InvestigatorAt (LocationWithTrait RitualSite) <> InvestigatorWithAnyClues)
        actionAbility
    , mkAbility a 2 $ forced $ PlacedDoomCounter #after AnySource (TargetControlledBy Anyone)
    ]

instance RunMessage CloseAllPortals where
  runMessage msg a@(CloseAllPortals attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      chooseBeginSkillTest sid iid (attrs.ability 1) iid [#willpower, #intellect] (Fixed 3)
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
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      push R1
      pure a
    _ -> CloseAllPortals <$> liftRunMessage msg attrs
