module Arkham.Act.Cards.CasingTheJoint (casingTheJoint) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Campaigns.TheScarletKeys.Key.Cards qualified as Keys
import Arkham.Campaigns.TheScarletKeys.Key.Matcher
import Arkham.Helpers.GameValue (perPlayer)
import Arkham.Investigator.Types (Field (InvestigatorResources))
import Arkham.Matcher
import Arkham.Message.Lifted.Log
import Arkham.Modifier
import Arkham.ScenarioLogKey
import Arkham.Trait (Trait (Game))

newtype CasingTheJoint = CasingTheJoint ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

casingTheJoint :: ActCard CasingTheJoint
casingTheJoint = act (1, A) CasingTheJoint Cards.casingTheJoint Nothing

instance HasAbilities CasingTheJoint where
  getAbilities = actAbilities \a ->
    [ mkAbility a 1
        $ freeReaction
        $ ActivateAbility #after You
        $ AbilityOnLocation (LocationWithTrait Game)
        <> AbilityIsActionAbility
    , restricted a 2 AllUndefeatedInvestigatorsResigned $ Objective $ forced AnyWindow
    ]

instance RunMessage CasingTheJoint where
  runMessage msg a@(CasingTheJoint attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      selectEach (scarletKeyIs Keys.theWellspringOfFortune) \wellspring -> do
        gotResources <- matches iid (InvestigatorWithModifier (ScenarioModifier "gotResources"))
        removeTokens (attrs.ability 1) wellspring #clue $ if gotResources then 2 else 1
      pure a
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      advancedWithOther attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      resources <- selectSum InvestigatorResources Anyone
      n <- perPlayer 10
      when (resources >= n) $ remember CleanedOutTheHouse
      push R2
      pure a
    _ -> CasingTheJoint <$> liftRunMessage msg attrs
